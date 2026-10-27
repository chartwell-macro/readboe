boe_mpr_source_record <- function(entry, previous = NULL) {
  if (identical(entry$status, 304L)) {
    if (is.null(previous$sha256)) boe_abort("A 304 response has no known revision.", "source_revision")
    previous$checked_at <- Sys.time()
    if (!is.na(entry$etag)) previous$etag <- entry$etag
    if (!is.na(entry$last_modified)) previous$last_modified <- entry$last_modified
    return(previous)
  }
  list(url = entry$url, sha256 = entry$raw_sha256, bytes = length(entry$body),
    etag = entry$etag, last_modified = entry$last_modified, checked_at = Sys.time(),
    retrieved_at = entry$retrieved_at, final_url = entry$final_url, status = entry$status)
}

boe_mpr_probe <- function(url, previous, deadline, raw_dir, force = FALSE,
    max_bytes = 40 * 1024^2, max_cache_bytes = 2 * 1024^3) {
  headers <- list()
  if (!force && !is.null(previous)) {
    if (is.character(previous$etag) && !is.na(previous$etag) && nzchar(previous$etag)) headers[["If-None-Match"]] <- previous$etag
    else if (is.character(previous$last_modified) && !is.na(previous$last_modified) && nzchar(previous$last_modified)) headers[["If-Modified-Since"]] <- previous$last_modified
  }
  entry <- boe_request_raw(url, deadline = deadline, max_bytes = max_bytes, cache = FALSE,
    refresh = TRUE, request_headers = headers, allow_not_modified = length(headers) > 0L,
    validate_response = boe_validate_not_error_html)
  source <- boe_mpr_source_record(entry, previous)
  if (entry$status != 304L && !is.null(raw_dir)) {
    key <- boe_cache_key(list(method = "GET", url = url, accept = "*/*", contract = 1L))
    boe_cache_write(raw_dir, key, entry, 1000L, max_cache_bytes)
  }
  list(source = source, entry = if (entry$status == 304L) NULL else entry,
    changed = is.null(previous$sha256) || !identical(previous$sha256, source$sha256))
}

boe_mpr_sources_from_records <- function(records) {
  sources <- list()
  for (record in records) for (receipt in c(list(boe_provenance(record$assets)$receipt), record$receipts)) {
    url <- boe_mpr_receipt_url(receipt)
    if (is.null(url)) next
    sources[[url]] <- list(url = url, sha256 = receipt$raw_sha256, bytes = receipt$raw_bytes,
      etag = receipt$etag, last_modified = receipt$last_modified, retrieved_at = receipt$retrieved_at,
      checked_at = receipt$retrieved_at, final_url = receipt$final_url)
  }
  sources
}

boe_mpr_page_signature <- function(rel, assets) {
  fields <- intersect(c("release", "release_date", "release_date_precision", "source_published_date", "publication_status", "landing_url"), names(rel))
  r <- rel[fields]; attr(r, "boe_provenance") <- NULL; rownames(r) <- NULL
  a <- assets[order(assets$url), intersect(c("url", "label", "file_type", "asset_role"), names(assets)), drop = FALSE]
  attr(a, "boe_provenance") <- NULL; rownames(a) <- NULL
  digest::digest(list(r, a), algo = "sha256")
}

boe_mpr_release_signature <- function(releases) {
  fields <- c("release", "release_date", "release_date_precision", "source_published_date",
    "publication_status", "landing_url")
  value <- releases[fields]; attr(value, "boe_provenance") <- NULL; rownames(value) <- NULL
  digest::digest(value, algo = "sha256")
}

boe_mpr_publication_pending <- function(entry) {
  doc <- boe_mpr_html(entry$body)
  heading <- paste(xml2::xml_text(xml2::xml_find_all(doc, "//main//h1|//main//h2|//h1")), collapse = " ")
  grepl("to be published|will be published", heading, ignore.case = TRUE)
}

boe_mpr_lock <- function(data_dir, stale_after = 60, name = ".update-lock") {
  if (!name %in% c(".update-lock", ".maintenance-lock")) boe_abort("Unknown forecast writer lock.", "config")
  path <- file.path(data_dir, name); dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.create(path, showWarnings = FALSE)) {
    owner <- boe_mpr_optional_rds(file.path(path, "owner.rds"))
    age <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "secs"))
    dead <- !is.null(owner) && identical(owner$host, Sys.info()[["nodename"]]) &&
      !isTRUE(tryCatch(tools::pskill(owner$pid, signal = 0L), error = function(e) TRUE))
    if (is.finite(age) && age > stale_after && (dead || is.null(owner))) {
      quarantine <- tempfile(".stale-lock-", tmpdir = data_dir)
      if (file.rename(path, quarantine)) unlink(quarantine, recursive = TRUE)
    }
    if (!dir.create(path, showWarnings = FALSE)) return(NULL)
  }
  token <- boe_cache_key(list(Sys.time(), Sys.getpid(), tempfile()))
  saveRDS(list(pid = Sys.getpid(), host = Sys.info()[["nodename"]], token = token, started_at = Sys.time()), file.path(path, "owner.rds"))
  list(path = path, token = token)
}

boe_mpr_unlock <- function(lock) {
  owner <- tryCatch(readRDS(file.path(lock$path, "owner.rds")), error = function(e) NULL)
  if (identical(owner$token, lock$token)) unlink(lock$path, recursive = TRUE)
  invisible(NULL)
}

boe_mpr_prune_sources <- function(raw_dir, max_bytes = 2 * 1024^3, max_age_days = 7) {
  files <- list.files(raw_dir, pattern = "^[a-f0-9]{64}[.]rds$", full.names = TRUE)
  if (length(files)) {
    age <- as.numeric(difftime(Sys.time(), file.info(files)$mtime, units = "days"))
    unlink(files[!is.na(age) & age > max_age_days])
  }
  boe_cache_prune(raw_dir, 1000L, max_bytes)
  invisible(NULL)
}

boe_mpr_clear_sources <- function(raw_dir, retain_raw = FALSE) {
  # Only this package's SHA-named HTTP cache entries are disposable. Metadata
  # and numeric generations live outside raw_dir; unrelated files are preserved.
  # An interrupted atomic cache write may leave a bounded temporary body too.
  pattern <- if (retain_raw) "^readboe-cache-[a-f0-9]+$" else "^([a-f0-9]{64}[.]rds|readboe-cache-[a-f0-9]+)$"
  files <- list.files(raw_dir, pattern = pattern, full.names = TRUE)
  if (length(files)) unlink(files)
  if (length(list.files(raw_dir, pattern = pattern))) {
    boe_abort("Could not remove disposable forecast source bodies.", "source_cleanup")
  }
  invisible(NULL)
}

boe_mpr_prune_generations <- function(data_dir) {
  pointer <- boe_mpr_optional_rds(file.path(data_dir, "CURRENT.rds"))
  if (is.null(pointer) || !identical(pointer$schema, 1L)) return(invisible(NULL))
  keep <- c(pointer$current$file, pointer$previous$file)
  files <- list.files(data_dir, pattern = "^mpr-[a-f0-9]{64}[.]rds$", full.names = TRUE)
  unused <- files[!basename(files) %in% keep]
  if (length(unused) > 2L) {
    unused <- unused[order(file.info(unused)$mtime, decreasing = TRUE)]
    unlink(unused[-seq_len(2L)])
  }
  # A process killed before on.exit may leave an uncommitted temporary file.
  staged <- list.files(data_dir, pattern = "^[.](candidate-|mpr-)", full.names = TRUE)
  if (length(staged)) unlink(staged[difftime(Sys.time(), file.info(staged)$mtime, units = "days") > 1])
  invisible(NULL)
}

boe_mpr_unpack_record <- function(snapshot, key) {
  record <- snapshot$editions[[key]]
  if (record$status == "parsed") record$data <- boe_mpr_decode(snapshot$observations[seq.int(record$first_row, length.out = record$rows), , drop = FALSE])
  record
}

boe_mpr_check_preservation <- function(old, candidate, allow_coverage_loss = FALSE) {
  old_keys <- names(old$editions); new_keys <- names(candidate$editions)
  if (!all(old_keys %in% new_keys)) boe_abort("An update omitted an existing published edition.", "coverage")
  if (!allow_coverage_loss && !all(boe_mpr_keys(old$observations) %in% boe_mpr_keys(candidate$observations))) {
    boe_abort("An update lost existing forecast identities/targets; the last good snapshot was retained.", "coverage")
  }
  invisible(TRUE)
}

boe_mpr_build_record <- function(key, releases, inventory, fetch, raw_dir, deadline) {
  options <- boe_mpr_options(deadline, 40 * 1024^2, 200L, 200 * 1024^2,
    150L, 10000L, 512L, 2e6, 1e6, TRUE, TRUE, FALSE, raw_dir)
  options$releases <- releases; options$asset_inventory <- inventory; options$asset_fetch <- fetch
  full <- tryCatch(boe_mpr_read_options(key, list(), TRUE, TRUE, TRUE, NULL, options), error = identity)
  default <- if (inherits(full, "error")) full else tryCatch(boe_mpr_read_options(key, list(), TRUE, FALSE, TRUE, NULL, options), error = identity)
  original <- tryCatch(boe_mpr_read_options(key, list(), TRUE, TRUE, FALSE, NULL, options), error = identity)
  original_default <- if (inherits(original, "readboe_no_numeric_forecast")) original else tryCatch(boe_mpr_read_options(key, list(), TRUE, FALSE, FALSE, NULL, options), error = identity)
  boe_mpr_record(full, default, inventory[[key]], original, original_default)
}

boe_mpr_update_plan <- function(old, releases, inventory, sources, changed, parser_changed) {
  published <- releases$release[releases$publication_status == "published"]
  if (!all(names(old$editions) %in% published)) boe_abort("Archive discovery lost a published edition.", "coverage")
  new <- setdiff(published, names(old$editions)); affected <- new
  if (parser_changed) return(published)
  for (key in names(old$editions)) {
    e <- old$editions[[key]]; rel <- releases[releases$release == key, , drop = FALSE]
    prior_rel <- old$releases[old$releases$release == key, , drop = FALSE]
    dependencies <- vapply(e$receipts, boe_mpr_receipt_url, character(1L))
    if (any(dependencies %in% changed) || !identical(boe_mpr_page_signature(prior_rel, e$assets), boe_mpr_page_signature(rel, inventory[[key]]))) affected <- c(affected, key)
  }
  # Added/revised multi-vintage sources, including a new link on an existing
  # report, can enrich prior historical searches and previously proved absences.
  numeric_urls <- unique(boe_mpr_bind(c(inventory, lapply(old$editions, `[[`, "assets"))))
  numeric_urls <- numeric_urls$url[numeric_urls$asset_role == "data_bundle"]
  new_numeric <- length(new) && any(vapply(inventory[new], function(x) any(x$asset_role == "data_bundle"), logical(1L)))
  if (new_numeric || any(changed %in% numeric_urls)) {
    historical <- names(old$editions)[vapply(old$editions, function(x) isTRUE(x$historical_search), logical(1L))]
    affected <- c(affected, historical)
  }
  published[published %in% unique(affected)]
}

boe_mpr_publish <- function(candidate, loaded, data_dir, status, sources, inventory) {
  boe_mpr_validate_snapshot(candidate)
  tmp <- tempfile(".candidate-", tmpdir = data_dir); on.exit(unlink(tmp), add = TRUE)
  saveRDS(candidate, tmp, version = 3, compress = "gzip")
  hash <- boe_sha256_file(tmp); name <- paste0("mpr-", hash, ".rds"); path <- file.path(data_dir, name)
  if (!file.exists(path) && !file.rename(tmp, path)) boe_abort("Cannot stage the forecast generation.", "snapshot_write")
  # Authenticate the actual staged bytes, not just the in-memory candidate.
  boe_mpr_read_generation(path, hash)
  status$snapshot_sha256 <- hash
  status$last_successful_check <- status$completed_at <- Sys.time()
  boe_mpr_atomic_rds(list(snapshot_sha256 = hash, sources = sources, inventory = inventory), file.path(data_dir, "SOURCES.rds"))
  prior <- if (loaded$origin == "refreshed") list(file = basename(loaded$path), sha256 = loaded$sha256, status = loaded$status) else NULL
  pointer <- list(schema = 1L, current = list(file = name, sha256 = hash, status = status), previous = prior)
  # Data identity and successful status share this sole commit point. STATUS is
  # reserved for no-op checks and failed attempts, filtered by generation on read.
  # An interrupt/crash before this rename cannot publish a new successful check.
  boe_mpr_atomic_rds(pointer, file.path(data_dir, "CURRENT.rds"))
  status
}

boe_mpr_failed_attempt <- function(e, loaded, data_dir, started, interrupted = FALSE) {
  current <- tryCatch(boe_mpr_load_saved(data_dir), error = function(e) loaded)
  status <- list(status = if (interrupted) "interrupted" else "failed",
    attempted_at = started, completed_at = Sys.time(),
    last_successful_check = boe_mpr_saved_provenance(current)$last_successful_check,
    snapshot_sha256 = current$sha256, error_class = class(e)[1L], error = conditionMessage(e))
  boe_mpr_atomic_rds(status, file.path(data_dir, "STATUS.rds"))
  status
}

#' Check for new/revised forecast reports and atomically update local saved data
#'
#' This explicit maintenance operation is separate from package startup and
#' ordinary selection. It checks archive membership, report links and every
#' relevant asset revision, not only the latest report date. Successful snapshots
#' feed ordinary reads from the same data_dir; the installed baseline is read-only.
#' @param data_dir Configurable refreshed-data directory.
#' @param force Bypass daily throttling and send unconditional revision checks.
#' @param rebuild Reparse the complete discovered corpus.
#' @param deadline Absolute deadline or total seconds for all update phases.
#' @param raw_max_bytes,max_raw_age_days Working source-cache byte bound and, when retained, maximum age in days.
#' @param retain_raw Keep downloaded source bodies after the update (default FALSE).
#'   By default the bounded working cache is cleared under the writer lock on
#'   success, failure or a graceful interrupt. Parsed data and validator metadata
#'   remain. Hard-kill leftovers are cleared by the next recovered writer.
#' @param stop_on_error Raise an update_failed condition after persisting failed status.
#' @param allow_coverage_loss Explicitly accept removed historic identities after review.
#' @return Update status, affected editions, checked source count and snapshot identity.
#' @export
boe_mpr_update <- function(data_dir = boe_mpr_data_dir(), force = FALSE, rebuild = FALSE,
    deadline = 3600, raw_max_bytes = 2 * 1024^3, max_raw_age_days = 7,
    stop_on_error = TRUE, allow_coverage_loss = FALSE, retain_raw = FALSE) {
  boe_mpr_update_run(data_dir, force, rebuild, deadline, raw_max_bytes,
    max_raw_age_days, stop_on_error, allow_coverage_loss, retain_raw = retain_raw)
}

# Maintenance can bypass the daily attempt throttle after a release signal while
# retaining conditional HTTP probes. Explicit force still means unconditional GET.
boe_mpr_update_run <- function(data_dir, force = FALSE, rebuild = FALSE,
    deadline = 3600, raw_max_bytes = 2 * 1024^3, max_raw_age_days = 7,
    stop_on_error = TRUE, allow_coverage_loss = FALSE, bypass_daily = FALSE, retain_raw = FALSE) {
  for (name in c("force", "rebuild", "stop_on_error", "allow_coverage_loss", "retain_raw")) boe_assert_scalar(get(name), name, "logical")
  boe_check_positive(raw_max_bytes, "raw_max_bytes", integer = TRUE); boe_check_positive(max_raw_age_days, "max_raw_age_days")
  deadline <- boe_as_deadline(deadline); boe_time_left(deadline)
  loaded <- boe_mpr_load_saved(data_dir); started <- Sys.time()
  lock <- boe_mpr_lock(data_dir)
  if (is.null(lock)) return(list(status = "busy", snapshot_sha256 = loaded$sha256, message = "Another updater owns this directory."))
  raw_dir <- file.path(data_dir, "raw")
  on.exit(tryCatch({
    if (dir.exists(raw_dir)) {
      boe_mpr_clear_sources(raw_dir, retain_raw)
      if (retain_raw) boe_mpr_prune_sources(raw_dir, raw_max_bytes, max_raw_age_days)
    }
    boe_mpr_prune_generations(data_dir)
  }, finally = boe_mpr_unlock(lock)), add = TRUE)
  # A killed process can leave bounded working bodies. Clear them only after
  # acquiring/recovering its writer lock, including on a throttled retry.
  if (dir.exists(raw_dir)) boe_mpr_clear_sources(raw_dir, retain_raw)
  # Reload after acquiring the lock: another writer may have committed meanwhile.
  loaded <- boe_mpr_load_saved(data_dir); prior <- loaded$status
  parser_changed <- !identical(loaded$snapshot$parser_fingerprint, boe_mpr_parser_fingerprint())
  if (!force && !rebuild && !bypass_daily && !parser_changed && !is.null(prior$attempted_at) && as.Date(prior$attempted_at, tz = "UTC") == as.Date(started, tz = "UTC")) {
    return(c(list(status = if (prior$status %in% c("failed", "interrupted")) "failed_throttled" else "throttled"), prior[setdiff(names(prior), "status")]))
  }
  dir.create(raw_dir, showWarnings = FALSE)
  result <- tryCatch({
    state <- boe_mpr_optional_rds(file.path(data_dir, "SOURCES.rds"))
    sources <- if (identical(state$snapshot_sha256, loaded$sha256)) state$sources else loaded$snapshot$sources
    previous_inventory <- if (identical(state$snapshot_sha256, loaded$sha256)) state$inventory else NULL
    old <- loaded$snapshot
    releases <- boe_mpr_discover_live(include_upcoming = TRUE, deadline = deadline,
      cache_dir = raw_dir, refresh = TRUE)
    inventory <- list(); changed <- character(); checked <- 0L
    for (i in seq_len(nrow(releases))) {
      rel <- releases[i, , drop = FALSE]; key <- rel$release; url <- rel$landing_url
      probe <- boe_mpr_probe(url, sources[[url]], deadline, raw_dir, force, 20 * 1024^2, raw_max_bytes)
      sources[[url]] <- probe$source; checked <- checked + 1L
      if (is.null(probe$entry)) {
        previous <- old$editions[[key]]$assets %||% previous_inventory[[key]]
        # Upcoming pages may not have a stored numeric outcome; require a body
        # when their link inventory is not already known.
        needs_publication_status <- rel$publication_status == "published" &&
          !key %in% names(old$editions) && is.null(probe$source$publication_pending)
        if (is.null(previous) || needs_publication_status) {
          probe <- boe_mpr_probe(url, NULL, deadline, raw_dir, TRUE, 20 * 1024^2, raw_max_bytes)
          sources[[url]] <- probe$source; checked <- checked + 1L
        }
      }
      pending <- if (!is.null(probe$entry)) boe_mpr_publication_pending(probe$entry) else probe$source$publication_pending
      sources[[url]]$publication_pending <- pending
      if (isTRUE(pending)) {
        if (key %in% names(old$editions)) boe_abort("A previously published report is now advertised as unpublished.", "coverage", release = key)
        releases$publication_status[[i]] <- "scheduled"; rel$publication_status <- "scheduled"
      }
      inventory[[key]] <- if (!is.null(probe$entry)) boe_mpr_assets_from_entry(rel, probe$entry) else previous
    }
    relevant <- boe_mpr_bind(inventory[releases$release[releases$publication_status == "published"]])
    urls <- unique(relevant$url[relevant$asset_role %in% c("data_bundle", "numeric_workbook", "report_pdf")])
    old_relevant <- boe_mpr_bind(lapply(old$editions, `[[`, "assets"))
    removed <- setdiff(old_relevant$url[old_relevant$asset_role %in% c("data_bundle", "numeric_workbook", "report_pdf")], urls)
    changed <- removed
    for (url in urls) {
      probe <- boe_mpr_probe(url, sources[[url]], deadline, raw_dir, force, 40 * 1024^2, raw_max_bytes)
      sources[[url]] <- probe$source; checked <- checked + 1L
      if (probe$changed) changed <- c(changed, url)
    }
    fetch <- function(url) {
      source <- sources[[url]]
      if (is.null(source$sha256)) boe_abort("Parser requested an unchecked source URL.", "source_revision", url = url)
      key <- boe_cache_key(list(method = "GET", url = url, accept = "*/*", contract = 1L))
      entry <- tryCatch(boe_cache_read(raw_dir, key, 30 * 86400, TRUE), error = function(e) NULL)
      if (is.null(entry) || !identical(entry$raw_sha256, source$sha256)) {
        probe <- boe_mpr_probe(url, NULL, deadline, raw_dir, TRUE, 40 * 1024^2, raw_max_bytes)
        if (!identical(probe$source$sha256, source$sha256)) boe_abort("A source changed during the update; candidate rejected.", "source_revision", url = url)
      }
      boe_download_asset(url, deadline = deadline, cache_only = TRUE, cache_dir = raw_dir, max_bytes = 40 * 1024^2)
    }
    affected <- boe_mpr_update_plan(old, releases, inventory, sources, changed, rebuild || parser_changed)
    metadata_changed <- !identical(boe_mpr_release_signature(releases), boe_mpr_release_signature(old$releases))
    status <- list(status = if (length(affected) || metadata_changed) "updated" else "unchanged", attempted_at = started,
      completed_at = Sys.time(), last_successful_check = Sys.time(), affected_editions = affected,
      checked_sources = checked, changed_sources = unique(changed), snapshot_sha256 = loaded$sha256,
      parser_fingerprint = boe_mpr_parser_fingerprint())
    if (length(affected) || metadata_changed) {
      records <- stats::setNames(lapply(names(old$editions), function(key) boe_mpr_unpack_record(old, key)), names(old$editions))
      for (key in affected) records[[key]] <- boe_mpr_build_record(key, releases, inventory, fetch, raw_dir, deadline)
      candidate <- boe_mpr_pack_snapshot(releases, records, Sys.time(), sources)
      boe_mpr_check_preservation(old, candidate, allow_coverage_loss)
      status <- boe_mpr_publish(candidate, loaded, data_dir, status, sources, inventory)
    } else {
      boe_mpr_validate_snapshot(old)
      status$last_successful_check <- status$completed_at <- Sys.time()
      boe_mpr_atomic_rds(list(snapshot_sha256 = loaded$sha256, sources = sources, inventory = inventory), file.path(data_dir, "SOURCES.rds"))
      boe_mpr_atomic_rds(status, file.path(data_dir, "STATUS.rds"))
    }
    status
  }, error = function(e) {
    status <- boe_mpr_failed_attempt(e, loaded, data_dir, started)
    if (stop_on_error) boe_abort(paste("Forecast update failed; previous data retained:", conditionMessage(e)), "update_failed", status = status)
    status
  }, interrupt = function(e) {
    # A graceful interrupt is an attempted check, but never a successful one.
    tryCatch(boe_mpr_failed_attempt(e, loaded, data_dir, started, interrupted = TRUE), error = function(write_error) NULL)
    stop(e)
  })
  result
}
