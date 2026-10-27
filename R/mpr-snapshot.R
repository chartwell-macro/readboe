# Structured forecast snapshots are the ordinary read path. Raw readers are
# explicit rebuild tools and are never called while loading/selecting a snapshot.
.boe_mpr_memory <- new.env(parent = emptyenv())
.boe_mpr_snapshot_schema <- 1L

boe_mpr_data_dir <- function() {
  value <- Sys.getenv("READBOE_MPR_DATA_DIR", unset = "")
  if (nzchar(value)) value else file.path(tools::R_user_dir("readboe", "data"), "mpr")
}

boe_mpr_receipt_url <- function(x) x$canonical_url %||% x$url %||% x$source_identity$url
boe_mpr_keys <- function(x) paste(x$series_id, x$target_period, sep = "\r")
boe_mpr_condition <- function(e) list(code = sub("^readboe_", "", class(e)[[1L]]),
  message = conditionMessage(e), fields = unclass(e)[setdiff(names(e), c("message", "call"))])
boe_mpr_stop_condition <- function(x) do.call(boe_abort, c(list(message = x$message, subclass = x$code), x$fields))
boe_mpr_decode <- function(x) {
  for (name in names(x)) if (is.factor(x[[name]])) x[[name]] <- as.character(x[[name]])
  x
}

boe_mpr_parser_fingerprint <- function() {
  ns <- asNamespace("readboe")
  names <- c("read_boe_mpr_forecasts_raw", "boe_mpr_discover_live", "boe_mpr_assets_live")
  # Follow parser/transport helper dependencies rather than a hand-maintained
  # prefix allowlist. This includes XLS/XML/PDF helpers and future new helpers,
  # but excludes unrelated saved-reader/updater implementation changes.
  repeat {
    dependencies <- unique(unlist(lapply(names, function(name) {
      fn <- get(name, ns)
      c(all.names(body(fn), functions = TRUE), all.names(formals(fn), functions = TRUE))
    })))
    dependencies <- dependencies[grepl("^boe_", dependencies)]
    dependencies <- dependencies[vapply(dependencies, function(name) exists(name, ns, inherits = FALSE) && is.function(get(name, ns)), logical(1L))]
    added <- setdiff(dependencies, names)
    if (!length(added)) break
    names <- c(names, added)
  }
  functions <- lapply(sort(names), function(name) {
    fn <- get(name, ns); list(name = name,
      formals = deparse(formals(fn), control = c("keepInteger", "niceNames")),
      body = deparse(body(fn), control = c("keepInteger", "niceNames")))
  })
  digest::digest(functions, algo = "sha256")
}

boe_mpr_record <- function(full, default, assets, original = full, original_default = default) {
  own <- list(original_condition = if (inherits(original, "error")) boe_mpr_condition(original) else NULL,
    original_provenance = if (!inherits(original, "error")) boe_provenance(original) else NULL,
    original_default_condition = if (inherits(original_default, "error")) boe_mpr_condition(original_default) else NULL,
    original_default_provenance = if (!inherits(original_default, "error")) boe_provenance(original_default) else NULL)
  for (condition in own[c("original_condition", "original_default_condition")]) if (!is.null(condition) &&
      !condition$code %in% c("no_numeric_forecast", "no_scalar_forecast", "unsupported_format")) boe_mpr_stop_condition(condition)
  if (inherits(full, "error")) {
    if (!inherits(full, "readboe_no_numeric_forecast")) stop(full)
    return(c(list(status = "no_numeric_forecast", condition = boe_mpr_condition(full), assets = assets,
      receipts = full$receipts, inventory = full$inventory, historical_search = TRUE), own))
  }
  p <- boe_provenance(full); x <- as.data.frame(full); attr(x, "boe_provenance") <- NULL
  if (inherits(default, "error")) {
    if (!inherits(default, "readboe_no_scalar_forecast") && !inherits(default, "readboe_no_numeric_forecast")) stop(default)
    defaults <- character(); dp <- NULL; condition <- boe_mpr_condition(default)
  } else {
    defaults <- boe_mpr_keys(default); dp <- boe_provenance(default); condition <- NULL
    match <- match(defaults, boe_mpr_keys(x))
    if (anyNA(match) || !isTRUE(all.equal(default$value, x$value[match], check.attributes = FALSE))) {
      boe_abort("Optional distributions changed an existing scalar path.", "schema_drift")
    }
  }
  x$.default <- boe_mpr_keys(x) %in% defaults
  c(list(status = "parsed", data = x, provenance = p, default_provenance = dp,
    default_condition = condition, assets = assets, receipts = p$receipts,
    # Even an unsuccessful historical search is a dependency on future data.
    # Conservatively revisit every prior outcome when a databank is added or
    # revised; absence of a match is not evidence that a later source cannot help.
    inventory = p$inventory, historical_search = TRUE), own)
}

boe_mpr_pack_snapshot <- function(releases, records, checked_at = Sys.time(), sources = list()) {
  records <- records[releases$release[releases$publication_status == "published"]]
  if (any(vapply(records, is.null, logical(1L)))) boe_abort("A published edition has no complete outcome.", "coverage")
  frames <- list(); cursor <- 0L
  for (key in names(records)) {
    record <- records[[key]]; n <- if (record$status == "parsed") nrow(record$data) else 0L
    record$first_row <- cursor + 1L; record$rows <- n; cursor <- cursor + n
    if (n) frames[[key]] <- record$data
    record$data <- NULL; records[[key]] <- record
  }
  data <- boe_mpr_bind(frames)
  month <- as.integer(substr(data$forecast_vintage, 6L, 7L))
  start <- as.Date(paste0(substr(data$forecast_vintage, 1L, 4L), ifelse(data$frequency == "Q",
    sprintf("-%02d-01", ((month - 1L) %/% 3L) * 3L + 1L), "-01-01")))
  data$.forecast <- data$target_date >= start
  for (name in names(data)) if (is.character(data[[name]])) data[[name]] <- factor(data[[name]])
  catalogue_fields <- c("release", "release_date", "release_date_precision", "source_edition", "source_sheet", "layout",
    "measure", "kind", "frequency", "statistic", "title", "units", "definition", "source_vintage",
    "scenario", "scenario_label", "forecast_vintage", "series_id", ".default", ".forecast")
  catalogue <- unique(data[intersect(catalogue_fields, names(data))])
  snapshot <- list(schema = .boe_mpr_snapshot_schema, parser_fingerprint = boe_mpr_parser_fingerprint(),
    built_at = Sys.time(), checked_at = checked_at, releases = releases, editions = records,
    observations = data, catalogue = catalogue, sources = sources, rights = boe_rights())
  boe_mpr_validate_snapshot(snapshot)
  snapshot
}

boe_mpr_validate_snapshot <- function(x, check_catalogue = TRUE) {
  if (!is.list(x) || !identical(x$schema, .boe_mpr_snapshot_schema) ||
      !is.character(x$parser_fingerprint) || length(x$parser_fingerprint) != 1L || is.na(x$parser_fingerprint) ||
      !grepl("^[a-f0-9]{64}$", x$parser_fingerprint) || !inherits(x$built_at, "POSIXct") || length(x$built_at) != 1L || anyNA(x$built_at) ||
      !inherits(x$checked_at, "POSIXct") || length(x$checked_at) != 1L || anyNA(x$checked_at)) boe_abort("Invalid forecast snapshot header.", "snapshot_corruption")
  r <- x$releases; d <- x$observations
  if (!is.data.frame(r) || !all(c("release", "release_date", "release_date_precision", "source_published_date", "publication_status", "landing_url") %in% names(r)) ||
      anyNA(r$release) || anyDuplicated(r$release) || nrow(r) > 10000L ||
      any(!grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", r$release)) ||
      !inherits(r$release_date, "Date") || !inherits(r$source_published_date, "Date") || anyNA(r$source_published_date) ||
      any(!r$publication_status %in% c("published", "scheduled")) ||
      any(!r$release_date_precision %in% c("day", "month")) ||
      any(is.na(r$release_date[r$release_date_precision == "day"]))) boe_abort("Invalid forecast edition inventory.", "snapshot_corruption")
  published <- r$release[r$publication_status == "published"]
  if (!identical(names(x$editions), published) || !is.data.frame(d) || nrow(d) > 1e6 ||
      !all(c("series_id", "release", "release_date", "forecast_vintage", "frequency", "measure", "target_period", "target_date", "value", "raw_value", "value_status", "units", "statistic", "kind", "scenario", "source_edition", "source_url", ".default", ".forecast") %in% names(d))) boe_abort("Snapshot coverage or observation schema is incomplete.", "snapshot_corruption")
  if (!inherits(d$target_date, "Date") || anyNA(d$target_date) || !is.numeric(d$value) ||
      any(!is.finite(d$value[!is.na(d$value)])) || !is.logical(d$.default) || anyNA(d$.default) ||
      !is.logical(d$.forecast) || anyNA(d$.forecast) || anyNA(d$frequency) ||
      any(!d$frequency %in% c("A", "Q"))) boe_abort("Invalid snapshot values or dates.", "snapshot_corruption")
  sid <- as.character(d$series_id); target <- as.character(d$target_period)
  if (anyNA(sid) || anyNA(target)) boe_abort("Missing snapshot observation identities.", "snapshot_corruption")
  levels <- unique(target)
  # Integer dictionary pairs are collision-free at the bounded million-row
  # scale (well below double's exact-integer limit), avoiding per-row strings.
  series <- unique(sid); group <- match(sid, series)
  if (anyDuplicated(group * (length(levels) + 1) + match(target, levels))) boe_abort("Duplicate snapshot observation identities.", "snapshot_corruption")
  valid <- grepl("^[0-9]{4}(-Q[1-4])?$", levels)
  if (!all(valid)) boe_abort("Invalid snapshot target period.", "snapshot_corruption")
  expected <- as.Date(paste0(substr(levels, 1, 4), "-", ifelse(grepl("-Q", levels),
    sprintf("%02d", (as.integer(sub(".*Q", "", ifelse(grepl("-Q", levels), levels, "Q1"))) - 1L) * 3L + 1L), "01"), "-01"))
  if (!all(d$target_date == expected[match(target, levels)])) boe_abort("Snapshot periods and dates disagree.", "snapshot_corruption")
  vintage <- as.character(d$forecast_vintage); vintages <- unique(vintage)
  if (anyNA(vintage) || any(!grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", vintages))) boe_abort("Invalid snapshot forecast vintage.", "snapshot_corruption")
  month <- as.integer(substr(vintages, 6L, 7L)); year <- substr(vintages, 1L, 4L)
  quarter_start <- as.numeric(as.Date(paste0(year, sprintf("-%02d-01", ((month - 1L) %/% 3L) * 3L + 1L))))
  year_start <- as.numeric(as.Date(paste0(year, "-01-01")))
  index <- match(vintage, vintages)
  forecast <- as.numeric(d$target_date) >= ifelse(d$frequency == "Q", quarter_start[index], year_start[index])
  if (!identical(d$.forecast, forecast)) boe_abort("Snapshot history flags disagree with its dates.", "snapshot_corruption")
  if (any(!as.character(d$release) %in% published) || anyNA(d$units) || anyNA(d$source_url)) boe_abort("Snapshot source identity is incomplete.", "snapshot_corruption")
  first <- !duplicated(sid)
  dims <- d[first, c("series_id", "release", "measure", "scenario", "statistic", "frequency", "kind")]
  for (name in setdiff(names(dims), "series_id")) {
    values <- d[[name]]; if (is.factor(values)) values <- as.integer(values)
    if (anyNA(values) || any(values != values[first][group])) boe_abort("Snapshot path dimensions are inconsistent.", "snapshot_corruption")
  }
  ids <- do.call(paste, c(list("mpr"), lapply(dims[setdiff(names(dims), "series_id")], as.character), sep = ":"))
  if (!identical(as.character(dims$series_id), ids) ||
      sum(vapply(x$editions, `[[`, numeric(1L), "rows")) != nrow(d) ||
      any(!as.character(d$source_edition) %in% published)) boe_abort("Snapshot identities or coverage disagree.", "snapshot_corruption")
  for (url in unique(as.character(d$source_url))) boe_validate_url(url)
  cursor <- 1L
  for (key in published) {
    e <- x$editions[[key]]
    if (!e$status %in% c("parsed", "no_numeric_forecast") || !is.numeric(e$rows) || length(e$rows) != 1L ||
        !is.finite(e$rows) || e$rows < 0L || e$rows != floor(e$rows) ||
        !identical(as.numeric(e$first_row), as.numeric(cursor))) boe_abort("Invalid edition outcome or row offsets.", "snapshot_corruption")
    cursor <- cursor + e$rows
    if (e$status == "parsed") {
      rows <- seq.int(e$first_row, length.out = e$rows)
      if (!length(rows) || max(rows) > nrow(d) || any(as.character(d$release[rows]) != key) ||
          is.null(e$provenance$raw_sha256)) boe_abort("Edition rows or provenance are incomplete.", "snapshot_corruption")
    } else if (e$rows != 0L || !identical(e$condition$code, "no_numeric_forecast") ||
      !e$condition$fields$source_evidence$pages_inspected > 0L) boe_abort("Numeric absence lacks source evidence.", "snapshot_corruption")
    for (receipt in e$receipts) if (is.null(boe_mpr_receipt_url(receipt)) ||
        !is.character(receipt$raw_sha256) || !grepl("^[a-f0-9]{64}$", receipt$raw_sha256)) boe_abort("Invalid source revision receipt.", "snapshot_corruption")
    if (e$status == "parsed") {
      urls <- vapply(e$receipts, boe_mpr_receipt_url, character(1L))
      if (!all(as.character(d$source_url[rows]) %in% urls) ||
          !e$provenance$raw_sha256 %in% vapply(e$receipts, `[[`, character(1L), "raw_sha256")) boe_abort("Observation sources lack revision receipts.", "snapshot_corruption")
    }
    for (receipt in e$receipts) {
      url <- boe_mpr_receipt_url(receipt); current <- x$sources[[url]]
      if (grepl("[.](pdf|zip|xlsx?|csv)([?].*)?$", url, ignore.case = TRUE) && !is.null(current) &&
          !identical(receipt$raw_sha256, current$sha256)) boe_abort("Candidate mixes different revisions of one source.", "source_revision")
    }
  }
  if (!is.data.frame(x$catalogue) || !all(c("series_id", ".default", ".forecast") %in% names(x$catalogue))) boe_abort("Snapshot catalogue is incomplete.", "snapshot_corruption")
  if (check_catalogue && !identical(x$catalogue, unique(d[names(x$catalogue)]))) boe_abort("Snapshot catalogue disagrees with its observations.", "snapshot_corruption")
  # Distribution contains structured values/receipts only, never raw payloads or
  # executable environments. Traverse the relatively small metadata, not cells.
  safe <- function(v, depth = 0L) {
    if (depth > 30L || is.raw(v) || is.environment(v) || is.function(v)) return(FALSE)
    if (is.data.frame(v)) return(all(vapply(v, safe, logical(1L), depth = depth + 1L)))
    if (is.atomic(v) || is.null(v)) return(TRUE)
    if (is.list(v)) return(all(vapply(v, safe, logical(1L), depth = depth + 1L)))
    FALSE
  }
  if (!safe(x)) boe_abort("Raw or executable objects cannot be distributed in a forecast snapshot.", "snapshot_corruption")
  invisible(TRUE)
}

boe_mpr_atomic_rds <- function(x, path, compress = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".mpr-", tmpdir = dirname(path)); on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 3, compress = compress)
  if (!file.rename(tmp, path)) boe_abort("Could not atomically replace forecast metadata.", "snapshot_write")
  invisible(path)
}

boe_mpr_optional_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  suppressWarnings(tryCatch(readRDS(path), error = function(e) NULL))
}

boe_mpr_snapshot_paths <- function() {
  c(data = system.file("extdata", "mpr-forecasts.rds", package = "readboe"),
    manifest = system.file("extdata", "mpr-forecasts-manifest.rds", package = "readboe"))
}

boe_mpr_read_generation <- function(path, hash) {
  info <- file.info(path)
  if (is.na(info$size) || info$size > 100 * 1024^2 || !grepl("^[a-f0-9]{64}$", hash)) boe_abort("Forecast snapshot is missing or over budget.", "snapshot_corruption")
  key <- paste(path, hash, sprintf("%.9f", as.numeric(info$mtime)),
    sprintf("%.9f", as.numeric(info$ctime)), info$size)
  if (identical(.boe_mpr_memory$key, key)) return(.boe_mpr_memory$snapshot)
  if (!identical(boe_sha256_file(path), hash)) boe_abort("Forecast snapshot hash mismatch.", "snapshot_corruption")
  x <- tryCatch(readRDS(path), error = function(e) boe_abort("Cannot decode forecast snapshot.", "snapshot_corruption"))
  boe_mpr_validate_snapshot(x, check_catalogue = FALSE)
  .boe_mpr_memory$key <- key; .boe_mpr_memory$snapshot <- x
  x
}

boe_mpr_consistent_status <- function(advisory, committed, hash, checked_at) {
  matches <- function(x) is.list(x) && identical(x$snapshot_sha256, hash) &&
    is.character(x$status) && length(x$status) == 1L && !is.na(x$status) &&
    x$status %in% c("updated", "unchanged", "failed", "interrupted") &&
    inherits(x$last_successful_check, "POSIXct") && length(x$last_successful_check) == 1L &&
    !is.na(x$last_successful_check)
  candidates <- Filter(matches, list(committed, advisory))
  if (length(candidates)) {
    time <- function(x, field) {
      value <- x[[field]]
      if (inherits(value, "POSIXct") && length(value) == 1L && !is.na(value)) as.numeric(value) else -Inf
    }
    attempted <- vapply(candidates, time, numeric(1L), field = "attempted_at")
    completed <- vapply(candidates, time, numeric(1L), field = "completed_at")
    newest <- order(attempted, completed, seq_along(candidates))
    return(candidates[[newest[[length(newest)]]]])
  }
  if (is.null(advisory)) return(NULL)
  if (!is.list(advisory)) advisory <- list()
  # Older interrupted writers could publish STATUS before committing CURRENT.
  # Never expose their successful timestamp or hash as this generation's state.
  list(status = "interrupted", attempted_at = advisory$attempted_at,
    snapshot_sha256 = hash, last_successful_check = checked_at,
    rejected_status_sha256 = advisory$snapshot_sha256,
    error = "Status for a different, uncommitted forecast generation was ignored.")
}

boe_mpr_load_saved <- function(data_dir = boe_mpr_data_dir(), .retry = TRUE) {
  boe_assert_scalar(data_dir, "data_dir", "character")
  paths <- boe_mpr_snapshot_paths()
  pointer_path <- file.path(data_dir, "CURRENT.rds")
  pointer <- boe_mpr_optional_rds(pointer_path)
  status <- boe_mpr_optional_rds(file.path(data_dir, "STATUS.rds"))
  choices <- if (is.list(pointer) && identical(pointer$schema, 1L)) c(list(pointer$current), list(pointer$previous)) else list()
  failure <- NULL
  for (choice in choices) {
    if (is.null(choice)) next
    if (!is.character(choice$file) || length(choice$file) != 1L || !grepl("^mpr-[a-f0-9]{64}[.]rds$", choice$file)) { failure <- "Invalid refreshed snapshot pointer"; next }
    x <- tryCatch(boe_mpr_read_generation(file.path(data_dir, choice$file), choice$sha256), error = identity)
    if (!inherits(x, "error")) {
      return(list(snapshot = x, sha256 = choice$sha256, origin = "refreshed", path = file.path(data_dir, choice$file),
        status = boe_mpr_consistent_status(status, choice$status, choice$sha256, x$checked_at), fallback = failure))
    }
    failure <- conditionMessage(x)
  }
  # A reader can hold an old pointer while a fast sequence of writers prunes
  # unreferenced generations. Retry the current pointer once before falling back.
  if (.retry && !is.null(failure) && !identical(pointer, boe_mpr_optional_rds(pointer_path))) return(boe_mpr_load_saved(data_dir, .retry = FALSE))
  if (file.exists(pointer_path) && is.null(failure)) failure <- "Unreadable refreshed snapshot pointer"
  manifest <- tryCatch(readRDS(paths[["manifest"]]), error = function(e) NULL)
  if (!is.list(manifest) || !identical(manifest$schema, .boe_mpr_snapshot_schema)) boe_abort("The bundled forecast manifest is unavailable.", "snapshot_corruption")
  x <- boe_mpr_read_generation(paths[["data"]], manifest$sha256)
  list(snapshot = x, sha256 = manifest$sha256, origin = "bundled", path = paths[["data"]],
    status = boe_mpr_consistent_status(status, NULL, manifest$sha256, x$checked_at), fallback = failure)
}

boe_mpr_saved_provenance <- function(loaded) {
  status <- loaded$status; checked <- loaded$snapshot$checked_at
  if (!is.null(status) && identical(status$snapshot_sha256, loaded$sha256) && !is.null(status$last_successful_check)) checked <- status$last_successful_check
  list(schema = loaded$snapshot$schema, snapshot_sha256 = loaded$sha256, origin = loaded$origin,
    path = loaded$path, built_at = loaded$snapshot$built_at, last_successful_check = checked,
    update_status = status, fallback_reason = loaded$fallback,
    parser_fingerprint = loaded$snapshot$parser_fingerprint)
}

boe_mpr_warn_stale <- function(loaded) {
  failed <- !is.null(loaded$status) && loaded$status$status %in% c("failed", "interrupted")
  if (is.null(loaded$fallback) && !failed) return(invisible(NULL))
  key <- digest::digest(list(loaded$sha256, loaded$fallback, loaded$status$attempted_at), algo = "sha256")
  if (!identical(.boe_mpr_memory$warned, key)) {
    .boe_mpr_memory$warned <- key
    boe_signal_warning(paste0("Using last good saved forecasts. ", loaded$fallback %||% loaded$status$error),
      "snapshot_fallback")
  }
  invisible(NULL)
}

boe_mpr_saved_releases <- function(loaded, release) {
  r <- loaded$snapshot$releases
  if (identical(release, "latest")) return(r$release[which.max(ifelse(r$publication_status == "published", as.numeric(r$source_published_date), -Inf))])
  if (identical(release, "all")) return(r$release[r$publication_status == "published"])
  if (!is.character(release) || !length(release) || anyNA(release) || anyDuplicated(release) ||
      any(!release %in% r$release[r$publication_status == "published"])) boe_abort("Select discovered published forecast editions, latest or all.", "identity_mismatch")
  release
}

#' Browse saved forecast-report editions without network access
#' @inheritParams boe_mpr_discover_live
#' @param source Saved structured data (default), or explicit raw-source discovery.
#' @param data_dir Refreshed snapshot directory; defaults to READBOE_MPR_DATA_DIR or R's user data directory.
#' @export
boe_mpr_releases <- function(include_upcoming = FALSE, as_of = Sys.Date(), deadline = NULL,
    max_pages = 10L, max_releases = 1000L, cache = TRUE, cache_only = FALSE,
    refresh = FALSE, cache_dir = boe_default_cache_dir(), source = c("saved", "raw"), data_dir = boe_mpr_data_dir()) {
  source <- match.arg(source)
  if (source == "raw") return(boe_mpr_discover_live(include_upcoming, as_of, deadline, max_pages, max_releases, cache, cache_only, refresh, cache_dir))
  boe_assert_scalar(refresh, "refresh", "logical")
  if (refresh) boe_abort("Use boe_mpr_update() or source='raw' for an explicit refresh.", "config")
  boe_time_left(boe_as_deadline(deadline)); boe_assert_scalar(include_upcoming, "include_upcoming", "logical")
  boe_check_positive(max_pages, "max_pages", integer = TRUE); boe_check_positive(max_releases, "max_releases", integer = TRUE)
  if (!inherits(as_of, "Date") || length(as_of) != 1L || is.na(as_of)) boe_abort("as_of must be one Date.", "config")
  loaded <- boe_mpr_load_saved(data_dir); r <- loaded$snapshot$releases
  boe_mpr_warn_stale(loaded)
  if (nrow(r) > max_releases) boe_abort("Saved editions exceed max_releases.", "bounds")
  r$coverage_status <- vapply(r$release, function(key) loaded$snapshot$editions[[key]]$status %||% "scheduled", character(1L))
  r$saved_rows <- vapply(r$release, function(key) loaded$snapshot$editions[[key]]$rows %||% 0L, numeric(1L))
  r$publication_status[r$source_published_date > as_of] <- "scheduled"
  if (!include_upcoming) r <- r[r$publication_status == "published", , drop = FALSE]
  p <- boe_provenance(loaded$snapshot$releases) %||% list(source_identity = list(surface = "forecast_report_archive"))
  p$snapshot <- boe_mpr_saved_provenance(loaded); attr(r, "boe_provenance") <- p
  r
}

#' Read saved forecast paths without HTTP or PDF extraction
#' @inheritParams read_boe_mpr_forecasts_raw
#' @inheritParams boe_mpr_releases
#' @param release Saved reads accept latest, all published numeric outcomes, or exact edition keys. The explicit raw route selects one edition.
#' @export
read_boe_mpr_forecasts <- function(release = "latest", measure = NULL, scenario = NULL,
    statistic = NULL, frequency = NULL, kind = NULL, series_id = NULL,
    include_history = FALSE, include_distributions = FALSE, historical_source = TRUE,
    expected_sha256 = NULL, deadline = NULL, max_bytes = 40 * 1024^2,
    max_archive_members = 200L, max_uncompressed_bytes = 200 * 1024^2,
    max_sheets = 150L, max_rows = 10000L, max_columns = 512L, max_cells = 2e6,
    max_output_rows = 200000L, cache = TRUE, cache_only = FALSE,
    refresh = FALSE, cache_dir = boe_default_cache_dir(), source = c("saved", "raw"), data_dir = boe_mpr_data_dir()) {
  source <- match.arg(source)
  if (source == "raw") {
    args <- as.list(environment()); args$source <- NULL; args$data_dir <- NULL
    return(do.call(read_boe_mpr_forecasts_raw, args))
  }
  options <- boe_mpr_options(deadline, max_bytes, max_archive_members, max_uncompressed_bytes,
    max_sheets, max_rows, max_columns, max_cells, max_output_rows, cache, cache_only, refresh, cache_dir)
  for (name in c("include_history", "include_distributions", "historical_source")) boe_assert_scalar(get(name), name, "logical")
  if (refresh) boe_abort("Use boe_mpr_update() or source='raw' for an explicit refresh.", "config")
  selections <- list(measure = measure, scenario = scenario, statistic = statistic, frequency = frequency, kind = kind)
  if (!is.null(series_id)) {
    if (any(!vapply(selections, is.null, logical(1L)))) boe_abort("Select IDs or dimensions, not both.", "config")
    selections <- list(series_id = series_id)
  }
  if (!is.null(expected_sha256) && (!is.character(expected_sha256) || length(expected_sha256) != 1L || is.na(expected_sha256) || !grepl("^[a-f0-9]{64}$", expected_sha256))) boe_abort("Invalid expected source hash.", "config")
  loaded <- boe_mpr_load_saved(data_dir); keys <- boe_mpr_saved_releases(loaded, release)
  boe_mpr_warn_stale(loaded)
  if (!is.null(expected_sha256) && length(keys) != 1L) boe_abort("A primary source pin selects one edition.", "config")
  parts <- list(); provenances <- list(); d <- loaded$snapshot$observations
  for (key in keys) {
    e <- loaded$snapshot$editions[[key]]
    if (e$status != "parsed") {
      if (length(keys) == 1L) boe_mpr_stop_condition(if (historical_source) e$condition else e$original_condition %||% e$condition)
      next
    }
    own_condition <- if (!include_distributions) e$original_default_condition else e$original_condition
    if (!historical_source && !is.null(own_condition)) {
      if (length(keys) == 1L) boe_mpr_stop_condition(own_condition)
      next
    }
    rows <- seq.int(e$first_row, length.out = e$rows)
    if (!include_distributions) rows <- rows[d$.default[rows]]
    if (!historical_source) rows <- rows[as.character(d$source_edition[rows]) == key]
    if (!length(rows)) {
      if (length(keys) == 1L) {
        if (!include_distributions && !is.null(e$default_condition)) boe_mpr_stop_condition(e$default_condition)
        boe_abort("No original numeric paths match this selection.", "no_data")
      }
      next
    }
    if (!include_history) rows <- rows[d$.forecast[rows]]
    for (name in names(selections)) if (!is.null(selections[[name]])) rows <- rows[as.character(d[[name]][rows]) %in% selections[[name]]]
    if (!length(rows)) next
    p <- if (historical_source) {
      if (include_distributions) e$provenance else e$default_provenance
    } else {
      if (include_distributions) e$original_provenance else e$original_default_provenance
    }
    if (!is.null(expected_sha256) && !identical(p$raw_sha256, expected_sha256)) boe_abort("Primary numeric source revision differs from its pin.", "identity_mismatch")
    parts[[key]] <- rows; provenances[[key]] <- p
  }
  rows <- unlist(parts, use.names = FALSE)
  if (length(rows) > max_output_rows) boe_abort("Saved forecast selection exceeds max_output_rows.", "bounds")
  columns <- setdiff(names(d), c(".default", ".forecast"))
  # Decode complete factor columns directly instead of copying their codes.
  # Other column types retain ordinary row-subset and serialization semantics.
  if (identical(rows, seq_len(nrow(d)))) {
    out <- d[columns]
    for (name in names(out)) if (!is.factor(out[[name]])) out[[name]] <- out[[name]][rows]
  } else out <- d[rows %||% integer(), columns, drop = FALSE]
  out <- boe_mpr_decode(out)
  rownames(out) <- NULL; out <- boe_mpr_select(out, selections)
  p <- if (length(provenances) == 1L) provenances[[1L]] else list(editions = provenances)
  p$snapshot <- boe_mpr_saved_provenance(loaded)
  p$source_identity$surface <- "mpr_forecasts"; p$source_identity$release <- keys
  p$source_identity$series_id <- unique(out$series_id)
  p$snapshot$selection <- list(include_history = include_history, include_distributions = include_distributions,
    historical_source = historical_source)
  p$transforms <- c(p$transforms[!grepl("Published past targets|Publication quarter/year", p$transforms)],
    "Selection from authenticated saved structured data; no source acquisition or PDF extraction",
    if (include_history) "Published past targets retained" else "Publication quarter/year and later targets retained")
  class(out) <- c("boe_mpr_forecasts", "data.frame"); attr(out, "boe_provenance") <- p
  boe_time_left(options$deadline); out
}

#' Browse saved source assets for forecast editions
#' @inheritParams boe_mpr_releases
#' @param release Latest, all, or exact edition keys.
#' @export
boe_mpr_assets <- function(release = "latest", deadline = NULL, cache = TRUE, cache_only = FALSE,
    refresh = FALSE, cache_dir = boe_default_cache_dir(), source = c("saved", "raw"), data_dir = boe_mpr_data_dir()) {
  if (match.arg(source) == "raw") return(boe_mpr_assets_live(release, deadline, cache, cache_only, refresh, cache_dir))
  boe_assert_scalar(refresh, "refresh", "logical")
  if (refresh) boe_abort("Use boe_mpr_update() or source='raw' for an explicit refresh.", "config")
  boe_time_left(boe_as_deadline(deadline)); loaded <- boe_mpr_load_saved(data_dir)
  boe_mpr_warn_stale(loaded)
  keys <- boe_mpr_saved_releases(loaded, release)
  out <- boe_mpr_bind(lapply(keys, function(key) loaded$snapshot$editions[[key]]$assets))
  p <- if (length(keys) == 1L) boe_provenance(loaded$snapshot$editions[[keys]]$assets) else {
    list(editions = stats::setNames(lapply(keys, function(key) boe_provenance(loaded$snapshot$editions[[key]]$assets)), keys))
  }
  p$snapshot <- boe_mpr_saved_provenance(loaded); attr(out, "boe_provenance") <- p; out
}

#' Browse saved forecast path identities and source inventory
#' @inheritParams boe_mpr_catalogue_live
#' @inheritParams boe_mpr_releases
#' @param release Latest, all published numeric outcomes, or exact saved edition keys; the raw route selects one edition.
#' @export
boe_mpr_catalogue <- function(release = "latest", include_excluded = FALSE, include_distributions = FALSE,
    deadline = NULL, cache = TRUE, cache_only = FALSE, refresh = FALSE, cache_dir = boe_default_cache_dir(),
    source = c("saved", "raw"), data_dir = boe_mpr_data_dir()) {
  if (match.arg(source) == "raw") return(boe_mpr_catalogue_live(release, include_excluded, include_distributions, deadline, cache, cache_only, refresh, cache_dir))
  for (name in c("include_excluded", "include_distributions", "refresh")) boe_assert_scalar(get(name), name, "logical")
  if (refresh) boe_abort("Use boe_mpr_update() or source='raw' for an explicit refresh.", "config")
  boe_time_left(boe_as_deadline(deadline)); loaded <- boe_mpr_load_saved(data_dir); boe_mpr_warn_stale(loaded)
  keys <- boe_mpr_saved_releases(loaded, release); d <- loaded$snapshot$catalogue
  rows <- as.character(d$release) %in% keys & d$.forecast & (include_distributions | d$.default)
  out <- unique(boe_mpr_decode(d[rows, setdiff(names(d), c(".default", ".forecast")), drop = FALSE])); out$supported <- TRUE
  if (!nrow(out) && length(keys) == 1L) {
    e <- loaded$snapshot$editions[[keys]]
    if (e$status != "parsed") boe_mpr_stop_condition(e$condition)
    if (!include_distributions && !is.null(e$default_condition)) boe_mpr_stop_condition(e$default_condition)
  }
  if (include_excluded) {
    inv <- boe_mpr_bind(lapply(keys, function(key) {
      e <- loaded$snapshot$editions[[key]]
      if (include_distributions) e$inventory else e$default_provenance$inventory %||% e$inventory
    }))
    if (nrow(inv)) {
      inv <- inv[!inv$source_sheet %in% out$source_sheet, , drop = FALSE]; inv$series_id <- NA_character_; inv$supported <- FALSE
      out <- boe_mpr_bind(list(out, inv))
    }
  }
  provenances <- lapply(keys, function(key) {
    e <- loaded$snapshot$editions[[key]]
    if (include_distributions) e$provenance else e$default_provenance
  }); names(provenances) <- keys
  p <- if (length(keys) == 1L) provenances[[1L]] else list(editions = provenances)
  p$snapshot <- boe_mpr_saved_provenance(loaded)
  p$source_identity$surface <- "mpr_forecasts"; p$source_identity$release <- keys
  p$source_identity$series_id <- unique(out$series_id[!is.na(out$series_id)])
  attr(out, "boe_provenance") <- p; rownames(out) <- NULL; out
}

#' Inspect local forecast snapshot coverage and update status
#' @inheritParams boe_mpr_releases
#' @return A list of validated snapshot identity, coverage and last update attempt.
#' @export
boe_mpr_snapshot_status <- function(data_dir = boe_mpr_data_dir()) {
  loaded <- boe_mpr_load_saved(data_dir)
  c(boe_mpr_saved_provenance(loaded), list(observations = nrow(loaded$snapshot$observations),
    editions = nrow(loaded$snapshot$releases), coverage = table(vapply(loaded$snapshot$editions, `[[`, character(1L), "status"))))
}

boe_mpr_write_bundle <- function(snapshot, directory) {
  boe_mpr_validate_snapshot(snapshot)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, "mpr-forecasts.rds")
  boe_mpr_atomic_rds(snapshot, path, compress = "gzip")
  manifest <- list(schema = .boe_mpr_snapshot_schema, sha256 = boe_sha256_file(path),
    bytes = file.info(path)$size, observations = nrow(snapshot$observations),
    editions = nrow(snapshot$releases), checked_at = snapshot$checked_at,
    parser_fingerprint = snapshot$parser_fingerprint, rights = snapshot$rights)
  boe_mpr_atomic_rds(manifest, file.path(directory, "mpr-forecasts-manifest.rds"), compress = "xz")
  invisible(manifest)
}
