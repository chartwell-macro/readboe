# Explicit maintenance only. No ordinary reader or package startup calls this file.
.boe_mpr_calendar_url <- "https://www.bankofengland.co.uk/monetary-policy/upcoming-mpc-dates"
.boe_mpr_times_url <- "https://www.bankofengland.co.uk/news/latest-and-upcoming"
.boe_mpr_maintenance_schema <- 1L

boe_mpr_utc <- function(x) {
  if (is.null(x) || !length(x) || is.na(x)) return(NULL)
  format(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}
boe_mpr_clock <- function(text) {
  text <- tolower(gsub("[[:space:]\u00a0]+", " ", text, perl = TRUE))
  pattern <- "(?<![0-9])[0-9]{1,2}(?:[.:][0-9]{2})? *(?:am|pm)\\b|\\b[0-9]{1,2}:[0-9]{2}\\b|\\bnoon\\b|\\bmidnight\\b"
  tokens <- unique(regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]])
  if (!length(tokens)) return(NA_character_)
  clocks <- vapply(tokens, function(token) {
    if (token == "noon") return("12:00")
    if (token == "midnight") return("00:00")
    part <- strsplit(gsub("[ .]", ":", sub(" *(am|pm)$", "", token)), ":", fixed = TRUE)[[1L]]
    hour <- as.integer(part[[1L]]); minute <- if (length(part) > 1L) as.integer(part[[2L]]) else 0L
    meridiem <- sub(".*(am|pm)$", "\\1", token)
    if (meridiem %in% c("am", "pm")) {
      if (hour < 1L || hour > 12L) boe_abort("Invalid official publication clock.", "schema_drift")
      hour <- hour %% 12L + if (meridiem == "pm") 12L else 0L
    }
    if (is.na(hour) || is.na(minute) || hour < 0L || hour > 23L || minute < 0L || minute > 59L) boe_abort("Invalid official publication clock.", "schema_drift")
    sprintf("%02d:%02d", hour, minute)
  }, character(1L))
  clocks <- unique(clocks)
  if (length(clocks) != 1L) boe_abort("Official publication time is ambiguous.", "schema_drift")
  clocks[[1L]]
}
boe_mpr_london_time <- function(date, clock) {
  text <- paste(as.character(date), clock)
  at <- as.POSIXct(strptime(text, "%Y-%m-%d %H:%M", tz = "Europe/London"))
  if (is.na(at) || format(at, tz = "Europe/London", format = "%Y-%m-%d %H:%M") != text) boe_abort("Invalid London publication time.", "schema_drift")
  alternate <- c(at - 3600, at + 3600)
  if (any(format(alternate, tz = "Europe/London", format = "%Y-%m-%d %H:%M") == text)) boe_abort("Ambiguous daylight-saving publication time.", "schema_drift")
  as.POSIXct(as.numeric(at), origin = "1970-01-01", tz = "UTC")
}

boe_mpr_parse_calendar <- function(calendar_entry, times_entry, now) {
  doc <- boe_mpr_html(calendar_entry$body); listing <- boe_mpr_html(times_entry$body)
  cards <- xml2::xml_find_all(listing, "//a[contains(@href,'/monetary-policy-report/')]")
  times <- list()
  for (card in cards) {
    url <- sub("#.*$", "", xml2::url_absolute(xml2::xml_attr(card, "href"), .boe_mpr_times_url))
    if (!boe_mpr_is_release_url(url)) next
    labels <- xml2::xml_text(xml2::xml_find_all(card, ".//h3"))
    label <- if (length(labels)) labels[[which.max(nchar(labels))]] else xml2::xml_text(card)
    stamp <- xml2::xml_attr(xml2::xml_find_first(card, ".//time[@datetime]"), "datetime")
    time <- list(clock = boe_mpr_clock(label), label = trimws(label), date = suppressWarnings(as.Date(substr(stamp, 1L, 10L))))
    if (!is.null(times[[url]]) && !identical(times[[url]], time)) boe_abort("Duplicate official MPR time entries disagree.", "schema_drift")
    times[[url]] <- time
  }
  rows <- list()
  for (row in xml2::xml_find_all(doc, "//table//tr")) {
    # boe_mpr_links() deliberately uses document-wide //a for archive pages.
    # A calendar row must use relative XPath, or every row sees the whole page.
    nodes <- xml2::xml_find_all(row, ".//a[@href]")
    urls <- sub("#.*$", "", xml2::url_absolute(xml2::xml_attr(nodes, "href"), .boe_mpr_calendar_url))
    urls <- unique(urls[boe_mpr_is_release_url(urls) & grepl("/monetary-policy-report/|/report/", urls)])
    links <- data.frame(url = urls, stringsAsFactors = FALSE)
    if (!nrow(links)) next
    if (nrow(links) != 1L) boe_abort("An MPC calendar row has ambiguous MPR identities.", "schema_drift")
    url <- links$url[[1L]]; boe_validate_url(url)
    key <- boe_mpr_month_key(url)
    cell <- xml2::xml_text(xml2::xml_find_first(row, "./td[1]"))
    cell <- trimws(gsub("[[:space:]\u00a0]+", " ", cell, perl = TRUE))
    date <- boe_mpr_iso_date(if (grepl("[0-9]{4}", cell)) cell else paste(cell, substr(key, 1L, 4L)))
    if (is.na(date) || format(date, "%Y-%m") != key) boe_abort("MPC calendar date and report edition disagree.", "schema_drift")
    time <- times[[url]]; clock <- time$clock %||% NA_character_
    if (!is.null(time) && !is.na(time$date) && time$date != date) boe_abort("MPC calendar and publication listing dates disagree.", "schema_drift")
    at <- if (!is.na(clock)) boe_mpr_london_time(date, clock) else as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    # A known date without a clock starts conservative metadata polling at local
    # midnight. That is a poll boundary, never an asserted publication time.
    due <- if (is.na(at)) boe_mpr_london_time(date, "00:00") else at
    rows[[length(rows) + 1L]] <- data.frame(edition = key, release_date = date,
      publication_at = at, due_at = due, local_time = clock, timezone = "Europe/London",
      due_basis = if (is.na(at)) "date_start_metadata_poll" else "explicit_publication_time",
      landing_url = url, time_label = time$label %||% NA_character_, stringsAsFactors = FALSE)
  }
  out <- boe_mpr_bind(rows)
  if (!nrow(out) || nrow(out) > 1000L || anyDuplicated(out$edition)) boe_abort("Official MPR calendar is empty, duplicate or over budget.", "schema_drift")
  out <- out[order(out$release_date), , drop = FALSE]; rownames(out) <- NULL
  attr(out, "boe_provenance") <- list(receipts = list(boe_receipt_from_entry(calendar_entry), boe_receipt_from_entry(times_entry)),
    calendar_url = .boe_mpr_calendar_url, times_url = .boe_mpr_times_url,
    timezone = "Europe/London", checked_at = now,
    rule = "Only row-scoped MPR identities; explicit clocks when available; unknown clocks use a labelled local-date metadata poll boundary")
  out
}

#' Discover the official MPR calendar and publication times
#' @param deadline Absolute deadline or total seconds for the two official pages.
#' @param now Current POSIXct time; injectable for deterministic scheduling tests.
#' @return A data frame of report editions, exact dates, London clock labels and UTC publication/due times, with source receipts.
#' @export
boe_mpr_calendar <- function(deadline = 60, now = Sys.time()) {
  boe_mpr_maintenance_now(now); deadline <- boe_as_deadline(deadline)
  fetch <- function(url) boe_request_raw(url, deadline = deadline, max_bytes = 10 * 1024^2,
    cache = FALSE, refresh = TRUE, validate_response = boe_validate_not_error_html)
  boe_mpr_parse_calendar(fetch(.boe_mpr_calendar_url), fetch(.boe_mpr_times_url), now)
}

boe_mpr_maintenance_now <- function(now) {
  if (!inherits(now, "POSIXct") || length(now) != 1L || is.na(now) || !is.finite(as.numeric(now))) boe_abort("now must be one finite POSIXct instant.", "config")
  invisible(now)
}

boe_mpr_maintenance_valid <- function(x) {
  is.list(x) && identical(x$schema, .boe_mpr_maintenance_schema) &&
    inherits(x$next_due_at, "POSIXct") && length(x$next_due_at) == 1L && !is.na(x$next_due_at) &&
    (is.null(x$calendar) || (is.data.frame(x$calendar) && nrow(x$calendar) <= 1000L &&
      all(c("edition", "release_date", "publication_at", "due_at", "landing_url") %in% names(x$calendar)) &&
      !anyDuplicated(x$calendar$edition) && inherits(x$calendar$release_date, "Date") &&
      inherits(x$calendar$due_at, "POSIXct") && !anyNA(x$calendar$due_at)))
}
boe_mpr_maintenance_empty <- function(now) list(schema = .boe_mpr_maintenance_schema,
  status = "waiting", outcome = "uninitialized", next_due_at = now,
  next_calendar_at = now, next_revision_at = now, calendar = NULL,
  releases = list(), history = list(), known_editions = character(),
  calendar_failures = 0L, data_failures = 0L)
boe_mpr_maintenance_read <- function(data_dir) {
  files <- file.path(data_dir, c("MAINTENANCE.rds", "MAINTENANCE.previous.rds"))
  for (i in seq_along(files)) {
    if (!file.exists(files[[i]]) || is.na(file.info(files[[i]])$size) || file.info(files[[i]])$size > 2 * 1024^2) next
    x <- boe_mpr_optional_rds(files[[i]])
    if (boe_mpr_maintenance_valid(x)) return(list(state = x, recovered = i > 1L))
  }
  list(state = NULL, recovered = any(file.exists(files)))
}
boe_mpr_maintenance_write <- function(x, data_dir) {
  if (!boe_mpr_maintenance_valid(x)) boe_abort("Invalid maintenance state candidate.", "maintenance_state")
  path <- file.path(data_dir, "MAINTENANCE.rds")
  old <- boe_mpr_optional_rds(path)
  if (boe_mpr_maintenance_valid(old)) boe_mpr_atomic_rds(old, file.path(data_dir, "MAINTENANCE.previous.rds"))
  boe_mpr_atomic_rds(x, path)
  invisible(x)
}
boe_mpr_maintenance_error <- function(e, component, now) {
  list(component = component, class = class(e)[[1L]], message = conditionMessage(e), at = boe_mpr_utc(now))
}
boe_mpr_maintenance_backoff <- function(n, interval, maximum) interval * 2^min(max(n - 1L, 0L), 10L) |> min(maximum)
boe_mpr_maintenance_update <- function(data_dir, deadline, bypass_daily, retain_raw = FALSE) {
  boe_mpr_update_run(data_dir, deadline = deadline, stop_on_error = FALSE,
    bypass_daily = bypass_daily, retain_raw = retain_raw)
}
boe_mpr_maintenance_data <- function(data_dir) {
  x <- boe_mpr_load_saved(data_dir)
  list(editions = names(x$snapshot$editions), snapshot_sha256 = x$sha256,
    checked_at = boe_mpr_saved_provenance(x)$last_successful_check)
}
boe_mpr_maintenance_identity <- function(row) {
  x <- row[c("edition", "release_date", "publication_at", "landing_url")]
  attr(x, "boe_provenance") <- NULL; rownames(x) <- NULL
  digest::digest(x, algo = "sha256")
}
boe_mpr_maintenance_prune <- function(s, now, window) {
  for (key in names(s$releases)) {
    row <- s$calendar[s$calendar$edition == key, , drop = FALSE]
    if (!nrow(row) || !identical(s$releases[[key]]$identity, boe_mpr_maintenance_identity(row)) ||
        (key %in% s$known_editions && row$due_at + window < now)) {
      s$history <- tail(c(s$history, list(list(edition = key, closed_at = now,
        record = s$releases[[key]]))), 32L)
      s$releases[[key]] <- NULL
    }
  }
  s
}
boe_mpr_maintenance_probe <- function(release, data_dir, deadline, snapshot_sha256 = NULL) {
  entry <- boe_request_raw(release$landing_url, deadline = deadline, max_bytes = 20 * 1024^2,
    cache = FALSE, refresh = TRUE, validate_response = boe_validate_not_error_html)
  pending <- boe_mpr_publication_pending(entry)
  rel <- data.frame(release = release$edition, release_date = release$release_date,
    source_published_date = release$release_date, release_date_precision = "day", landing_url = release$landing_url)
  assets <- boe_mpr_assets_from_entry(rel, entry)
  assets <- assets[assets$asset_role %in% c("data_bundle", "numeric_workbook", "report_pdf"), , drop = FALSE]
  semantic <- assets[order(assets$url), c("url", "file_type", "asset_role"), drop = FALSE]
  attr(semantic, "boe_provenance") <- NULL; rownames(semantic) <- NULL
  fp <- digest::digest(list(edition = release$edition, date = release$release_date,
    published = !pending, assets = semantic), algo = "sha256")
  revision <- NULL
  # After publication, an available validator can cheaply reveal a corrected
  # primary asset. Never redownload unvalidated archives on every hourly probe.
  source_state <- boe_mpr_optional_rds(file.path(data_dir, "SOURCES.rds"))
  sources <- if (identical(source_state$snapshot_sha256, snapshot_sha256)) source_state$sources else NULL
  if (nrow(assets) && !pending) {
    order <- order(match(assets$asset_role, c("data_bundle", "numeric_workbook", "report_pdf")))
    url <- assets$url[order[[1L]]]; previous <- sources[[url]]
    has_validator <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
    if (!is.null(previous) && (has_validator(previous$etag) || has_validator(previous$last_modified))) {
      # This metadata probe is outside the numeric writer lock. Its response is
      # hashed in memory; only the locked updater may write a source working cache.
      probe <- boe_mpr_probe(url, previous, deadline, raw_dir = NULL)
      if (probe$changed) revision <- probe$source$sha256
    }
  }
  list(ready = !pending && nrow(assets) > 0L, fingerprint = fp, revision = revision,
    receipt = boe_receipt_from_entry(entry), assets = nrow(assets))
}
boe_mpr_maintenance_next_release <- function(s, now) {
  c <- s$calendar
  if (is.null(c) || !nrow(c)) return(NULL)
  unseen <- !c$edition %in% (s$known_editions %||% character())
  choose <- which(c$due_at > now | unseen)
  if (!length(choose)) return(NULL)
  c[choose[[which.min(as.numeric(c$due_at[choose]))]], , drop = FALSE]
}
boe_mpr_maintenance_public <- function(s, data_dir, now, status = NULL) {
  release <- boe_mpr_maintenance_next_release(s, now)
  errors <- unname(Filter(Negate(is.null), c(list(s$calendar_error, s$data_error), lapply(s$releases, `[[`, "error"))))
  list(schema = .boe_mpr_maintenance_schema, status = status %||% s$status,
    outcome = s$outcome, next_due_at = boe_mpr_utc(s$next_due_at),
    next_release_at = if (!is.null(release)) boe_mpr_utc(release$publication_at) else NULL,
    next_release_date = if (!is.null(release)) as.character(release$release_date) else NULL,
    next_release_edition = if (!is.null(release)) release$edition else NULL,
    next_release_time_known = !is.null(release) && !is.na(release$publication_at),
    next_release_poll_at = if (!is.null(release)) boe_mpr_utc(release$due_at) else NULL,
    calendar_checked_at = boe_mpr_utc(s$calendar_checked_at),
    calendar_next_check_at = boe_mpr_utc(s$next_calendar_at), calendar_error = s$calendar_error,
    calendar_fresh = !is.null(s$calendar_checked_at) && is.null(s$calendar_error) && now < s$next_calendar_at,
    calendar_horizon_exhausted = if (is.null(s$calendar)) NULL else is.null(release),
    revision_next_check_at = boe_mpr_utc(s$next_revision_at),
    updater_outcome = s$updater_outcome, snapshot_sha256 = s$snapshot_sha256,
    data_last_successful_check = boe_mpr_utc(s$data_checked_at),
    revision_overdue = !is.null(s$data_checked_at) && now >= s$data_checked_at + (s$check_interval %||% 86400),
    release_overdue_seconds = if (!is.null(release)) max(0, as.numeric(difftime(now, release$due_at, units = "secs"))) else 0,
    errors = errors, error = if (length(errors)) paste(vapply(errors, `[[`, character(1L), "message"), collapse = "; ") else NULL,
    history_events = length(s$history),
    data_dir = data_dir, state_path = file.path(data_dir, "MAINTENANCE.rds"))
}

#' Read persisted maintenance status without network access or data-table loading
#' @param data_dir Saved forecast data directory, shared with ordinary readers.
#' @inheritParams boe_mpr_calendar
#' @return A machine-readable list with UTC ISO8601 next_due_at and next-release fields.
#'   calendar_horizon_exhausted is NULL until a calendar is available, TRUE only
#'   for a known calendar with no remaining future or missing edition, and FALSE
#'   when a known upcoming or missed release remains.
#' @export
boe_mpr_maintenance_status <- function(data_dir = boe_mpr_data_dir(), now = Sys.time()) {
  boe_assert_scalar(data_dir, "data_dir", "character"); boe_mpr_maintenance_now(now)
  loaded <- boe_mpr_maintenance_read(data_dir); s <- loaded$state
  if (is.null(s)) s <- boe_mpr_maintenance_empty(now)
  if (loaded$recovered) { s$status <- "failed"; s$outcome <- "state_recovery_required"; s$next_due_at <- now }
  boe_mpr_maintenance_public(s, data_dir, now)
}

#' Maintain saved forecasts using official release dates and bounded due checks
#'
#' Suitable for an hourly external runner. Idle invocations read only small
#' maintenance metadata; no network, data-table load, subprocess or write occurs.
#' This function does not install or activate a scheduler and is never called by
#' package startup or ordinary readers. Dates/times are rediscovered from the
#' official calendar and publication listing, including after a release update.
#'
#' The schema-1 MAINTENANCE.rds state and its previous valid copy live beside
#' numeric snapshots. next_due_at is the earliest calendar refresh, daily
#' revision check, release metadata probe or retry. Explicit clocks are converted
#' from Europe/London to UTC; a known date with no official clock uses a labelled
#' local-date metadata poll boundary and keeps next_release_at unknown.
#' Defaults limit release-triggered full checks to four per rolling day and
#' repeat unchanged signals no sooner than six hours. Historical daily checks
#' remain independent. Interrupted attempts retain their retry journal.
#' @inheritParams boe_mpr_maintenance_status
#' @param deadline Total deadline for calendar, probe and update work.
#' @param force Refresh the calendar/evaluate due work now; does not force unconditional archive downloads.
#' @param check_interval Seconds between successful daily revision/calendar checks (default one day).
#' @param probe_interval Minimum seconds between release-window metadata probes.
#' @param retry_max Maximum metadata backoff and minimum repeat full-check delay for an unchanged release signal.
#' @param release_window Seconds to continue metadata probes after publication to catch late assets/revisions.
#' @param stop_on_error Raise a maintenance_failed condition after saving failed/retry state.
#' @param retain_raw Keep update source bodies after work (default FALSE); passed
#'   to the locked updater. Ordinary maintenance uses disposable bounded staging.
#' @return Status/outcome, next_due_at UTC ISO8601, next release/time/date/edition,
#' calendar freshness/errors and updater outcome. Updated data uses the same directory as readers.
#' @export
boe_mpr_maintain <- function(data_dir = boe_mpr_data_dir(), now = Sys.time(), deadline = 3600,
    force = FALSE, check_interval = 86400, probe_interval = 900, retry_max = 21600,
    release_window = 172800, stop_on_error = FALSE, retain_raw = FALSE) {
  boe_assert_scalar(data_dir, "data_dir", "character"); boe_mpr_maintenance_now(now)
  if (!nzchar(data_dir)) boe_abort("data_dir must be a nonempty path.", "config")
  for (name in c("force", "stop_on_error", "retain_raw")) boe_assert_scalar(get(name), name, "logical")
  for (name in c("check_interval", "probe_interval", "retry_max", "release_window")) boe_check_positive(get(name), name)
  if (retry_max < probe_interval) boe_abort("retry_max must be at least probe_interval.", "config")
  loaded <- boe_mpr_maintenance_read(data_dir); s <- loaded$state
  if (!force && !loaded$recovered && !is.null(s) && now < s$next_due_at) return(boe_mpr_maintenance_public(s, data_dir, now,
    status = if (s$status == "failed") "failed" else "idle"))
  deadline <- boe_as_deadline(deadline); boe_time_left(deadline)
  lock <- boe_mpr_lock(data_dir, name = ".maintenance-lock")
  if (is.null(lock)) {
    if (is.null(s)) s <- boe_mpr_maintenance_empty(now)
    s$outcome <- "busy"; s$next_due_at <- now + probe_interval
    return(boe_mpr_maintenance_public(s, data_dir, now, status = "waiting"))
  }
  on.exit(boe_mpr_unlock(lock), add = TRUE)
  loaded <- boe_mpr_maintenance_read(data_dir); s <- loaded$state
  if (is.null(s)) s <- boe_mpr_maintenance_empty(now)
  if (!force && !loaded$recovered && now < s$next_due_at) return(boe_mpr_maintenance_public(s, data_dir, now,
    status = if (s$status == "failed") "failed" else "idle"))
  if (loaded$recovered) s$next_calendar_at <- now
  s$check_interval <- check_interval
  refreshed <- FALSE; ran <- FALSE; waiting <- FALSE
  refresh_calendar <- function() {
    tryCatch({
      calendar <- boe_mpr_calendar(deadline, now)
      # Carry missed editions only across a year-horizon rollover, not a removed
      # date within a still-covered year (which may be a cancellation).
      old <- s$calendar
      if (!is.null(old) && nrow(old)) {
        carry <- !old$edition %in% calendar$edition & !old$edition %in% s$known_editions &
          as.integer(format(old$release_date, "%Y")) < min(as.integer(format(calendar$release_date, "%Y"))) & old$due_at <= now
        if (any(carry)) calendar <- boe_mpr_bind(list(old[carry, , drop = FALSE], calendar))
      }
      s$calendar <<- calendar; s$calendar_checked_at <<- now; s$next_calendar_at <<- now + check_interval
      s$calendar_error <<- NULL; s$calendar_failures <<- 0L; refreshed <<- TRUE
      TRUE
    }, error = function(e) {
      s$calendar_failures <<- s$calendar_failures + 1L
      s$calendar_error <<- boe_mpr_maintenance_error(e, "calendar", now)
      s$next_calendar_at <<- now + boe_mpr_maintenance_backoff(s$calendar_failures, probe_interval, retry_max)
      FALSE
    })
  }
  result <- tryCatch({
    data <- boe_mpr_maintenance_data(data_dir)
    s$known_editions <- data$editions; s$snapshot_sha256 <- data$snapshot_sha256; s$data_checked_at <- data$checked_at
    if (!isTRUE(s$data_initialized)) {
      s$next_revision_at <- max(now, data$checked_at + check_interval); s$data_initialized <- TRUE
    }
    # Recover a fully completed numeric commit after a maintenance-process crash.
    if (!is.null(s$in_progress) && data$checked_at >= s$in_progress$at) {
      s$next_revision_at <- data$checked_at + check_interval; s$data_error <- NULL
      key <- s$in_progress$edition
      if (!is.null(key) && key %in% data$editions) s$releases[[key]]$handled <- s$in_progress$fingerprint
      s$updater_outcome <- "recovered"; s$in_progress <- NULL
    } else if (!is.null(s$in_progress)) {
      s$data_error <- list(component = "maintenance", class = "interrupted",
        message = "A previous maintenance attempt did not complete; its retry limit was retained.", at = boe_mpr_utc(now))
    }
    if (force || now >= s$next_calendar_at || is.null(s$calendar)) refresh_calendar()
    if (!is.null(s$calendar)) s <- boe_mpr_maintenance_prune(s, now, release_window)
    calendar <- s$calendar; active <- NULL; record <- NULL; probe <- NULL; signal <- FALSE
    if (!is.null(calendar) && nrow(calendar)) {
      missing <- !calendar$edition %in% data$editions
      eligible <- which(calendar$due_at <= now & (missing | calendar$due_at + release_window >= now))
      if (length(eligible)) {
        unseen <- eligible[missing[eligible]]
        i <- if (length(unseen)) unseen[[which.min(calendar$due_at[unseen])]] else eligible[[which.max(calendar$due_at[eligible])]]
        active <- calendar[i, , drop = FALSE]; key <- active$edition
        release_identity <- boe_mpr_maintenance_identity(active)
        record <- s$releases[[key]]
        if (is.null(record) || !identical(record$identity, release_identity)) record <- list(identity = release_identity, next_probe_at = active$due_at, failures = 0L, attempts = numeric())
        if (now >= record$next_probe_at) {
          probe <- tryCatch(boe_mpr_maintenance_probe(active, data_dir, deadline, data$snapshot_sha256), error = function(e) e)
          if (inherits(probe, "error")) {
            record$failures <- record$failures + 1L; record$error <- boe_mpr_maintenance_error(probe, "release_probe", now)
            record$next_probe_at <- now + boe_mpr_maintenance_backoff(record$failures, probe_interval, retry_max)
            probe <- NULL
          } else {
            record$error <- NULL; record$failures <- 0L
            record$next_probe_at <- now + if (active$due_at + release_window < now) retry_max else probe_interval
            key_fp <- digest::digest(list(probe$fingerprint, probe$revision), algo = "sha256")
            changed <- !identical(record$handled, probe$fingerprint) || !is.null(probe$revision) || missing[[i]]
            same_attempt <- identical(record$attempt_key, key_fp)
            allowed <- !same_attempt || is.null(record$attempt_at) || now >= record$attempt_at + retry_max
            record$attempts <- record$attempts[record$attempts > as.numeric(now) - 86400]
            budget <- length(record$attempts) < 4L
            signal <- probe$ready && changed && allowed && budget
            if (probe$ready && changed && !budget) record$next_probe_at <- max(record$next_probe_at, as.POSIXct(min(record$attempts) + 86400, origin = "1970-01-01", tz = "UTC"))
            record$probe_fingerprint <- probe$fingerprint
            record$last_probe_receipt <- probe$receipt
          }
        }
        s$releases[[key]] <- record; waiting <- TRUE
      }
    }
    daily <- now >= s$next_revision_at
    # Historical revision checks remain independent of a late or unavailable
    # release. The updater keeps explicit unpublished placeholders scheduled;
    # an inaccessible/ambiguous source instead reports a truthful failed check.
    if (signal || daily) {
      bypass <- signal || !is.null(s$data_error) || !is.null(s$in_progress)
      s$in_progress <- list(at = now, edition = if (!is.null(active)) active$edition else NULL,
        fingerprint = if (!is.null(probe)) probe$fingerprint else NULL)
      s$status <- "waiting"; s$outcome <- "updater_in_progress"; s$updater_outcome <- "running"
      s$next_revision_at <- now + retry_max; s$next_due_at <- now + probe_interval
      if (signal) {
        key <- active$edition; record$attempt_at <- now
        record$attempt_key <- digest::digest(list(probe$fingerprint, probe$revision), algo = "sha256")
        record$attempts <- c(record$attempts, as.numeric(now)); s$releases[[key]] <- record
      }
      # Persist the attempt/rate limit before the expensive updater. A hard
      # process exit cannot cause another forced archive scan each hourly run.
      boe_mpr_maintenance_write(s, data_dir)
      update <- boe_mpr_maintenance_update(data_dir, deadline, bypass, retain_raw = retain_raw)
      s$updater_outcome <- update$status; s$last_revision_run_at <- now; ran <- TRUE
      if (update$status %in% c("updated", "unchanged")) {
        data <- boe_mpr_maintenance_data(data_dir)
        s$known_editions <- data$editions; s$snapshot_sha256 <- data$snapshot_sha256; s$data_checked_at <- data$checked_at
        s$next_revision_at <- now + check_interval; s$data_error <- NULL; s$data_failures <- 0L
        if (signal) { s$releases[[active$edition]]$handled <- probe$fingerprint; refresh_calendar() }
      } else if (update$status == "throttled") {
        s$next_revision_at <- max(now + probe_interval, (update$last_successful_check %||% now) + check_interval)
      } else if (update$status == "busy") {
        s$next_revision_at <- now + probe_interval
      } else {
        s$data_failures <- s$data_failures + 1L
        s$data_error <- list(component = "updater", class = update$error_class %||% "update_failed",
          message = update$error %||% paste("Updater returned", update$status), at = boe_mpr_utc(now))
        s$next_revision_at <- now + retry_max
      }
      s$in_progress <- NULL
    }
    # Successful release ingestion recomputes the upcoming event from refreshed
    # official rows; probes for the just-published edition may continue briefly.
    if (!is.null(s$calendar)) s <- boe_mpr_maintenance_prune(s, now, release_window)
    candidates <- c(s$next_calendar_at, s$next_revision_at)
    if (!is.null(s$calendar) && nrow(s$calendar)) {
      future <- s$calendar$due_at[s$calendar$due_at > now]
      if (length(future)) candidates <- c(candidates, min(future))
      for (key in intersect(names(s$releases), s$calendar$edition)) {
        row <- s$calendar[s$calendar$edition == key, , drop = FALSE]
        if (row$due_at <= now && (row$due_at + release_window >= now || !key %in% s$known_editions)) candidates <- c(candidates, s$releases[[key]]$next_probe_at)
      }
    }
    s$next_due_at <- min(candidates); s$last_run_at <- now
    errors <- Filter(Negate(is.null), c(list(s$calendar_error, s$data_error), lapply(s$releases, `[[`, "error")))
    s$status <- if (length(errors)) "failed" else if (ran && s$updater_outcome == "updated") "success" else if (ran) "unchanged" else if (waiting) "waiting" else if (refreshed) "success" else "idle"
    s$outcome <- if (ran) paste0("updater_", s$updater_outcome) else if (waiting) "release_metadata_wait" else if (refreshed) "calendar_refreshed" else "not_due"
    boe_mpr_maintenance_write(s, data_dir)
    boe_mpr_maintenance_public(s, data_dir, now)
  }, error = function(e) {
    s$status <- "failed"; s$outcome <- "maintenance_failed"
    s$data_error <- boe_mpr_maintenance_error(e, "maintenance", now)
    s$next_due_at <- now + retry_max; s$next_revision_at <- max(s$next_revision_at, now + retry_max)
    boe_mpr_maintenance_write(s, data_dir)
    boe_mpr_maintenance_public(s, data_dir, now)
  }, interrupt = function(e) {
    s$status <- "failed"; s$outcome <- "interrupted"; s$next_due_at <- now + retry_max
    s$data_error <- boe_mpr_maintenance_error(e, "interrupt", now)
    tryCatch(boe_mpr_maintenance_write(s, data_dir), error = function(e) NULL)
    stop(e)
  })
  if (stop_on_error && result$status == "failed") boe_abort(result$error %||% "Forecast maintenance failed.", "maintenance_failed", result = result)
  result
}
