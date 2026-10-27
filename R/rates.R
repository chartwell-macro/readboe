.boe_bank_rate_url <- "https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy/baserate.xls"

boe_parse_bank_rate <- function(raw, max_rows = 5000L) {
  extension <- if (length(raw) >= 2L && identical(as.integer(raw[1:2]), c(80L, 75L))) ".xlsx" else ".xls"
  path <- boe_write_raw_temp(raw, extension)
  on.exit(unlink(path), add = TRUE)
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) boe_abort("The Bank Rate workbook cannot be inspected.", "parse"))
  sheet <- "HISTORICAL SINCE 1694"
  if (!sheet %in% sheets) boe_abort("The Bank Rate historical sheet is absent.", "schema_drift")
  x <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE,
    n_max = max_rows + 1L, .name_repair = "minimal"))
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (nrow(x) > max_rows || ncol(x) < 4L) boe_abort("The Bank Rate workbook violates its row/column contract.", "bounds")
  x <- x[seq.int(8L, nrow(x)), seq_len(4L), drop = FALSE]
  names(x) <- c("year_raw", "day_raw", "month_raw", "rate_raw")
  year_marker <- suppressWarnings(as.integer(x$year_raw))
  year <- rep(NA_integer_, nrow(x))
  current <- NA_integer_
  for (i in seq_len(nrow(x))) {
    if (!is.na(year_marker[[i]]) && year_marker[[i]] >= 1600L && year_marker[[i]] <= 2200L) current <- year_marker[[i]]
    year[[i]] <- current
  }
  rate <- suppressWarnings(as.numeric(x$rate_raw))
  day <- suppressWarnings(as.integer(x$day_raw))
  month_names <- c(Jan = 1L, Feb = 2L, Mar = 3L, Apr = 4L, May = 5L, Jun = 6L,
    Jul = 7L, Aug = 8L, Sep = 9L, Oct = 10L, Nov = 11L, Dec = 12L)
  month_text <- trimws(as.character(x$month_raw))
  month <- unname(month_names[month_text])
  keep <- !is.na(year) & !is.na(month) & !is.na(rate)
  year <- year[keep]; day <- day[keep]; month <- month[keep]; rate <- rate[keep]
  source_row <- which(keep) + 7L
  date <- as.Date(rep(NA_character_, length(year)))
  exact <- !is.na(day)
  date[exact] <- as.Date(sprintf("%04d-%02d-%02d", year[exact], month[exact], day[exact]))
  period <- sprintf("%04d-%02d%s", year, month, ifelse(exact, sprintf("-%02d", day), ""))
  out <- data.frame(
    period = period,
    date = date,
    date_precision = ifelse(exact, "day", "month"),
    year = year,
    month = month,
    day = day,
    rate_raw = as.character(x$rate_raw[keep]),
    rate = rate,
    source_sheet = sheet,
    source_row = source_row,
    stringsAsFactors = FALSE
  )
  if (!nrow(out) || anyDuplicated(out[c("period", "source_row")])) boe_abort("Bank Rate history has an invalid identity contract.", "schema_drift")
  out
}

#' Read official Bank Rate history from 1694
#'
#' Downloads and parses the official `baserate.xls` workbook.  Early
#' observations published only to month precision retain `date = NA`, their
#' exact official `period`, and `date_precision = "month"`; no day is inferred.
#'
#' @param deadline A caller-owned absolute deadline, `POSIXct`, positive number
#'   of seconds, or `NULL` for 60 seconds.
#' @param max_bytes Positive response byte bound (default 10 MiB).
#' @param cache Whether to read and write the managed cache (default `TRUE`).
#' @param cache_only If `TRUE`, require a matching cached workbook and never
#'   contact the network; a miss signals `readboe_cache_miss`.
#' @param refresh If `TRUE`, bypass an existing cache entry.  It cannot be
#'   combined with `cache_only = TRUE`.
#' @param cache_dir Cache directory; defaults to
#'   `tools::R_user_dir("readboe", "cache")`.
#' @return A data frame with 10 columns: `period`, `date` (`Date`),
#'   `date_precision` (`"month"` or `"day"`), `year`, `month`, `day`,
#'   `rate_raw`, `rate`, `source_sheet`, and integer `source_row`.  Raw rate
#'   text and numeric conversion are both retained.  A `boe_provenance`
#'   attribute identifies the official workbook and historical sheet.
#' @details The workbook is fetched over allowlisted HTTPS with bounded
#'   redirects/retries/deadlines/bytes and parsed through a temporary file that
#'   is removed on exit.  Only successful responses are cached.  Invalid
#'   options, cache failures, transport/rate-limit/timeout errors, malformed
#'   workbook structure, schema drift, and bounds violations signal typed
#'   `readboe_*` conditions.
#' @seealso [boe_rate_series()], [boe_download_asset()], [boe_provenance()]
#' @examples
#' ## This validates a bound without downloading the workbook.
#' bad <- tryCatch(boe_bank_rate_history(max_bytes = 0),
#'   error = function(error) error)
#' stopifnot(inherits(bad, "readboe_config"))
#' if (identical(Sys.getenv("READBOE_LIVE_EXAMPLES"), "true")) {
#'   rates <- boe_bank_rate_history(deadline = boe_deadline(20))
#'   stopifnot(is.data.frame(rates),
#'     identical(names(rates), c("period", "date", "date_precision", "year",
#'       "month", "day", "rate_raw", "rate", "source_sheet", "source_row")))
#' }
#' @export
boe_bank_rate_history <- function(deadline = NULL, max_bytes = 10 * 1024^2,
                                  cache = TRUE, cache_only = FALSE, refresh = FALSE,
                                  cache_dir = boe_default_cache_dir()) {
  raw <- boe_download_asset(.boe_bank_rate_url, deadline, max_bytes, cache, cache_only, refresh, cache_dir)
  out <- boe_parse_bank_rate(raw)
  provenance <- boe_provenance(raw)
  attr(out, "boe_provenance") <- c(provenance, list(
    source_identity = list(surface = "bank_rate_history", url = .boe_bank_rate_url,
      sheet = "HISTORICAL SINCE 1694"),
    transforms = "Workbook syntax parsed; month-only dates remain month precision"
  ))
  out
}

#' List official IADB policy and benchmark rate series
#'
#' Returns a static convenience registry of exact identifiers used for policy
#' and benchmark rates.  It does not fetch observations and should be paired
#' with [read_boe_series()] when values are required.
#'
#' @return A deterministic 5-row data frame with columns `series_id`, `label`,
#'   and `surface`.  The identifiers are `IUDBEDR` (Official Bank Rate),
#'   `IUMABEDR` (monthly average), `IUDSOIA` (SONIA), `IUMASOIA` (SONIA monthly
#'   average), and `IUAASOIA` (SONIA annual average).  No provenance attribute,
#'   network request, or cache write is produced.
#' @seealso [read_boe_series()], [boe_series_metadata()], [boe_rights()]
#' @examples
#' rates <- boe_rate_series()
#' stopifnot(identical(dim(rates), c(5L, 3L)),
#'           identical(names(rates), c("series_id", "label", "surface")),
#'           "IUDBEDR" %in% rates$series_id)
#' @export
boe_rate_series <- function() {
  data.frame(
    series_id = c("IUDBEDR", "IUMABEDR", "IUDSOIA", "IUMASOIA", "IUAASOIA"),
    label = c("Official Bank Rate", "Official Bank Rate monthly average", "SONIA",
      "SONIA monthly average", "SONIA annual average"),
    surface = c("iadb;bank_rate_workbook", rep("iadb", 4L)),
    stringsAsFactors = FALSE
  )
}
