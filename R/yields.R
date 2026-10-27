#' List official fitted yield-curve file families
#'
#' Returns a static registry of the exact latest and archive ZIP families used
#' by [read_boe_yield_curve()].  It does not download files or promise that an
#' archived file has not subsequently been revised upstream.
#'
#' @return A deterministic 11-row data frame with columns `asset_id`, `scope`,
#'   `curve`, `frequency`, `url`, and `archive_revisable`: one
#'   `latest_daily` all-curves bundle plus nominal, real, inflation, OIS, and
#'   BLC daily/monthly archive families.  `scope = "latest"` is daily only and
#'   BLC is archive-only.  URLs are references and no provenance/network/cache
#'   side effect is produced.
#' @seealso [read_boe_yield_curve()], [boe_rights()]
#' @examples
#' assets <- boe_yield_assets()
#' stopifnot(nrow(assets) == 11L,
#'           identical(names(assets), c("asset_id", "scope", "curve",
#'             "frequency", "url", "archive_revisable")),
#'           assets$scope[[1L]] == "latest")
#' @export
boe_yield_assets <- function() {
  base <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/yield-curves"
  data.frame(
    asset_id = c("latest_daily", "nominal_daily", "real_daily", "inflation_daily", "blc_daily", "ois_daily",
      "nominal_monthly", "real_monthly", "inflation_monthly", "blc_monthly", "ois_monthly"),
    scope = c("latest", rep("archive", 10L)),
    curve = c("all", "nominal", "real", "inflation", "blc", "ois", "nominal", "real", "inflation", "blc", "ois"),
    frequency = c(rep("daily", 6L), rep("monthly", 5L)),
    url = paste0(base, "/", c("latest-yield-curve-data.zip", "glcnominalddata.zip", "glcrealddata.zip",
      "glcinflationddata.zip", "blcnomddata.zip", "oisddata.zip", "glcnominalmonthedata.zip",
      "glcrealmonthedata.zip", "glcinflationmonthedata.zip", "blcnominalmonthedata.zip", "oismonthedata.zip")),
    archive_revisable = c(TRUE, rep(TRUE, 10L)),
    stringsAsFactors = FALSE
  )
}

boe_yield_member_pattern <- function(curve, frequency) {
  curve_pattern <- switch(curve,
    nominal = "GLC Nominal", real = "GLC Real", inflation = "GLC Inflation",
    ois = "OIS", blc = "BLC Nominal")
  frequency_pattern <- if (frequency == "daily") "daily" else "month end"
  list(curve = curve_pattern, frequency = frequency_pattern)
}

boe_yield_sheet_pattern <- function(measure, segment) {
  if (segment == "short") {
    if (measure == "spot") "spot,? short end$" else "fwds?,? short end$"
  } else {
    if (measure == "spot") "spot curve$" else "fwd curve$"
  }
}

boe_detect_maturity_row <- function(raw) {
  candidates <- intersect(c(4L, 3L, 5L, 6L), seq_len(nrow(raw)))
  for (r in candidates) {
    values <- suppressWarnings(as.numeric(unlist(raw[r, -1L, drop = FALSE], use.names = FALSE)))
    values <- values[!is.na(values)]
    if (length(values) >= 5L && all(values > 0) && all(diff(utils::head(values, 10L)) >= 0)) return(r)
  }
  NA_integer_
}

boe_parse_yield_workbook <- function(path, member, measure, segment, max_cells, max_output_rows) {
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) boe_abort("A yield workbook cannot be inspected.", "parse", member = member))
  pattern <- boe_yield_sheet_pattern(measure, segment)
  hits <- grep(pattern, trimws(sheets), ignore.case = TRUE)
  if (!length(hits)) return(NULL)
  if (length(hits) > 1L) boe_abort("A yield workbook has duplicate matching curve sheets.", "schema_drift", member = member)
  sheet <- sheets[[hits]]
  probe <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE,
    n_max = 6L, .name_repair = "minimal"))
  probe <- as.data.frame(probe, stringsAsFactors = FALSE)
  if (nrow(probe) < 6L || ncol(probe) < 2L || ncol(probe) > max_cells) {
    boe_abort("A yield worksheet violates size/schema bounds.", "bounds", member = member, sheet = sheet)
  }
  bounded_rows <- floor(max_cells / ncol(probe))
  raw <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE,
    n_max = bounded_rows + 1L, .name_repair = "minimal"))
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (nrow(raw) < 6L || ncol(raw) < 2L || nrow(raw) > bounded_rows || nrow(raw) * ncol(raw) > max_cells) {
    boe_abort("A yield worksheet violates size/schema bounds.", "bounds", member = member, sheet = sheet)
  }
  maturity_row <- boe_detect_maturity_row(raw)
  if (is.na(maturity_row)) boe_abort("A yield worksheet has no stable maturity row.", "schema_drift", member = member, sheet = sheet)
  maturities_all <- suppressWarnings(as.numeric(unlist(raw[maturity_row, -1L, drop = FALSE], use.names = FALSE)))
  good_columns <- !is.na(maturities_all) & maturities_all > 0
  maturities <- maturities_all[good_columns]
  body_start <- maturity_row + 2L
  if (body_start > nrow(raw)) boe_abort("A yield worksheet has no observation body.", "schema_drift")
  body <- raw[seq.int(body_start, nrow(raw)), c(TRUE, good_columns), drop = FALSE]
  date_source <- body[[1L]]
  if (inherits(date_source, "POSIXt") || inherits(date_source, "Date")) {
    dates <- as.Date(date_source)
  } else {
    serial <- suppressWarnings(as.numeric(date_source))
    dates <- as.Date(serial, origin = "1899-12-30")
    character_dates <- is.na(dates) & !is.na(date_source)
    if (any(character_dates)) dates[character_dates] <- suppressWarnings(as.Date(as.character(date_source[character_dates])))
  }
  data_rows <- which(!is.na(dates))
  if (!length(data_rows)) return(NULL)
  body <- body[data_rows, , drop = FALSE]
  dates <- dates[data_rows]
  value_frame <- body[-1L]
  rows <- length(dates) * length(maturities)
  if (rows > max_output_rows) boe_abort("A yield worksheet exceeds the output-row bound.", "bounds", member = member)
  rate_raw <- unlist(lapply(value_frame, as.character), use.names = FALSE)
  rate <- suppressWarnings(as.numeric(rate_raw))
  data.frame(
    date = rep(dates, times = length(maturities)),
    maturity_years = rep(maturities, each = length(dates)),
    rate_raw = rate_raw,
    rate = rate,
    source_member = member,
    source_sheet = sheet,
    source_row = rep(data_rows + body_start - 1L, times = length(maturities)),
    source_column = rep(which(good_columns) + 1L, each = length(dates)),
    stringsAsFactors = FALSE
  )
}

#' Read an official fitted yield-curve family
#'
#' Downloads and parses one exact official fitted yield-curve ZIP family.
#' Official missing values and duplicate dates are retained.  The tuple
#' `source_member`, `source_sheet`, `source_row`, and `source_column` is the
#' stable observation identity; no duplicate collapse or imputation occurs.
#'
#' @param curve Character choice `"nominal"`, `"real"`, `"inflation"`,
#'   `"ois"`, or discontinued `"blc"`.
#' @param frequency Character choice `"daily"` (default) or `"monthly"`.
#' @param scope Character choice `"archive"` (default) or `"latest"`; the
#'   latest bundle is daily only and has no BLC family.
#' @param measure Character choice `"spot"` (default) or `"forward"`.
#' @param segment Character choice `"standard"` (default) or `"short"` for
#'   the separately fitted short end.
#' @param from Optional inclusive start date, applied after parsing.
#' @param to Optional inclusive end date, applied after parsing.
#' @param deadline A caller-owned absolute deadline, `POSIXct`, positive number
#'   of seconds, or `NULL` for 60 seconds.
#' @param max_bytes Positive compressed-response byte bound (default 75 MiB).
#' @param max_archive_members Positive integer ZIP member bound (default 50).
#' @param max_uncompressed_bytes Positive extracted-byte bound (default 500
#'   MiB).
#' @param max_cells Positive integer per-workbook cell bound (default 5e6).
#' @param max_output_rows Positive integer parsed-output bound (default 2e6).
#' @param cache Whether to use the managed response cache (default `TRUE`).
#' @param cache_only If `TRUE`, require a matching cache entry and never use
#'   the network; a miss signals `readboe_cache_miss`.
#' @param refresh If `TRUE`, bypass an existing cache entry.  It cannot be
#'   combined with `cache_only = TRUE`.
#' @param cache_dir Cache directory; defaults to
#'   `tools::R_user_dir("readboe", "cache")`.
#' @return A plain long data frame with columns `date` (`Date`),
#'   `maturity_years` (numeric), `rate_raw` (character), `rate` (numeric),
#'   `source_member`, `source_sheet`, integer `source_row`, and integer
#'   `source_column`.  A `boe_provenance` attribute identifies the selected
#'   asset, curve, frequency, scope, measure, segment, and members.
#' @details `from` and `to` filtering is inclusive and occurs after all ZIP and
#'   workbook parsing.  `scope = "latest"` with monthly frequency or BLC is a
#'   `readboe_config` error.  Requests use allowlisted HTTPS, bounded
#'   redirects/retries/deadlines/bytes/members/cells/rows, and temporary files
#'   that are removed on exit.  Explicit package validation, schema, identity,
#'   transport, cache, timeout, and bounds failures signal typed `readboe_*`
#'   conditions.  An underlying `readxl` error may propagate if a workbook
#'   cannot be decoded.  The source's NAs and duplicate observations are
#'   intentionally preserved.
#' @seealso [boe_yield_assets()], [boe_download_asset()], [boe_provenance()],
#'   [boe_rights()]
#' @examples
#' ## Incompatible scope/frequency is validated without network access.
#' bad <- tryCatch(
#'   read_boe_yield_curve(scope = "latest", frequency = "monthly"),
#'   error = function(error) error
#' )
#' stopifnot(inherits(bad, "readboe_config"))
#' if (identical(Sys.getenv("READBOE_LIVE_EXAMPLES"), "true")) {
#'   curves <- read_boe_yield_curve(curve = "nominal", frequency = "daily",
#'     scope = "latest", from = "2024-01-01", to = "2024-01-02",
#'     deadline = boe_deadline(30))
#'   stopifnot(is.data.frame(curves),
#'     all(c("date", "maturity_years", "rate_raw", "rate") %in% names(curves)))
#' }
#' @export
read_boe_yield_curve <- function(curve = c("nominal", "real", "inflation", "ois", "blc"),
                                 frequency = c("daily", "monthly"), scope = c("archive", "latest"),
                                 measure = c("spot", "forward"), segment = c("standard", "short"),
                                 from = NULL, to = NULL, deadline = NULL, max_bytes = 75 * 1024^2,
                                 max_archive_members = 50L, max_uncompressed_bytes = 500 * 1024^2,
                                 max_cells = 5e6, max_output_rows = 2e6,
                                 cache = TRUE, cache_only = FALSE, refresh = FALSE,
                                 cache_dir = boe_default_cache_dir()) {
  curve <- match.arg(curve); frequency <- match.arg(frequency); scope <- match.arg(scope)
  measure <- match.arg(measure); segment <- match.arg(segment)
  if (scope == "latest" && frequency != "daily") boe_abort("The latest bundle contains daily files only.", "config")
  if (scope == "latest" && curve == "blc") boe_abort("BLC is available only from its archive.", "config")
  assets <- boe_yield_assets()
  idx <- if (scope == "latest") which(assets$asset_id == "latest_daily") else which(assets$scope == "archive" & assets$curve == curve & assets$frequency == frequency)
  if (length(idx) != 1L) boe_abort("The yield asset identity is not unique.", "identity_mismatch")
  raw <- boe_download_asset(assets$url[[idx]], deadline, max_bytes, cache, cache_only, refresh, cache_dir)
  provenance <- boe_provenance(raw)
  zip_path <- boe_write_raw_temp(raw, ".zip")
  on.exit(unlink(zip_path), add = TRUE)
  listing <- boe_safe_zip_members(zip_path, max_archive_members, max_uncompressed_bytes)
  pat <- boe_yield_member_pattern(curve, frequency)
  members <- listing$Name[grepl(pat$curve, listing$Name, fixed = TRUE) &
    grepl(pat$frequency, listing$Name, ignore.case = TRUE) & grepl("\\.xlsx?$", listing$Name, ignore.case = TRUE)]
  if (!length(members)) boe_abort("No exact yield workbook members match the requested family.", "schema_drift")
  paths <- boe_extract_members(zip_path, members, max_archive_members, max_uncompressed_bytes)
  on.exit(unlink(attr(paths, "extraction_dir"), recursive = TRUE), add = TRUE)
  remaining <- max_output_rows
  parts <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    part <- boe_parse_yield_workbook(paths[[i]], members[[i]], measure, segment, max_cells, remaining)
    parts[[i]] <- part
    if (!is.null(part)) remaining <- remaining - nrow(part)
  }
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) boe_abort("No requested yield-curve sheets exist in the official family.", "schema_drift")
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  if (!is.null(from)) {
    from <- as.Date(from); if (length(from) != 1L || is.na(from)) boe_abort("`from` must be a valid date.", "config")
    out <- out[out$date >= from, , drop = FALSE]
  }
  if (!is.null(to)) {
    to <- as.Date(to); if (length(to) != 1L || is.na(to)) boe_abort("`to` must be a valid date.", "config")
    out <- out[out$date <= to, , drop = FALSE]
  }
  if (!is.null(from) && !is.null(to) && from > to) boe_abort("`from` must not be later than `to`.", "config")
  attr(out, "boe_provenance") <- c(provenance, list(
    source_identity = list(surface = "yield_curve", asset_id = assets$asset_id[[idx]], curve = curve,
      frequency = frequency, scope = scope, measure = measure, segment = segment, members = members),
    transforms = c("ZIP and workbook syntax parsed", "optional explicit date filtering; no NA or duplicate removal")
  ))
  out
}
