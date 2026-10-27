boe_write_raw_temp <- function(raw, extension = "") {
  if (!is.raw(raw)) boe_abort("A downloaded asset is not a raw byte vector.", "parse")
  path <- tempfile("readboe-asset-", fileext = extension)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(as.raw(raw), con)
  path
}

boe_safe_zip_members <- function(path, max_members = 100L, max_uncompressed_bytes = 500 * 1024^2) {
  boe_check_positive(max_members, "max_members", integer = TRUE)
  boe_check_positive(max_uncompressed_bytes, "max_uncompressed_bytes", integer = TRUE)
  members <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) boe_abort("The ZIP central directory cannot be parsed.", "parse"))
  if (!nrow(members) || nrow(members) > max_members) boe_abort("The ZIP member count is empty or exceeds its bound.", "bounds")
  names <- members$Name
  # Some older official ZIPs contain non-UTF-8 names for ancillary documents.
  # Validate ASCII path syntax bytewise and preserve exact extraction names.
  # A separate valid string is used only for the conservative case-alias check.
  case_names <- names
  invalid <- !is.na(names) & !validUTF8(names)
  case_names[invalid] <- iconv(names[invalid], from = "latin1", to = "UTF-8")
  if (anyNA(names) || any(!nzchar(names)) || anyDuplicated(tolower(case_names)) ||
      any(grepl("\\\\", names, useBytes = TRUE)) ||
      any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.?(/|$)|//)", names, useBytes = TRUE))) {
    boe_abort("The ZIP contains an unsafe or duplicate member path.", "bounds")
  }
  sizes <- suppressWarnings(as.numeric(members$Length))
  if (anyNA(sizes) || any(sizes < 0) || sum(sizes) > max_uncompressed_bytes) {
    boe_abort("The ZIP uncompressed size exceeds its bound or is invalid.", "bounds")
  }
  members$Name <- names
  members
}

boe_extract_members <- function(zip_path, members, max_members = 100L, max_uncompressed_bytes = 500 * 1024^2) {
  listing <- boe_safe_zip_members(zip_path, max_members, max_uncompressed_bytes)
  if (!is.character(members) || !length(members) || anyNA(members) ||
      anyDuplicated(members) || any(!members %in% listing$Name) || any(grepl("/$", members))) {
    boe_abort("Requested ZIP members must be unique exact file entries.", "identity_mismatch")
  }
  out_dir <- tempfile("readboe-unzip-")
  dir.create(out_dir)
  on_error <- TRUE
  on.exit(if (on_error) unlink(out_dir, recursive = TRUE), add = TRUE)
  paths <- file.path(out_dir, members)
  # Stream selected entries into new regular files. Archive symlink attributes
  # are never applied, and central-directory sizes are checked against bytes read.
  for (i in seq_along(members)) {
    size <- listing$Length[match(members[[i]], listing$Name)]
    con <- unz(zip_path, members[[i]], open = "rb")
    bytes <- tryCatch(readBin(con, "raw", n = size + 1), finally = close(con))
    if (length(bytes) != size) boe_abort("ZIP member size disagrees with its directory.", "bounds")
    dir.create(dirname(paths[[i]]), recursive = TRUE, showWarnings = FALSE)
    dest <- file(paths[[i]], open = "wb")
    tryCatch(writeBin(bytes, dest), finally = close(dest))
  }
  if (any(!file.exists(paths))) boe_abort("A ZIP member could not be extracted.", "parse")
  extracted_sizes <- file.info(paths)$size
  if (anyNA(extracted_sizes) || any(extracted_sizes < 0) || sum(extracted_sizes) > max_uncompressed_bytes) {
    boe_abort("Extracted ZIP members exceed the uncompressed-byte bound.", "bounds")
  }
  on_error <- FALSE
  structure(paths, extraction_dir = out_dir)
}

boe_read_workbook_raw <- function(raw, extension, max_sheets = 50L, max_rows = 1e6,
                                  max_columns = 2000L, max_cells = 5e6,
                                  sheets = NULL, deadline = NULL, col_types = NULL,
                                  dimensions = NULL) {
  boe_check_positive(max_sheets, "max_sheets", integer = TRUE)
  boe_check_positive(max_rows, "max_rows", integer = TRUE)
  boe_check_positive(max_columns, "max_columns", integer = TRUE)
  boe_check_positive(max_cells, "max_cells", integer = TRUE)
  if (!is.null(deadline)) deadline <- boe_as_deadline(deadline)
  check_deadline <- function() if (!is.null(deadline)) boe_time_left(deadline)
  check_deadline()
  path <- boe_write_raw_temp(raw, extension)
  on.exit(unlink(path), add = TRUE)
  available <- tryCatch(readxl::excel_sheets(path), error = function(e) boe_abort(paste("Could not inspect workbook:", conditionMessage(e)), "parse"))
  check_deadline()
  if (!length(available) || length(available) > max_sheets || anyDuplicated(available)) boe_abort("Workbook sheets violate the sheet identity bound.", "bounds")
  if (is.null(sheets)) sheets <- available
  if (!is.character(sheets) || !length(sheets) || anyNA(sheets) || anyDuplicated(sheets) ||
      any(!sheets %in% available)) boe_abort("Workbook selection must contain unique exact sheet names.", "identity_mismatch")
  if (!is.null(dimensions)) {
    if (!is.data.frame(dimensions) || !all(c("source_sheet", "rows", "columns") %in% names(dimensions)) ||
        anyNA(dimensions) || anyDuplicated(dimensions$source_sheet) ||
        !all(sheets %in% dimensions$source_sheet) ||
        !is.numeric(dimensions$rows) || !is.numeric(dimensions$columns) ||
        any(!is.finite(dimensions$rows) | dimensions$rows < 1 | dimensions$rows != floor(dimensions$rows)) ||
        any(!is.finite(dimensions$columns) | dimensions$columns < 1 | dimensions$columns != floor(dimensions$columns))) {
      boe_abort("Preflight dimensions must identify unique, bounded sheet sizes.", "config")
    }
    if (any(dimensions$rows > max_rows) || any(dimensions$columns > max_columns)) {
      boe_abort("Preflight worksheet dimensions exceed workbook bounds.", "bounds")
    }
  }
  out <- stats::setNames(vector("list", length(sheets)), sheets)
  cells <- 0
  for (i in seq_along(sheets)) {
    check_deadline()
    if (is.null(dimensions)) {
      probe <- tryCatch(suppressMessages(readxl::read_excel(path, sheet = sheets[[i]], col_names = FALSE, n_max = 1L, col_types = col_types)),
        error = function(e) boe_abort(paste("Could not parse workbook sheet:", conditionMessage(e)), "parse", sheet = sheets[[i]]))
      width <- ncol(probe)
      height <- max_rows
    } else {
      idx <- match(sheets[[i]], dimensions$source_sheet)
      width <- dimensions$columns[[idx]]
      height <- dimensions$rows[[idx]]
    }
    if (width < 1L || width > max_columns) {
      boe_abort("A workbook sheet exceeded column bounds or was empty.", "bounds", sheet = sheets[[i]])
    }
    remaining_cells <- max_cells - cells
    bounded_rows <- min(max_rows, height, floor(remaining_cells / width))
    if (bounded_rows < 1L) boe_abort("Workbook cells exceeded `max_cells`.", "bounds")
    sheet <- tryCatch(suppressMessages(readxl::read_excel(path, sheet = sheets[[i]], col_names = FALSE,
      n_max = bounded_rows + 1L, col_types = col_types)),
      error = function(e) boe_abort(paste("Could not parse workbook sheet:", conditionMessage(e)), "parse", sheet = sheets[[i]]))
    sheet <- as.data.frame(sheet, stringsAsFactors = FALSE, check.names = FALSE)
    if (nrow(sheet) > bounded_rows || ncol(sheet) > max_columns) {
      boe_abort("A workbook sheet exceeded row, column, or cell bounds.", "bounds", sheet = sheets[[i]])
    }
    cells <- cells + nrow(sheet) * ncol(sheet)
    if (cells > max_cells) boe_abort("Workbook cells exceeded `max_cells`.", "bounds")
    out[[i]] <- sheet
    check_deadline()
  }
  structure(out, class = c("boe_workbook", "list"))
}

#' Download an exact official machine-readable asset
#'
#' Downloads one caller-selected HTTPS asset from an allowlisted Bank of
#' England host.  The function is intentionally byte-preserving: it does not
#' guess a file format, parse a workbook, or alter the downloaded bytes.
#'
#' @param url Exact official HTTPS URL.  It must use `www.bankofengland.co.uk`
#'   or `bankofengland.co.uk`; arbitrary URLs and HTTP are rejected.
#' @param deadline A caller-owned absolute deadline created by
#'   [boe_deadline()], a single `POSIXct`, a positive number of seconds, or
#'   `NULL` for the default 60-second budget.  The same budget covers retries
#'   and response handling.
#' @param max_bytes Positive finite byte limit for the response.  Oversized
#'   responses fail closed with `readboe_bounds`.
#' @param cache Whether to read and write the managed response cache.
#' @param cache_only If `TRUE`, require an exact fresh or stored matching cache
#'   entry and never make a network request.  A miss signals
#'   `readboe_cache_miss`.
#' @param refresh If `TRUE`, bypass a cached entry and fetch a fresh response.
#'   It cannot be combined with `cache_only = TRUE`.
#' @param cache_dir Cache directory used when `cache` is enabled.  The default
#'   is `tools::R_user_dir("readboe", "cache")`.
#' @return A raw byte vector.  It has no custom S3 class, but carries a
#'   `boe_provenance` attribute containing `source_identity`, `canonical_url`,
#'   `final_url`, `retrieved_at`, `raw_bytes`, `raw_sha256`, HTTP validators,
#'   `from_cache`, `transforms`, package name, and package version.
#' @details Requests use HTTPS, bounded redirects, per-attempt timeouts and
#'   bounded retries for transient failures.  Error pages, non-2xx responses,
#'   rate limits, invalid URLs, expired deadlines, and malformed cache entries
#'   signal typed `readboe_*` conditions (including `readboe_config`,
#'   `readboe_transport`, `readboe_rate_limit`, `readboe_timeout`,
#'   `readboe_bounds`, `readboe_cache_miss`, and
#'   `readboe_cache_corruption`).  Temporary state is removed on exit; only a
#'   successful response may be committed to the managed cache.  The cache
#'   default TTL for an asset is 30 days.
#' @seealso [boe_provenance()], [boe_deadline()], [read_boe_bankstats()],
#'   [read_boe_release_asset()]
#' @examples
#' ## URL validation is deterministic and does not contact the network.
#' bad <- tryCatch(
#'   boe_download_asset("http://example.invalid/data.csv"),
#'   error = function(error) error
#' )
#' stopifnot(inherits(bad, "readboe_config"))
#' ## Opt in explicitly to a live, bounded example when desired.
#' if (identical(Sys.getenv("READBOE_LIVE_EXAMPLES"), "true")) {
#'   bytes <- boe_download_asset(
#'     "https://www.bankofengland.co.uk/-/media/boe/files/monetary-policy/baserate.xls",
#'     deadline = boe_deadline(10), max_bytes = 10 * 1024^2
#'   )
#'   stopifnot(is.raw(bytes), !is.null(boe_provenance(bytes)))
#' }
#' @export
boe_download_asset <- function(url, deadline = NULL, max_bytes = 100 * 1024^2,
                               cache = TRUE, cache_only = FALSE, refresh = FALSE,
                               cache_dir = boe_default_cache_dir()) {
  entry <- boe_request_raw(url, deadline = deadline, max_bytes = max_bytes, cache = cache,
    cache_only = cache_only, refresh = refresh, cache_dir = cache_dir, ttl = 30 * 86400,
    max_cache_bytes = max(250 * 1024^2, max_bytes * 2),
    validate_response = boe_validate_not_error_html)
  boe_attach_provenance(entry$body, boe_receipt_from_entry(entry),
    list(surface = "official_asset", url = entry$url), transforms = character())
}
