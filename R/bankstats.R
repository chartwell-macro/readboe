boe_bankstats_path <- function() {
  path <- system.file("extdata", "bankstats-assets.rds", package = "readboe")
  if (!nzchar(path) || !file.exists(path)) boe_abort("The pinned Bankstats asset snapshot is unavailable.", "config")
  path
}

#' Browse pinned Bankstats bulk files
#'
#' Lists the exact bulk assets in the committed Bankstats asset snapshot.
#' Results are suitable for passing an exact `url` to
#' [read_boe_bankstats()] or for choosing a table for
#' [read_boe_bankstats_series()].  The operation is offline and does not
#' search the live site.
#'
#' @param table_id Optional non-empty character vector of exact table IDs.  A
#'   row matches if any semicolon-delimited table member equals a requested ID,
#'   case-insensitively.
#' @param section Optional non-empty character vector of exact top-level
#'   sections, matched case-insensitively.
#' @param archive Logical; if `TRUE` (the default), include archived assets;
#'   if `FALSE`, retain current assets only.
#' @param query Optional one-string case-insensitive implicit-AND query over
#'   table IDs, labels, sections, and URLs.  Unlike [boe_catalogue()], quoted
#'   phrases have no special treatment.
#' @param limit Positive integer maximum after filtering.  The current
#'   implementation accepts any positive value; use a modest value for a UI.
#' @return A data frame with `table_id`, `table_label`, `section`, `url`,
#'   `file_type`, `archive`, `member_count`, `member_names`,
#'   `source_page_url`, `source_page_sha256`, and `source_sha256`.  Rows retain
#'   snapshot order.  The `boe_catalogue_provenance` attribute contains the
#'   snapshot hash and `matched_before_cap`.
#' @details The current bundled snapshot contains the current bulk ZIP and no
#'   historical bulk archive; the `archive` field and filter are retained for
#'   snapshots that may include explicitly audited archives.  The bundled row
#'   is hash-authenticated by the asset manifest.  Missing snapshots signal
#'   `readboe_config`; invalid filters,
#'   malformed snapshots, and hash/identity mismatches signal typed
#'   `readboe_config`, `readboe_bounds`, `readboe_schema_drift`, or
#'   `readboe_identity_mismatch` conditions.  No network or cache writes occur.
#' @seealso [read_boe_bankstats()], [read_boe_bankstats_series()],
#'   [boe_catalogue()]
#' @examples
#' assets <- boe_bankstats_assets(limit = 1L)
#' stopifnot(is.data.frame(assets),
#'           all(c("table_id", "url", "file_type", "archive") %in%
#'             names(assets)),
#'           !is.null(attr(assets, "boe_catalogue_provenance")))
#' @export
boe_bankstats_assets <- function(table_id = NULL, section = NULL, archive = TRUE,
                                 query = NULL, limit = 1000L) {
  boe_check_positive(limit, "limit", integer = TRUE)
  path <- boe_bankstats_path()
  boe_assert_pinned_snapshot(path, "bankstats_assets_sha256")
  out <- readRDS(path)
  required <- c("table_id", "table_label", "section", "url", "file_type", "archive")
  if (!is.data.frame(out) || !all(required %in% names(out)) || anyNA(out$url) || anyDuplicated(out$url)) {
    boe_abort("The Bankstats snapshot violates its schema or URL identity contract.", "schema_drift")
  }
  if (!is.null(table_id)) out <- out[boe_match_list_field(out$table_id, table_id), , drop = FALSE]
  if (!is.null(section)) out <- out[tolower(out$section) %in% tolower(section), , drop = FALSE]
  if (!isTRUE(archive)) out <- out[!out$archive, , drop = FALSE]
  if (!is.null(query)) {
    if (!is.character(query) || length(query) != 1L || is.na(query)) boe_abort("`query` must be one string.", "config")
    terms <- strsplit(tolower(trimws(query)), "[[:space:]]+")[[1L]]
    hay <- tolower(paste(out$table_id, out$table_label, out$section, out$url))
    keep <- Reduce(`&`, lapply(terms[nzchar(terms)], function(term) grepl(term, hay, fixed = TRUE)))
    out <- out[keep, , drop = FALSE]
  }
  matched <- nrow(out)
  out <- utils::head(out, limit)
  attr(out, "boe_catalogue_provenance") <- list(snapshot_sha256 = boe_sha256_file(path), matched_before_cap = matched)
  out
}

#' Read an exact Bankstats workbook or workbook ZIP
#'
#' Reads one exact URL returned by [boe_bankstats_assets()] and preserves the
#' source workbook's cell layout.  No header inference, aggregation, NA
#' dropping, type normalisation, or duplicate removal occurs.  This makes the
#' result appropriate for an audited downstream parser rather than a tidy-data
#' convenience wrapper.
#'
#' @param url Exact HTTPS URL present in the pinned Bankstats asset snapshot.
#'   Arbitrary local paths and URLs are rejected with
#'   `readboe_identity_mismatch`.
#' @param member Optional non-empty character vector of exact workbook member
#'   names for a ZIP asset.  By default every bounded `.xls`/`.xlsx` member is
#'   parsed; for a single member, pass its archive path exactly as listed by
#'   `member_names`.
#' @param deadline A caller-owned absolute deadline, `POSIXct`, positive number
#'   of seconds, or `NULL` for 60 seconds.
#' @param max_bytes Positive compressed-response byte bound (default 150 MiB).
#' @param max_archive_members Positive integer ZIP member bound (default 100).
#' @param max_uncompressed_bytes Positive byte bound on extracted members
#'   (default 500 MiB).
#' @param max_sheets Positive integer workbook-sheet bound (default 50).
#' @param max_rows Positive integer row bound per worksheet (default 1e6).
#' @param max_columns Positive integer column bound per worksheet (default
#'   2000).
#' @param max_cells Positive integer total-cell bound per workbook (default
#'   5e6).
#' @param cache Whether to use the managed response cache (default `TRUE`).
#' @param cache_only If `TRUE`, require a matching cached response and never
#'   contact the network; a miss signals `readboe_cache_miss`.
#' @param refresh If `TRUE`, bypass an existing cache entry.  It cannot be
#'   combined with `cache_only = TRUE`.
#' @param cache_dir Cache directory; defaults to
#'   `tools::R_user_dir("readboe", "cache")`.
#' @return For `.xls`/`.xlsx`, a named list of worksheet data frames with class
#'   `c("boe_workbook", "list")`; names are official sheet names and each data
#'   frame retains raw cell values and original column names.  For `.zip`, a
#'   named list of those workbook objects with class
#'   `c("boe_workbook_archive", "list")`; names are archive member paths.
#'   Both return shapes carry a `boe_provenance` attribute with source URL,
#'   receipt hashes, cache state, and transform descriptions.
#' @details Requests use the Bank of England HTTPS allowlist, bounded
#'   redirects/retries/deadlines/bytes/members/sheets/rows/columns/cells, and
#'   temporary files that are removed on exit.  Unsafe ZIP paths, absent
#'   members, unsupported file types, transport failures, malformed workbooks,
#'   schema drift, identity mismatches, and resource-limit breaches signal
#'   typed `readboe_*` conditions.  A successful response is the only state
#'   written to the managed cache.
#' @seealso [boe_bankstats_assets()], [read_boe_bankstats_series()],
#'   [boe_download_asset()], [boe_provenance()]
#' @examples
#' ## The identity check is offline and avoids downloading an arbitrary URL.
#' bad <- tryCatch(
#'   read_boe_bankstats("https://www.bankofengland.co.uk/not-a-pinned-asset.xlsx"),
#'   error = function(error) error
#' )
#' stopifnot(inherits(bad, "readboe_identity_mismatch"))
#' ## Set READBOE_LIVE_EXAMPLES=true to parse the current pinned ZIP.
#' if (identical(Sys.getenv("READBOE_LIVE_EXAMPLES"), "true")) {
#'   asset <- boe_bankstats_assets(archive = FALSE, limit = 1L)
#'   if (nrow(asset)) {
#'     workbook <- read_boe_bankstats(asset$url[[1L]],
#'       deadline = boe_deadline(20), max_sheets = 5L)
#'     stopifnot(inherits(workbook, "boe_workbook"), is.list(workbook))
#'   }
#' }
#' @export
read_boe_bankstats <- function(url, member = NULL, deadline = NULL, max_bytes = 150 * 1024^2,
                               max_archive_members = 100L, max_uncompressed_bytes = 500 * 1024^2,
                               max_sheets = 50L, max_rows = 1e6, max_columns = 2000L,
                               max_cells = 5e6, cache = TRUE, cache_only = FALSE,
                               refresh = FALSE, cache_dir = boe_default_cache_dir()) {
  assets <- boe_bankstats_assets(limit = 100000L)
  idx <- which(assets$url == url)
  if (length(idx) != 1L) boe_abort("`url` is not one exact URL in the pinned Bankstats snapshot.", "identity_mismatch")
  raw <- boe_download_asset(url, deadline, max_bytes, cache, cache_only, refresh, cache_dir)
  receipt <- boe_provenance(raw)
  ext <- tolower(tools::file_ext(strsplit(url, "[?#]")[[1L]][[1L]]))
  if (ext %in% c("xls", "xlsx")) {
    out <- boe_read_workbook_raw(raw, paste0(".", ext), max_sheets, max_rows, max_columns, max_cells)
  } else if (ext == "zip") {
    zip_path <- boe_write_raw_temp(raw, ".zip")
    on.exit(unlink(zip_path), add = TRUE)
    listing <- boe_safe_zip_members(zip_path, max_archive_members, max_uncompressed_bytes)
    workbook_members <- listing$Name[grepl("\\.xlsx?$", listing$Name, ignore.case = TRUE)]
    if (!is.null(member)) {
      if (!is.character(member) || !length(member) || anyNA(member)) boe_abort("`member` must contain exact member names.", "config")
      workbook_members <- intersect(workbook_members, member)
      if (!setequal(workbook_members, member)) boe_abort("A requested workbook member is absent.", "identity_mismatch")
    }
    if (!length(workbook_members)) boe_abort("The Bankstats ZIP has no bounded workbook members.", "schema_drift")
    paths <- boe_extract_members(zip_path, workbook_members, max_archive_members, max_uncompressed_bytes)
    on.exit(unlink(attr(paths, "extraction_dir"), recursive = TRUE), add = TRUE)
    out <- stats::setNames(vector("list", length(paths)), workbook_members)
    total_cells <- 0
    for (i in seq_along(paths)) {
      bytes <- readBin(paths[[i]], "raw", n = file.info(paths[[i]])$size)
      wb <- boe_read_workbook_raw(bytes, paste0(".", tools::file_ext(paths[[i]])), max_sheets, max_rows, max_columns, max_cells - total_cells)
      total_cells <- total_cells + sum(vapply(wb, function(x) nrow(x) * ncol(x), numeric(1)))
      out[[i]] <- wb
    }
    class(out) <- c("boe_workbook_archive", "list")
  } else {
    boe_abort("The pinned Bankstats asset is not a supported workbook or ZIP.", "schema_drift", file_type = ext)
  }
  attr(out, "boe_provenance") <- c(receipt, list(source_identity = list(surface = "bankstats", url = url,
    table_id = assets$table_id[[idx]], archive = assets$archive[[idx]]), transforms = "Workbook syntax parsed without header inference"))
  out
}

boe_bankstats_cell_matrix <- function(sheet) {
  if (!is.data.frame(sheet) || !nrow(sheet) || !ncol(sheet)) {
    boe_abort("A Bankstats worksheet is empty or malformed.", "schema_drift")
  }
  values <- lapply(sheet, function(column) {
    value <- as.character(column)
    value[is.na(value)] <- ""
    trimws(value)
  })
  as.matrix(data.frame(values, check.names = FALSE, stringsAsFactors = FALSE))
}

boe_bankstats_cell_index <- function(workbook) {
  if (!inherits(workbook, "boe_workbook") || !length(workbook)) {
    boe_abort("The Bankstats cell index received a malformed workbook.",
              "schema_drift")
  }
  matrices <- lapply(workbook, boe_bankstats_cell_matrix)
  matrices <- lapply(matrices, function(cells) {
    tokens <- strsplit(as.vector(cells), "[^A-Za-z0-9]+")
    lengths <- lengths(tokens)
    token_values <- toupper(unlist(tokens, use.names = FALSE))
    token_cells <- rep.int(seq_along(tokens), lengths)
    keep <- nzchar(token_values) & grepl("[A-Z]", token_values)
    attr(cells, "boe_token_index") <- split(
      token_cells[keep],
      token_values[keep]
    )
    cells
  })
  structure(
    matrices,
    names = names(workbook),
    class = c("boe_bankstats_cell_index", "list")
  )
}

boe_bankstats_clean_title_piece <- function(value) {
  value <- trimws(gsub("[[:space:]]+", " ", as.character(value)))
  value <- sub("([[:space:]]*\\([A-Za-z0-9]+\\))+$", "", value, perl = TRUE)
  trimws(value)
}

boe_bankstats_format_series_title <- function(value) {
  value <- boe_bankstats_clean_title_piece(value)
  value <- sub(
    "^By instrument of which: ",
    "Of which \u2014 ",
    value,
    ignore.case = TRUE
  )
  value <- sub("^By instrument ", "", value, ignore.case = TRUE)
  value <- gsub(
    " \u2014 By sector ",
    " \u2014 ",
    value,
    fixed = TRUE
  )
  value <- sub(
    "^(Issues|Repayments|Net issuance) ",
    "\\1 \u2014 ",
    value,
    perl = TRUE
  )
  value <- sub(
    "^([^\u2014]+) \u2014 (Issues|Repayments|Net issuance) ",
    "\\1 \u2014 \\2 \u2014 ",
    value,
    perl = TRUE
  )
  value <- sub(" (Bonds|Shares|Total) paper$", " \\1", value, perl = TRUE)
  trimws(gsub("[[:space:]]+", " ", value))
}

boe_bankstats_title_piece_is_useful <- function(value, series_id, code, prefix) {
  value <- boe_bankstats_clean_title_piece(value)
  if (!nzchar(value)) return(FALSE)
  lowered <- tolower(value)
  if (grepl(
    paste0(
      "^(table[[:space:]]|last updated|notes? to table|",
      "seasonally adjusted$|not seasonally adjusted$|",
      "per cent$|percent$|[\u00a3$].*millions?$)"
    ),
    lowered,
    perl = TRUE
  )) return(FALSE)
  if (grepl("^[0-9.,/-]+$", value) ||
      grepl("^(Q[1-4]|[12][0-9]{3})$", value, ignore.case = TRUE)) {
    return(FALSE)
  }
  tokens <- toupper(c(series_id, code, prefix))
  if (toupper(value) %in% tokens) return(FALSE)
  TRUE
}

boe_bankstats_nearest_title_piece <- function(values, target, series_id,
                                               code, prefix) {
  positions <- which(vapply(
    values,
    boe_bankstats_title_piece_is_useful,
    logical(1L),
    series_id = series_id,
    code = code,
    prefix = prefix
  ))
  if (!length(positions)) return(NA_character_)
  left <- positions[positions <= target]
  candidates <- if (length(left)) left else positions
  position <- candidates[[which.min(abs(candidates - target))]]
  boe_bankstats_clean_title_piece(values[[position]])
}

boe_bankstats_series_title <- function(cells, code_row, code_column,
                                       orientation, series_id, code, prefix,
                                       sheet_name) {
  sheet_title <- boe_bankstats_clean_title_piece(sheet_name)
  if (identical(orientation, "time_down_rows")) {
    rows <- seq.int(max(1L, code_row - 5L), max(1L, code_row - 1L))
    pieces <- vapply(rows, function(row) {
      boe_bankstats_nearest_title_piece(
        cells[row, ], code_column, series_id, code, prefix
      )
    }, character(1L))
    pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
    pieces <- pieces[!duplicated(tolower(pieces))]
    if (length(pieces) > 4L) pieces <- utils::tail(pieces, 4L)
    detail <- paste(pieces, collapse = " ")
  } else {
    columns <- if (code_column > 1L) seq_len(code_column - 1L) else integer()
    pieces <- if (length(columns)) {
      values <- cells[code_row, columns, drop = TRUE]
      values[vapply(
        values,
        boe_bankstats_title_piece_is_useful,
        logical(1L),
        series_id = series_id,
        code = code,
        prefix = prefix
      )]
    } else {
      character()
    }
    pieces <- vapply(pieces, boe_bankstats_clean_title_piece, character(1L))
    pieces <- pieces[nzchar(pieces)]
    pieces <- pieces[!duplicated(tolower(pieces))]
    detail <- paste(pieces, collapse = " ")
  }
  if (!nzchar(detail)) return(boe_bankstats_format_series_title(sheet_title))
  if (!nzchar(sheet_title) || grepl(
    tolower(sheet_title),
    tolower(detail),
    fixed = TRUE
  )) return(boe_bankstats_format_series_title(detail))
  boe_bankstats_format_series_title(
    paste(sheet_title, detail, sep = " \u2014 ")
  )
}

boe_bankstats_series_frequency <- function(date, precision) {
  precision <- unique(tolower(trimws(as.character(precision))))
  precision <- precision[nzchar(precision)]
  precise <- c(
    day = "Daily", week = "Weekly", month = "Monthly",
    quarter = "Quarterly", year = "Annual"
  )
  if (length(precision) == 1L && precision %in% names(precise) &&
      !identical(precision, "day")) {
    return(unname(precise[[precision]]))
  }
  dates <- sort(unique(as.Date(date)))
  if (length(dates) < 2L) return("Unknown")
  gap <- stats::median(as.numeric(diff(dates)), na.rm = TRUE)
  if (!is.finite(gap)) return("Unknown")
  if (gap <= 2) "Daily" else if (gap <= 10) "Weekly" else if (gap <= 40) {
    "Monthly"
  } else if (gap <= 110) {
    "Quarterly"
  } else if (gap <= 210) {
    "Semiannual"
  } else if (gap <= 410) {
    "Annual"
  } else {
    "Irregular"
  }
}

boe_bankstats_cell_has_token <- function(value, token) {
  if (!nzchar(value) || !nzchar(token)) return(FALSE)
  token %in% strsplit(value, "[^A-Za-z0-9]+")[[1L]]
}

boe_bankstats_numeric <- function(value) {
  cleaned <- gsub(",", "", trimws(as.character(value)), fixed = TRUE)
  cleaned[!nzchar(cleaned)] <- NA_character_
  suppressWarnings(as.numeric(cleaned))
}

boe_bankstats_date <- function(value) {
  value <- trimws(as.character(value))
  if (!nzchar(value)) return(as.Date(NA))
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!is.na(numeric_value) && numeric_value >= 20000 && numeric_value <= 100000) {
    return(as.Date(numeric_value, origin = "1899-12-30"))
  }
  if (grepl("[A-Za-z]", value)) {
    pieces <- strsplit(value, "[[:space:]]+")[[1L]]
    if (length(pieces) != 3L ||
        !grepl("^[0-9]{1,2}$", pieces[[1L]]) ||
        !grepl("^[0-9]{4}$", pieces[[3L]])) {
      return(as.Date(NA))
    }
    day <- as.integer(pieces[[1L]])
    month <- boe_english_month_number(pieces[[2L]])
    year <- as.integer(pieces[[3L]])
    if (is.na(month)) return(as.Date(NA))
    value <- sprintf("%04d-%02d-%02d", year, month, day)
    format <- "%Y-%m-%d"
  } else {
    format <- if (grepl("/", value, fixed = TRUE)) "%d/%m/%Y" else "%Y-%m-%d"
  }
  parsed <- suppressWarnings(as.Date(strptime(value, format = format, tz = "UTC")))
  if (length(parsed) && !is.na(parsed)) parsed else as.Date(NA)
}

boe_bankstats_sheet_metadata <- function(cells) {
  top <- cells[seq_len(min(12L, nrow(cells))), , drop = FALSE]
  text <- trimws(as.character(top))
  text <- text[nzchar(text)]
  unit <- text[grepl(
    "(^| )(per cent|percent|\u00a3 millions|US\\$ millions|index|number)( |$)",
    text,
    ignore.case = TRUE
  )]
  adjustment <- text[grepl(
    "^(not seasonally adjusted|seasonally adjusted)$",
    text,
    ignore.case = TRUE
  )]
  updated <- regmatches(
    text,
    regexpr("[0-9]{1,2} [A-Za-z]+ [0-9]{4}", text)
  )
  updated <- updated[nzchar(updated)]
  list(
    unit = if (length(unit)) unit[[1L]] else NA_character_,
    seasonal_adjustment = if (length(adjustment)) adjustment[[1L]] else NA_character_,
    release_date = if (length(updated)) boe_bankstats_date(updated[[1L]]) else as.Date(NA)
  )
}

boe_bankstats_vertical_rows <- function(cells, code_row, code_column,
                                        date_column, identity) {
  if (code_row >= nrow(cells)) return(data.frame())
  rows <- seq.int(code_row + 1L, nrow(cells))
  dates <- as.Date(vapply(cells[rows, date_column], function(value) {
    as.numeric(boe_bankstats_date(value))
  }, numeric(1L)), origin = "1970-01-01")
  keep <- !is.na(dates)
  if (!any(keep)) return(data.frame())
  rows <- rows[keep]
  raw_value <- cells[rows, code_column]
  metadata <- boe_bankstats_sheet_metadata(cells)
  data.frame(
    series_id = identity$series_id,
    period = format(dates[keep], "%Y-%m-%d"),
    date = dates[keep],
    date_precision = "day",
    value = boe_bankstats_numeric(raw_value),
    value_status = ifelse(is.na(boe_bankstats_numeric(raw_value)), raw_value, ""),
    raw_value = raw_value,
    unit = metadata$unit,
    seasonal_adjustment = metadata$seasonal_adjustment,
    release_date = metadata$release_date,
    source_table_id = identity$table_id,
    source_member = identity$member,
    source_sheet = identity$sheet,
    source_row = rows,
    source_column = code_column,
    orientation = "time_down_rows",
    stringsAsFactors = FALSE
  )
}

boe_bankstats_horizontal_period <- function(cells, column, header_end_row) {
  header_rows <- seq_len(max(1L, header_end_row))
  current <- cells[header_rows, column]
  exact_date <- as.Date(NA)
  for (value in rev(current)) {
    candidate <- boe_bankstats_date(value)
    if (!is.na(candidate)) {
      exact_date <- candidate
      break
    }
  }
  quarter <- current[grepl("^Q[1-4]$", current, ignore.case = TRUE)]
  month <- current[grepl(
    paste0(
      "^(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|",
      "Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)$"
    ),
    current,
    ignore.case = TRUE
  )]
  current_years <- suppressWarnings(as.integer(current))
  current_years <- current_years[
    !is.na(current_years) & current_years >= 1600L & current_years <= 2200L
  ]
  year <- NA_integer_
  for (row in header_rows) {
    left <- cells[row, seq_len(column)]
    candidates <- suppressWarnings(as.integer(left))
    candidates <- candidates[!is.na(candidates) & candidates >= 1600L & candidates <= 2200L]
    if (length(candidates)) year <- utils::tail(candidates, 1L)
  }
  if (length(quarter) && !is.na(year)) {
    q <- as.integer(sub("^Q", "", toupper(utils::tail(quarter, 1L))))
    date <- as.Date(sprintf("%04d-%02d-01", year, 1L + (q - 1L) * 3L))
    return(list(period = sprintf("%04d-Q%d", year, q), date = date,
                precision = "quarter"))
  }
  if (length(month) && !is.na(year)) {
    month_number <- boe_english_month_number(utils::tail(month, 1L))
    if (!is.na(month_number)) {
      parsed <- as.Date(sprintf("%04d-%02d-01", year, month_number))
      return(list(period = format(parsed, "%Y-%m"), date = parsed,
                  precision = "month"))
    }
  }
  if (!is.na(exact_date)) {
    return(list(period = format(exact_date, "%Y-%m-%d"), date = exact_date,
                precision = "day"))
  }
  if (length(current_years) && !is.na(year)) {
    date <- as.Date(sprintf("%04d-01-01", year))
    return(list(period = as.character(year), date = date, precision = "year"))
  }
  NULL
}

boe_bankstats_horizontal_rows <- function(cells, code_row, code_column,
                                          header_end_row, identity) {
  if (code_column >= ncol(cells)) return(data.frame())
  columns <- seq.int(code_column + 1L, ncol(cells))
  periods <- lapply(columns, function(column) {
    boe_bankstats_horizontal_period(cells, column, header_end_row)
  })
  keep <- !vapply(periods, is.null, logical(1L))
  if (!any(keep)) return(data.frame())
  columns <- columns[keep]
  periods <- periods[keep]
  raw_value <- cells[code_row, columns]
  metadata <- boe_bankstats_sheet_metadata(cells)
  data.frame(
    series_id = identity$series_id,
    period = vapply(periods, `[[`, character(1L), "period"),
    date = as.Date(vapply(periods, function(x) as.numeric(x$date), numeric(1L)),
                   origin = "1970-01-01"),
    date_precision = vapply(periods, `[[`, character(1L), "precision"),
    value = boe_bankstats_numeric(raw_value),
    value_status = ifelse(is.na(boe_bankstats_numeric(raw_value)), raw_value, ""),
    raw_value = raw_value,
    unit = metadata$unit,
    seasonal_adjustment = metadata$seasonal_adjustment,
    release_date = metadata$release_date,
    source_table_id = identity$table_id,
    source_member = identity$member,
    source_sheet = identity$sheet,
    source_row = code_row,
    source_column = columns,
    orientation = "time_across_columns",
    stringsAsFactors = FALSE
  )
}

boe_extract_bankstats_series <- function(workbook, series_id, displayed_codes,
                                         table_id, member, max_output_rows) {
  boe_check_positive(max_output_rows, "max_output_rows", integer = TRUE)
  if (!inherits(workbook, c("boe_workbook", "boe_bankstats_cell_index")) ||
      !length(workbook)) {
    boe_abort("The Bankstats series reader received a malformed workbook.", "schema_drift")
  }
  cell_index <- if (inherits(workbook, "boe_bankstats_cell_index")) {
    workbook
  } else {
    boe_bankstats_cell_index(workbook)
  }
  displayed_codes <- unique(trimws(unlist(strsplit(
    displayed_codes,
    ";",
    fixed = TRUE
  ))))
  displayed_codes <- displayed_codes[nzchar(displayed_codes)]
  pieces <- list()
  for (sheet_name in names(cell_index)) {
    cells <- cell_index[[sheet_name]]
    for (code in displayed_codes) {
      if (!endsWith(series_id, code)) next
      prefix <- substr(series_id, 1L, nchar(series_id) - nchar(code))
      token_index <- attr(cells, "boe_token_index", exact = TRUE) %||% list()
      hit_index <- unique(token_index[[toupper(code)]] %||% integer())
      if (!length(hit_index)) next
      hits <- arrayInd(hit_index, dim(cells))
      for (hit in seq_len(nrow(hits))) {
        code_row <- hits[hit, 1L]
        code_column <- hits[hit, 2L]
        row_prefix <- which(vapply(
          cells[code_row, ],
          boe_bankstats_cell_has_token,
          logical(1L),
          token = prefix
        ))
        column_prefix <- which(vapply(
          cells[, code_column],
          boe_bankstats_cell_has_token,
          logical(1L),
          token = prefix
        ))
        orientation <- if (length(row_prefix)) {
          "time_down_rows"
        } else if (length(column_prefix)) {
          "time_across_columns"
        } else {
          ""
        }
        if (!nzchar(orientation)) next
        identity <- list(
          series_id = series_id,
          table_id = table_id,
          member = member,
          sheet = sheet_name,
          series_label = boe_bankstats_series_title(
            cells, code_row, code_column, orientation,
            series_id, code, prefix, sheet_name
          )
        )
        if (length(row_prefix)) {
          date_column <- row_prefix[which.min(abs(row_prefix - code_column))]
          part <- boe_bankstats_vertical_rows(
            cells,
            code_row,
            code_column,
            date_column,
            identity
          )
        } else if (length(column_prefix)) {
          prefix_above <- column_prefix[column_prefix < code_row]
          if (!length(prefix_above)) next
          part <- boe_bankstats_horizontal_rows(
            cells,
            code_row,
            code_column,
            max(prefix_above) - 1L,
            identity
          )
        }
        if (nrow(part)) {
          part$series_label <- identity$series_label
          part$series_frequency <- boe_bankstats_series_frequency(
            part$date,
            part$date_precision
          )
          pieces[[length(pieces) + 1L]] <- part
        }
      }
    }
  }
  if (!length(pieces)) return(data.frame())
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  if (nrow(out) > max_output_rows) {
    boe_abort("The Bankstats series result exceeded its row bound.", "bounds",
              series_id = series_id, rows = nrow(out))
  }
  out
}

#' Read one current Bankstats series with full official history
#'
#' Selects one exact identity from the pinned current Bankstats catalogue.  With
#' `history = "full"`, it reads the canonical IADB XML history over the exact
#' first and last dates pinned in the catalogue.  With
#' `history = "current_workbook"`, it exposes the shorter rolling window in the
#' current bulk workbook, including workbook coordinates.  Neither mode drops
#' raw missing markers or duplicate periods.
#'
#' @param series_id One non-empty exact identifier returned by
#'   [boe_catalogue()].  It is trimmed and upper-cased before lookup.
#' @param table_id Optional exact current Bankstats table.  When the identity
#'   is published in several tables, this argument is required and must match
#'   one semicolon-delimited membership exactly.
#' @param history Character choice: `"full"` (the default) for canonical IADB
#'   history, or `"current_workbook"` for the rolling bulk workbook window.
#' @param deadline A caller-owned absolute deadline, `POSIXct`, positive number
#'   of seconds, or `NULL` for 60 seconds.  It is shared by delegated IADB or
#'   workbook work.
#' @param max_bytes Positive response byte bound (default 150 MiB).
#' @param max_archive_members Positive integer ZIP member bound (default 100).
#' @param max_uncompressed_bytes Positive extracted-byte bound (default 500
#'   MiB).
#' @param max_sheets Positive integer workbook-sheet bound (default 50).
#' @param max_rows Positive integer worksheet-row bound (default 1e6).
#' @param max_columns Positive integer worksheet-column bound (default 2000).
#' @param max_cells Positive integer workbook-cell bound (default 5e6).
#' @param max_output_rows Positive integer output-row bound (default 250000).
#' @param cache Whether delegated reads use the managed cache (default `TRUE`).
#' @param cache_only If `TRUE`, delegated reads require matching cache entries
#'   and never contact the network.
#' @param refresh If `TRUE`, delegated reads bypass existing cache entries.
#' @param cache_dir Cache directory for delegated reads; defaults to
#'   `tools::R_user_dir("readboe", "cache")`.
#' @return A data frame with class
#'   `c("boe_bankstats_series", "data.frame")` and a `boe_provenance`
#'   attribute.  Canonical/full rows have 31 columns: `series_id`, `period`,
#'   `date`, `date_precision`, `value`, `value_status`, `raw_value`, `unit`,
#'   `seasonal_adjustment`, `release_date`, `source_table_id`,
#'   `source_member`, `source_sheet`, `source_row`, `source_column`,
#'   `orientation`, `series_label`, `series_frequency`, `series_country`,
#'   `concatenated_label`, `definition`, `category_name`, `category_value`,
#'   `category_label`, `series_comment`, `provisional_value_raw`,
#'   `provisional_value`, `confidential`, `last_updated`, `last_revised`, and
#'   `observation_comment`.  Current-workbook rows expose the first 16 fields
#'   plus `series_label` and `series_frequency` (18 fields).  Raw values and
#'   status markers are retained; period-start dates and workbook coordinates
#'   make each source occurrence auditable.
#' @details A non-chartable current-page identity cannot be read in
#'   `current_workbook` mode and signals `readboe_upstream_unavailable`; use
#'   `history = "full"` when canonical bounds are available.  Duplicate table
#'   membership without an explicit `table_id`, bad identities, schema drift,
#'   transport failures, cache misses/corruption, deadlines, and resource
#'   bounds signal typed `readboe_*` conditions.  Network and temporary-file
#'   behaviour is inherited from [read_boe_series()] and
#'   [read_boe_bankstats()]; only successful responses enter the managed cache.
#' @seealso [boe_catalogue()], [boe_bankstats_assets()], [read_boe_series()],
#'   [boe_provenance()]
#' @examples
#' ## Lookup and input validation are offline; no remote read is attempted.
#' bad <- tryCatch(
#'   read_boe_bankstats_series("NOT_A_PINNED_SERIES"),
#'   error = function(error) error
#' )
#' stopifnot(inherits(bad, "readboe_identity_mismatch"))
#' @export
read_boe_bankstats_series <- function(
    series_id,
    table_id = NULL,
    history = c("full", "current_workbook"),
    deadline = NULL,
    max_bytes = 150 * 1024^2,
    max_archive_members = 100L,
    max_uncompressed_bytes = 500 * 1024^2,
    max_sheets = 50L,
    max_rows = 1e6,
    max_columns = 2000L,
    max_cells = 5e6,
    max_output_rows = 250000L,
    cache = TRUE,
    cache_only = FALSE,
    refresh = FALSE,
    cache_dir = boe_default_cache_dir()) {
  history <- match.arg(history)
  if (!is.character(series_id) || length(series_id) != 1L || is.na(series_id) ||
      !nzchar(trimws(series_id))) {
    boe_abort("`series_id` must be one exact Bankstats identifier.", "config")
  }
  series_id <- toupper(trimws(series_id))
  catalogue <- boe_catalogue(series_id = series_id, limit = 10L)
  if (nrow(catalogue) != 1L || catalogue$status[[1L]] != "bankstats_current") {
    boe_abort("The requested identity is not in the pinned current Bankstats catalogue.",
              "identity_mismatch", series_id = series_id)
  }
  tables <- unique(trimws(strsplit(catalogue$table_id[[1L]], ";", fixed = TRUE)[[1L]]))
  tables <- tables[nzchar(tables)]
  if (is.null(table_id)) {
    if (length(tables) != 1L) {
      boe_abort(
        "This Bankstats identity appears in several tables; choose one exact `table_id`.",
        "config",
        series_id = series_id,
        table_ids = tables
      )
    }
    table_id <- tables[[1L]]
  }
  if (!is.character(table_id) || length(table_id) != 1L || is.na(table_id) ||
      !table_id %in% tables) {
    boe_abort("`table_id` is not an exact table for this Bankstats identity.",
              "identity_mismatch", series_id = series_id, table_id = table_id)
  }
  if (identical(history, "full")) {
    first <- suppressWarnings(as.Date(catalogue$first_observation[[1L]]))
    last <- suppressWarnings(as.Date(catalogue$last_observation[[1L]]))
    if (is.na(first) || is.na(last) || first > last) {
      boe_abort(
        "The pinned catalogue lacks valid canonical IADB history bounds.",
        "schema_drift",
        series_id = series_id
      )
    }
    iadb <- read_boe_series(
      series_id,
      from = first,
      to = last,
      format = "xml",
      provisional = TRUE,
      footnotes = TRUE,
      deadline = deadline,
      max_bytes = max_bytes,
      max_observations = max_output_rows,
      cache = cache,
      cache_only = cache_only,
      refresh = refresh,
      cache_dir = cache_dir
    )
    return(boe_bankstats_canonical_rows(
      iadb,
      catalogue = catalogue,
      table_id = table_id,
      max_output_rows = max_output_rows
    ))
  }
  if ("chartable" %in% names(catalogue) &&
      !isTRUE(catalogue$chartable[[1L]])) {
    boe_abort(
      paste(
        "The exact series is listed on the current Bankstats page, but the",
        "rolling bulk workbook does not expose a chartable occurrence.",
        "Use `history = \"full\"` for canonical IADB history."
      ),
      "upstream_unavailable",
      series_id = series_id,
      table_id = table_id
    )
  }
  assets <- boe_bankstats_assets(table_id = table_id, archive = FALSE,
                                  limit = 100000L)
  if (nrow(assets) != 1L) {
    boe_abort("The current Bankstats bulk asset identity is unavailable.",
              "identity_mismatch", table_id = table_id)
  }
  members <- trimws(strsplit(assets$member_names[[1L]], ";", fixed = TRUE)[[1L]])
  expected <- paste0("Tab", table_id, ".xls")
  member <- members[tolower(basename(members)) == tolower(expected)]
  if (length(member) != 1L) {
    boe_abort("The exact Bankstats workbook member is absent from the pinned asset.",
              "identity_mismatch", table_id = table_id)
  }
  archive <- read_boe_bankstats(
    assets$url[[1L]],
    member = member,
    deadline = deadline,
    max_bytes = max_bytes,
    max_archive_members = max_archive_members,
    max_uncompressed_bytes = max_uncompressed_bytes,
    max_sheets = max_sheets,
    max_rows = max_rows,
    max_columns = max_columns,
    max_cells = max_cells,
    cache = cache,
    cache_only = cache_only,
    refresh = refresh,
    cache_dir = cache_dir
  )
  out <- boe_extract_bankstats_series(
    archive[[member]],
    series_id,
    catalogue$displayed_code[[1L]],
    table_id,
    member,
    max_output_rows
  )
  if (!nrow(out)) {
    boe_abort(
      "The current bulk workbook does not contain an exact, chartable occurrence of this identity.",
      "identity_mismatch",
      series_id = series_id,
      table_id = table_id
    )
  }
  provenance <- boe_provenance(archive)
  provenance$source_identity <- list(
    surface = "bankstats_current_bulk",
    series_id = series_id,
    table_id = table_id,
    member = member,
    url = assets$url[[1L]],
    source_page_url = assets$source_page_url[[1L]],
    source_page_sha256 = assets$source_page_sha256[[1L]]
  )
  provenance$transforms <- c(
    provenance$transforms,
    "Exact series cell selected by official series suffix and date-prefix identity",
    "Workbook values parsed numerically while preserving raw missing/suppression markers",
    "Time-across period headings represented by explicit period-start dates"
  )
  attr(out, "boe_provenance") <- provenance
  class(out) <- c("boe_bankstats_series", "data.frame")
  out
}

boe_iadb_category_label <- function(metadata, wanted) {
  if (!all(c("category_name", "category_label") %in% names(metadata)) ||
      nrow(metadata) != 1L) {
    return(NA_character_)
  }
  names <- trimws(strsplit(
    metadata$category_name[[1L]] %||% "",
    ";",
    fixed = TRUE
  )[[1L]])
  labels <- trimws(strsplit(
    metadata$category_label[[1L]] %||% "",
    ";",
    fixed = TRUE
  )[[1L]])
  if (!length(names) || length(names) != length(labels)) {
    return(NA_character_)
  }
  matched <- labels[toupper(names) %in% toupper(wanted)]
  matched <- unique(matched[!is.na(matched) & nzchar(matched)])
  if (length(matched)) paste(matched, collapse = ";") else NA_character_
}

boe_bankstats_canonical_rows <- function(iadb, catalogue, table_id,
                                         max_output_rows = 250000L) {
  boe_check_positive(max_output_rows, "max_output_rows", integer = TRUE)
  required_catalogue <- c(
    "series_id", "label", "frequency", "first_observation",
    "last_observation", "displayed_code", "source_series_url",
    "source_page_url", "source_page_sha256"
  )
  if (!inherits(iadb, "boe_iadb") || !is.list(iadb) ||
      !all(c("metadata", "observations") %in% names(iadb)) ||
      !is.data.frame(catalogue) || nrow(catalogue) != 1L ||
      !all(required_catalogue %in% names(catalogue))) {
    boe_abort("Canonical Bankstats history inputs violate their schema.",
              "schema_drift")
  }
  series_id <- catalogue$series_id[[1L]]
  metadata <- iadb$metadata
  observations <- iadb$observations
  if (nrow(metadata) != 1L ||
      !identical(metadata$series_id[[1L]], series_id) ||
      !is.data.frame(observations) || !nrow(observations) ||
      any(observations$series_id != series_id) ||
      anyNA(observations$date) || nrow(observations) > max_output_rows) {
    boe_abort(
      "Canonical IADB history does not match the exact Bankstats identity.",
      "identity_mismatch",
      series_id = series_id
    )
  }
  expected_first <- as.Date(catalogue$first_observation[[1L]])
  expected_last <- as.Date(catalogue$last_observation[[1L]])
  if (!identical(min(observations$date), expected_first) ||
      !identical(max(observations$date), expected_last)) {
    boe_abort(
      "Canonical IADB observations do not match the pinned history bounds.",
      "identity_mismatch",
      series_id = series_id,
      expected_first = expected_first,
      observed_first = min(observations$date),
      expected_last = expected_last,
      observed_last = max(observations$date)
    )
  }
  value_status <- ifelse(
    is.na(observations$value),
    ifelse(is.na(observations$value_raw), "", observations$value_raw),
    ""
  )
  unit <- boe_iadb_category_label(
    metadata,
    c("UNIT", "UNITS", "OUTPUT IN", "DENOMINATION")
  )
  seasonal_adjustment <- boe_iadb_category_label(
    metadata,
    c("SEASONAL ADJUSTMENT")
  )
  out <- data.frame(
    series_id = observations$series_id,
    period = format(observations$date, "%Y-%m-%d"),
    date = observations$date,
    date_precision = "day",
    value = observations$value,
    value_status = value_status,
    raw_value = observations$value_raw,
    unit = unit,
    seasonal_adjustment = seasonal_adjustment,
    release_date = as.Date(NA),
    source_table_id = table_id,
    source_member = NA_character_,
    source_sheet = NA_character_,
    source_row = observations$source_row,
    source_column = NA_integer_,
    orientation = "iadb_canonical_xml",
    series_label = metadata$label[[1L]],
    series_frequency = metadata$frequency[[1L]],
    series_country = if ("country" %in% names(metadata)) {
      metadata$country[[1L]]
    } else {
      NA_character_
    },
    concatenated_label = if ("concatenated_label" %in% names(metadata)) {
      metadata$concatenated_label[[1L]]
    } else {
      NA_character_
    },
    definition = if ("definition" %in% names(metadata)) {
      metadata$definition[[1L]]
    } else {
      NA_character_
    },
    category_name = if ("category_name" %in% names(metadata)) {
      metadata$category_name[[1L]]
    } else {
      NA_character_
    },
    category_value = if ("category_value" %in% names(metadata)) {
      metadata$category_value[[1L]]
    } else {
      NA_character_
    },
    category_label = if ("category_label" %in% names(metadata)) {
      metadata$category_label[[1L]]
    } else {
      NA_character_
    },
    series_comment = if ("series_comment" %in% names(metadata)) {
      metadata$series_comment[[1L]]
    } else {
      NA_character_
    },
    provisional_value_raw = observations$provisional_value_raw,
    provisional_value = observations$provisional_value,
    confidential = observations$confidential,
    last_updated = observations$last_updated,
    last_revised = observations$last_revised,
    observation_comment = observations$observation_comment,
    stringsAsFactors = FALSE
  )
  provenance <- boe_provenance(iadb)
  if (is.null(provenance) ||
      !grepl("^[0-9a-f]{64}$", provenance$raw_sha256 %||% "")) {
    boe_abort("Canonical IADB history lacks exact source provenance.",
              "identity_mismatch", series_id = series_id)
  }
  catalogue_provenance <- attr(
    catalogue,
    "boe_catalogue_provenance",
    exact = TRUE
  ) %||% list()
  provenance$source_identity <- utils::modifyList(
    provenance$source_identity %||% list(),
    list(
      surface = "iadb_xml_canonical_bankstats_history",
      table_id = table_id,
      catalogue_snapshot_sha256 =
        catalogue_provenance$snapshot_sha256 %||% "",
      source_series_url = catalogue$source_series_url[[1L]],
      source_page_url = catalogue$source_page_url[[1L]],
      source_page_sha256 = catalogue$source_page_sha256[[1L]]
    )
  )
  provenance$transforms <- c(
    provenance$transforms,
    "Current Bankstats table membership attached without altering observations"
  )
  attr(out, "boe_provenance") <- provenance
  class(out) <- c("boe_bankstats_series", "data.frame")
  out
}
