# Shared scalar, identity, period and bounded workbook contracts.

boe_mpr_select <- function(catalogue, selections) {
  for (name in names(selections)) {
    values <- selections[[name]]
    if (is.null(values)) next
    if (!is.character(values) || !length(values) || anyNA(values) || any(!nzchar(values)) || anyDuplicated(values)) {
      boe_abort(paste0("`", name, "` must contain unique exact catalogue values."), "config")
    }
    catalogue <- catalogue[catalogue[[name]] %in% values, , drop = FALSE]
  }
  for (name in names(selections)) {
    if (!all(selections[[name]] %in% catalogue[[name]])) {
      boe_abort("The requested combination has no published scalar path in this release.", "no_data",
        selector = name, requested = selections[[name]])
    }
  }
  if (!nrow(catalogue)) boe_abort("No published MPR scalar paths match this selection.", "no_data")
  catalogue
}

boe_mpr_periods <- function(x, frequency, statistic) {
  raw <- trimws(as.character(x))
  q4 <- grepl("(^|_)q4_", statistic)
  pattern <- if (frequency == "Q") "^[0-9]{4} ?Q[1-4]$" else if (q4) "^[0-9]{4}( ?Q4)?$" else "^[0-9]{4}$"
  if (anyNA(raw) || any(!grepl(pattern, raw))) boe_abort("MPR target-period labels changed schema.", "schema_drift")
  year <- substr(raw, 1L, 4L)
  period <- if (frequency == "Q") sub(" ?Q", "-Q", raw) else if (q4) paste0(year, "-Q4") else year
  date <- as.Date(paste0(year, if (frequency == "Q") sprintf("-%02d-01", (as.integer(sub(".*Q", "", raw)) - 1L) * 3L + 1L) else if (q4) "-10-01" else "-01-01"))
  if (anyDuplicated(period) || is.unsorted(date, strictly = TRUE)) boe_abort("MPR target periods are duplicate or unordered.", "schema_drift")
  data.frame(target_period = period, target_date = date, target_label = raw, stringsAsFactors = FALSE)
}

boe_mpr_vintages <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))
  serial <- suppressWarnings(as.numeric(x))
  numeric <- !is.na(serial) & is.finite(serial) & serial >= 0 & serial <= 2958465 & serial == floor(serial)
  out[numeric] <- format(as.Date(serial[numeric], origin = "1899-12-30"), "%Y-%m")
  iso <- !is.na(x) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}(T.*)?$", x)
  valid_iso <- suppressWarnings(as.Date(substr(x[iso], 1L, 10L), format = "%Y-%m-%d"))
  out[which(iso)[!is.na(valid_iso)]] <- format(valid_iso[!is.na(valid_iso)], "%Y-%m")
  # English month matching is local and does not mutate LC_TIME.
  clean <- sub("[[:space:]]*\\([a-z]\\)$", "", x)
  for (m in seq_along(month.name)) {
    hit <- !is.na(clean) & grepl(paste0("^", month.name[[m]], " [0-9]{4}$"), clean)
    out[hit] <- paste0(substr(clean[hit], nchar(month.name[[m]]) + 2L, nchar(clean[hit])), sprintf("-%02d", m))
  }
  out
}

boe_mpr_values <- function(x, allow_censored = FALSE, units = NULL) {
  raw <- as.character(x)
  blank <- is.na(raw) | trimws(raw) %in% c("", "-", "..", "n/a", "n.a.", "NA")
  value <- suppressWarnings(as.numeric(raw))
  percent <- !is.na(raw) & grepl("^[+-]?[0-9]+([.][0-9]+)?%$", trimws(raw))
  if (allow_censored && identical(units, "fraction")) {
    value[percent] <- as.numeric(sub("%$", "", trimws(raw[percent]))) / 100
  }
  censored <- !is.na(raw) & grepl("^[<>]=? *[0-9]+([.][0-9]+)?%?$", trimws(raw))
  if (any(!blank & !(allow_censored & censored) & (!is.finite(value) | is.na(value)))) {
    boe_abort("An MPR scalar cell is non-numeric and is not a recognised missing marker.", "schema_drift",
      raw_values = unique(raw[!blank & !(allow_censored & censored) & (!is.finite(value) | is.na(value))]))
  }
  value[blank] <- NA_real_
  lower <- upper <- rep(NA_real_, length(raw))
  threshold <- suppressWarnings(as.numeric(gsub("[<>= %]", "", raw)))
  if (!is.null(units) && identical(units, "fraction")) threshold[grepl("%", raw, fixed = TRUE)] <- threshold[grepl("%", raw, fixed = TRUE)] / 100
  lower[allow_censored & censored & grepl("^>", trimws(raw))] <- threshold[allow_censored & censored & grepl("^>", trimws(raw))]
  upper[allow_censored & censored & grepl("^<", trimws(raw))] <- threshold[allow_censored & censored & grepl("^<", trimws(raw))]
  list(value = value, raw_value = raw,
    value_status = ifelse(is.na(raw) | trimws(raw) == "", "missing", ifelse(blank, raw,
      ifelse(allow_censored & censored, "censored", ""))), value_lower_bound = lower, value_upper_bound = upper,
    value_bound_inclusive = ifelse(allow_censored & censored, grepl("=", raw, fixed = TRUE), NA))
}

boe_mpr_zip_xml <- function(path, entry, listing) {
  idx <- which(listing$Name == entry)
  if (length(idx) != 1L) boe_abort("An MPR workbook XML part is absent or ambiguous.", "schema_drift")
  con <- unz(path, entry, open = "rb")
  bytes <- tryCatch(readBin(con, "raw", n = listing$Length[[idx]] + 1), finally = close(con))
  if (length(bytes) != listing$Length[[idx]]) boe_abort("Workbook XML exceeds its declared size.", "bounds")
  tryCatch(xml2::read_xml(bytes, options = "NONET"), error = function(e)
    boe_abort("MPR workbook XML cannot be parsed.", "schema_drift"))
}

boe_mpr_preflight <- function(path, inventory, selected, deadline, max_uncompressed_bytes,
    max_sheets, max_rows, max_columns, max_cells, retain_xml = FALSE) {
  boe_time_left(deadline)
  listing <- boe_safe_zip_members(path, max_members = 1000L, max_uncompressed_bytes = max_uncompressed_bytes)
  ns <- c(m = "http://schemas.openxmlformats.org/spreadsheetml/2006/main",
    r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
  wb <- boe_mpr_zip_xml(path, "xl/workbook.xml", listing)
  date1904 <- xml2::xml_attr(xml2::xml_find_all(wb, "./m:workbookPr", ns), "date1904")
  if (any(!is.na(date1904) & !date1904 %in% c("0", "false"))) {
    boe_abort("MPR workbook date system changed schema.", "schema_drift")
  }
  tabs <- xml2::xml_find_all(wb, ".//m:sheets/m:sheet", ns)
  names <- xml2::xml_attr(tabs, "name")
  if (length(names) > max_sheets) boe_abort("MPR workbook exceeds `max_sheets`.", "bounds")
  if (!identical(names, inventory$source_sheet) || anyDuplicated(names)) {
    boe_abort("MPR workbook sheet identities changed schema.", "schema_drift")
  }
  rels <- boe_mpr_zip_xml(path, "xl/_rels/workbook.xml.rels", listing)
  links <- xml2::xml_children(rels)
  ids <- xml2::xml_attr(links, "Id")
  if (anyNA(ids) || anyDuplicated(ids)) boe_abort("MPR workbook relationships are ambiguous.", "schema_drift")
  cells <- 0
  dimensions <- data.frame(source_sheet = selected, rows = 0, columns = 0, stringsAsFactors = FALSE)
  documents <- list()
  for (name in selected) {
    boe_time_left(deadline)
    id <- xml2::xml_attr(tabs[which(names == name)], "r:id", ns = ns)
    link <- links[which(ids == id)]
    if (length(link) != 1L || identical(xml2::xml_attr(link, "TargetMode"), "External")) {
      boe_abort("MPR worksheet relationship is absent or external.", "schema_drift")
    }
    target <- sub("^/", "", xml2::xml_attr(link, "Target"))
    if (!startsWith(target, "xl/")) target <- paste0("xl/", target)
    if (is.na(target) || !grepl("^xl/worksheets/[^/]+\\.xml$", target)) {
      boe_abort("MPR worksheet relationship contains an unsafe target.", "bounds")
    }
    sheet <- boe_mpr_zip_xml(path, target, listing)
    refs <- xml2::xml_attr(xml2::xml_find_all(sheet, ".//m:sheetData/m:row/m:c", ns), "r")
    if (anyDuplicated(refs)) boe_abort("MPR worksheet cell coordinates are duplicate.", "schema_drift")
    dimension <- xml2::xml_attr(xml2::xml_find_all(sheet, "./m:dimension", ns), "ref")
    refs <- c(refs, unlist(strsplit(dimension, ":", fixed = TRUE)))
    if (!length(refs) || anyNA(refs) || any(!grepl("^[A-Z]+[1-9][0-9]*$", refs))) {
      boe_abort("MPR worksheet cell coordinates changed schema.", "schema_drift")
    }
    letters <- sub("[0-9]+$", "", refs)
    columns <- vapply(strsplit(letters, ""), function(x) {
      value <- 0
      for (letter in x) value <- value * 26 + match(letter, LETTERS)
      value
    }, numeric(1L))
    rows <- as.numeric(sub("^[A-Z]+", "", refs))
    cells <- cells + max(rows) * max(columns)
    if (any(!is.finite(rows)) || max(rows) > max_rows || max(columns) > max_columns || cells > max_cells) {
      boe_abort("MPR worksheet dimensions exceed row, column or cell bounds.", "bounds", sheet = name)
    }
    dimensions$rows[dimensions$source_sheet == name] <- max(rows)
    dimensions$columns[dimensions$source_sheet == name] <- max(columns)
    if (retain_xml) documents[[name]] <- sheet
    boe_time_left(deadline)
  }
  if (retain_xml) attr(dimensions, "xml_parts") <- documents
  dimensions
}

boe_mpr_options <- function(deadline, max_bytes, max_archive_members, max_uncompressed_bytes,
    max_sheets, max_rows, max_columns, max_cells, max_output_rows, cache, cache_only, refresh, cache_dir) {
  bounds <- list(max_bytes = max_bytes, max_archive_members = max_archive_members,
    max_uncompressed_bytes = max_uncompressed_bytes, max_sheets = max_sheets,
    max_rows = max_rows, max_columns = max_columns, max_cells = max_cells, max_output_rows = max_output_rows)
  for (name in names(bounds)) boe_check_positive(bounds[[name]], name, integer = TRUE)
  c(list(deadline = boe_as_deadline(deadline)), bounds,
    list(cache = cache, cache_only = cache_only, refresh = refresh, cache_dir = cache_dir))
}

boe_mpr_discover <- function(options, include_upcoming = FALSE) {
  if (!is.null(options$releases)) return(options$releases[include_upcoming | options$releases$publication_status == "published", , drop = FALSE])
  boe_mpr_discover_live(include_upcoming = include_upcoming, deadline = options$deadline,
    cache = options$cache, cache_only = options$cache_only, refresh = options$refresh, cache_dir = options$cache_dir)
}

boe_mpr_release <- function(release, options, releases = NULL) {
  boe_assert_scalar(release, "release", "character")
  if (is.null(releases)) releases <- boe_mpr_discover(options)
  if (release == "latest") release <- releases$release[[which.max(releases$source_published_date)]]
  out <- releases[releases$release == release, , drop = FALSE]
  if (nrow(out) != 1L) boe_abort("Select one discovered published forecast-report edition.", "identity_mismatch", release = release)
  out
}

boe_mpr_download <- function(url, options) {
  boe_time_left(options$deadline)
  if (is.function(options$asset_fetch)) return(options$asset_fetch(url))
  raw <- boe_download_asset(url, options$deadline, options$max_bytes, options$cache,
    options$cache_only, options$refresh, options$cache_dir)
  boe_time_left(options$deadline)
  raw
}

boe_mpr_assets_for_release <- function(rel, options) {
  if (!is.null(options$asset_inventory)) return(options$asset_inventory[[rel$release]])
  entry <- boe_request_raw(rel$landing_url, deadline = options$deadline,
    max_bytes = 20 * 1024^2, cache = options$cache, cache_only = options$cache_only,
    refresh = options$refresh, cache_dir = options$cache_dir)
  boe_mpr_assets_from_entry(rel, entry)
}

boe_mpr_assets_from_entry <- function(rel, entry) {
  doc <- boe_mpr_html(entry$body)
  links <- boe_mpr_links(doc, rel$landing_url)
  keep <- grepl("\\.(zip|xlsx?|csv|pdf)([?].*)?$", links$url, ignore.case = TRUE) &
    grepl("/files/(inflation-report|monetary-policy-report)/", links$url, ignore.case = TRUE)
  links <- links[keep, , drop = FALSE]
  links <- links[!duplicated(links$url), , drop = FALSE]
  # On joint MPR/FSR pages this path/lineage guard excludes the FSR bundle.
  for (url in links$url) boe_validate_url(url)
  links$file_type <- tolower(sub(".*[.]([^.?#]+)([?].*)?$", "\\1", links$url))
  text <- tolower(paste(links$url, links$label))
  links$asset_role <- ifelse(links$file_type == "zip", "data_bundle",
    ifelse(links$file_type %in% c("xls", "xlsx", "csv"), "numeric_workbook",
    ifelse(grepl("transcript|opening|press.conference|fan.chart|slides|summary", text), "ancillary_pdf", "report_pdf")))
  asset_month <- vapply(paste(links$url, links$label), boe_mpr_month_key, character(1L))
  links$asset_role[links$asset_role == "report_pdf" & (is.na(asset_month) | asset_month != rel$release)] <- "ancillary_pdf"
  links$release <- rep(rel$release, nrow(links))
  attr(links, "boe_provenance") <- list(receipt = boe_receipt_from_entry(entry), landing_url = rel$landing_url)
  links
}

#' Discover numeric assets for a forecast-report edition
#' @inheritParams boe_mpr_catalogue
#' @inheritParams boe_download_asset
#' @return Exact currently published asset URLs, types and roles, with a page receipt.
#' @keywords internal
boe_mpr_assets_live <- function(release = "latest", deadline = NULL, cache = TRUE,
    cache_only = FALSE, refresh = FALSE, cache_dir = boe_default_cache_dir()) {
  options <- list(deadline = boe_as_deadline(deadline), cache = cache, cache_only = cache_only,
    refresh = refresh, cache_dir = cache_dir)
  rel <- boe_mpr_release(release, options)
  boe_mpr_assets_for_release(rel, options)
}

boe_mpr_bind <- function(parts) {
  parts <- parts[vapply(parts, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1L))]
  if (!length(parts)) return(data.frame())
  columns <- unique(unlist(lapply(parts, names)))
  parts <- lapply(parts, function(x) {
    for (name in setdiff(columns, names(x))) x[[name]] <- NA
    x[columns]
  })
  out <- do.call(rbind, parts); rownames(out) <- NULL; out
}

boe_mpr_contextualize_historical <- function(historical, original) {
  # A general historical databank describes the path, but the original report
  # establishes whether it was a central forecast or an illustrative scenario.
  # Require an unambiguous original scenario covering all three core variables;
  # a supplementary scenario beside a central path cannot trigger this rule.
  core <- c("cpi_inflation", "gdp_growth", "unemployment")
  own <- original[original$measure %in% core, , drop = FALSE]
  if (!nrow(historical) || !nrow(own) || !all(core %in% own$measure) ||
      !all(own$kind == "scenario_projection") || length(unique(own$scenario)) != 1L) return(historical)
  changed <- historical$scenario == "central"
  if (!any(changed)) return(historical)
  historical$scenario[changed] <- own$scenario[[1L]]
  historical$scenario_label[changed] <- own$scenario_label[[1L]]
  historical$kind[changed & historical$kind == "central_forecast"] <- "scenario_projection"
  historical$classification_basis <- ifelse(changed,
    paste0("Original report scenario: ", own$source_url[[1L]], " (", own$source_sheet[[1L]], ")"), NA_character_)
  historical$series_id <- with(historical, paste("mpr", release, measure, scenario,
    statistic, frequency, kind, sep = ":"))
  historical
}

boe_mpr_finalize <- function(out, rel, options, include_history) {
  if (!nrow(out)) return(out)
  if (!include_history) {
    month <- as.integer(substr(out$forecast_vintage, 6L, 7L))
    year <- substr(out$forecast_vintage, 1L, 4L)
    start <- as.Date(paste0(year, ifelse(out$frequency == "Q",
      sprintf("-%02d-01", ((month - 1L) %/% 3L) * 3L + 1L), "-01-01")))
    out <- out[out$target_date >= start, , drop = FALSE]
  }
  if (nrow(out) > options$max_output_rows) boe_abort("MPR observations exceed `max_output_rows`.", "bounds")
  if (anyDuplicated(out[c("series_id", "target_period")])) {
    boe_abort("Forecast scalar identities are duplicate; source selection is ambiguous.", "schema_drift")
  }
  out <- out[order(out$series_id, out$target_date), , drop = FALSE]
  rownames(out) <- NULL; out
}

#' Discover published forecast paths and their source definitions
#'
#' The catalogue is parsed from the discovered edition's numeric source, rather
#' than an edition or sheet allowlist. Older requested vintages supplied by a
#' later official historical workbook retain a distinct `source_edition`.
#' @param release Exact discovered month key, or `latest` for the newest published edition.
#' @param include_excluded Include source sheet/table inventory entries not represented by paths.
#' @param include_distributions Include explicitly published probabilities, quantiles and distribution parameters.
#' @inheritParams boe_download_asset
#' @return A data frame of exact scalar path IDs, measures, source statistics,
#'   scenarios, units and target frequency, with provenance and coverage inventory.
#' @keywords internal
boe_mpr_catalogue_live <- function(release = "latest", include_excluded = FALSE,
    include_distributions = FALSE, deadline = NULL, cache = TRUE, cache_only = FALSE,
    refresh = FALSE, cache_dir = boe_default_cache_dir()) {
  x <- read_boe_mpr_forecasts_raw(release, include_distributions = include_distributions,
    deadline = deadline, cache = cache, cache_only = cache_only, refresh = refresh, cache_dir = cache_dir)
  keep <- intersect(c("release", "release_date", "release_date_precision", "source_edition", "source_sheet", "layout",
    "measure", "kind", "frequency", "statistic", "title", "units", "definition", "source_vintage",
    "scenario", "scenario_label", "forecast_vintage", "series_id"), names(x))
  out <- unique(as.data.frame(x)[keep]); out$supported <- TRUE
  if (include_excluded) {
    inv <- boe_provenance(x)$inventory
    if (is.data.frame(inv) && nrow(inv)) {
      inv <- inv[!inv$source_sheet %in% out$source_sheet, , drop = FALSE]
      inv$series_id <- NA_character_; inv$supported <- FALSE
      out <- boe_mpr_bind(list(out, inv))
    }
  }
  attr(out, "boe_provenance") <- boe_provenance(x)
  rownames(out) <- NULL; out
}

#' Read forecasts across the full official report lineage
#'
#' Discovers the requested official MPR, joint MPR/FSR or historical Inflation
#' Report edition. Layout rules recognize vintage matrices, scenario columns,
#' published scalar distribution summaries and numeric report tables. Historical
#' workbook fallback records its later source edition and original vintage label.
#' No scenario probability, quantile or chart point is reconstructed.
#' @inheritParams boe_mpr_catalogue
#' @param release One exact discovered edition key, or latest for the newest published edition.
#' @param measure,scenario,statistic,frequency,kind Exact path selectors; NULL requests all available scalar paths.
#' @param series_id Exact path IDs, instead of dimensional selectors.
#' @param include_history Include past targets from the requested path.
#' @param historical_source Allow a later discovered official historical workbook
#'   to supply an older forecast vintage when its own edition lacks a numeric path.
#' @param expected_sha256 Optional exact primary numeric-asset revision hash.
#' @param max_bytes,max_archive_members,max_uncompressed_bytes Asset/ZIP bounds.
#' @param max_sheets,max_rows,max_columns,max_cells Workbook bounds, including formatted cells.
#' @param max_output_rows Hard observation bound.
#' @inheritParams boe_download_asset
#' @return A `boe_mpr_forecasts` data frame with source cells/pages, original
#'   units/vintages, classifications, publication precision and revision receipts.
#'   Genuinely absent numeric forecasts, unavailable sources, dependency failures
#'   and unrecognized layouts produce distinct typed conditions with source evidence.
#' @export
read_boe_mpr_forecasts_raw <- function(release = "latest", measure = NULL, scenario = NULL,
    statistic = NULL, frequency = NULL, kind = NULL, series_id = NULL,
    include_history = FALSE, include_distributions = FALSE, historical_source = TRUE,
    expected_sha256 = NULL, deadline = NULL, max_bytes = 40 * 1024^2,
    max_archive_members = 200L, max_uncompressed_bytes = 200 * 1024^2,
    max_sheets = 150L, max_rows = 10000L, max_columns = 512L, max_cells = 2e6,
    max_output_rows = 200000L, cache = TRUE, cache_only = FALSE,
    refresh = FALSE, cache_dir = boe_default_cache_dir()) {
  options <- boe_mpr_options(deadline, max_bytes, max_archive_members, max_uncompressed_bytes,
    max_sheets, max_rows, max_columns, max_cells, max_output_rows, cache, cache_only, refresh, cache_dir)
  for (name in c("include_history", "include_distributions", "historical_source")) boe_assert_scalar(get(name), name, "logical")
  if (!is.null(expected_sha256) && (!is.character(expected_sha256) || length(expected_sha256) != 1L ||
      is.na(expected_sha256) || !grepl("^[a-f0-9]{64}$", expected_sha256))) boe_abort("`expected_sha256` must be one SHA-256 hash.", "config")
  selections <- list(measure = measure, scenario = scenario, statistic = statistic, frequency = frequency, kind = kind)
  if (!is.null(series_id)) {
    if (any(!vapply(selections, is.null, logical(1L)))) boe_abort("Select by `series_id` or by dimensions, not both.", "config")
    selections <- list(series_id = series_id)
  }
  boe_mpr_read_options(release, selections, include_history, include_distributions,
    historical_source, expected_sha256, options)
}

boe_mpr_read_options <- function(release, selections, include_history, include_distributions,
    historical_source, expected_sha256, options) {
  releases <- boe_mpr_discover(options)
  rel <- boe_mpr_release(release, options, releases)
  assets <- boe_mpr_assets_for_release(rel, options)
  result <- boe_mpr_parse_assets(rel, assets, options, include_distributions)
  if (!isTRUE(result$databank) && historical_source) {
    fallback <- boe_mpr_historical_fallback(rel, releases, options, include_distributions)
    original_keys <- if (nrow(result$data)) paste(result$data$series_id, result$data$target_period) else character()
    if (nrow(fallback$data)) {
      fallback$data <- boe_mpr_contextualize_historical(fallback$data, result$data)
      fallback_keys <- paste(fallback$data$series_id, fallback$data$target_period)
      result$alternate_sources <- boe_mpr_bind(list(result$alternate_sources,
        fallback$data[fallback_keys %in% original_keys, , drop = FALSE]))
      result$data <- boe_mpr_bind(list(result$data, fallback$data[!fallback_keys %in% original_keys, , drop = FALSE]))
    }
    result$receipts <- c(result$receipts, fallback$receipts)
    result$inventory <- boe_mpr_bind(list(result$inventory, fallback$inventory))
    result$evidence$historical_source_search <- list(inspected_editions = fallback$inspected_editions,
      matched_observations = nrow(fallback$data), source_rule = "Exact published vintage labels only")
    if (is.null(result$primary)) result$primary <- fallback$primary
  }
  if (!nrow(result$data)) {
    distributions_only <- !include_distributions && length(result$available_kinds) > 0L
    boe_abort(if (distributions_only) "This edition supplies distribution tables; request `include_distributions = TRUE` to read them." else if (isTRUE(result$numeric_table_seen))
      "A numeric forecast source is present but its structure is unrecognized; this is not evidence of numeric absence." else
      "This edition has no published scalar numeric forecast in the inspected sources. No chart values were inferred.",
      if (distributions_only) "no_scalar_forecast" else if (isTRUE(result$numeric_table_seen)) "unsupported_format" else "no_numeric_forecast",
      release = rel$release, assets = assets, inventory = result$inventory,
      source_evidence = result$evidence, available_kinds = result$available_kinds, receipts = result$receipts)
  }
  out <- boe_mpr_finalize(result$data, rel, options, include_history)
  out <- boe_mpr_select(out, selections)
  provenance <- result$primary
  if (!is.null(expected_sha256) && !identical(provenance$raw_sha256, expected_sha256)) {
    boe_abort("The primary numeric source differs from the expected revision hash.", "identity_mismatch")
  }
  provenance$source_identity <- list(surface = "mpr_forecasts", release = rel$release,
    landing_url = rel$landing_url, release_date = rel$release_date,
    release_date_precision = rel$release_date_precision, series_id = unique(out$series_id))
  provenance$parser_version <- "mpr-layouts-v2"
  provenance$receipts <- c(list(boe_provenance(assets)$receipt), result$receipts)
  provenance$inventory <- result$inventory
  provenance$alternate_sources <- result$alternate_sources %||% data.frame()
  provenance$sheet_inventory <- result$inventory
  provenance$rights <- boe_rights()
  provenance$transforms <- c("Official archive discovery; exact source structure and original forecast vintage",
    "No interpolation or inferred distributions; missing and censored cells retain status",
    "Target dates are representative period starts; unknown publication days remain missing",
    if (include_history) "Published past targets retained" else "Publication quarter/year and later targets retained")
  class(out) <- c("boe_mpr_forecasts", "data.frame")
  attr(out, "boe_provenance") <- provenance
  boe_time_left(options$deadline)
  out
}
