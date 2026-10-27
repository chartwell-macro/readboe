args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) normalizePath(args[[1L]], mustWork = TRUE) else normalizePath(".", mustWork = TRUE)
cache_dir <- if (length(args) >= 2L) args[[2L]] else file.path(tempdir(), "readboe-bankstats-build-cache")

for (file in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) {
  sys.source(file, envir = globalenv())
}

extdata <- file.path(root, "inst", "extdata")
dir.create(extdata, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
deadline <- boe_deadline(90 * 60)

request <- function(url, max_bytes) {
  boe_request_raw(
    url,
    deadline = deadline,
    attempt_timeout = 45,
    max_bytes = max_bytes,
    cache = TRUE,
    cache_dir = cache_dir,
    ttl = 24 * 60 * 60,
    max_cache_entries = 20L,
    max_cache_bytes = 250 * 1024^2,
    max_tries = 4L,
    validate_response = boe_validate_not_error_html
  )
}

collapse_values <- function(x, separator = ";") {
  x <- trimws(as.character(x))
  x <- sort(unique(x[!is.na(x) & nzchar(x)]))
  if (length(x)) paste(x, collapse = separator) else NA_character_
}

tables_url <- "https://www.bankofengland.co.uk/statistics/tables"
tables_entry <- request(tables_url, max_bytes = 10 * 1024^2)
tables_doc <- xml2::read_html(boe_raw_text(tables_entry$body))
all_links <- xml2::xml_find_all(tables_doc, "//a[@href]")
all_hrefs <- xml2::xml_attr(all_links, "href")
series_idx <- which(!is.na(all_hrefs) & grepl("[?&]EC=", all_hrefs, perl = TRUE))
series_links <- all_links[series_idx]

bankstats_links <- do.call(rbind, lapply(series_links, function(link) {
  href <- xml2::xml_attr(link, "href")
  series_id <- toupper(sub(".*[?&]EC=([^&]+).*", "\\1", href))
  table_node <- xml2::xml_find_first(link, "preceding::h3[1]")
  section_node <- xml2::xml_find_first(link, "preceding::h2[1]")
  table_label <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(table_node)))
  section <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(section_node)))
  table_id <- trimws(sub("[[:space:]]+-.*$", "", table_label))
  source_series_url <- if (startsWith(href, "/")) paste0("https://www.bankofengland.co.uk", href) else href
  data.frame(
    series_id = series_id,
    displayed_code = trimws(xml2::xml_text(link)),
    table_id = table_id,
    table_label = table_label,
    section = section,
    source_series_url = source_series_url,
    stringsAsFactors = FALSE
  )
}))

valid_id <- grepl("^[A-Z0-9._-]+$", bankstats_links$series_id)
bankstats_links <- bankstats_links[valid_id, , drop = FALSE]
if (nrow(bankstats_links) < 5000L) {
  stop("The official Bankstats page exposed fewer than 5000 valid series links")
}
if (anyNA(bankstats_links$table_id) || any(!nzchar(bankstats_links$table_id)) ||
    anyNA(bankstats_links$section) || any(!nzchar(bankstats_links$section))) {
  stop("The official Bankstats page no longer exposes stable table/section membership")
}

collapse_series <- function(x) {
  table_label <- collapse_values(x$table_label)
  data.frame(
    series_id = x$series_id[[1L]],
    label = table_label,
    displayed_code = collapse_values(x$displayed_code),
    status = "bankstats_current",
    metadata_status = "unavailable_bankstats_page_only",
    frequency = NA_character_,
    frequency_code = NA_character_,
    frequency_order = NA_integer_,
    first_observation = NA_character_,
    last_observation = NA_character_,
    country = NA_character_,
    concatenated_label = NA_character_,
    definition = NA_character_,
    definition_url = NA_character_,
    category_name = NA_character_,
    category_value = NA_character_,
    category_label = NA_character_,
    category_order = NA_character_,
    series_comment = NA_character_,
    table_id = collapse_values(x$table_id),
    table_label = table_label,
    section = collapse_values(x$section),
    latest_period_available = NA_character_,
    replacement_series_id = NA_character_,
    discontinued_note = NA_character_,
    surface_membership = "bankstats_current",
    source_series_url = collapse_values(x$source_series_url),
    source_page_url = tables_entry$url,
    source_page_sha256 = tables_entry$raw_sha256,
    stringsAsFactors = FALSE
  )
}

page_catalogue <- do.call(rbind, lapply(split(bankstats_links, bankstats_links$series_id), collapse_series))
page_catalogue <- page_catalogue[order(page_catalogue$series_id), , drop = FALSE]
rownames(page_catalogue) <- NULL
if (nrow(page_catalogue) < 5000L || anyNA(page_catalogue$series_id) || anyDuplicated(page_catalogue$series_id)) {
  stop("The current Bankstats catalogue violates its identity/count contract")
}

search_receipts <- data.frame(
  surface = "bankstats_tables_catalogue",
  catalogue_scope = "bankstats_current_page_retrievable_exact",
  url = tables_entry$url,
  final_url = tables_entry$final_url %||% tables_entry$url,
  retrieved_at = format(tables_entry$retrieved_at, tz = "UTC", usetz = TRUE),
  raw_bytes = length(tables_entry$body),
  raw_sha256 = tables_entry$raw_sha256,
  etag = tables_entry$etag,
  last_modified = tables_entry$last_modified,
  status = tables_entry$status,
  link_rows = nrow(bankstats_links),
  unique_series = nrow(page_catalogue),
  stringsAsFactors = FALSE
)
metadata_receipts <- data.frame(
  surface = character(), requested_series = integer(), returned_series = integer(),
  note = character(), stringsAsFactors = FALSE
)

bulk_url <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/bankstats-latest-tables.zip"
bulk_entry <- request(bulk_url, max_bytes = 30 * 1024^2)
bulk_path <- boe_write_raw_temp(bulk_entry$body, ".zip")
on.exit(unlink(bulk_path), add = TRUE)
bulk_listing <- boe_safe_zip_members(
  bulk_path,
  max_members = 100L,
  max_uncompressed_bytes = 500 * 1024^2
)
workbook_members <- bulk_listing$Name[grepl("[.]xls$", bulk_listing$Name, ignore.case = TRUE)]
if (length(workbook_members) != 64L) stop("Expected exactly 64 current Bankstats workbook members")
member_table_ids <- sub("^.*Tab(.*)[.]xls$", "\\1", workbook_members, ignore.case = TRUE)
bankstats_assets <- data.frame(
  table_id = paste(member_table_ids, collapse = ";"),
  table_label = "Bankstats (Monetary & Financial Statistics) - Latest Tables",
  section = paste(sort(unique(substr(member_table_ids, 1L, 1L))), collapse = ";"),
  url = bulk_entry$url,
  file_type = "zip",
  archive = FALSE,
  member_count = length(workbook_members),
  member_names = paste(workbook_members, collapse = ";"),
  source_page_url = tables_entry$url,
  source_page_sha256 = tables_entry$raw_sha256,
  source_sha256 = bulk_entry$raw_sha256,
  stringsAsFactors = FALSE
)

choose_workbook_label <- function(x) {
  x <- trimws(as.character(x))
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (!length(x)) return(NA_character_)
  x[[1L]]
}

frequency_code <- function(value) {
  unname(c(
    "Daily" = "D", "Weekly" = "W", "Monthly" = "M",
    "Quarterly" = "Q", "Semiannual" = "H", "Annual" = "A",
    "Irregular" = "I", "Unknown" = NA_character_
  )[value])
}

workbook_paths <- boe_extract_members(
  bulk_path,
  workbook_members,
  max_members = 100L,
  max_uncompressed_bytes = 500 * 1024^2
)
on.exit(unlink(attr(workbook_paths, "extraction_dir"), recursive = TRUE), add = TRUE)

chartable_pieces <- list()
for (member_index in seq_along(workbook_members)) {
  member <- workbook_members[[member_index]]
  table_id <- member_table_ids[[member_index]]
  candidates <- bankstats_links[
    bankstats_links$table_id == table_id,
    ,
    drop = FALSE
  ]
  if (!nrow(candidates)) next
  member_bytes <- readBin(
    workbook_paths[[member_index]],
    "raw",
    n = file.info(workbook_paths[[member_index]])$size
  )
  workbook <- boe_bankstats_cell_index(boe_read_workbook_raw(
    member_bytes,
    paste0(".", tools::file_ext(member)),
    max_sheets = 50L,
    max_rows = 1e6,
    max_columns = 2000L,
    max_cells = 5e6
  ))
  for (candidate_index in seq_len(nrow(candidates))) {
    candidate <- candidates[candidate_index, , drop = FALSE]
    parsed <- boe_extract_bankstats_series(
      workbook,
      candidate$series_id[[1L]],
      candidate$displayed_code[[1L]],
      table_id,
      member,
      max_output_rows = 250000L
    )
    if (!nrow(parsed)) next
    chartable_pieces[[length(chartable_pieces) + 1L]] <- data.frame(
      series_id = candidate$series_id[[1L]],
      displayed_code = candidate$displayed_code[[1L]],
      workbook_label = choose_workbook_label(parsed$series_label),
      frequency = collapse_values(parsed$series_frequency),
      first_observation = format(min(parsed$date), "%Y-%m-%d"),
      last_observation = format(max(parsed$date), "%Y-%m-%d"),
      table_id = table_id,
      table_label = candidate$table_label[[1L]],
      section = candidate$section[[1L]],
      workbook_member = member,
      workbook_sheet = collapse_values(parsed$source_sheet),
      workbook_orientation = collapse_values(parsed$orientation),
      source_series_url = candidate$source_series_url[[1L]],
      stringsAsFactors = FALSE
    )
  }
}

if (!length(chartable_pieces)) {
  stop("The current Bankstats bulk workbooks exposed no chartable identities")
}
chartable_occurrences <- unique(do.call(rbind, chartable_pieces))
rownames(chartable_occurrences) <- NULL

collapse_chartable_series <- function(x) {
  label <- choose_workbook_label(x$workbook_label)
  frequencies <- trimws(unique(x$frequency[!is.na(x$frequency) & nzchar(x$frequency)]))
  frequency <- if (length(frequencies) == 1L) frequencies[[1L]] else
    collapse_values(frequencies)
  first <- min(as.Date(x$first_observation))
  last <- max(as.Date(x$last_observation))
  data.frame(
    series_id = x$series_id[[1L]],
    label = label,
    displayed_code = collapse_values(x$displayed_code),
    status = "bankstats_current",
    metadata_status = "bankstats_bulk_workbook",
    frequency = frequency,
    frequency_code = collapse_values(frequency_code(frequencies)),
    frequency_order = NA_integer_,
    first_observation = format(first, "%Y-%m-%d"),
    last_observation = format(last, "%Y-%m-%d"),
    country = "United Kingdom",
    concatenated_label = label,
    definition = NA_character_,
    definition_url = NA_character_,
    category_name = NA_character_,
    category_value = NA_character_,
    category_label = NA_character_,
    category_order = NA_character_,
    series_comment = NA_character_,
    table_id = collapse_values(x$table_id),
    table_label = collapse_values(x$table_label),
    section = collapse_values(x$section),
    workbook_member = collapse_values(x$workbook_member),
    workbook_sheet = collapse_values(x$workbook_sheet),
    workbook_orientation = collapse_values(x$workbook_orientation),
    chartable = TRUE,
    latest_period_available = format(last, "%Y-%m-%d"),
    replacement_series_id = NA_character_,
    discontinued_note = NA_character_,
    surface_membership = "bankstats_current;bankstats_current_bulk",
    source_series_url = collapse_values(x$source_series_url),
    source_page_url = tables_entry$url,
    source_page_sha256 = tables_entry$raw_sha256,
    stringsAsFactors = FALSE
  )
}

chartable_catalogue <- do.call(rbind, lapply(
  split(chartable_occurrences, chartable_occurrences$series_id),
  collapse_chartable_series
))
chartable_catalogue <- chartable_catalogue[
  order(chartable_catalogue$series_id),
  ,
  drop = FALSE
]
rownames(chartable_catalogue) <- NULL
if (nrow(chartable_catalogue) < 500L ||
    anyNA(chartable_catalogue$series_id) ||
    anyDuplicated(chartable_catalogue$series_id) ||
    anyNA(chartable_catalogue$label) ||
    any(!nzchar(chartable_catalogue$label)) ||
    !all(chartable_catalogue$chartable)) {
  stop("The chartable Bankstats catalogue violates its identity/title contract")
}
chartable_first <- as.Date(chartable_catalogue$first_observation)
chartable_last <- as.Date(chartable_catalogue$last_observation)
if (anyNA(chartable_first) || anyNA(chartable_last) ||
    any(chartable_first > chartable_last) ||
    any(chartable_last > Sys.Date() + 366L)) {
  stop("The chartable Bankstats catalogue violates its period-range contract")
}

# Browse scope is every exact identity linked by the official current tables
# page. Workbook extraction is a secondary capability: the rolling workbooks
# are not historical authorities and must not decide whether an official page
# identity is browseable or whether it has full history.
catalogue <- page_catalogue
chartable_match <- match(catalogue$series_id, chartable_catalogue$series_id)
catalogue$workbook_label <- chartable_catalogue$label[chartable_match]
catalogue$workbook_frequency <- chartable_catalogue$frequency[chartable_match]
catalogue$workbook_first_observation <-
  chartable_catalogue$first_observation[chartable_match]
catalogue$workbook_last_observation <-
  chartable_catalogue$last_observation[chartable_match]
catalogue$workbook_member <-
  chartable_catalogue$workbook_member[chartable_match]
catalogue$workbook_sheet <-
  chartable_catalogue$workbook_sheet[chartable_match]
catalogue$workbook_orientation <-
  chartable_catalogue$workbook_orientation[chartable_match]
catalogue$chartable <- !is.na(chartable_match)
catalogue$surface_membership <- ifelse(
  catalogue$chartable,
  "bankstats_current;bankstats_current_bulk",
  "bankstats_current"
)
if (nrow(catalogue) != nrow(page_catalogue) ||
    anyDuplicated(catalogue$series_id) ||
    anyNA(catalogue$table_id) ||
    any(!nzchar(catalogue$table_id))) {
  stop("The exact page-linked Bankstats catalogue is incomplete")
}

# Current Bankstats workbooks are rolling publication tables rather than full
# history files. Canonical IADB XML metadata supplies the true series title,
# frequency, and first/last observation bounds. Twenty-five identifiers keep
# the official GET route below its practical URL-length ceiling; every response is
# exact-set checked and receipt-hashed.
metadata_batches <- split(
  catalogue$series_id,
  ceiling(seq_len(nrow(catalogue)) / 25L)
)
empty_unavailable <- function() data.frame(
  series_id = character(),
  error_class = character(),
  error_message = character(),
  stringsAsFactors = FALSE
)
metadata_receipt <- function(value, requested) {
  provenance <- boe_provenance(value)
  data.frame(
    surface = "iadb_xml_metadata",
    requested_series = requested,
    returned_series = nrow(value),
    url = provenance$canonical_url,
    final_url = provenance$final_url,
    retrieved_at = format(provenance$retrieved_at, tz = "UTC", usetz = TRUE),
    raw_bytes = provenance$raw_bytes,
    raw_sha256 = provenance$raw_sha256,
    stringsAsFactors = FALSE
  )
}
fetch_metadata <- function(ids) {
  value <- tryCatch(
    boe_series_metadata(
      ids,
      deadline = deadline,
      max_bytes = 20 * 1024^2,
      cache = TRUE,
      cache_dir = cache_dir
    ),
    error = identity
  )
  if (!inherits(value, "error")) {
    if (!setequal(ids, value$series_id) || anyDuplicated(value$series_id)) {
      stop("Canonical IADB metadata did not return an exact identity set")
    }
    return(list(
      metadata = value,
      receipts = metadata_receipt(value, length(ids)),
      unavailable = empty_unavailable()
    ))
  }
  if (length(ids) == 1L) {
    return(list(
      metadata = NULL,
      receipts = NULL,
      unavailable = data.frame(
        series_id = ids[[1L]],
        error_class = class(value)[[1L]],
        error_message = conditionMessage(value),
        stringsAsFactors = FALSE
      )
    ))
  }
  midpoint <- floor(length(ids) / 2L)
  left <- fetch_metadata(ids[seq_len(midpoint)])
  right <- fetch_metadata(ids[seq.int(midpoint + 1L, length(ids))])
  list(
    metadata = rbind(left$metadata, right$metadata),
    receipts = rbind(left$receipts, right$receipts),
    unavailable = rbind(left$unavailable, right$unavailable)
  )
}
metadata_results <- lapply(metadata_batches, fetch_metadata)
metadata_pieces <- lapply(metadata_results, `[[`, "metadata")
metadata_receipt_pieces <- lapply(metadata_results, `[[`, "receipts")
metadata <- do.call(rbind, metadata_pieces)
metadata_receipts <- do.call(rbind, metadata_receipt_pieces)
unavailable <- do.call(rbind, lapply(metadata_results, `[[`, "unavailable"))
if (is.null(unavailable)) unavailable <- empty_unavailable()
rownames(metadata) <- rownames(metadata_receipts) <- NULL
invalid_bounds <- is.na(suppressWarnings(as.Date(metadata$first_observation))) |
  is.na(suppressWarnings(as.Date(metadata$last_observation)))
if (any(invalid_bounds)) {
  unavailable <- rbind(
    unavailable,
    data.frame(
      series_id = metadata$series_id[invalid_bounds],
      error_class = "readboe_upstream_unavailable",
      error_message = paste0(
        "Canonical metadata has no observation bounds: ",
        metadata$label[invalid_bounds]
      ),
      stringsAsFactors = FALSE
    )
  )
  metadata <- metadata[!invalid_bounds, , drop = FALSE]
}
if (!setequal(
      catalogue$series_id,
      c(metadata$series_id, unavailable$series_id)
    ) ||
    anyDuplicated(c(metadata$series_id, unavailable$series_id))) {
  stop("Canonical IADB metadata is incomplete or duplicated")
}
if (nrow(unavailable)) {
  unavailable <- merge(
    unavailable,
    catalogue[c(
      "series_id", "displayed_code", "table_id", "table_label", "section",
      "source_series_url", "source_page_url", "source_page_sha256",
      "chartable"
    )],
    by = "series_id",
    all.x = TRUE,
    sort = FALSE
  )
}
page_catalogue_rows <- nrow(catalogue)
catalogue <- catalogue[
  catalogue$series_id %in% metadata$series_id,
  ,
  drop = FALSE
]
metadata <- metadata[match(catalogue$series_id, metadata$series_id), ,
                     drop = FALSE]
canonical_fields <- c(
  "label", "country", "concatenated_label", "frequency",
  "frequency_code", "frequency_order", "first_observation",
  "last_observation", "definition", "definition_url", "category_name",
  "category_value", "category_label", "category_order", "series_comment"
)
for (field in canonical_fields) catalogue[[field]] <- metadata[[field]]
catalogue$metadata_status <- "iadb_xml_canonical"
catalogue$latest_period_available <- catalogue$last_observation
catalogue$surface_membership <- vapply(
  catalogue$surface_membership,
  function(value) paste(sort(unique(c(
    "iadb_xml",
    trimws(strsplit(value, ";", fixed = TRUE)[[1L]])
  ))), collapse = ";"),
  character(1L)
)
canonical_first <- as.Date(catalogue$first_observation)
canonical_last <- as.Date(catalogue$last_observation)
if (anyNA(canonical_first) || anyNA(canonical_last) ||
    any(canonical_first > canonical_last) ||
    any(canonical_last > Sys.Date() + 366L)) {
  stop("Canonical IADB metadata violates the history-range contract")
}
search_receipts$chartable_link_rows <- nrow(chartable_occurrences)
search_receipts$chartable_series <- sum(catalogue$chartable)
search_receipts$non_chartable_page_series <-
  sum(!catalogue$chartable)
search_receipts$unavailable_page_series <- nrow(unavailable)
search_receipts$excluded_page_series <- nrow(unavailable)

audit_dir <- file.path(root, "docs", "audits")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  unavailable,
  file.path(audit_dir, "bankstats-unavailable-page-identities.csv"),
  row.names = FALSE,
  na = ""
)

saveRDS(catalogue, file.path(extdata, "iadb-catalogue.rds"), version = 3)
saveRDS(search_receipts, file.path(extdata, "iadb-catalogue-search-receipts.rds"), version = 3)
saveRDS(metadata_receipts, file.path(extdata, "iadb-catalogue-metadata-receipts.rds"), version = 3)
saveRDS(bankstats_assets, file.path(extdata, "bankstats-assets.rds"), version = 3)

manifest <- data.frame(
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  catalogue_scope = "bankstats_current_page_retrievable_exact",
  catalogue_complete_within_scope = TRUE,
  catalogue_rows = nrow(catalogue),
  bankstats_link_rows = nrow(bankstats_links),
  bankstats_page_series_rows = page_catalogue_rows,
  bankstats_chartable_link_rows = nrow(chartable_occurrences),
  bankstats_series_rows = nrow(catalogue),
  bankstats_chartable_series_rows = sum(catalogue$chartable),
  bankstats_non_chartable_page_series =
    sum(!catalogue$chartable),
  bankstats_unavailable_page_series = nrow(unavailable),
  bankstats_excluded_page_series = nrow(unavailable),
  metadata_rows = nrow(catalogue),
  metadata_status = "iadb_xml_canonical",
  metadata_request_rows = nrow(metadata_receipts),
  canonical_history_series = nrow(catalogue),
  canonical_history_precedes_workbook = sum(
    as.Date(catalogue$first_observation) <
      as.Date(catalogue$workbook_first_observation),
    na.rm = TRUE
  ),
  bankstats_workbook_members = length(workbook_members),
  search_source_url = tables_entry$url,
  bankstats_page_url = tables_entry$url,
  bankstats_bulk_url = bulk_entry$url,
  bankstats_page_sha256 = tables_entry$raw_sha256,
  bankstats_bulk_sha256 = bulk_entry$raw_sha256,
  catalogue_snapshot_sha256 = boe_sha256_file(file.path(extdata, "iadb-catalogue.rds")),
  search_receipts_sha256 = boe_sha256_file(file.path(extdata, "iadb-catalogue-search-receipts.rds")),
  metadata_receipts_sha256 = boe_sha256_file(file.path(extdata, "iadb-catalogue-metadata-receipts.rds")),
  bankstats_assets_sha256 = boe_sha256_file(file.path(extdata, "bankstats-assets.rds")),
  stringsAsFactors = FALSE
)
saveRDS(manifest, file.path(extdata, "iadb-catalogue-manifest.rds"), version = 3)
utils::write.csv(
  manifest,
  file.path(extdata, "iadb-catalogue-manifest.csv"),
  row.names = FALSE,
  na = ""
)

cat(
  "Retrievable Bankstats catalogue:", nrow(catalogue), "unique series from",
  nrow(bankstats_links), "page links;", sum(catalogue$chartable),
  "have current-workbook occurrences and", nrow(unavailable),
  "stale upstream identities were excluded.\n"
)
print(manifest)
