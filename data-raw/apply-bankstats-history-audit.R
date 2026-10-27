args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) {
  normalizePath(args[[1L]], mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
audit_dir <- if (length(args) >= 2L) {
  normalizePath(args[[2L]], mustWork = TRUE)
} else {
  normalizePath(
    file.path(root, "docs", "audits", "bankstats-history"),
    mustWork = TRUE
  )
}

for (file in list.files(file.path(root, "R"), pattern = "[.]R$",
                        full.names = TRUE)) {
  sys.source(file, envir = globalenv())
}

extdata <- file.path(root, "inst", "extdata")
catalogue_path <- file.path(extdata, "iadb-catalogue.rds")
manifest_path <- file.path(extdata, "iadb-catalogue-manifest.rds")
search_receipts_path <- file.path(
  extdata,
  "iadb-catalogue-search-receipts.rds"
)
receipts_path <- file.path(
  extdata,
  "iadb-catalogue-metadata-receipts.rds"
)
catalogue <- readRDS(file.path(audit_dir, "page-catalogue.rds"))
manifest <- readRDS(manifest_path)
metadata <- readRDS(file.path(audit_dir, "canonical-metadata.rds"))
receipts <- readRDS(file.path(audit_dir, "metadata-receipts.rds"))
page_receipt <- readRDS(file.path(audit_dir, "page-catalogue-receipt.rds"))
unavailable <- utils::read.csv(
  file.path(audit_dir, "unavailable-page-identities.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
if (nrow(unavailable)) {
  catalogue <- catalogue[
    !catalogue$series_id %in% unavailable$series_id,
    ,
    drop = FALSE
  ]
}

if (!is.data.frame(catalogue) || !is.data.frame(metadata) ||
    !setequal(catalogue$series_id, metadata$series_id) ||
    nrow(catalogue) != nrow(metadata) ||
    anyDuplicated(metadata$series_id)) {
  stop("The history audit does not exactly cover the bundled catalogue")
}
if (!is.data.frame(receipts) || !nrow(receipts) ||
    anyNA(receipts$raw_sha256) ||
    any(!grepl("^[0-9a-f]{64}$", receipts$raw_sha256))) {
  stop("The history audit receipts violate their hash contract")
}

metadata <- metadata[match(catalogue$series_id, metadata$series_id), ,
                     drop = FALSE]
canonical_fields <- c(
  "label", "country", "concatenated_label", "frequency",
  "frequency_code", "frequency_order", "first_observation",
  "last_observation", "definition", "definition_url", "category_name",
  "category_value", "category_label", "category_order", "series_comment"
)
if (!all(canonical_fields %in% names(metadata))) {
  stop("Canonical metadata is missing required IADB fields")
}

for (field in canonical_fields) catalogue[[field]] <- metadata[[field]]
catalogue$metadata_status <- "iadb_xml_canonical"
catalogue$latest_period_available <- catalogue$last_observation
catalogue$surface_membership <- vapply(
  catalogue$surface_membership,
  function(value) {
    paste(sort(unique(c(
      "iadb_xml",
      trimws(strsplit(value, ";", fixed = TRUE)[[1L]])
    ))), collapse = ";")
  },
  character(1L)
)

first <- as.Date(catalogue$first_observation)
last <- as.Date(catalogue$last_observation)
workbook_first <- as.Date(catalogue$workbook_first_observation)
if (anyNA(first) || anyNA(last) || any(first > last) ||
    any(first > workbook_first, na.rm = TRUE)) {
  stop("Canonical catalogue history bounds are invalid")
}
if (anyNA(catalogue$label) || any(!nzchar(trimws(catalogue$label))) ||
    anyNA(catalogue$frequency) || any(!nzchar(trimws(catalogue$frequency)))) {
  stop("Canonical catalogue labels or frequencies are incomplete")
}

saveRDS(catalogue, catalogue_path, version = 3)
saveRDS(receipts, receipts_path, version = 3)
search_receipts <- data.frame(
  surface = "bankstats_tables_catalogue",
  catalogue_scope = "bankstats_current_page_retrievable_exact",
  url = page_receipt$url,
  final_url = page_receipt$final_url,
  retrieved_at = page_receipt$retrieved_at,
  raw_bytes = page_receipt$raw_bytes,
  raw_sha256 = page_receipt$raw_sha256,
  status = page_receipt$status %||% 200L,
  link_rows = page_receipt$link_rows,
  unique_series = page_receipt$unique_series,
  retrievable_series = nrow(catalogue),
  unavailable_page_series = nrow(unavailable),
  table_count = page_receipt$table_count,
  chartable_series = sum(catalogue$chartable),
  non_chartable_page_series = sum(!catalogue$chartable),
  excluded_page_series = nrow(unavailable),
  stringsAsFactors = FALSE
)
saveRDS(search_receipts, search_receipts_path, version = 3)
manifest$generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
manifest$catalogue_scope <- "bankstats_current_page_retrievable_exact"
manifest$catalogue_complete_within_scope <- TRUE
manifest$catalogue_rows <- nrow(catalogue)
manifest$bankstats_link_rows <- page_receipt$link_rows
manifest$bankstats_page_series_rows <- page_receipt$unique_series
manifest$bankstats_chartable_link_rows <- NA_integer_
manifest$bankstats_series_rows <- nrow(catalogue)
manifest$bankstats_chartable_series_rows <- sum(catalogue$chartable)
manifest$bankstats_non_chartable_page_series <- sum(!catalogue$chartable)
manifest$bankstats_unavailable_page_series <- nrow(unavailable)
manifest$bankstats_excluded_page_series <- nrow(unavailable)
manifest$metadata_rows <- nrow(metadata)
manifest$metadata_status <- "iadb_xml_canonical"
manifest$metadata_request_rows <- nrow(receipts)
manifest$canonical_history_series <- nrow(catalogue)
manifest$canonical_history_precedes_workbook <- sum(
  first < workbook_first,
  na.rm = TRUE
)
manifest$bankstats_page_table_count <- page_receipt$table_count
manifest$bankstats_page_url <- page_receipt$url
manifest$search_source_url <- page_receipt$url
manifest$bankstats_page_sha256 <- page_receipt$raw_sha256
manifest$catalogue_snapshot_sha256 <- boe_sha256_file(catalogue_path)
manifest$search_receipts_sha256 <- boe_sha256_file(search_receipts_path)
manifest$metadata_receipts_sha256 <- boe_sha256_file(receipts_path)
saveRDS(manifest, manifest_path, version = 3)
utils::write.csv(
  manifest,
  file.path(extdata, "iadb-catalogue-manifest.csv"),
  row.names = FALSE,
  na = ""
)

cat(
  "Applied canonical IADB metadata to", nrow(catalogue), "series;",
  sum(first < workbook_first, na.rm = TRUE),
  "histories begin before the rolling current-workbook window.\n"
)
print(manifest)
