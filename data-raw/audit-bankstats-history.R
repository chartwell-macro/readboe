args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) {
  normalizePath(args[[1L]], mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
output_dir <- if (length(args) >= 2L) {
  args[[2L]]
} else {
  file.path(root, "docs", "audits", "bankstats-history")
}
workers <- if (length(args) >= 3L) {
  suppressWarnings(as.integer(args[[3L]]))
} else {
  2L
}
if (is.na(workers) || workers < 1L || workers > 4L) {
  stop("`workers` must be between one and four")
}

for (file in list.files(file.path(root, "R"), pattern = "[.]R$",
                        full.names = TRUE)) {
  sys.source(file, envir = globalenv())
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
batch_dir <- file.path(output_dir, "batches")
dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)

workbook_catalogue <- readRDS(
  file.path(root, "inst", "extdata", "iadb-catalogue.rds")
)
if (!nrow(workbook_catalogue) || anyNA(workbook_catalogue$series_id) ||
    anyDuplicated(workbook_catalogue$series_id)) {
  stop("The bundled catalogue does not have an exact unique identity set")
}

# Audit the full explicitly accepted browse scope: every exact identity linked
# by the official current Bankstats tables page. The rolling workbook snapshot
# is joined as a secondary capability, never used to exclude page identities.
tables_url <- "https://www.bankofengland.co.uk/statistics/tables"
tables_entry <- boe_request_raw(
  tables_url,
  deadline = boe_deadline(60),
  attempt_timeout = 30,
  max_bytes = 10 * 1024^2,
  cache = FALSE,
  refresh = TRUE,
  validate_response = boe_validate_not_error_html
)
tables_doc <- xml2::read_html(boe_raw_text(tables_entry$body))
links <- xml2::xml_find_all(tables_doc, "//a[@href]")
hrefs <- xml2::xml_attr(links, "href")
links <- links[!is.na(hrefs) & grepl("[?&]EC=", hrefs, perl = TRUE)]
collapse_values <- function(x) {
  x <- sort(unique(trimws(as.character(x))))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x)) paste(x, collapse = ";") else NA_character_
}
page_links <- do.call(rbind, lapply(links, function(link) {
  href <- xml2::xml_attr(link, "href")
  series_id <- toupper(sub(".*[?&]EC=([^&]+).*", "\\1", href))
  table_node <- xml2::xml_find_first(link, "preceding::h3[1]")
  section_node <- xml2::xml_find_first(link, "preceding::h2[1]")
  table_label <- trimws(gsub(
    "[[:space:]]+",
    " ",
    xml2::xml_text(table_node)
  ))
  source_series_url <- if (startsWith(href, "/")) {
    paste0("https://www.bankofengland.co.uk", href)
  } else {
    href
  }
  data.frame(
    series_id = series_id,
    displayed_code = trimws(xml2::xml_text(link)),
    table_id = trimws(sub("[[:space:]]+-.*$", "", table_label)),
    table_label = table_label,
    section = trimws(gsub(
      "[[:space:]]+",
      " ",
      xml2::xml_text(section_node)
    )),
    source_series_url = source_series_url,
    stringsAsFactors = FALSE
  )
}))
page_links <- page_links[
  grepl("^[A-Z0-9._-]+$", page_links$series_id),
  ,
  drop = FALSE
]
page_catalogue <- do.call(rbind, lapply(
  split(page_links, page_links$series_id),
  function(x) data.frame(
    series_id = x$series_id[[1L]],
    displayed_code = collapse_values(x$displayed_code),
    table_id = collapse_values(x$table_id),
    table_label = collapse_values(x$table_label),
    section = collapse_values(x$section),
    source_series_url = collapse_values(x$source_series_url),
    source_page_url = tables_entry$url,
    source_page_sha256 = tables_entry$raw_sha256,
    stringsAsFactors = FALSE
  )
))
page_catalogue <- page_catalogue[
  order(page_catalogue$series_id),
  ,
  drop = FALSE
]
rownames(page_catalogue) <- NULL
page_tables <- unique(trimws(unlist(strsplit(
  page_catalogue$table_id,
  ";",
  fixed = TRUE
))))
if (nrow(page_catalogue) < 5000L || length(page_tables) < 50L ||
    anyNA(page_catalogue$series_id) ||
    anyDuplicated(page_catalogue$series_id)) {
  stop("The official current Bankstats page identity set is incomplete")
}

catalogue <- page_catalogue
workbook_match <- match(catalogue$series_id, workbook_catalogue$series_id)
workbook_field <- function(preferred, fallback) {
  field <- if (preferred %in% names(workbook_catalogue)) {
    preferred
  } else {
    fallback
  }
  workbook_catalogue[[field]][workbook_match]
}
catalogue$workbook_label <- workbook_field("workbook_label", "label")
catalogue$workbook_frequency <- workbook_field(
  "workbook_frequency",
  "frequency"
)
catalogue$workbook_first_observation <- workbook_field(
  "workbook_first_observation",
  "first_observation"
)
catalogue$workbook_last_observation <- workbook_field(
  "workbook_last_observation",
  "last_observation"
)
catalogue$workbook_member <- workbook_field(
  "workbook_member",
  "workbook_member"
)
catalogue$workbook_sheet <- workbook_field(
  "workbook_sheet",
  "workbook_sheet"
)
catalogue$workbook_orientation <- workbook_field(
  "workbook_orientation",
  "workbook_orientation"
)
catalogue$chartable <- !is.na(workbook_match)
catalogue$status <- "bankstats_current"
catalogue$replacement_series_id <- NA_character_
catalogue$discontinued_note <- NA_character_
catalogue$surface_membership <- ifelse(
  catalogue$chartable,
  "bankstats_current;bankstats_current_bulk",
  "bankstats_current"
)
saveRDS(
  catalogue,
  file.path(output_dir, "page-catalogue.rds"),
  version = 3
)

saveRDS(
  data.frame(
    surface = "bankstats_tables_catalogue",
    url = tables_entry$url,
    final_url = tables_entry$final_url %||% tables_entry$url,
    retrieved_at = as.character(tables_entry$retrieved_at),
    raw_bytes = length(tables_entry$body),
    raw_sha256 = tables_entry$raw_sha256,
    status = tables_entry$status,
    link_rows = nrow(page_links),
    unique_series = nrow(page_catalogue),
    table_count = length(page_tables),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "page-catalogue-receipt.rds"),
  version = 3
)

receipt_row <- function(provenance, requested, returned) {
  data.frame(
    surface = "iadb_xml_metadata",
    requested_series = requested,
    returned_series = returned,
    url = provenance$canonical_url %||% "",
    final_url = provenance$final_url %||% "",
    retrieved_at = as.character(provenance$retrieved_at %||% ""),
    raw_bytes = as.numeric(provenance$raw_bytes %||% NA_real_),
    raw_sha256 = provenance$raw_sha256 %||% "",
    request_sha256 = boe_sha256_raw(charToRaw(
      provenance$canonical_url %||% ""
    )),
    stringsAsFactors = FALSE
  )
}

empty_unavailable <- function() data.frame(
  series_id = character(),
  error_class = character(),
  error_message = character(),
  stringsAsFactors = FALSE
)

fetch_metadata_once <- function(ids) {
  query <- list(
    CodeVer = "new",
    `xml.x` = "yes",
    Datefrom = "03/Jun/2007",
    Dateto = "now",
    SeriesCodes = paste(ids, collapse = ","),
    VPD = "Y",
    VFD = "Y",
    Omit = "-G"
  )
  entry <- boe_request_raw(
    .boe_iadb_url,
    query,
    deadline = boe_deadline(25),
    attempt_timeout = 20,
    max_bytes = 20 * 1024^2,
    max_tries = 1L,
    cache = FALSE,
    refresh = TRUE,
    validate_response = boe_validate_not_error_html
  )
  parsed <- boe_parse_iadb_xml(
    entry$body,
    expected_ids = ids,
    max_observations = 1L,
    max_series = max(300L, length(ids))
  )
  boe_attach_provenance(
    parsed$metadata,
    boe_receipt_from_entry(entry),
    list(surface = "iadb_metadata", series_id = ids),
    transforms = "XML syntax parsed; no observation data requested"
  )
}

fetch_ids <- function(ids) {
  ids <- unique(as.character(ids))
  last_error <- NULL
  for (attempt in seq_len(2L)) {
    value <- tryCatch(
      fetch_metadata_once(ids),
      error = identity
    )
    if (!inherits(value, "error")) {
      if (!setequal(ids, value$series_id) || anyDuplicated(value$series_id)) {
        stop("IADB metadata did not return the exact requested identity set")
      }
      return(list(
        metadata = value,
        receipts = receipt_row(
          boe_provenance(value),
          length(ids),
          nrow(value)
        ),
        unavailable = empty_unavailable()
      ))
    }
    last_error <- value
    if (attempt < 2L) Sys.sleep(0.5)
  }
  if (length(ids) == 1L) {
    return(list(
      metadata = NULL,
      receipts = NULL,
      unavailable = data.frame(
        series_id = ids[[1L]],
        error_class = class(last_error)[[1L]],
        error_message = conditionMessage(last_error),
        stringsAsFactors = FALSE
      )
    ))
  }
  midpoint <- floor(length(ids) / 2L)
  left <- fetch_ids(ids[seq_len(midpoint)])
  right <- fetch_ids(ids[seq.int(midpoint + 1L, length(ids))])
  list(
    metadata = rbind(left$metadata, right$metadata),
    receipts = rbind(left$receipts, right$receipts),
    unavailable = rbind(left$unavailable, right$unavailable)
  )
}

batch_size <- 25L
batch_indices <- split(
  seq_len(nrow(catalogue)),
  ceiling(seq_len(nrow(catalogue)) / batch_size)
)
fetch_batch <- function(batch_number) {
  path <- file.path(batch_dir, sprintf("batch-%03d.rds", batch_number))
  ids <- catalogue$series_id[batch_indices[[batch_number]]]
  if (file.exists(path)) {
    saved <- readRDS(path)
    if (is.null(saved$unavailable)) {
      saved$unavailable <- empty_unavailable()
    }
    saved_ids <- c(
      saved$metadata$series_id,
      saved$unavailable$series_id
    )
    if (setequal(ids, saved_ids) && !anyDuplicated(saved_ids)) {
      return(saved)
    }
  }
  cat(sprintf(
    "[%s] batch %d/%d (%d series)\n",
    format(Sys.time(), "%H:%M:%S"),
    batch_number,
    length(batch_indices),
    length(ids)
  ))
  flush.console()
  value <- fetch_ids(ids)
  saveRDS(value, path, version = 3)
  cat(sprintf(
    "[%s] saved batch %d/%d\n",
    format(Sys.time(), "%H:%M:%S"),
    batch_number,
    length(batch_indices)
  ))
  flush.console()
  value
}

batch_results <- if (workers == 1L) {
  lapply(seq_along(batch_indices), fetch_batch)
} else {
  cluster <- parallel::makeCluster(workers, outfile = "")
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterExport(
    cluster,
    c(
      "root", "batch_dir", "catalogue", "batch_indices",
      "receipt_row", "empty_unavailable", "fetch_metadata_once",
      "fetch_ids", "fetch_batch"
    ),
    envir = environment()
  )
  parallel::clusterEvalQ(cluster, {
    for (file in list.files(
      file.path(root, "R"),
      pattern = "[.]R$",
      full.names = TRUE
    )) {
      sys.source(file, envir = globalenv())
    }
    NULL
  })
  parallel::parLapplyLB(
    cluster,
    seq_along(batch_indices),
    fetch_batch,
    chunk.size = 1L
  )
}
if (any(vapply(batch_results, inherits, logical(1L), "try-error"))) {
  stop("At least one metadata batch failed")
}

metadata <- do.call(rbind, lapply(batch_results, `[[`, "metadata"))
receipts <- do.call(rbind, lapply(batch_results, `[[`, "receipts"))
unavailable <- do.call(rbind, lapply(batch_results, `[[`, "unavailable"))
if (is.null(unavailable)) unavailable <- empty_unavailable()
rownames(metadata) <- rownames(receipts) <- NULL
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
    anyDuplicated(metadata$series_id) ||
    anyDuplicated(unavailable$series_id) ||
    length(intersect(metadata$series_id, unavailable$series_id))) {
  stop("The canonical metadata crawl is incomplete or has duplicate identities")
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
catalogue <- catalogue[
  catalogue$series_id %in% metadata$series_id,
  ,
  drop = FALSE
]

canonical <- metadata[match(catalogue$series_id, metadata$series_id), ,
                      drop = FALSE]
series_audit <- data.frame(
  series_id = catalogue$series_id,
  table_id = catalogue$table_id,
  section = catalogue$section,
  workbook_label = catalogue$workbook_label,
  canonical_label = canonical$label,
  workbook_frequency = catalogue$workbook_frequency,
  canonical_frequency = canonical$frequency,
  workbook_first_observation = catalogue$workbook_first_observation,
  canonical_first_observation = canonical$first_observation,
  workbook_last_observation = catalogue$workbook_last_observation,
  canonical_last_observation = canonical$last_observation,
  workbook_chartable = catalogue$chartable,
  stringsAsFactors = FALSE
)
series_audit$workbook_first_date <- as.Date(
  series_audit$workbook_first_observation
)
series_audit$canonical_first_date <- as.Date(
  series_audit$canonical_first_observation
)
series_audit$workbook_last_date <- as.Date(
  series_audit$workbook_last_observation
)
series_audit$canonical_last_date <- as.Date(
  series_audit$canonical_last_observation
)
if (anyNA(series_audit$canonical_first_date) ||
    anyNA(series_audit$canonical_last_date)) {
  stop("Canonical metadata contains missing or invalid observation bounds")
}
series_audit$history_days_recovered <- as.integer(
  series_audit$workbook_first_date - series_audit$canonical_first_date
)
series_audit$canonical_precedes_workbook <-
  series_audit$canonical_first_date < series_audit$workbook_first_date
series_audit$canonical_extends_workbook <-
  series_audit$canonical_last_date > series_audit$workbook_last_date

membership <- do.call(rbind, lapply(seq_len(nrow(series_audit)), function(i) {
  ids <- trimws(strsplit(series_audit$table_id[[i]], ";", fixed = TRUE)[[1L]])
  data.frame(
    table_id = ids[nzchar(ids)],
    series_row = i,
    stringsAsFactors = FALSE
  )
}))
safe_date_summary <- function(x, fun) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  format(fun(x), "%Y-%m-%d")
}
safe_max_integer <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) max(x) else NA_integer_
}
table_audit <- do.call(rbind, lapply(
  split(membership$series_row, membership$table_id),
  function(rows) {
    rows <- unique(rows)
    x <- series_audit[rows, , drop = FALSE]
    data.frame(
      table_id = trimws(strsplit(x$table_id[[1L]], ";", fixed = TRUE)[[1L]])[[1L]],
      series_count = nrow(x),
      canonical_metadata_count = sum(!is.na(x$canonical_first_date)),
      current_workbook_chartable = sum(x$workbook_chartable),
      workbook_first = safe_date_summary(x$workbook_first_date, min),
      canonical_first = format(min(x$canonical_first_date), "%Y-%m-%d"),
      canonical_median_first = format(
        as.Date(stats::median(as.numeric(x$canonical_first_date)),
                origin = "1970-01-01"),
        "%Y-%m-%d"
      ),
      workbook_last = safe_date_summary(x$workbook_last_date, max),
      canonical_last = format(max(x$canonical_last_date), "%Y-%m-%d"),
      series_with_earlier_history = sum(
        x$canonical_precedes_workbook,
        na.rm = TRUE
      ),
      series_extending_past_workbook = sum(
        x$canonical_extends_workbook,
        na.rm = TRUE
      ),
      maximum_history_days_recovered = safe_max_integer(
        x$history_days_recovered
      ),
      stringsAsFactors = FALSE
    )
  }
))
table_audit$table_id <- rownames(table_audit)
rownames(table_audit) <- NULL
table_audit <- table_audit[order(table_audit$table_id), , drop = FALSE]

saveRDS(metadata, file.path(output_dir, "canonical-metadata.rds"), version = 3)
saveRDS(receipts, file.path(output_dir, "metadata-receipts.rds"), version = 3)
utils::write.csv(
  series_audit,
  file.path(output_dir, "series-history-audit.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  table_audit,
  file.path(output_dir, "table-history-audit.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  unavailable,
  file.path(output_dir, "unavailable-page-identities.csv"),
  row.names = FALSE,
  na = ""
)

cat(
  "Audited", nrow(series_audit), "series across", nrow(table_audit),
  "tables;", sum(series_audit$canonical_precedes_workbook, na.rm = TRUE),
  "series have canonical history before the current workbook window;",
  nrow(unavailable),
  "official page identities are rejected by the upstream IADB application.\n"
)
print(table_audit, row.names = FALSE)
