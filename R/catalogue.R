boe_catalogue_path <- function(catalogue_path = NULL) {
  path <- catalogue_path %||% system.file("extdata", "iadb-catalogue.rds", package = "readboe")
  if (!nzchar(path) || !file.exists(path)) boe_abort("The pinned IADB catalogue snapshot is unavailable.", "config")
  path
}

`%||%` <- function(x, y) if (is.null(x)) y else x

boe_assert_pinned_snapshot <- function(path, hash_column) {
  manifest_path <- file.path(dirname(path), "iadb-catalogue-manifest.rds")
  manifest <- tryCatch(readRDS(manifest_path), error = function(e) NULL)
  if (!is.data.frame(manifest) || nrow(manifest) != 1L || !hash_column %in% names(manifest)) {
    boe_abort("The catalogue manifest cannot authenticate a pinned snapshot.", "schema_drift",
      snapshot = basename(path), hash_column = hash_column)
  }
  expected <- manifest[[hash_column]][[1L]]
  if (!is.character(expected) || is.na(expected) || !grepl("^[0-9a-f]{64}$", expected)) {
    boe_abort("The catalogue manifest contains an invalid snapshot hash.", "schema_drift",
      snapshot = basename(path), hash_column = hash_column)
  }
  actual <- boe_sha256_file(path)
  if (!identical(actual, expected)) boe_abort("A pinned snapshot does not match its manifest hash.",
    "identity_mismatch", snapshot = basename(path), expected_sha256 = expected, actual_sha256 = actual)
  invisible(TRUE)
}

boe_load_catalogue <- function(catalogue_path = NULL) {
  path <- boe_catalogue_path(catalogue_path)
  if (is.null(catalogue_path)) boe_assert_pinned_snapshot(path, "catalogue_snapshot_sha256")
  out <- tryCatch(readRDS(path), error = function(e) boe_abort("The IADB catalogue snapshot cannot be read.", "cache_corruption"))
  required <- c("series_id", "label", "status", "metadata_status", "frequency", "frequency_code",
    "table_id", "section", "surface_membership")
  if (!is.data.frame(out) || !all(required %in% names(out)) || anyNA(out$series_id) || anyDuplicated(out$series_id)) {
    boe_abort("The IADB catalogue snapshot violates its schema or identity contract.", "schema_drift")
  }
  out
}

boe_match_list_field <- function(x, wanted) {
  wanted <- tolower(trimws(wanted))
  vapply(strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE), function(values) {
    any(tolower(trimws(values)) %in% wanted)
  }, logical(1))
}

boe_filter_catalogue <- function(out, query = NULL, series_id = NULL, status = NULL,
                                 metadata_status = NULL, frequency = NULL, table_id = NULL,
                                 section = NULL, surface_membership = NULL) {
  validate_filter <- function(x, name) {
    if (!is.null(x) && (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(trimws(x))))) {
      boe_abort(sprintf("`%s` must be a non-empty character vector when supplied.", name), "config")
    }
  }
  for (name in c("series_id", "status", "metadata_status", "frequency", "table_id", "section", "surface_membership")) {
    validate_filter(get(name), name)
  }
  if (!is.null(series_id)) out <- out[toupper(out$series_id) %in% toupper(trimws(series_id)), , drop = FALSE]
  if (!is.null(status)) out <- out[tolower(out$status) %in% tolower(trimws(status)), , drop = FALSE]
  if (!is.null(metadata_status)) out <- out[tolower(out$metadata_status) %in% tolower(trimws(metadata_status)), , drop = FALSE]
  if (!is.null(frequency)) {
    wanted <- tolower(trimws(frequency))
    out <- out[tolower(out$frequency) %in% wanted | tolower(out$frequency_code) %in% wanted, , drop = FALSE]
  }
  if (!is.null(table_id)) out <- out[boe_match_list_field(out$table_id, table_id), , drop = FALSE]
  if (!is.null(section)) out <- out[boe_match_list_field(out$section, section), , drop = FALSE]
  if (!is.null(surface_membership)) out <- out[boe_match_list_field(out$surface_membership, surface_membership), , drop = FALSE]
  if (!is.null(query)) {
    if (!is.character(query) || length(query) != 1L || is.na(query)) boe_abort("`query` must be one non-missing character value.", "config")
    terms <- regmatches(query, gregexpr('"[^"]+"|\\S+', query, perl = TRUE))[[1L]]
    terms <- gsub('^"|"$', "", terms)
    terms <- tolower(terms[nzchar(terms)])
    if (length(terms)) {
      cols <- intersect(c("series_id", "label", "concatenated_label", "frequency", "frequency_code",
        "definition", "category_name", "category_value", "category_label", "table_id", "table_label", "section"), names(out))
      hay <- tolower(do.call(paste, c(out[cols], sep = " ")))
      keep <- Reduce(`&`, lapply(terms, function(term) grepl(term, hay, fixed = TRUE)))
      out <- out[keep, , drop = FALSE]
    }
  }
  rownames(out) <- NULL
  out
}

#' Browse the pinned Bankstats series catalogue
#'
#' Searches the committed, hash-authenticated catalogue of exact series
#' identities exposed by the current official Bankstats tables page.  Every
#' exact facet is applied before keyword matching and before `limit`.  `query`
#' is a case-insensitive implicit-AND search over the documented identity and
#' label fields; quoted text is tokenised as one phrase.  The snapshot is an
#' offline discovery authority: it does not infer other IADB identifiers or
#' contact the Bank of England.
#'
#' @param query Optional one-string keyword or phrase query.  Matching is fixed
#'   substring matching after lower-casing; an empty query is equivalent to no
#'   query.
#' @param series_id Optional non-empty character vector of exact identifiers;
#'   matching is case-insensitive.
#' @param status Optional non-empty character vector of exact status values.
#' @param metadata_status Optional non-empty character vector of exact metadata
#'   availability values.
#' @param frequency Optional non-empty character vector of frequency labels or
#'   official frequency codes.
#' @param table_id Optional non-empty character vector.  A row matches when any
#'   semicolon-delimited table membership equals a requested value.
#' @param section Optional non-empty character vector with the same
#'   semicolon-membership semantics as `table_id`.
#' @param surface_membership Optional non-empty character vector with the same
#'   semicolon-membership semantics as `table_id`.
#' @param limit Positive integer maximum after all filters, from 1 through
#'   100000.  Rows retain snapshot order and are not randomly sampled.
#' @param catalogue_path Optional path to a compatible catalogue snapshot for
#'   deterministic testing.  A custom path is read but is not authenticated
#'   against the package's pinned manifest; the default bundled path is
#'   hash-authenticated.
#' @return A data frame with 37 columns, in snapshot order:
#'   `series_id`, `displayed_code`, `table_id`, `table_label`, `section`,
#'   `source_series_url`, `source_page_url`, `source_page_sha256`,
#'   `workbook_label`, `workbook_frequency`, `workbook_first_observation`,
#'   `workbook_last_observation`, `workbook_member`, `workbook_sheet`,
#'   `workbook_orientation`, `chartable`, `status`, `replacement_series_id`,
#'   `discontinued_note`, `surface_membership`, `label`, `country`,
#'   `concatenated_label`, `frequency`, `frequency_code`, `frequency_order`,
#'   `first_observation`, `last_observation`, `definition`, `definition_url`,
#'   `category_name`, `category_value`, `category_label`, `category_order`,
#'   `series_comment`, `metadata_status`, and `latest_period_available`.
#'   Row names are reset.  The `boe_catalogue_provenance` attribute is a list
#'   with `snapshot_sha256`, normalised `snapshot_path`,
#'   `matched_before_cap`, and `returned`.
#' @details The bundled snapshot currently represents 5,252 retrievable rows
#'   from the current Bankstats page.  `chartable` describes whether the
#'   shorter rolling bulk workbook has an extractable occurrence; it does not
#'   restrict canonical IADB history.  Invalid filters signal
#'   `readboe_config`; a limit above 100000 signals `readboe_bounds`; missing,
#'   corrupt, schema-drifting, or hash-mismatched snapshots signal typed
#'   `readboe_config`, `readboe_cache_corruption`, `readboe_schema_drift`, or
#'   `readboe_identity_mismatch` conditions.  No cache, network, option, or
#'   global-state side effect occurs.
#' @seealso [boe_catalogue_facets()], [boe_catalogue_manifest()],
#'   [boe_catalogue_receipts()], [read_boe_bankstats_series()]
#' @examples
#' one <- boe_catalogue(series_id = "IUDBEDR", limit = 1L)
#' stopifnot(is.data.frame(one), nrow(one) <= 1L,
#'           all(c("series_id", "label", "frequency") %in% names(one)),
#'           !is.null(attr(one, "boe_catalogue_provenance")))
#' @export
boe_catalogue <- function(query = NULL, series_id = NULL, status = NULL,
                          metadata_status = NULL, frequency = NULL, table_id = NULL,
                          section = NULL, surface_membership = NULL, limit = 100L,
                          catalogue_path = NULL) {
  boe_check_positive(limit, "limit", integer = TRUE)
  if (limit > 100000L) boe_abort("`limit` may not exceed 100000.", "bounds")
  path <- boe_catalogue_path(catalogue_path)
  out <- boe_filter_catalogue(boe_load_catalogue(path), query, series_id, status,
    metadata_status, frequency, table_id, section, surface_membership)
  total <- nrow(out)
  if (total > limit) out <- out[seq_len(limit), , drop = FALSE]
  attr(out, "boe_catalogue_provenance") <- list(
    snapshot_sha256 = boe_sha256_file(path),
    snapshot_path = normalizePath(path, winslash = "/", mustWork = TRUE),
    matched_before_cap = total,
    returned = nrow(out)
  )
  out
}

#' Count browse facets in the pinned Bankstats catalogue
#'
#' Applies the same offline filters as [boe_catalogue()] and summarises the
#' resulting rows for user-interface facets.  It does not make a remote
#' request, and its result intentionally carries no provenance attribute.
#'
#' @inheritParams boe_catalogue
#' @return A named list with exactly six elements: `status`,
#'   `metadata_status`, `frequency`, `table_id`, `section`, and
#'   `surface_membership`.  Each element is a data frame with columns
#'   `facet`, `value`, and `n`.  Simple fields count values and represent
#'   missing values as `"<missing>"`; semicolon-delimited membership fields are
#'   split before counting.  Counts are sorted decreasingly.
#' @details Validation, snapshot authentication, and missing/corrupt-snapshot
#'   errors follow [boe_catalogue()].  `limit` is not an argument because all
#'   rows surviving the filters are counted.  No network or cache side effect
#'   occurs.
#' @seealso [boe_catalogue()], [boe_catalogue_manifest()]
#' @examples
#' facets <- boe_catalogue_facets(frequency = "Daily")
#' stopifnot(is.list(facets),
#'           identical(names(facets), c("status", "metadata_status",
#'             "frequency", "table_id", "section", "surface_membership")),
#'           all(vapply(facets, function(x) identical(names(x),
#'             c("facet", "value", "n")), logical(1))))
#' @export
boe_catalogue_facets <- function(query = NULL, series_id = NULL, status = NULL,
                                 metadata_status = NULL, frequency = NULL, table_id = NULL,
                                 section = NULL, surface_membership = NULL,
                                 catalogue_path = NULL) {
  out <- boe_filter_catalogue(boe_load_catalogue(catalogue_path), query, series_id, status,
    metadata_status, frequency, table_id, section, surface_membership)
  count_simple <- function(x, name) {
    tab <- sort(table(ifelse(is.na(x) | !nzchar(x), "<missing>", x)), decreasing = TRUE)
    data.frame(facet = name, value = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
  }
  count_list <- function(x, name) {
    values <- trimws(unlist(strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE)))
    count_simple(values[nzchar(values)], name)
  }
  list(
    status = count_simple(out$status, "status"),
    metadata_status = count_simple(out$metadata_status, "metadata_status"),
    frequency = count_simple(out$frequency, "frequency"),
    table_id = count_list(out$table_id, "table_id"),
    section = count_list(out$section, "section"),
    surface_membership = count_list(out$surface_membership, "surface_membership")
  )
}

#' Describe the pinned catalogue snapshot and its declared scope
#'
#' Reads the one-row build manifest paired with the bundled catalogue and
#' verifies the catalogue snapshot hash before returning.  The manifest is
#' declaration metadata, not observation data.
#'
#' @param catalogue_path Optional path to a compatible snapshot.  The sibling
#'   `iadb-catalogue-manifest.rds` is read from the same directory; the default
#'   bundled snapshot is authenticated against its pinned hash.
#' @return A one-row data frame with stable scope/count fields and source,
#'   snapshot, and receipt SHA-256 fields, including
#'   `generated_at_utc`, `catalogue_scope`, `catalogue_complete_within_scope`,
#'   `catalogue_rows`, Bankstats page/link counts, `metadata_rows`, workbook
#'   member counts, source URLs, `*_sha256` hashes, chartability/unavailability
#'   counts, and canonical-history counts.  Column names and values are
#'   returned exactly as stored; no provenance attribute is added.
#' @details This function is deterministic and offline.  Missing manifest or
#'   snapshot files signal `readboe_config`; a successfully decoded manifest
#'   with the wrong shape signals `readboe_schema_drift`; and a mismatched
#'   snapshot hash signals `readboe_identity_mismatch`.  Invalid RDS
#'   serialization can propagate an ordinary R deserialization error.
#' @seealso [boe_catalogue()], [boe_catalogue_receipts()]
#' @examples
#' manifest <- boe_catalogue_manifest()
#' stopifnot(is.data.frame(manifest), nrow(manifest) == 1L,
#'           all(c("catalogue_rows", "catalogue_snapshot_sha256") %in%
#'             names(manifest)))
#' @export
boe_catalogue_manifest <- function(catalogue_path = NULL) {
  path <- boe_catalogue_path(catalogue_path)
  manifest_path <- file.path(dirname(path), "iadb-catalogue-manifest.rds")
  if (!file.exists(manifest_path)) boe_abort("The catalogue manifest is unavailable.", "config")
  out <- readRDS(manifest_path)
  if (!is.data.frame(out) || nrow(out) != 1L) boe_abort("The catalogue manifest violates its schema.", "schema_drift")
  boe_assert_pinned_snapshot(path, "catalogue_snapshot_sha256")
  out
}

#' Read catalogue build receipts
#'
#' Reads one of the committed, hash-authenticated build receipt tables used to
#' create the offline catalogue.  This is useful when an auditor needs source
#' URLs, retrieval hashes, or counts without repeating the upstream requests.
#'
#' @param kind Which receipt family to read: `"search"` for the Bankstats
#'   search-page receipt or `"metadata"` for batched IADB metadata requests.
#' @param limit Positive integer maximum number of rows returned (at most
#'   100000).  Truncation is performed with [utils::head()] after reading.
#' @return A data frame with the exact stored receipt columns.  Search receipts
#'   contain `surface`, `catalogue_scope`, `url`, `final_url`, `retrieved_at`,
#'   `raw_bytes`, `raw_sha256`, `status`, `link_rows`, `unique_series`,
#'   `retrievable_series`, `unavailable_page_series`, `table_count`,
#'   `chartable_series`, `non_chartable_page_series`, and
#'   `excluded_page_series`.  Metadata receipts contain `surface`,
#'   `requested_series`, `returned_series`, `url`, `final_url`,
#'   `retrieved_at`, `raw_bytes`, `raw_sha256`, and `request_sha256`.
#' @details The receipt files are bundled offline and authenticated against
#'   hashes declared in [boe_catalogue_manifest()].  Invalid `kind`, an
#'   oversized `limit`, missing files, hash mismatches, or successfully decoded
#'   objects with the wrong shape signal the corresponding `readboe_config`,
#'   `readboe_bounds`, `readboe_identity_mismatch`, or `readboe_schema_drift`
#'   condition.  Invalid RDS serialization can propagate an ordinary R
#'   deserialization error.
#' @seealso [boe_catalogue_manifest()], [boe_catalogue()]
#' @examples
#' search_receipt <- boe_catalogue_receipts("search", limit = 1L)
#' metadata_receipt <- boe_catalogue_receipts("metadata", limit = 1L)
#' stopifnot(is.data.frame(search_receipt), is.data.frame(metadata_receipt),
#'           nrow(search_receipt) <= 1L, nrow(metadata_receipt) <= 1L)
#' @export
boe_catalogue_receipts <- function(kind = c("search", "metadata"), limit = 100000L) {
  kind <- match.arg(kind)
  boe_check_positive(limit, "limit", integer = TRUE)
  if (limit > 100000L) boe_abort("`limit` may not exceed 100000.", "bounds")
  path <- system.file("extdata", paste0("iadb-catalogue-", kind, "-receipts.rds"), package = "readboe")
  if (!nzchar(path) || !file.exists(path)) boe_abort("Catalogue build receipts are unavailable.", "config")
  boe_assert_pinned_snapshot(path, paste0(kind, "_receipts_sha256"))
  out <- readRDS(path)
  if (!is.data.frame(out)) boe_abort("Catalogue build receipts violate their schema.", "schema_drift")
  utils::head(out, limit)
}
