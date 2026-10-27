#' List audited statistical and research release families
#'
#' Returns the static affirmative registry used to describe supported official
#' release surfaces.  This registry is intentionally separate from the pinned
#' asset snapshot: it records family identity and parser capability, not live
#' files or observation data.
#'
#' @return A deterministic 14-row data frame with columns `family`, `label`,
#'   `landing_url`, and `parser`.  `parser` currently records
#'   `"raw_or_workbook"` for each family.  The function performs no network,
#'   cache, or filesystem operations; the URLs are references only.
#' @seealso [boe_release_assets()], [read_boe_release_asset()], [boe_rights()]
#' @examples
#' registry <- boe_release_registry()
#' stopifnot(is.data.frame(registry), nrow(registry) == 14L,
#'           identical(names(registry), c("family", "label", "landing_url",
#'             "parser")))
#' @export
boe_release_registry <- function() {
  data.frame(
    family = c("requested_data", "gdp_realtime", "research_datasets", "payments", "gold_custody",
      "banknotes", "international_reserves", "mlar", "capital_issuance", "credit_unions",
      "insurance_annual", "insurance_quarterly", "external_business", "external_claims"),
    label = c("Requested data", "GDP real-time database", "Research datasets", "Payment and settlement statistics",
      "Gold custody data", "Banknote statistics", "UK international reserves", "Mortgage Lenders and Administrators",
      "Capital issuance", "Credit union statistics", "Insurance aggregate annual statistics",
      "Insurance aggregate quarterly statistics", "External business of UK MFIs", "Country analysis of UK external claims"),
    landing_url = c(
      "https://www.bankofengland.co.uk/statistics/requested-data",
      "https://www.bankofengland.co.uk/statistics/gdp-real-time-database",
      "https://www.bankofengland.co.uk/statistics/research-datasets",
      "https://www.bankofengland.co.uk/payments/payment-and-settlement-statistics",
      "https://www.bankofengland.co.uk/statistics/gold-custody-data",
      "https://www.bankofengland.co.uk/statistics/banknote",
      "https://www.bankofengland.co.uk/statistics/details/further-details-about-uk-international-reserves-data",
      "https://www.bankofengland.co.uk/statistics/details/further-details-about-mortgage-lenders-and-administrators-statistics-data",
      "https://www.bankofengland.co.uk/statistics/details/capital-issuance",
      "https://www.bankofengland.co.uk/statistics/details/further-details-about-credit-union-statistics-data",
      "https://www.bankofengland.co.uk/statistics/insurance-aggregate-data/annual",
      "https://www.bankofengland.co.uk/statistics/insurance-aggregate-data/quarterly",
      "https://www.bankofengland.co.uk/statistics/details/further-details-about-external-business-of-monetary-financial-institutions-operating-in-the-uk-data",
      "https://www.bankofengland.co.uk/statistics/data-collection/statistical-reporting/form-cc"),
    parser = c(rep("raw_or_workbook", 14L)),
    stringsAsFactors = FALSE
  )
}

boe_release_assets_path <- function() {
  path <- system.file("extdata", "release-assets.rds", package = "readboe")
  if (!nzchar(path) || !file.exists(path)) boe_abort("The pinned release-asset snapshot is unavailable.", "config")
  path
}

#' Browse pinned release assets
#'
#' Searches the committed release-asset snapshot by family, file type, or
#' text.  The result is intended to supply exact URLs to
#' [read_boe_release_asset()].  It is an offline operation and never crawls an
#' official landing page.
#'
#' @param family Optional non-empty character vector of exact families from
#'   [boe_release_registry()].
#' @param file_type Optional non-empty character vector of exact file
#'   extensions, matched case-insensitively.
#' @param query Optional one-string case-insensitive implicit-AND query over
#'   family, label, and URL.  Quoted phrases have no special treatment.
#' @param limit Positive integer maximum after filtering (default 1000).
#' @return When the pinned snapshot is present, a data frame with at least
#'   `family`, `label`, `url`, `file_type`, and `parser`; additional sitemap
#'   provenance columns are retained exactly as stored.  A
#'   `boe_catalogue_provenance` attribute contains `snapshot_sha256` and
#'   `matched_before_cap`.
#' @details Version 0.1.6 does not include
#'   `inst/extdata/release-assets.rds`; therefore the default call signals
#'   `readboe_config` with `pinned release-asset snapshot is unavailable`.
#'   This documented limitation is deliberate until a verified snapshot is
#'   shipped.  Once present, the file is authenticated by its manifest;
#'   invalid `query` or `limit`, schema or identity mismatches, and bound
#'   failures produce typed `readboe_*` conditions.  No network or cache side
#'   effect occurs.
#' @seealso [boe_release_registry()], [read_boe_release_asset()]
#' @examples
#' unavailable <- tryCatch(boe_release_assets(limit = 1L),
#'   error = function(error) error)
#' stopifnot(inherits(unavailable, "readboe_config"))
#' @export
boe_release_assets <- function(family = NULL, file_type = NULL, query = NULL, limit = 1000L) {
  boe_check_positive(limit, "limit", integer = TRUE)
  path <- boe_release_assets_path()
  boe_assert_pinned_snapshot(path, "release_assets_sha256")
  out <- readRDS(path)
  required <- c("family", "label", "url", "file_type", "parser")
  if (!is.data.frame(out) || !all(required %in% names(out)) || anyNA(out$url) || anyDuplicated(out$url)) {
    boe_abort("The release-asset snapshot violates its schema or URL identity contract.", "schema_drift")
  }
  if (!is.null(family)) out <- out[out$family %in% family, , drop = FALSE]
  if (!is.null(file_type)) out <- out[tolower(out$file_type) %in% tolower(file_type), , drop = FALSE]
  if (!is.null(query)) {
    if (!is.character(query) || length(query) != 1L || is.na(query)) boe_abort("`query` must be one string.", "config")
    terms <- strsplit(tolower(trimws(query)), "[[:space:]]+")[[1L]]
    hay <- tolower(paste(out$family, out$label, out$url))
    keep <- Reduce(`&`, lapply(terms[nzchar(terms)], function(term) grepl(term, hay, fixed = TRUE)))
    out <- out[keep, , drop = FALSE]
  }
  matched <- nrow(out)
  out <- utils::head(out, limit)
  attr(out, "boe_catalogue_provenance") <- list(snapshot_sha256 = boe_sha256_file(path), matched_before_cap = matched)
  out
}

#' Read an exact pinned release asset
#'
#' Downloads one exact URL from [boe_release_assets()] and either returns its
#' bytes or losslessly parses an Excel workbook.  No release-asset discovery or
#' URL guessing occurs inside this reader.
#'
#' @param url Exact HTTPS URL present in the pinned release-asset snapshot.
#' @param parse Character choice: `"raw"` (default) for bytes, or
#'   `"workbook"` for lossless worksheet parsing.  Workbook parsing is
#'   available only for `.xls` and `.xlsx`; other file types are raw-only.
#' @param deadline A caller-owned absolute deadline, `POSIXct`, positive number
#'   of seconds, or `NULL` for 60 seconds.
#' @param max_bytes Positive response byte bound (default 150 MiB).
#' @param max_sheets Positive integer workbook-sheet bound (default 50).
#' @param max_rows Positive integer worksheet-row bound (default 1e6).
#' @param max_columns Positive integer worksheet-column bound (default 2000).
#' @param max_cells Positive integer workbook-cell bound (default 5e6).
#' @param cache Whether to use the managed response cache (default `TRUE`).
#' @param cache_only If `TRUE`, require a matching cache entry and never use
#'   the network; a miss signals `readboe_cache_miss`.
#' @param refresh If `TRUE`, bypass a cached response.  It cannot be combined
#'   with `cache_only = TRUE`.
#' @param cache_dir Cache directory; defaults to
#'   `tools::R_user_dir("readboe", "cache")`.
#' @return With `parse = "raw"`, a raw byte vector carrying `boe_provenance`.
#'   With `parse = "workbook"`, a named list of raw worksheet data frames with
#'   class `c("boe_workbook", "list")` and a `boe_provenance` attribute.  Sheet
#'   names, cell values, and original column names are preserved.
#' @details The base checkout currently lacks the required
#'   `inst/extdata/release-assets.rds`, so every call fails during exact URL
#'   validation with `readboe_config` until that verified snapshot is shipped.
#'   With a snapshot, HTTPS allowlisting, bounded retries/deadlines/bytes and
#'   workbook dimensions apply; temporary files are cleaned on exit, and only
#'   successful responses enter the managed cache.  Unsupported workbook
#'   parsing signals `readboe_config` with `upstream_capability = "raw_only"`.
#' @seealso [boe_release_assets()], [boe_download_asset()],
#'   [boe_provenance()]
#' @examples
#' unavailable <- tryCatch(
#'   read_boe_release_asset("https://www.bankofengland.co.uk/example.csv"),
#'   error = function(error) error
#' )
#' stopifnot(inherits(unavailable, "readboe_config"))
#' @export
read_boe_release_asset <- function(url, parse = c("raw", "workbook"), deadline = NULL,
                                   max_bytes = 150 * 1024^2, max_sheets = 50L,
                                   max_rows = 1e6, max_columns = 2000L, max_cells = 5e6,
                                   cache = TRUE, cache_only = FALSE, refresh = FALSE,
                                   cache_dir = boe_default_cache_dir()) {
  parse <- match.arg(parse)
  assets <- boe_release_assets(limit = 100000L)
  idx <- which(assets$url == url)
  if (length(idx) != 1L) boe_abort("`url` is not one exact pinned release asset.", "identity_mismatch")
  raw <- boe_download_asset(url, deadline, max_bytes, cache, cache_only, refresh, cache_dir)
  if (parse == "raw") return(raw)
  ext <- tolower(assets$file_type[[idx]])
  if (!ext %in% c("xls", "xlsx")) {
    boe_abort(sprintf("Lossless worksheet parsing is unavailable for `%s`; request raw bytes.", ext), "config", upstream_capability = "raw_only")
  }
  out <- boe_read_workbook_raw(raw, paste0(".", ext), max_sheets, max_rows, max_columns, max_cells)
  provenance <- boe_provenance(raw)
  attr(out, "boe_provenance") <- c(provenance, list(
    source_identity = list(surface = "release_asset", family = assets$family[[idx]], url = url),
    transforms = "Workbook syntax parsed without header inference"
  ))
  out
}
