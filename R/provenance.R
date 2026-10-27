boe_sha256_raw <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)

boe_sha256_file <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)

boe_attach_provenance <- function(x, receipt, source_identity, transforms = character()) {
  attr(x, "boe_provenance") <- list(
    source_identity = source_identity,
    canonical_url = receipt$url,
    final_url = receipt$final_url %||% receipt$url,
    retrieved_at = receipt$retrieved_at,
    raw_bytes = receipt$raw_bytes,
    raw_sha256 = receipt$raw_sha256,
    etag = receipt$etag,
    last_modified = receipt$last_modified,
    from_cache = isTRUE(receipt$from_cache),
    transforms = transforms,
    package = "readboe",
    package_version = as.character(utils::packageVersion("readboe"))
  )
  x
}

#' Extract exact source provenance
#'
#' Returns the receipt attached to a successful remote or parsed result.  This
#' accessor does not recompute hashes, contact the source, or copy the object.
#'
#' @param x Any object returned by a provenance-bearing readboe function, such
#'   as [read_boe_series()], [read_boe_bankstats()], or
#'   [boe_download_asset()].  Objects from static registries and catalogue
#'   functions may instead use `boe_catalogue_provenance` or have no receipt.
#' @return The exact `boe_provenance` attribute, normally a named list with
#'   `source_identity`, `canonical_url`, `final_url`, `retrieved_at`,
#'   `raw_bytes`, `raw_sha256`, `etag`, `last_modified`, `from_cache`,
#'   `transforms`, `package`, and `package_version`; or `NULL` when `x` has no
#'   such attribute.  The list is returned without normalisation or mutation.
#' @details Provenance records source identity and transformations needed for
#'   reproducibility.  It is distinct from the catalogue snapshot attribute
#'   used by [boe_catalogue()] and [boe_bankstats_assets()].
#' @seealso [boe_download_asset()], [read_boe_series()], [boe_catalogue()]
#' @examples
#' stopifnot(is.null(boe_provenance(data.frame(value = 1))))
#' bytes <- structure(as.raw(1:2), boe_provenance = list(
#'   source_identity = list(surface = "example")))
#' stopifnot(identical(boe_provenance(bytes)$source_identity$surface, "example"))
#' @export
boe_provenance <- function(x) attr(x, "boe_provenance", exact = TRUE)
