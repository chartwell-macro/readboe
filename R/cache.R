boe_default_cache_dir <- function() tools::R_user_dir("readboe", "cache")

boe_cache_key <- function(identity) digest::digest(identity, algo = "sha256", serialize = TRUE)

boe_cache_path <- function(cache_dir, key) file.path(cache_dir, paste0(key, ".rds"))

boe_cache_read <- function(cache_dir, key, ttl, cache_only = FALSE) {
  path <- boe_cache_path(cache_dir, key)
  if (!file.exists(path)) {
    if (cache_only) boe_abort("No matching success entry exists in the cache.", "cache_miss", cache_key = key)
    return(NULL)
  }
  entry <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(entry) || !is.raw(entry$body) || !identical(entry$key, key) ||
      !identical(boe_sha256_raw(entry$body), entry$raw_sha256)) {
    boe_abort("A cached response is corrupt or has the wrong identity.", "cache_corruption", cache_key = key)
  }
  age <- as.numeric(difftime(Sys.time(), entry$stored_at, units = "secs"))
  if (!cache_only && (is.na(age) || age > ttl)) return(NULL)
  entry$from_cache <- TRUE
  entry
}

boe_cache_write <- function(cache_dir, key, entry, max_entries, max_cache_bytes) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  entry$key <- key
  entry$stored_at <- Sys.time()
  entry$from_cache <- FALSE
  target <- boe_cache_path(cache_dir, key)
  tmp <- tempfile("readboe-cache-", tmpdir = cache_dir)
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(entry, tmp, version = 3)
  if (!file.rename(tmp, target)) boe_abort("Could not atomically commit the cache entry.", "cache_corruption")
  boe_cache_prune(cache_dir, max_entries, max_cache_bytes)
  invisible(entry)
}

boe_cache_prune <- function(cache_dir, max_entries, max_cache_bytes) {
  files <- list.files(cache_dir, pattern = "^[0-9a-f]{64}\\.rds$", full.names = TRUE)
  if (!length(files)) return(invisible(NULL))
  info <- file.info(files)
  ord <- order(info$mtime, decreasing = TRUE, na.last = TRUE)
  info <- info[ord, , drop = FALSE]
  files <- files[ord]
  keep <- seq_along(files) <= max_entries & cumsum(info$size) <= max_cache_bytes
  stale <- files[!keep]
  if (length(stale)) unlink(stale)
  invisible(NULL)
}

#' Remove readboe cache entries
#'
#' Removes files whose names match readboe's cache-entry pattern: a lowercase
#' SHA-256 key with the `.rds` extension.  The function selects by filename and
#' does not inspect file contents; other files and subdirectories are left
#' untouched.
#'
#' @param cache_dir Directory containing readboe cache entries.  The default
#'   is the package-managed `tools::R_user_dir("readboe", "cache")` location.
#'   A missing directory is treated as an empty cache.
#' @return Invisibly, a length-one integer giving the number of matching cache
#'   files removed.  No value is returned for unrelated files because they are
#'   deliberately preserved.
#' @details This is the package's only cache-removal operation.  It performs
#'   local filesystem mutation but never contacts the network and does not
#'   remove the cache directory itself.  Readers write only successful,
#'   hash-checked responses to the managed cache; `cache_only = TRUE` readers
#'   can then replay those entries without network access.  Use a temporary
#'   directory in examples and tests when isolation is important.
#' @seealso [boe_provenance()], [boe_deadline()], [read_boe_series()],
#'   [boe_download_asset()]
#' @examples
#' cache_dir <- tempfile("readboe-cache-")
#' dir.create(cache_dir)
#' writeLines("unrelated", file.path(cache_dir, "notes.txt"))
#' removed <- boe_cache_clear(cache_dir)
#' stopifnot(identical(as.integer(removed), 0L),
#'           file.exists(file.path(cache_dir, "notes.txt")))
#' @export
boe_cache_clear <- function(cache_dir = boe_default_cache_dir()) {
  files <- list.files(cache_dir, pattern = "^[0-9a-f]{64}\\.rds$", full.names = TRUE)
  if (length(files)) unlink(files)
  invisible(length(files))
}
