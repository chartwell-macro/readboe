# Build the deterministic public-documentation replay for monthly SONIA.
# Run from the package root after installing the current readboe source.

local({
library(readboe)

expected_series <- "IUMASOIA"
expected_hash <- "3e1baa084a68d9e98e5ca01fabe3bf113c933f4e0cf530ebd99e7ac1b89ddfa0"
expected_cache_file <- "b0472c30be393d72bb4416c8dbdb8d1aac026a66bee072ee7495d56a4e9d9a54.rds"
expected_observations <- 348L
from <- "1997-01-31"
to <- "2025-12-31"

selected <- boe_catalogue(query = "SONIA", frequency = "Monthly", limit = 1L)
stopifnot(
  nrow(selected) == 1L,
  identical(selected$series_id[[1L]], expected_series),
  identical(selected$frequency[[1L]], "Monthly")
)

download_cache <- tempfile("readboe-sonia-download-")
replay_cache <- tempfile("readboe-sonia-replay-")
dir.create(download_cache)
dir.create(replay_cache)
on.exit(unlink(c(download_cache, replay_cache), recursive = TRUE), add = TRUE)

downloaded <- read_boe_series(
  series_id = expected_series,
  from = from,
  to = to,
  format = "xml",
  deadline = boe_deadline(45),
  max_bytes = 5 * 1024^2,
  cache_dir = download_cache,
  refresh = TRUE
)

download_provenance <- boe_provenance(downloaded)
stopifnot(
  inherits(downloaded, "boe_iadb"),
  nrow(downloaded$observations) == expected_observations,
  identical(
    downloaded$observations$date[c(1L, expected_observations)],
    as.Date(c(from, to))
  ),
  !anyNA(downloaded$observations$value),
  !anyDuplicated(downloaded$observations[c("series_id", "date")]),
  identical(download_provenance$raw_sha256, expected_hash)
)

cache_files <- list.files(
  download_cache,
  pattern = "^[0-9a-f]{64}\\.rds$",
  full.names = TRUE
)
stopifnot(length(cache_files) == 1L)

fixture_dir <- file.path(
  "inst", "extdata", "sonia"
)
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
old_cache_files <- list.files(
  fixture_dir,
  pattern = "^[0-9a-f]{64}\\.rds$",
  full.names = TRUE
)
if (length(old_cache_files)) unlink(old_cache_files)

entry <- readRDS(cache_files[[1L]])
stopifnot(
  identical(basename(cache_files[[1L]]), expected_cache_file),
  identical(paste0(entry$key, ".rds"), expected_cache_file),
  identical(entry$url, download_provenance$canonical_url),
  identical(entry$final_url, download_provenance$final_url),
  identical(entry$status, 200L),
  length(entry$headers) > 0L,
  identical(length(entry$body), download_provenance$raw_bytes),
  identical(
    digest::digest(entry$body, algo = "sha256", serialize = FALSE),
    expected_hash
  ),
  identical(entry$raw_sha256, expected_hash),
  inherits(entry$retrieved_at, "POSIXct"),
  inherits(entry$stored_at, "POSIXct"),
  all(c("etag", "last_modified") %in% names(entry))
)

# Keep the real response receipt while never committing a server-issued cookie.
# All non-sensitive response headers, including validators when supplied, are
# preserved exactly as received.
entry$headers <- entry$headers[
  tolower(names(entry$headers)) != "set-cookie"
]
fixture_file <- file.path(fixture_dir, basename(cache_files[[1L]]))
saveRDS(entry, fixture_file, version = 3)

manifest <- data.frame(
  series_id = expected_series,
  label = selected$label[[1L]],
  from = from,
  to = to,
  observed_first = format(min(downloaded$observations$date)),
  observed_last = format(max(downloaded$observations$date)),
  format = "xml",
  observations = nrow(downloaded$observations),
  retrieved_at_utc = format(
    download_provenance$retrieved_at,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  ),
  raw_bytes = download_provenance$raw_bytes,
  raw_sha256 = download_provenance$raw_sha256,
  canonical_url = download_provenance$canonical_url,
  source_terms = "https://www.bankofengland.co.uk/legal",
  attribution = paste(
    "SONIA and/or SONIA Compounded Index data licensed under the Open",
    "Government Licence v3.0 and copyright the Governor and Company of the",
    "Bank of England. The trade marks “Bank of England” and “SONIA” are",
    "registered trade marks of the Bank of England."
  ),
  redacted_response_headers = "Set-Cookie",
  cache_file = basename(fixture_file),
  cache_file_sha256 = digest::digest(
    fixture_file,
    algo = "sha256",
    serialize = FALSE,
    file = TRUE
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  manifest,
  file.path(fixture_dir, "manifest.csv"),
  row.names = FALSE,
  na = ""
)

stopifnot(file.copy(fixture_file, replay_cache))
replayed <- read_boe_series(
  series_id = expected_series,
  from = from,
  to = to,
  format = "xml",
  cache_only = TRUE,
  cache_dir = replay_cache
)
stopifnot(
  identical(downloaded$metadata, replayed$metadata),
  identical(downloaded$observations, replayed$observations),
  isTRUE(boe_provenance(replayed)$from_cache),
  identical(boe_provenance(replayed)$raw_sha256, expected_hash)
)
})
