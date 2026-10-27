test_that("the installed SONIA documentation fixture replays the public reader", {
  fixture_dir <- system.file(
    "extdata", "sonia",
    package = "readboe", mustWork = TRUE
  )
  fixture_file <- list.files(
    fixture_dir,
    pattern = "^[0-9a-f]{64}\\.rds$",
    full.names = TRUE
  )
  manifest <- utils::read.csv(
    file.path(fixture_dir, "manifest.csv"),
    stringsAsFactors = FALSE
  )
  entry <- readRDS(fixture_file)
  expected_url <- paste0(
    "https://www.bankofengland.co.uk/boeapps/database/",
    "_iadb-fromshowcolumns.asp?CodeVer=new&Datefrom=31%2FJan%2F1997&",
    "Dateto=31%2FDec%2F2025&SeriesCodes=IUMASOIA&VFD=Y&VPD=Y&xml.x=yes"
  )
  expected_attribution <- paste(
    "SONIA and/or SONIA Compounded Index data licensed under the Open",
    "Government Licence v3.0 and copyright the Governor and Company of the",
    "Bank of England. The trade marks “Bank of England” and “SONIA” are",
    "registered trade marks of the Bank of England."
  )

  cache_dir <- tempfile("readboe-documentation-fixture-")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  expect_true(file.copy(fixture_file, cache_dir))

  selected <- boe_catalogue(query = "SONIA", frequency = "Monthly", limit = 1L)
  history <- read_boe_series(
    series_id = selected$series_id[[1L]],
    from = "1997-01-31",
    to = "2025-12-31",
    format = "xml",
    cache_only = TRUE,
    cache_dir = cache_dir
  )
  provenance <- boe_provenance(history)

  expect_identical(selected$series_id, "IUMASOIA")
  expect_s3_class(history, "boe_iadb")
  expect_identical(names(history), c("metadata", "observations"))
  expect_identical(nrow(history$observations), 348L)
  expect_s3_class(history$observations$date, "Date")
  expect_type(history$observations$value, "double")
  expect_false(anyNA(history$observations$value))
  expect_identical(
    anyDuplicated(history$observations[c("series_id", "date")]),
    0L
  )
  expect_identical(
    history$observations$date[c(1L, 348L)],
    as.Date(c("1997-01-31", "2025-12-31"))
  )
  expect_true(isTRUE(provenance$from_cache))
  expect_identical(provenance$canonical_url, expected_url)
  expect_identical(provenance$final_url, expected_url)
  expect_identical(provenance$raw_bytes, 43319L)
  expect_identical(
    provenance$raw_sha256,
    "3e1baa084a68d9e98e5ca01fabe3bf113c933f4e0cf530ebd99e7ac1b89ddfa0"
  )
  expect_identical(entry$url, expected_url)
  expect_identical(entry$final_url, expected_url)
  expect_identical(entry$status, 200L)
  expect_gt(length(entry$headers), 0L)
  expect_false(any(tolower(names(entry$headers)) == "set-cookie"))
  expect_true(all(c("etag", "last_modified") %in% names(entry)))
  expect_true(is.na(entry$etag))
  expect_true(is.na(entry$last_modified))
  expect_false(entry$from_cache)
  expect_s3_class(entry$retrieved_at, "POSIXct")
  expect_s3_class(entry$stored_at, "POSIXct")
  expect_gte(entry$stored_at, entry$retrieved_at)
  expect_identical(basename(fixture_file), paste0(entry$key, ".rds"))
  expect_identical(length(entry$body), 43319L)
  expect_identical(
    digest::digest(entry$body, algo = "sha256", serialize = FALSE),
    entry$raw_sha256
  )
  expect_identical(manifest$series_id, "IUMASOIA")
  expect_identical(
    manifest$label,
    "Monthly average Sterling overnight index average (SONIA) rate"
  )
  expect_identical(manifest$from, "1997-01-31")
  expect_identical(manifest$to, "2025-12-31")
  expect_identical(manifest$observed_first, "1997-01-31")
  expect_identical(manifest$observed_last, "2025-12-31")
  expect_identical(manifest$observations, 348L)
  expect_identical(manifest$format, "xml")
  expect_identical(
    manifest$retrieved_at_utc,
    format(entry$retrieved_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  expect_identical(manifest$raw_bytes, length(entry$body))
  expect_identical(manifest$raw_sha256, entry$raw_sha256)
  expect_identical(manifest$canonical_url, expected_url)
  expect_identical(
    manifest$source_terms,
    "https://www.bankofengland.co.uk/legal"
  )
  expect_identical(manifest$attribution, expected_attribution)
  expect_identical(manifest$redacted_response_headers, "Set-Cookie")
  expect_identical(manifest$cache_file, basename(fixture_file))
  expect_identical(
    manifest$cache_file_sha256,
    digest::digest(fixture_file, algo = "sha256", serialize = FALSE, file = TRUE)
  )
  expect_identical(manifest$raw_sha256, provenance$raw_sha256)
})
