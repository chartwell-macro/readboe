live <- identical(tolower(Sys.getenv("READBOE_RUN_LIVE_TESTS", "false")), "true")
bankstats_live <- identical(tolower(Sys.getenv("READBOE_RUN_BANKSTATS_LIVE_TESTS", "false")), "true")

test_that("live scoped Bankstats catalogue and representative workbook agree", {
  skip_if_not(bankstats_live)
  cache <- Sys.getenv("READBOE_LIVE_CACHE_DIR", tempfile("readboe-live-"))
  manifest <- boe_catalogue_manifest()
  catalogue <- boe_catalogue(status = "bankstats_current", limit = 100000L)
  expect_identical(
    manifest$catalogue_scope,
    "bankstats_current_page_retrievable_exact"
  )
  expect_equal(nrow(catalogue), manifest$catalogue_rows)
  expect_gte(nrow(catalogue), 5250L)
  expect_equal(anyDuplicated(catalogue$series_id), 0L)
  expect_true(all(grepl("iadb_xml", catalogue$surface_membership)))
  expect_true(all(
    grepl("bankstats_current_bulk", catalogue$surface_membership) ==
      catalogue$chartable
  ))

  bulk <- boe_bankstats_assets(table_id = "A1.1.1")
  expect_equal(nrow(bulk), 1L)
  workbook <- read_boe_bankstats(
    bulk$url[[1]],
    member = "Latest Tables/TabA1.1.1.xls",
    deadline = boe_deadline(120),
    max_cells = 1e6,
    cache_dir = cache
  )
  expect_s3_class(workbook, "boe_workbook_archive")
  expect_match(boe_provenance(workbook)$raw_sha256, "^[0-9a-f]{64}$")

  series <- read_boe_bankstats_series(
    "LPMAVAB",
    table_id = "A1.1.1",
    history = "current_workbook",
    deadline = boe_deadline(120),
    max_cells = 1e6,
    max_output_rows = 10000L,
    cache_dir = cache
  )
  expect_s3_class(series, "boe_bankstats_series")
  expect_gt(nrow(series), 0L)
  expect_identical(unique(series$series_id), "LPMAVAB")
  expect_true(all(c("date", "value", "raw_value", "source_row", "source_column") %in% names(series)))
  expect_match(boe_provenance(series)$raw_sha256, "^[0-9a-f]{64}$")

  matrix_series <- read_boe_bankstats_series(
    "VPQB254AD",
    table_id = "C3.2",
    history = "current_workbook",
    deadline = boe_deadline(120),
    max_cells = 1e6,
    max_output_rows = 10000L,
    cache_dir = cache
  )
  expect_s3_class(matrix_series, "boe_bankstats_series")
  expect_true(all(matrix_series$date_precision == "quarter"))
  expect_true(all(matrix_series$orientation == "time_across_columns"))
  expect_identical(boe_provenance(matrix_series)$source_identity$table_id, "C3.2")

  canonical_series <- read_boe_bankstats_series(
    "CPMB66I",
    table_id = "E3.1",
    history = "full",
    deadline = boe_deadline(120),
    max_output_rows = 10000L,
    cache_dir = cache
  )
  expect_equal(min(canonical_series$date), as.Date("2003-01-31"))
  expect_gt(nrow(canonical_series), 250L)
  expect_identical(
    boe_provenance(canonical_series)$source_identity$surface,
    "iadb_xml_canonical_bankstats_history"
  )
})

test_that("live canonical XML and CSV reconcile exactly on non-missing values", {
  skip_if_not(live)
  cache <- Sys.getenv("READBOE_LIVE_CACHE_DIR", tempfile("readboe-live-"))
  xml <- read_boe_series("IUDBEDR", "2024-01-01", "2024-01-31", format = "xml",
    deadline = boe_deadline(60), max_bytes = 2 * 1024^2, cache_dir = cache)
  csv <- read_boe_series("IUDBEDR", "2024-01-01", "2024-01-31", format = "csv",
    deadline = boe_deadline(60), max_bytes = 2 * 1024^2, cache_dir = cache)
  xo <- xml$observations[!is.na(xml$observations$value), c("series_id", "date", "value")]
  co <- csv[!is.na(csv$value), c("series_id", "date", "value")]
  expect_identical(xo, co)
  expect_match(boe_provenance(xml)$raw_sha256, "^[0-9a-f]{64}$")
  replay <- read_boe_series("IUDBEDR", "2024-01-01", "2024-01-31", format = "xml",
    deadline = boe_deadline(10), max_bytes = 2 * 1024^2, cache_only = TRUE, cache_dir = cache)
  expect_true(boe_provenance(replay)$from_cache)
})

test_that("live VUD, metadata, Bank Rate, Bankstats, and latest yield canaries work", {
  skip_if_not(live)
  cache <- Sys.getenv("READBOE_LIVE_CACHE_DIR", tempfile("readboe-live-"))
  metadata <- boe_series_metadata(boe_rate_series()$series_id, deadline = boe_deadline(90), cache_dir = cache)
  expect_setequal(metadata$series_id, boe_rate_series()$series_id)
  updates <- boe_series_updates("IUMABEDR", "2025-01-01", "2025-12-31", updated_since = "latest",
    deadline = boe_deadline(60), cache_dir = cache)
  expect_s3_class(updates, "boe_iadb")
  rate <- boe_bank_rate_history(deadline = boe_deadline(120), cache_dir = cache)
  expect_equal(nrow(rate), 852L)
  expect_equal(rate$period[[1]], "1694-10")
  bulk <- boe_bankstats_assets(table_id = "A1.1.1")
  workbook <- read_boe_bankstats(bulk$url[[1]], member = "Latest Tables/TabA1.1.1.xls",
    deadline = boe_deadline(120), max_cells = 1e6, cache_dir = cache)
  expect_s3_class(workbook, "boe_workbook_archive")
  curve <- read_boe_yield_curve("nominal", scope = "latest", from = "2026-07-01",
    deadline = boe_deadline(180), cache_dir = cache)
  expect_gt(nrow(curve), 0L)
  expect_true(all(c("source_member", "source_row", "source_column") %in% names(curve)))
})
