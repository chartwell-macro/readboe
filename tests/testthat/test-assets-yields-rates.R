fixture_bytes <- function(name) readBin(test_path("fixtures", name), "raw", n = file.info(test_path("fixtures", name))$size)

test_that("provenance-bearing raw assets remain byte-exact temporary files", {
  raw <- charToRaw("PK fixture")
  attr(raw, "boe_provenance") <- list(raw_sha256 = digest::digest(raw, algo = "sha256", serialize = FALSE))
  path <- readboe:::boe_write_raw_temp(raw, ".zip"); on.exit(unlink(path), add = TRUE)
  restored <- readBin(path, "raw", n = file.info(path)$size)
  expect_identical(restored, as.raw(raw))
})

test_that("yield parser retains official NAs and duplicate dates with source identity", {
  path <- test_path("fixtures", "yield.xlsx")
  x <- readboe:::boe_parse_yield_workbook(path, "fixture/yield.xlsx", "spot", "standard", 10000L, 100L)
  expect_equal(nrow(x), 15L)
  expect_equal(sum(is.na(x$rate)), 1L)
  expect_true(anyDuplicated(x[c("date", "maturity_years")]) > 0L)
  expect_false(anyDuplicated(x[c("source_member", "source_sheet", "source_row", "source_column")]) > 0L)
})

test_that("Bank Rate parser retains month precision without inventing a day", {
  x <- readboe:::boe_parse_bank_rate(fixture_bytes("bank-rate.xlsx"))
  expect_equal(nrow(x), 4L)
  expect_equal(x$period[1:2], c("1694-10", "1694-11"))
  expect_true(all(is.na(x$date[1:2])))
  expect_equal(x$date_precision, c("month", "month", "day", "day"))
  expect_equal(x$date[3], as.Date("1975-01-20"))
})

test_that("workbook parser has sheet and cell bounds", {
  raw <- fixture_bytes("workbook.xlsx")
  x <- readboe:::boe_read_workbook_raw(raw, ".xlsx", max_cells = 10L)
  expect_s3_class(x, "boe_workbook")
  expect_true(is.na(x[[1]][2, 1]))
  expect_error(readboe:::boe_read_workbook_raw(raw, ".xlsx", max_cells = 1L), class = "readboe_bounds")
})

test_that("yield registry is the complete fixed eleven-asset surface", {
  x <- boe_yield_assets()
  expect_equal(nrow(x), 11L)
  expect_setequal(x$curve, c("all", "nominal", "real", "inflation", "blc", "ois"))
  expect_true(all(startsWith(x$url, "https://www.bankofengland.co.uk/")))
})

test_that("yield output rows and archive members are bounded", {
  expect_error(readboe:::boe_parse_yield_workbook(test_path("fixtures", "yield.xlsx"),
    "fixture/yield.xlsx", "spot", "standard", 10000L, 10L), class = "readboe_bounds")
  zipfile <- tempfile(fileext = ".zip"); on.exit(unlink(zipfile), add = TRUE)
  old <- setwd(test_path("fixtures")); on.exit(setwd(old), add = TRUE)
  utils::zip(zipfile, files = c("yield.xlsx", "workbook.xlsx"), flags = "-q")
  expect_error(readboe:::boe_safe_zip_members(zipfile, max_members = 1L), class = "readboe_bounds")
  expect_equal(nrow(readboe:::boe_safe_zip_members(zipfile, max_members = 2L)), 2L)
})

test_that("Bankstats exact-series parser supports time-down worksheets", {
  cells <- matrix("", nrow = 13L, ncol = 6L)
  cells[1L, 1L] <- "Table A1.1.1"
  cells[2L, 1L] <- "Notes and coin"
  cells[3L, 1L] <- "£ millions"
  cells[3L, 3L] <- "Seasonally adjusted"
  cells[5L, 1L] <- "Notes held by UK banks"
  cells[8L, 4L] <- "Notes in circulation"
  cells[7L, 6L] <- "Last updated:  01 July 2026"
  cells[9L, 2L] <- "LPM"
  cells[9L, 4L] <- "AVAB (a)"
  cells[10L:13L, 2L] <- c("45474", "45505", "45536", "45566")
  cells[10L:13L, 4L] <- c("95986", "-", "96463", "96686")
  workbook <- structure(
    list(`SA amounts outstanding` = as.data.frame(cells, stringsAsFactors = FALSE)),
    class = c("boe_workbook", "list")
  )

  out <- readboe:::boe_extract_bankstats_series(
    workbook,
    "LPMAVAB",
    "AVAB",
    "A1.1.1",
    "Latest Tables/TabA1.1.1.xls",
    100L
  )

  expect_equal(out$date, as.Date(c(
    "2024-07-01", "2024-08-01", "2024-09-01", "2024-10-01"
  )))
  expect_equal(out$value, c(95986, NA, 96463, 96686))
  expect_equal(out$raw_value, c("95986", "-", "96463", "96686"))
  expect_equal(out$value_status, c("", "-", "", ""))
  expect_true(all(out$orientation == "time_down_rows"))
  expect_true(all(out$source_column == 4L))
  expect_equal(out$unit, rep("£ millions", 4L))
  expect_equal(out$seasonal_adjustment, rep("Seasonally adjusted", 4L))
  expect_equal(out$release_date, rep(as.Date("2026-07-01"), 4L))
  expect_equal(
    unique(out$series_label),
    "SA amounts outstanding — Notes held by UK banks Notes in circulation"
  )
  expect_equal(unique(out$series_frequency), "Monthly")
})

test_that("Bankstats English dates are independent of LC_TIME", {
  expect_equal(
    readboe:::boe_bankstats_date("01 July 2026"),
    as.Date("2026-07-01")
  )
  expect_equal(
    readboe:::boe_bankstats_date("1 Jan 2026"),
    as.Date("2026-01-01")
  )
  expect_equal(
    readboe:::boe_bankstats_date("2026-07-01"),
    as.Date("2026-07-01")
  )
  expect_equal(
    readboe:::boe_bankstats_date("01/07/2026"),
    as.Date("2026-07-01")
  )
})

test_that("Bankstats exact-series parser supports time-across worksheets", {
  cells <- matrix("", nrow = 10L, ncol = 12L)
  cells[1L, 1L] <- "Table C3.2"
  cells[2L, 1L] <- "External business"
  cells[3L, 1L] <- "US$ millions"
  cells[3L, 3L] <- "Not seasonally adjusted"
  cells[6L, 5L] <- "2025"
  cells[6L, 11L] <- "2026"
  cells[7L, c(5L, 7L, 9L, 11L)] <- c("Q1", "Q2", "Q3", "Q1")
  cells[5L, 2L] <- "Amounts outstanding of liabilities"
  cells[7L, 2L] <- "Advanced economies"
  cells[8L, 2L] <- "Europe"
  cells[8L, 3L] <- "VPQ"
  # A preceding observation row contains values that look like years. Period
  # inference must stop at the official series-prefix header instead of
  # treating those data cells as headings for later rows.
  cells[9L, 2L] <- "Austria"
  cells[9L, 3L] <- "B254AT"
  cells[9L, c(5L, 7L, 9L, 11L)] <- c("1897", "2191", "2158", "2162")
  cells[10L, 2L] <- "Andorra"
  cells[10L, 3L] <- "B254AD"
  cells[10L, c(5L, 7L, 9L, 11L)] <- c("32", "71", "-", "33")
  workbook <- structure(
    list(`Total liabilities` = as.data.frame(cells, stringsAsFactors = FALSE)),
    class = c("boe_workbook", "list")
  )

  out <- readboe:::boe_extract_bankstats_series(
    workbook,
    "VPQB254AD",
    "B254AD",
    "C3.2",
    "Latest Tables/TabC3.2.xls",
    100L
  )

  expect_equal(out$period, c("2025-Q1", "2025-Q2", "2025-Q3", "2026-Q1"))
  expect_equal(out$date, as.Date(c(
    "2025-01-01", "2025-04-01", "2025-07-01", "2026-01-01"
  )))
  expect_equal(out$value, c(32, 71, NA, 33))
  expect_equal(out$value_status, c("", "", "-", ""))
  expect_true(all(out$date_precision == "quarter"))
  expect_true(all(out$orientation == "time_across_columns"))
  expect_equal(
    unique(out$series_label),
    "Total liabilities — Andorra"
  )
  expect_equal(unique(out$series_frequency), "Quarterly")
  expect_error(
    readboe:::boe_extract_bankstats_series(
      workbook,
      "VPQB254AD",
      "B254AD",
      "C3.2",
      "Latest Tables/TabC3.2.xls",
      3L
    ),
    class = "readboe_bounds"
  )
})

test_that("Bankstats exact-series parser fails closed on missing identity", {
  workbook <- structure(
    list(Sheet1 = data.frame(x = c("LPM", "45474"), y = c("OTHER", "1"))),
    class = c("boe_workbook", "list")
  )
  out <- readboe:::boe_extract_bankstats_series(
    workbook,
    "LPMAVAB",
    "AVAB",
    "A1.1.1",
    "Latest Tables/TabA1.1.1.xls",
    10L
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("canonical Bankstats history preserves IADB values and table identity", {
  iadb <- readboe:::boe_parse_iadb_xml(
    fixture_bytes("iadb.xml"),
    c("TEST001", "TEST002"),
    100L
  )
  iadb$metadata <- iadb$metadata[iadb$metadata$series_id == "TEST001", ,
                                 drop = FALSE]
  iadb$metadata$category_name <- "TYPE;OUTPUT IN;SEASONAL ADJUSTMENT"
  iadb$metadata$category_value <- "R;PCT;U"
  iadb$metadata$category_label <-
    "Rate;Per cent;Not seasonally adjusted"
  iadb$observations <- iadb$observations[
    iadb$observations$series_id == "TEST001",
    ,
    drop = FALSE
  ]
  receipt <- list(
    url = "https://www.bankofengland.co.uk/boeapps/database/test.xml",
    final_url = "https://www.bankofengland.co.uk/boeapps/database/test.xml",
    retrieved_at = as.POSIXct("2026-08-03", tz = "UTC"),
    raw_bytes = length(fixture_bytes("iadb.xml")),
    raw_sha256 = digest::digest(
      fixture_bytes("iadb.xml"),
      algo = "sha256",
      serialize = FALSE
    ),
    etag = NA_character_,
    last_modified = NA_character_,
    from_cache = FALSE
  )
  iadb <- readboe:::boe_attach_provenance(
    iadb,
    receipt,
    list(
      surface = "iadb",
      format = "xml",
      series_id = "TEST001",
      from = "2024-01-31",
      to = "2024-02-29"
    ),
    transforms = "XML syntax parsed"
  )
  catalogue <- data.frame(
    series_id = "TEST001",
    label = "Test rate",
    frequency = "Monthly",
    first_observation = "2024-01-31",
    last_observation = "2024-02-29",
    displayed_code = "001",
    source_series_url = "https://www.bankofengland.co.uk/series/TEST001",
    source_page_url = "https://www.bankofengland.co.uk/statistics/tables",
    source_page_sha256 = paste(rep("a", 64L), collapse = ""),
    stringsAsFactors = FALSE
  )
  attr(catalogue, "boe_catalogue_provenance") <- list(
    snapshot_sha256 = paste(rep("b", 64L), collapse = "")
  )

  out <- readboe:::boe_bankstats_canonical_rows(
    iadb,
    catalogue,
    table_id = "A1.1.1",
    max_output_rows = 10L
  )

  expect_s3_class(out, "boe_bankstats_series")
  expect_equal(out$date, as.Date(c("2024-01-31", "2024-02-29")))
  expect_equal(out$value, c(1.25, NA_real_))
  expect_equal(out$raw_value, c("1.25", ".."))
  expect_equal(out$value_status, c("", ".."))
  expect_equal(out$provisional_value, c(NA_real_, 1.5))
  expect_identical(unique(out$unit), "Per cent")
  expect_identical(
    unique(out$seasonal_adjustment),
    "Not seasonally adjusted"
  )
  expect_identical(unique(out$series_country), "UK")
  expect_identical(unique(out$definition), "Definition")
  expect_identical(unique(out$source_table_id), "A1.1.1")
  expect_identical(unique(out$orientation), "iadb_canonical_xml")
  expect_identical(
    boe_provenance(out)$source_identity$surface,
    "iadb_xml_canonical_bankstats_history"
  )
  expect_identical(
    boe_provenance(out)$source_identity$catalogue_snapshot_sha256,
    paste(rep("b", 64L), collapse = "")
  )

  catalogue$last_observation <- "2024-03-31"
  expect_error(
    readboe:::boe_bankstats_canonical_rows(
      iadb,
      catalogue,
      table_id = "A1.1.1",
      max_output_rows = 10L
    ),
    class = "readboe_identity_mismatch"
  )
})

test_that("Bankstats workbook headings become concise browse titles", {
  expect_equal(
    readboe:::boe_bankstats_nearest_title_piece(
      c("", "Net issuance", "", "of which:"),
      target = 3L,
      series_id = "CPMB24K",
      code = "B24K",
      prefix = "CPM"
    ),
    "Net issuance"
  )
  expect_equal(
    readboe:::boe_bankstats_format_series_title(
      "By instrument Issues Shares paper"
    ),
    "Issues — Shares"
  )
  expect_equal(
    readboe:::boe_bankstats_format_series_title(
      "MFI — By sector Repayments Bonds paper"
    ),
    "MFI — Repayments — Bonds"
  )
  expect_equal(
    readboe:::boe_bankstats_format_series_title(
      "By instrument of which: Sterling (£) paper"
    ),
    "Of which — Sterling (£) paper"
  )
})
