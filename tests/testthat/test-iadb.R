fixture_raw <- function(name) readBin(test_path("fixtures", name), "raw", n = file.info(test_path("fixtures", name))$size)

test_that("canonical XML preserves metadata, missing markers, provisional values, and revisions", {
  x <- readboe:::boe_parse_iadb_xml(fixture_raw("iadb.xml"), c("TEST001", "TEST002"), 100L)
  expect_s3_class(x, "boe_iadb")
  expect_equal(x$metadata$series_id, c("TEST001", "TEST002"))
  expect_equal(x$metadata$frequency_code, c("M", "A"))
  expect_equal(nrow(x$observations), 3L)
  suppressed <- x$observations[x$observations$value_raw == "..", ]
  expect_true(is.na(suppressed$value))
  expect_equal(suppressed$provisional_value, 1.5)
  expect_equal(suppressed$confidential, "Y")
  expect_equal(suppressed$last_revised, "2024-03-01 09:30:00")
  expect_equal(x$observations$observation_comment[[1]], "Observation note")
  expect_error(readboe:::boe_parse_iadb_xml(fixture_raw("iadb.xml"), "WRONG", 100L), class = "readboe_identity_mismatch")
  expect_error(readboe:::boe_parse_iadb_xml(fixture_raw("iadb.xml"), c("TEST001", "TEST002"), 2L), class = "readboe_bounds")
})

test_that("XML duplicate/missing identity and schema drift fail closed", {
  doc <- xml2::read_xml(fixture_raw("iadb.xml"))
  node <- xml2::xml_find_first(doc, "//*[local-name()='Cube'][@SCODE]")
  xml2::xml_add_child(xml2::xml_root(doc), node)
  expect_error(readboe:::boe_parse_iadb_xml(charToRaw(as.character(doc))), class = "readboe_identity_mismatch")
  expect_error(readboe:::boe_parse_iadb_xml(charToRaw("<Envelope><Cube/></Envelope>")), class = "readboe_schema_drift")
})

test_that("CSV compatibility retains missing/suppression rows and exact IDs", {
  x <- readboe:::boe_parse_iadb_csv(fixture_raw("iadb.csv"), c("TEST001", "TEST002"), 100L)
  expect_equal(nrow(x), 4L)
  expect_true(any(x$value_raw == ".."))
  expect_true(any(is.na(x$value)))
  expect_equal(x$value[x$series_id == "TEST002" & x$date == as.Date("2024-01-31")], 2000)
  expect_error(readboe:::boe_parse_iadb_csv(fixture_raw("iadb.csv"), "TEST001", 100L), class = "readboe_identity_mismatch")
  duplicate <- charToRaw("DATE,TEST001,TEST001\n31 Jan 2024,1,2\n")
  expect_error(readboe:::boe_parse_iadb_csv(duplicate, "TEST001", 100L), class = "readboe_identity_mismatch")
})

test_that("deterministic XML and CSV non-missing values reconcile exactly", {
  xml <- readboe:::boe_parse_iadb_xml(fixture_raw("iadb.xml"), c("TEST001", "TEST002"), 100L)$observations
  csv <- readboe:::boe_parse_iadb_csv(fixture_raw("iadb.csv"), c("TEST001", "TEST002"), 100L)
  xml <- xml[!is.na(xml$value), c("series_id", "date", "value")]
  csv <- csv[!is.na(csv$value), c("series_id", "date", "value")]
  common <- merge(xml, csv, by = c("series_id", "date"), suffixes = c("_xml", "_csv"), sort = TRUE)
  expect_gt(nrow(common), 0L)
  expect_identical(common$value_xml, common$value_csv)
})

test_that("exact provenance survives save and restore", {
  receipt <- list(url = "https://www.bankofengland.co.uk/test", retrieved_at = as.POSIXct("2026-01-01", tz = "UTC"),
    raw_bytes = 3L, raw_sha256 = paste(rep("a", 64), collapse = ""), etag = '"x"', last_modified = NA_character_, from_cache = FALSE)
  x <- readboe:::boe_attach_provenance(data.frame(value = c(1, NA)), receipt,
    list(surface = "fixture", id = "A"), transforms = character())
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path), add = TRUE); saveRDS(x, path)
  restored <- readRDS(path)
  expect_identical(boe_provenance(restored), boe_provenance(x))
  expect_identical(restored$value, x$value)
})
