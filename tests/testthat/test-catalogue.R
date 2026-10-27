catalogue_fixture <- function(path) {
  x <- data.frame(
    series_id = c("A", "B", "C"), label = c("Money amount", "Bank rate monthly", "Bank rate daily"),
    status = c("discontinued", "bankstats_current", "bankstats_current"),
    metadata_status = c("unavailable_discontinued_only", "available", "available"),
    frequency = c(NA, "Monthly", "Daily"), frequency_code = c(NA, "M", "D"),
    table_id = c(NA, "G1.4", "G1.4;G1.5"), section = c(NA, "Rates", "Rates;Money"),
    surface_membership = c("discontinued_workbook", "iadb_advanced_search;bankstats_current", "bankstats_current"),
    concatenated_label = NA_character_, definition = NA_character_, category_name = NA_character_,
    category_value = NA_character_, category_label = NA_character_, table_label = NA_character_,
    stringsAsFactors = FALSE)
  saveRDS(x, path)
}

test_that("exact browse facets and implicit-AND phrases run before caps", {
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path), add = TRUE); catalogue_fixture(path)
  x <- boe_catalogue(query = '"bank rate" monthly', status = "bankstats_current", frequency = "M",
    table_id = "G1.4", section = "Rates", surface_membership = "iadb_advanced_search", limit = 1L,
    catalogue_path = path)
  expect_equal(x$series_id, "B")
  expect_equal(attr(x, "boe_catalogue_provenance")$matched_before_cap, 1L)
  expect_equal(boe_catalogue(series_id = "c", limit = 1L, catalogue_path = path)$series_id, "C")
  facets <- boe_catalogue_facets(table_id = "G1.4", catalogue_path = path)
  expect_equal(sum(facets$status$n), 2L)
  expect_error(boe_catalogue(status = NA_character_, catalogue_path = path), class = "readboe_config")
})

test_that("pinned snapshot hashes fail closed on identity mismatch", {
  dir <- tempfile("readboe-snapshot-"); dir.create(dir); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- file.path(dir, "iadb-catalogue.rds")
  catalogue_fixture(path)
  manifest <- data.frame(catalogue_snapshot_sha256 = paste(rep("0", 64L), collapse = ""),
    stringsAsFactors = FALSE)
  saveRDS(manifest, file.path(dir, "iadb-catalogue-manifest.rds"))
  expect_error(readboe:::boe_assert_pinned_snapshot(path, "catalogue_snapshot_sha256"),
    class = "readboe_identity_mismatch")
})

test_that("bundled browse snapshot authenticates every exact Bankstats page identity", {
  path <- system.file("extdata", "iadb-catalogue.rds", package = "readboe")
  manifest_path <- system.file("extdata", "iadb-catalogue-manifest.rds", package = "readboe")
  expect_true(nzchar(path) && file.exists(path))
  expect_true(nzchar(manifest_path) && file.exists(manifest_path))

  catalogue <- readRDS(path)
  manifest <- readRDS(manifest_path)
  expect_identical(
    manifest$catalogue_scope,
    "bankstats_current_page_retrievable_exact"
  )
  expect_true(manifest$catalogue_complete_within_scope)
  expect_equal(nrow(catalogue), manifest$catalogue_rows)
  expect_gte(nrow(catalogue), 5250L)
  expect_equal(
    nrow(catalogue),
    manifest$bankstats_page_series_rows -
      manifest$bankstats_unavailable_page_series
  )
  expect_false(anyNA(catalogue$series_id))
  expect_equal(anyDuplicated(catalogue$series_id), 0L)
  expect_true(all(catalogue$status == "bankstats_current"))
  expect_true(all(catalogue$metadata_status == "iadb_xml_canonical"))
  expect_true(any(catalogue$chartable))
  expect_true(any(!catalogue$chartable))
  expect_true(all(grepl("iadb_xml", catalogue$surface_membership)))
  expect_true(all(
    grepl("bankstats_current_bulk", catalogue$surface_membership) ==
      catalogue$chartable
  ))
  expect_true(any(catalogue$series_id == "IUMAAMIJ"))
  expect_true(all(!is.na(catalogue$label) & nzchar(catalogue$label)))
  expect_true(all(!is.na(catalogue$frequency) & nzchar(catalogue$frequency)))
  expect_true(all(!is.na(catalogue$table_id) & nzchar(catalogue$table_id)))
  first <- as.Date(catalogue$first_observation)
  last <- as.Date(catalogue$last_observation)
  generated <- as.Date(substr(manifest$generated_at_utc, 1L, 10L))
  expect_false(anyNA(first))
  expect_false(anyNA(last))
  expect_true(all(first <= last))
  expect_true(all(last <= generated + 366L))
  facets <- boe_catalogue_facets(catalogue_path = path)
  expect_gte(nrow(facets$table_id), 60L)
  expect_identical(readboe:::boe_sha256_file(path), manifest$catalogue_snapshot_sha256)
})

test_that("page identities without bulk cells fail closed only in workbook mode", {
  path <- system.file("extdata", "iadb-catalogue.rds", package = "readboe")
  catalogue <- readRDS(path)
  unavailable <- catalogue[!catalogue$chartable, , drop = FALSE]
  expect_gt(nrow(unavailable), 0L)
  table_id <- trimws(strsplit(
    unavailable$table_id[[1L]],
    ";",
    fixed = TRUE
  )[[1L]])[[1L]]
  expect_error(
    read_boe_bankstats_series(
      unavailable$series_id[[1L]],
      table_id = table_id,
      history = "current_workbook",
      cache_only = TRUE
    ),
    class = "readboe_upstream_unavailable"
  )
})
