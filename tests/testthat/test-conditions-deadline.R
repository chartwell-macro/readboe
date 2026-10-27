test_that("typed conditions and absolute deadlines are stable", {
  expect_s3_class(boe_deadline(1), "readboe_deadline")
  expect_error(readboe:::boe_time_left(boe_deadline(at = Sys.time() - 1)), class = "readboe_timeout")
  expect_error(readboe:::boe_validate_ids(character()), class = "readboe_config")
  expect_error(readboe:::boe_validate_ids(rep("A", 301)), class = "readboe_bounds")
  expect_equal(readboe:::boe_validate_ids(" iudbedr "), "IUDBEDR")
})

test_that("package helpers do not mutate process-global state", {
  before <- list(options = options(), env = Sys.getenv(), wd = getwd())
  invisible(boe_rate_series())
  invisible(boe_yield_assets())
  invisible(boe_release_registry())
  after <- list(options = options(), env = Sys.getenv(), wd = getwd())
  expect_identical(after, before)
})
