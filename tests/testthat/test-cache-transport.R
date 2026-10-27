test_that("cache is success-only, exact, atomic, corrupt-detecting, and bounded", {
  cache <- tempfile("readboe-cache-"); dir.create(cache); on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  key <- readboe:::boe_cache_key(list(a = 1L))
  entry <- list(body = charToRaw("abc"), raw_sha256 = digest::digest(charToRaw("abc"), algo = "sha256", serialize = FALSE))
  readboe:::boe_cache_write(cache, key, entry, 2L, 100000L)
  hit <- readboe:::boe_cache_read(cache, key, 60, FALSE)
  expect_true(hit$from_cache)
  path <- readboe:::boe_cache_path(cache, key)
  bad <- readRDS(path); bad$body <- charToRaw("bad"); saveRDS(bad, path)
  expect_error(readboe:::boe_cache_read(cache, key, 60, FALSE), class = "readboe_cache_corruption")
  expect_error(readboe:::boe_cache_read(cache, paste0(key, "0"), 60, TRUE), class = "readboe_cache_miss")
})

test_that("cache eviction obeys entry and aggregate-byte bounds", {
  cache <- tempfile("readboe-cache-"); dir.create(cache); on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  for (i in 1:4) {
    body <- charToRaw(paste(rep(as.character(i), 100), collapse = ""))
    entry <- list(body = body, raw_sha256 = digest::digest(body, algo = "sha256", serialize = FALSE))
    readboe:::boe_cache_write(cache, sprintf("%064d", i), entry, max_entries = 2L, max_cache_bytes = 1e6)
  }
  expect_lte(length(list.files(cache, pattern = "[.]rds$")), 2L)
  readboe:::boe_cache_prune(cache, max_entries = 10L, max_cache_bytes = 1L)
  expect_length(list.files(cache, pattern = "[.]rds$"), 0L)
})

test_that("transport canonicalizes identity, retries only transient status, and enforces bytes", {
  calls <- 0L
  local_mocked_bindings(
    boe_perform = function(req) {
      calls <<- calls + 1L
      if (calls == 1L) return(httr2::response(429, url = req$url, headers = list(`retry-after` = "0")))
      httr2::response(200, url = req$url, headers = list(etag = '"fixture"'), body = charToRaw("okay"))
    },
    boe_sleep = function(seconds) NULL,
    .package = "readboe"
  )
  cache <- tempfile("readboe-cache-"); dir.create(cache); on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  out <- readboe:::boe_request_raw("https://www.bankofengland.co.uk/test", list(z = "2", a = "1"),
    deadline = boe_deadline(10), cache_dir = cache, max_tries = 2L)
  expect_equal(calls, 2L)
  expect_match(out$url, "[?]a=1&z=2$", fixed = FALSE)
  expect_identical(rawToChar(out$body), "okay")
  cached <- readboe:::boe_request_raw("https://www.bankofengland.co.uk/test", list(a = "1", z = "2"),
    deadline = boe_deadline(10), cache_dir = cache)
  expect_true(cached$from_cache)
  expect_equal(calls, 2L)
  expect_error(readboe:::boe_request_raw("https://www.bankofengland.co.uk/test", list(a = "1", z = "2"),
    deadline = boe_deadline(10), cache_dir = cache, cache_only = TRUE, max_bytes = 3L), class = "readboe_bounds")
  expect_error(readboe:::boe_request_raw("http://www.bankofengland.co.uk/test", deadline = 1), class = "readboe_config")
})

test_that("transport byte bounds fail closed", {
  local_mocked_bindings(
    boe_perform = function(req) httr2::response(200, url = req$url, body = charToRaw("12345")),
    .package = "readboe"
  )
  expect_error(readboe:::boe_request_raw("https://www.bankofengland.co.uk/test",
    deadline = boe_deadline(10), max_bytes = 4L, cache = FALSE), class = "readboe_bounds")
})

test_that("non-transient failures are not retried", {
  calls <- 0L
  local_mocked_bindings(
    boe_perform = function(req) { calls <<- calls + 1L; httr2::response(404, url = req$url) },
    boe_sleep = function(seconds) stop("must not sleep"),
    .package = "readboe"
  )
  expect_error(readboe:::boe_request_raw("https://www.bankofengland.co.uk/missing",
    deadline = boe_deadline(10), cache = FALSE, max_tries = 3L), class = "readboe_transport")
  expect_equal(calls, 1L)
})

test_that("transfer byte errors and Retry-After deadline overruns are typed", {
  local_mocked_bindings(
    boe_perform = function(req) stop("Maximum file size exceeded"),
    .package = "readboe"
  )
  expect_error(readboe:::boe_request_raw("https://www.bankofengland.co.uk/large",
    deadline = boe_deadline(10), cache = FALSE, max_bytes = 4L), class = "readboe_bounds")

  local_mocked_bindings(
    boe_perform = function(req) httr2::response(429, url = req$url, headers = list(`retry-after` = "60")),
    boe_sleep = function(seconds) stop("must not sleep past the caller deadline"),
    .package = "readboe"
  )
  expect_error(readboe:::boe_request_raw("https://www.bankofengland.co.uk/rate-limited",
    deadline = boe_deadline(1), cache = FALSE, max_tries = 2L), class = "readboe_timeout")
})

test_that("Retry-After HTTP dates are independent of LC_TIME", {
  headers <- list(`retry-after` = "Wed, 21 Oct 2015 07:28:00 GMT")
  expect_equal(readboe:::boe_retry_after(headers, default = 0.5), 0)
  expect_equal(
    readboe:::boe_retry_after(list(`retry-after` = "15"), default = 0.5),
    15
  )
})

test_that("HTTP-200 application error pages are retried and never cached", {
  calls <- 0L
  local_mocked_bindings(
    boe_perform = function(req) {
      calls <<- calls + 1L
      body <- if (calls == 1L) charToRaw("<html><title>Error</title>Access denied</html>") else charToRaw("okay")
      httr2::response(200, url = req$url, body = body)
    },
    boe_sleep = function(seconds) NULL,
    .package = "readboe"
  )
  cache <- tempfile("readboe-cache-"); dir.create(cache); on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  out <- readboe:::boe_request_raw("https://www.bankofengland.co.uk/application-error",
    deadline = boe_deadline(10), cache_dir = cache, max_tries = 2L,
    validate_response = readboe:::boe_validate_not_error_html)
  expect_equal(calls, 2L)
  expect_identical(rawToChar(out$body), "okay")
  expect_length(list.files(cache, pattern = "[.]rds$"), 1L)
  expect_true(readboe:::boe_validate_not_error_html(as.raw(c(80L, 75L, 3L, 4L, 255L)),
    "https://www.bankofengland.co.uk/archive.zip"))
})
