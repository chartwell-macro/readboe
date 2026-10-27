test_that("Windows lock checks use a read-only process list and fail closed", {
  expect_true(readboe:::boe_mpr_process_alive(Sys.getpid(), "windows"))
  live <- as.double(Sys.getpid()) + 100
  absent <- live + 1
  local_mocked_bindings(boe_mpr_windows_pids = function() c(0, 4, live), .package = "readboe")
  expect_true(readboe:::boe_mpr_process_alive(live, "windows"))
  expect_false(readboe:::boe_mpr_process_alive(absent, "windows"))
  local_mocked_bindings(boe_mpr_windows_pids = function() NULL, .package = "readboe")
  expect_true(readboe:::boe_mpr_process_alive(live, "windows"))
  for (pid in list(NULL, NA_integer_, 0, -1, 1.5, "123", Inf, c(1, 2))) {
    expect_true(readboe:::boe_mpr_process_alive(pid, "windows"))
  }
})

test_that("an uncertain Windows process cannot release an expired writer lock", {
  dir <- tempfile("readboe-lock-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  lock <- readboe:::boe_mpr_lock(dir)
  owner_file <- file.path(lock$path, "owner.rds")
  owner <- readRDS(owner_file)
  owner$pid <- as.double(Sys.getpid()) + 100
  saveRDS(owner, owner_file)
  Sys.setFileTime(lock$path, Sys.time() - 120)
  alive <- readboe:::boe_mpr_process_alive
  local_mocked_bindings(boe_mpr_process_alive = function(pid) alive(pid, "windows"),
    boe_mpr_windows_pids = function() NULL, .package = "readboe")
  expect_null(readboe:::boe_mpr_lock(dir))
  expect_identical(readRDS(owner_file), owner)
  local_mocked_bindings(boe_mpr_windows_pids = function() c(0, 4), .package = "readboe")
  replacement <- readboe:::boe_mpr_lock(dir)
  expect_type(replacement, "list")
  expect_false(identical(replacement$token, lock$token))
  readboe:::boe_mpr_unlock(replacement)
})

test_that("the native Windows query and lock contention leave this process alive", {
  skip_on_os(c("linux", "mac", "solaris"))
  expect_true(Sys.getpid() %in% readboe:::boe_mpr_windows_pids())
  dir <- tempfile("readboe-native-windows-lock-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  lock <- readboe:::boe_mpr_lock(dir)
  expect_null(readboe:::boe_mpr_lock(dir))
  readboe:::boe_mpr_unlock(lock)
})
