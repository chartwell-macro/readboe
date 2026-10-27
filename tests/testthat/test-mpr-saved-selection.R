test_that('the explicit raw route forwards every public parser argument', {
  local_mocked_bindings(read_boe_mpr_forecasts_raw = function(...) list(...),
    .package = 'readboe')
  args <- list(release = '2034-03', measure = 'cpi_inflation', scenario = 'market',
    statistic = 'mean', frequency = 'Q', kind = 'central_forecast', series_id = NULL,
    include_history = TRUE, include_distributions = TRUE, historical_source = FALSE,
    expected_sha256 = strrep('a', 64), deadline = 17, max_bytes = 1234,
    max_archive_members = 3L, max_uncompressed_bytes = 5678,
    max_sheets = 4L, max_rows = 55L, max_columns = 6L, max_cells = 77,
    max_output_rows = 88L, cache = FALSE, cache_only = FALSE,
    refresh = TRUE, cache_dir = tempfile('raw-forward-'))
  actual <- do.call(read_boe_mpr_forecasts, c(args,
    list(source = 'raw', data_dir = tempfile('unused-saved-'))))
  expect_setequal(names(actual), names(args))
  expect_identical(actual[names(args)], args)
})

test_that('complete saved selections retain typed values and copy isolation', {
  fixture <- saved_fixture()
  on.exit(unlink(c(fixture$bundle_dir, fixture$data_dir), recursive = TRUE), add = TRUE)
  forbidden <- function(...) stop('Ordinary saved reads must not acquire or persist data')
  local_mocked_bindings(boe_mpr_snapshot_paths = fixture$paths,
    boe_perform = forbidden, boe_cache_read = forbidden, boe_cache_write = forbidden,
    boe_mpr_download = forbidden, boe_mpr_pdf_words = forbidden,
    boe_mpr_atomic_rds = forbidden, .package = 'readboe')
  stored <- readRDS(fixture$paths()[['data']])$observations
  expected <- stored[seq_len(nrow(stored)),
    setdiff(names(stored), c('.default', '.forecast')), drop = FALSE]
  for (name in names(expected)) {
    if (is.factor(expected[[name]])) expected[[name]] <- as.character(expected[[name]])
  }
  rownames(expected) <- NULL
  class(expected) <- c('boe_mpr_forecasts', 'data.frame')
  for (edition in c('latest', 'all', '2034-03')) {
    actual <- read_boe_mpr_forecasts(edition, include_history = TRUE,
      include_distributions = TRUE, data_dir = fixture$data_dir)
    provenance <- boe_provenance(actual)
    attr(actual, 'boe_provenance') <- NULL
    expect_identical(actual, expected)
    expect_identical(lapply(actual, serialize, connection = NULL, version = 3),
      lapply(expected, serialize, connection = NULL, version = 3))
    expect_identical(provenance$snapshot$snapshot_sha256,
      readRDS(fixture$paths()[['manifest']])$sha256)
  }
  selected <- read_boe_mpr_forecasts('2034-03',
    series_id = unique(expected$series_id), data_dir = fixture$data_dir)
  expect_identical(selected$value, expected$value)
  cached <- readboe:::.boe_mpr_memory$snapshot
  complete <- read_boe_mpr_forecasts('all', include_history = TRUE,
    include_distributions = TRUE, data_dir = fixture$data_dir)
  complete$value[1L] <- -100
  complete$target_date[1L] <- as.Date('1900-01-01')
  complete$units[1L] <- 'changed by caller'
  again <- read_boe_mpr_forecasts('all', include_history = TRUE,
    include_distributions = TRUE, data_dir = fixture$data_dir)
  expect_identical(readboe:::.boe_mpr_memory$snapshot, cached)
  attr(again, 'boe_provenance') <- NULL
  expect_identical(again, expected)
  expect_false(file.exists(fixture$data_dir))
})

test_that('whole-table selection preserves refreshed generations and caller edition order', {
  fixture <- saved_fixture()
  on.exit(unlink(c(fixture$bundle_dir, fixture$data_dir), recursive = TRUE), add = TRUE)
  local_mocked_bindings(boe_mpr_snapshot_paths = fixture$paths,
    boe_mpr_parser_fingerprint = fixture$fingerprint, boe_perform = fixture$perform,
    boe_mpr_discover_live = function(...) fixture$releases, .package = 'readboe')
  before <- read_boe_mpr_forecasts('all', include_history = TRUE,
    include_distributions = TRUE, data_dir = fixture$data_dir)
  fixture$set_edition('2034-06', 7, '2034-06-15')
  changed <- boe_mpr_update(data_dir = fixture$data_dir, force = TRUE)
  expect_identical(changed$status, 'updated')
  forbidden <- function(...) stop('Ordinary saved reads must not acquire or persist data')
  local_mocked_bindings(boe_perform = forbidden, boe_cache_read = forbidden,
    boe_cache_write = forbidden, boe_mpr_download = forbidden,
    boe_mpr_pdf_words = forbidden, boe_mpr_atomic_rds = forbidden, .package = 'readboe')
  complete <- read_boe_mpr_forecasts('all', include_history = TRUE,
    include_distributions = TRUE, data_dir = fixture$data_dir)
  expect_identical(complete$value, c(2, 3, 7, 8))
  expect_false(identical(boe_provenance(before)$snapshot$snapshot_sha256,
    boe_provenance(complete)$snapshot$snapshot_sha256))
  expect_identical(boe_provenance(complete)$snapshot$snapshot_sha256,
    changed$snapshot_sha256)
  reordered <- read_boe_mpr_forecasts(c('2034-06', '2034-03'),
    include_history = TRUE, include_distributions = TRUE, data_dir = fixture$data_dir)
  expect_identical(reordered$value, c(7, 8, 2, 3))
  expect_identical(boe_provenance(reordered)$source_identity$release,
    c('2034-06', '2034-03'))
  expect_identical(read_boe_mpr_forecasts('latest', data_dir = fixture$data_dir)$value, 8)
  expect_identical(read_boe_mpr_forecasts('all', data_dir = fixture$data_dir)$value, c(2, 3, 8))
})

test_that('complete bundled reads retain literal typed-column serialization', {
  forbidden <- function(...) stop('Ordinary saved reads must not acquire or persist data')
  local_mocked_bindings(boe_perform = forbidden, boe_cache_read = forbidden,
    boe_cache_write = forbidden, boe_mpr_download = forbidden,
    boe_mpr_pdf_words = forbidden, boe_mpr_atomic_rds = forbidden, .package = 'readboe')
  directory <- tempfile('literal-saved-columns-')
  stored <- readRDS(readboe:::boe_mpr_snapshot_paths()[['data']])$observations
  actual <- read_boe_mpr_forecasts('all', include_history = TRUE,
    include_distributions = TRUE, data_dir = directory)
  expect_identical(nrow(actual), nrow(stored))
  for (name in names(actual)) {
    value <- stored[[name]]
    expected <- if (is.factor(value)) as.character(value) else value[seq_len(nrow(stored))]
    expect_identical(serialize(actual[[name]], NULL, version = 3),
      serialize(expected, NULL, version = 3), info = name)
  }
  expect_false(file.exists(directory))
})
