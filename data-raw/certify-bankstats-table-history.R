args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) {
  normalizePath(args[[1L]], mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
output <- if (length(args) >= 2L) {
  args[[2L]]
} else {
  file.path(root, "docs", "audits", "bankstats-table-history-live.csv")
}
library_path <- if (length(args) >= 3L) {
  normalizePath(args[[3L]], mustWork = TRUE)
} else {
  .libPaths()[[1L]]
}
workers <- if (length(args) >= 4L) {
  suppressWarnings(as.integer(args[[4L]]))
} else {
  4L
}
if (is.na(workers) || workers < 1L || workers > 4L) {
  stop("`workers` must be between one and four")
}

library(readboe, lib.loc = library_path)
catalogue <- boe_catalogue(limit = 100000L)
membership <- do.call(rbind, lapply(seq_len(nrow(catalogue)), function(i) {
  ids <- trimws(strsplit(catalogue$table_id[[i]], ";", fixed = TRUE)[[1L]])
  data.frame(table_id = ids[nzchar(ids)], catalogue_row = i)
}))
membership <- membership[order(membership$table_id), , drop = FALSE]
table_rows <- split(membership$catalogue_row, membership$table_id)
tasks <- do.call(rbind, lapply(names(table_rows), function(table_id) {
  rows <- unique(table_rows[[table_id]])
  x <- catalogue[rows, , drop = FALSE]
  first <- as.Date(x$first_observation)
  chosen <- which.min(first)
  data.frame(
    table_id = table_id,
    table_label = x$table_label[[chosen]],
    series_count = nrow(x),
    series_id = x$series_id[[chosen]],
    label = x$label[[chosen]],
    frequency = x$frequency[[chosen]],
    expected_first = x$first_observation[[chosen]],
    expected_last = x$last_observation[[chosen]],
    workbook_chartable = x$chartable[[chosen]],
    stringsAsFactors = FALSE
  )
}))
rownames(tasks) <- NULL

certify_one <- function(task) {
  started <- Sys.time()
  value <- tryCatch(
    read_boe_bankstats_series(
      task$series_id,
      table_id = task$table_id,
      history = "full",
      deadline = boe_deadline(90),
      max_bytes = 25 * 1024^2,
      max_output_rows = 250000L,
      cache = FALSE,
      refresh = TRUE
    ),
    error = identity
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  if (inherits(value, "error")) {
    return(data.frame(
      task,
      status = "failed",
      observations = NA_integer_,
      observed_first = NA_character_,
      observed_last = NA_character_,
      raw_missing_rows = NA_integer_,
      provisional_rows = NA_integer_,
      response_bytes = NA_real_,
      response_sha256 = NA_character_,
      elapsed_seconds = elapsed,
      error_class = class(value)[[1L]],
      error_message = conditionMessage(value),
      stringsAsFactors = FALSE
    ))
  }
  provenance <- boe_provenance(value)
  observed_first <- format(min(value$date), "%Y-%m-%d")
  observed_last <- format(max(value$date), "%Y-%m-%d")
  exact <- identical(unique(value$series_id), task$series_id) &&
    identical(unique(value$source_table_id), task$table_id) &&
    identical(observed_first, task$expected_first) &&
    identical(observed_last, task$expected_last) &&
    is.list(provenance) &&
    is.character(provenance$raw_sha256) &&
    length(provenance$raw_sha256) == 1L &&
    grepl("^[0-9a-f]{64}$", provenance$raw_sha256)
  data.frame(
    task,
    status = if (exact) "passed" else "failed_identity",
    observations = nrow(value),
    observed_first = observed_first,
    observed_last = observed_last,
    raw_missing_rows = sum(is.na(value$value)),
    provisional_rows = sum(!is.na(value$provisional_value)),
    response_bytes = provenance$raw_bytes,
    response_sha256 = provenance$raw_sha256,
    elapsed_seconds = elapsed,
    error_class = NA_character_,
    error_message = NA_character_,
    stringsAsFactors = FALSE
  )
}

if (workers == 1L) {
  pieces <- lapply(seq_len(nrow(tasks)), function(i) {
    certify_one(tasks[i, , drop = FALSE])
  })
} else {
  cluster <- parallel::makeCluster(workers, outfile = "")
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterExport(
    cluster,
    c("tasks", "certify_one", "library_path"),
    envir = environment()
  )
  parallel::clusterEvalQ(cluster, {
    library(readboe, lib.loc = library_path)
    NULL
  })
  pieces <- parallel::parLapplyLB(
    cluster,
    seq_len(nrow(tasks)),
    function(i) certify_one(tasks[i, , drop = FALSE])
  )
}
result <- do.call(rbind, pieces)
rownames(result) <- NULL
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(result, output, row.names = FALSE, na = "")
cat(
  "Certified",
  sum(result$status == "passed"),
  "of",
  nrow(result),
  "linked Bankstats tables.\n"
)
if (any(result$status != "passed")) {
  print(result[result$status != "passed", , drop = FALSE], row.names = FALSE)
  stop("At least one linked Bankstats table failed its canonical history canary")
}
