#!/usr/bin/env Rscript
# Explicit scheduler/application entry point. Ordinary reads never run this.
args <- commandArgs(trailingOnly = TRUE)
value <- function(flag, default = NULL) {
  i <- match(flag, args); if (is.na(i)) return(default)
  if (i == length(args)) stop('Missing value after ', flag)
  args[[i + 1L]]
}
suppressPackageStartupMessages(library(readboe))
dir <- value('--data-dir', readboe:::boe_mpr_data_dir())
if ('--maintain' %in% args) {
  x <- boe_mpr_maintain(data_dir = dir, force = '--force' %in% args,
    deadline = as.numeric(value('--deadline', '3600')), retain_raw = '--retain-raw' %in% args)
} else if ('--maintenance-status' %in% args) {
  x <- boe_mpr_maintenance_status(data_dir = dir)
} else {
  x <- boe_mpr_update(data_dir = dir, force = '--force' %in% args,
    rebuild = '--rebuild' %in% args, deadline = as.numeric(value('--deadline', '3600')),
    stop_on_error = FALSE, retain_raw = '--retain-raw' %in% args)
}
if ('--json' %in% args) {
  cat(jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null', na = 'null', POSIXt = 'ISO8601'), '\n')
} else {
  cat('Forecast check:', x$status, '\n')
  cat('Data directory:', normalizePath(dir, mustWork = FALSE), '\n')
  cat('Snapshot:', x$snapshot_sha256, '\n')
  cat('Affected editions:', paste(x$affected_editions, collapse = ', '), '\n')
  if (!is.null(x$next_due_at)) cat('Next due UTC:', x$next_due_at, '\n')
  if (!is.null(x$next_release_edition)) cat('Next release:', x$next_release_edition, x$next_release_date, x$next_release_at, '\n')
  if (!is.null(x$error)) cat('Error:', x$error, '\n')
}
# Metadata only: no numeric data or raw sources are exported to CI summaries.
summary <- Sys.getenv('GITHUB_STEP_SUMMARY')
if (nzchar(summary)) cat('Forecast source check: ', x$status, '\n\nAffected editions: ',
  paste(x$affected_editions, collapse = ', '), '\n\nSnapshot SHA256: ', x$snapshot_sha256,
  '\n\nThis runner-local refresh does not distribute data to package consumers.\n', file = summary, append = TRUE, sep = '')
if (x$status %in% c('failed', 'failed_throttled', 'interrupted')) quit(status = 1L)
