#' Create a caller-owned absolute deadline
#'
#' A deadline is an absolute point in time shared by transport retries and
#' parsing work.  Passing one deadline through a workflow prevents a retrying
#' request from silently extending the caller's time budget.  This constructor
#' does not perform I/O and does not modify options, the environment, or the
#' working directory.
#'
#' @param seconds Positive, finite number of seconds from the current time.
#'   It is ignored when `at` is supplied.
#' @param at One non-missing absolute [`POSIXct`][base::DateTimeClasses]
#'   value.  The value's timezone is retained; the class is normalised to the
#'   package's deadline class.
#' @return A length-one object with classes
#'   `c("readboe_deadline", "POSIXct", "POSIXt")`.  Its value is
#'   `at` when supplied, otherwise approximately `Sys.time() + seconds`.
#' @details `seconds` must be strictly positive and finite.  Invalid values,
#'   and `at` values that are not a single non-missing `POSIXct`, signal a
#'   `readboe_config` condition.  A deadline may later expire while a reader
#'   is running; readers then signal `readboe_timeout`.
#' @seealso [read_boe_series()], [boe_download_asset()],
#'   [boe_cache_clear()]
#' @examples
#' deadline <- boe_deadline(seconds = 5)
#' stopifnot(inherits(deadline, "readboe_deadline"), length(deadline) == 1L)
#' fixed <- boe_deadline(at = as.POSIXct("2030-01-01", tz = "UTC"))
#' stopifnot(identical(as.character(fixed), "2030-01-01"))
#' @export
boe_deadline <- function(seconds = 60, at = NULL) {
  if (is.null(at)) {
    boe_check_positive(seconds, "seconds")
    at <- Sys.time() + seconds
  }
  if (!inherits(at, "POSIXct") || length(at) != 1L || is.na(at)) {
    boe_abort("`at` must be one non-missing POSIXct value.", "config")
  }
  structure(at, class = c("readboe_deadline", "POSIXct", "POSIXt"))
}

boe_as_deadline <- function(deadline, default_seconds = 60) {
  if (is.null(deadline)) return(boe_deadline(default_seconds))
  if (inherits(deadline, "readboe_deadline")) return(deadline)
  if (inherits(deadline, "POSIXct")) return(boe_deadline(at = deadline))
  if (is.numeric(deadline) && length(deadline) == 1L) return(boe_deadline(deadline))
  boe_abort("`deadline` must be a readboe deadline, POSIXct time, or seconds.", "config")
}

boe_time_left <- function(deadline) {
  deadline <- boe_as_deadline(deadline)
  left <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
  if (!is.finite(left) || left <= 0) boe_abort("The caller-owned absolute deadline has expired.", "timeout", deadline = deadline)
  left
}
