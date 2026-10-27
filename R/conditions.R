#' @keywords internal
boe_abort <- function(message, subclass, ..., call = NULL) {
  cond <- structure(
    c(list(message = message, call = call), list(...)),
    class = c(paste0("readboe_", subclass), "readboe_error", "error", "condition")
  )
  stop(cond)
}

boe_assert_scalar <- function(x, name, type = c("character", "numeric", "logical"), allow_null = FALSE) {
  type <- match.arg(type)
  if (allow_null && is.null(x)) return(invisible(x))
  good <- length(x) == 1L && !is.na(x) && switch(type,
    character = is.character(x), numeric = is.numeric(x), logical = is.logical(x)
  )
  if (!good) boe_abort(sprintf("`%s` must be one non-missing %s value.", name, type), "config")
  invisible(x)
}

boe_check_positive <- function(x, name, integer = FALSE) {
  boe_assert_scalar(x, name, "numeric")
  if (!is.finite(x) || x <= 0 || (integer && x != floor(x))) {
    boe_abort(sprintf("`%s` must be a positive%s number.", name, if (integer) " integer" else ""), "config")
  }
  invisible(x)
}

boe_check_choice <- function(x, choices, name) {
  if (length(x) != 1L || is.na(x) || !x %in% choices) {
    boe_abort(sprintf("`%s` must be one of: %s.", name, paste(choices, collapse = ", ")), "config")
  }
  x
}

boe_validate_ids <- function(x, max_series = 300L) {
  if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(trimws(x)))) {
    boe_abort("`series_id` must be a non-empty character vector.", "config")
  }
  x <- toupper(trimws(x))
  if (length(x) > max_series) boe_abort(sprintf("At most %d series may be requested.", max_series), "bounds")
  if (any(!grepl("^[A-Z0-9._-]+$", x))) boe_abort("A series identifier contains unsupported characters.", "config")
  x
}

boe_signal_warning <- function(message, subclass, ...) {
  warning(structure(c(list(message = message, call = NULL), list(...)),
    class = c(paste0("readboe_", subclass), "readboe_warning", "warning", "condition")))
}

.boe_english_month_numbers <- c(
  jan = 1L, january = 1L, feb = 2L, february = 2L,
  mar = 3L, march = 3L, apr = 4L, april = 4L, may = 5L,
  jun = 6L, june = 6L, jul = 7L, july = 7L, aug = 8L, august = 8L,
  sep = 9L, september = 9L, oct = 10L, october = 10L,
  nov = 11L, november = 11L, dec = 12L, december = 12L
)

boe_english_month_number <- function(value) {
  key <- tolower(trimws(as.character(value)))
  unname(as.integer(.boe_english_month_numbers[key]))
}
