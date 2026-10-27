.boe_allowed_hosts <- c("www.bankofengland.co.uk", "bankofengland.co.uk")

boe_validate_url <- function(url) {
  boe_assert_scalar(url, "url", "character")
  parsed <- httr2::url_parse(url)
  if (!identical(tolower(parsed$scheme), "https") || !tolower(parsed$hostname) %in% .boe_allowed_hosts) {
    boe_abort("Only allowlisted HTTPS Bank of England hosts may be requested.", "config", url = url)
  }
  invisible(url)
}

boe_canonical_url <- function(url, query = list()) {
  boe_validate_url(url)
  if (length(query)) {
    query <- query[order(names(query))]
    query <- lapply(query, function(x) if (is.logical(x)) ifelse(x, "Y", "N") else as.character(x))
    url <- do.call(httr2::req_url_query, c(list(.req = httr2::request(url)), query))$url
  }
  url
}

boe_header <- function(headers, name) {
  idx <- which(tolower(names(headers)) == tolower(name))
  if (length(idx)) unname(headers[[idx[[1L]]]]) else NA_character_
}

boe_receipt_from_entry <- function(entry) {
  list(
    url = entry$url,
    final_url = entry$final_url %||% entry$url,
    retrieved_at = entry$retrieved_at,
    raw_bytes = length(entry$body),
    raw_sha256 = entry$raw_sha256,
    etag = entry$etag,
    last_modified = entry$last_modified,
    status = entry$status,
    from_cache = isTRUE(entry$from_cache)
  )
}

boe_retry_after <- function(headers, default) {
  value <- boe_header(headers, "retry-after")
  if (is.na(value) || !nzchar(value)) return(default)
  seconds <- suppressWarnings(as.numeric(value))
  if (is.finite(seconds) && seconds >= 0) return(seconds)
  match <- regmatches(
    value,
    regexec(
      paste0(
        "^(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun),[[:space:]]+([0-9]{2})",
        "[[:space:]]+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)",
        "[[:space:]]+([0-9]{4})[[:space:]]+([0-9]{2}):([0-9]{2}):",
        "([0-9]{2})[[:space:]]+GMT$"
      ),
      value,
      ignore.case = TRUE,
      perl = TRUE
    )
  )[[1L]]
  if (length(match) != 7L) return(default)
  month <- boe_english_month_number(match[[3L]])
  if (is.na(month)) return(default)
  at <- suppressWarnings(as.POSIXct(
    sprintf(
      "%04d-%02d-%02d %02d:%02d:%02d",
      as.integer(match[[4L]]), month, as.integer(match[[2L]]),
      as.integer(match[[5L]]), as.integer(match[[6L]]), as.integer(match[[7L]])
    ),
    tz = "GMT",
    format = "%Y-%m-%d %H:%M:%S"
  ))
  if (!is.na(at)) return(max(0, as.numeric(difftime(at, Sys.time(), units = "secs"))))
  default
}

boe_perform <- function(req) httr2::req_perform(req)

boe_sleep <- function(seconds) Sys.sleep(seconds)

boe_run_response_validator <- function(validate_response, body, url) {
  if (is.null(validate_response)) return(invisible(TRUE))
  if (!is.function(validate_response)) boe_abort("`validate_response` must be a function.", "config")
  valid <- validate_response(body, url)
  if (!isTRUE(valid)) boe_abort("The response failed its surface-specific success contract.",
    "schema_drift", url = url)
  invisible(TRUE)
}

boe_validate_not_error_html <- function(body, url) {
  values <- as.integer(body)
  first <- which(!values %in% c(9L, 10L, 13L, 32L))
  if (!length(first) || values[[first[[1L]]]] != 60L) return(TRUE)
  bytes <- body[seq_len(min(length(body), 5000L))]
  prefix <- tolower(intToUtf8(as.integer(bytes)))
  if (grepl("<title[^>]*>[[:space:]]*error|access denied|request rejected", prefix, perl = TRUE)) {
    boe_abort("The official host returned an application error page.", "transport", url = url)
  }
  TRUE
}

boe_request_raw <- function(url, query = list(), deadline = NULL,
                            attempt_timeout = 30, max_bytes = 25 * 1024^2,
                            cache = TRUE, cache_only = FALSE, refresh = FALSE,
                            cache_dir = boe_default_cache_dir(), ttl = 86400,
                            max_cache_entries = 200L, max_cache_bytes = 250 * 1024^2,
                            max_redirects = 3L, max_tries = 3L,
                            validate_response = NULL, method = "GET", form = NULL,
                            request_headers = list(), allow_not_modified = FALSE) {
  deadline <- boe_as_deadline(deadline)
  boe_assert_scalar(allow_not_modified, "allow_not_modified", "logical")
  if ((length(request_headers) || allow_not_modified) && cache) boe_abort("Conditional probes must use cache=FALSE.", "config")
  if (!is.list(request_headers) || (length(request_headers) && (is.null(names(request_headers)) ||
      anyDuplicated(names(request_headers)) || any(!vapply(request_headers, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1L)))))) boe_abort("Invalid conditional request headers.", "config")
  boe_check_choice(method, c("GET", "POST"), "method")
  if ((method == "GET" && !is.null(form)) || (method == "POST" &&
      (!is.list(form) || !length(form) || is.null(names(form)) || anyDuplicated(names(form)) ||
       any(!nzchar(names(form))) || any(!vapply(form, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1L)))))) {
    boe_abort("Search POST forms must contain uniquely named scalar strings.", "config")
  }
  boe_check_positive(attempt_timeout, "attempt_timeout")
  boe_check_positive(max_bytes, "max_bytes", integer = TRUE)
  boe_check_positive(ttl, "ttl")
  boe_check_positive(max_cache_entries, "max_cache_entries", integer = TRUE)
  boe_check_positive(max_cache_bytes, "max_cache_bytes", integer = TRUE)
  boe_check_positive(max_redirects, "max_redirects", integer = TRUE)
  boe_check_positive(max_tries, "max_tries", integer = TRUE)
  canonical <- boe_canonical_url(url, query)
  request_identity <- list(method = "GET", url = canonical, accept = "*/*", contract = 1L)
  if (method == "POST") request_identity <- list(method = method, url = canonical,
    accept = "*/*", form = form[order(names(form))], contract = 1L)
  key <- boe_cache_key(request_identity)
  if (cache && !refresh) {
    hit <- boe_cache_read(cache_dir, key, ttl, cache_only)
    if (!is.null(hit)) {
      if (length(hit$body) > max_bytes) boe_abort("The cached response exceeds `max_bytes`.",
        "bounds", observed_bytes = length(hit$body), max_bytes = max_bytes, url = canonical)
      validation_error <- tryCatch({
        boe_run_response_validator(validate_response, hit$body, canonical)
        NULL
      }, error = identity)
      if (is.null(validation_error)) return(hit)
      unlink(boe_cache_path(cache_dir, key))
      if (cache_only) stop(validation_error)
    }
  } else if (cache_only) {
    boe_abort("`cache_only = TRUE` cannot be combined with disabled cache or refresh.", "config")
  }
  transient_status <- c(408L, 425L, 429L, 500L, 502L, 503L, 504L)
  resp <- NULL
  response_body <- NULL
  last_error <- NULL
  for (attempt in seq_len(max_tries)) {
    left <- boe_time_left(deadline)
    timeout <- max(0.001, min(attempt_timeout, left))
    req <- httr2::request(canonical) |>
      httr2::req_user_agent("readboe/0.1.0 (+https://github.com/chartwell-macro/readboe)") |>
      httr2::req_headers(Accept = "*/*") |>
      httr2::req_timeout(timeout) |>
      httr2::req_options(maxredirs = max_redirects, maxfilesize = max_bytes, http_version = 2L) |>
      httr2::req_error(is_error = function(resp) FALSE)
    if (method == "POST") req <- do.call(httr2::req_body_form, c(list(.req = req), form))
    if (length(request_headers)) req <- do.call(httr2::req_headers, c(list(.req = req), request_headers))
    resp <- tryCatch(boe_perform(req), error = function(e) {
      last_error <<- e
      NULL
    })
    if (is.null(resp)) {
      msg <- conditionMessage(last_error)
      if (grepl("maximum file size|file size exceeded|CURLE_FILESIZE_EXCEEDED", msg, ignore.case = TRUE)) {
        boe_abort("The response exceeded `max_bytes` during transfer.", "bounds", url = canonical,
          max_bytes = max_bytes, attempt = attempt)
      }
      transient_error <- grepl("timed out|timeout|Could not resolve|Failed to connect|Connection reset|Empty reply|HTTP/?2.*(stream|framing)|SSL connect", msg, ignore.case = TRUE)
      if (!transient_error || attempt == max_tries) {
        if (grepl("timed out|timeout", msg, ignore.case = TRUE)) boe_abort(msg, "timeout", url = canonical, attempt = attempt)
        boe_abort(msg, "transport", url = canonical, attempt = attempt)
      }
      delay <- 0.5 * 2^(attempt - 1L)
      if (delay >= boe_time_left(deadline)) boe_abort("Retry delay exceeds the caller-owned absolute deadline.", "timeout", url = canonical)
      if (delay > 0) boe_sleep(delay)
      next
    }
    status <- httr2::resp_status(resp)
    if (status >= 200L && status < 300L && !is.null(validate_response)) {
      response_body <- httr2::resp_body_raw(resp)
      validation_error <- tryCatch({
        boe_run_response_validator(validate_response, response_body, canonical)
        NULL
      }, error = identity)
      if (!is.null(validation_error)) {
        if (attempt == max_tries) stop(validation_error)
        delay <- 0.5 * 2^(attempt - 1L)
        if (delay >= boe_time_left(deadline)) boe_abort("Retry delay exceeds the caller-owned absolute deadline.",
          "timeout", url = canonical)
        if (delay > 0) boe_sleep(delay)
        next
      }
    }
    if (!status %in% transient_status || attempt == max_tries) break
    delay <- boe_retry_after(httr2::resp_headers(resp), 0.5 * 2^(attempt - 1L))
    left <- boe_time_left(deadline)
    if (delay >= left) boe_abort("Retry-After exceeds the caller-owned absolute deadline.", "timeout", url = canonical)
    if (delay > 0) boe_sleep(delay)
  }
  final_url <- httr2::resp_url(resp)
  boe_validate_url(final_url)
  status <- httr2::resp_status(resp)
  if (status == 304L && allow_not_modified) {
    headers <- httr2::resp_headers(resp)
    return(list(url = canonical, final_url = final_url, status = 304L,
      etag = boe_header(headers, "etag"), last_modified = boe_header(headers, "last-modified"),
      retrieved_at = Sys.time(), not_modified = TRUE))
  }
  if (status == 429L) boe_abort(sprintf("The Bank of England rate-limited %s.", canonical), "rate_limit", url = canonical)
  if (status < 200L || status >= 300L) boe_abort(sprintf("Bank of England returned HTTP %d for %s.", status, canonical), "transport", status = status, url = canonical)
  body <- response_body %||% httr2::resp_body_raw(resp)
  if (length(body) > max_bytes) boe_abort("The response exceeded `max_bytes`.", "bounds", observed_bytes = length(body), max_bytes = max_bytes)
  headers <- httr2::resp_headers(resp)
  entry <- list(
    url = canonical,
    final_url = final_url,
    status = status,
    headers = headers,
    body = body,
    raw_sha256 = boe_sha256_raw(body),
    retrieved_at = Sys.time(),
    etag = boe_header(headers, "etag"),
    last_modified = boe_header(headers, "last-modified"),
    from_cache = FALSE
  )
  if (method == "POST") entry$request_identity <- request_identity
  if (cache) boe_cache_write(cache_dir, key, entry, max_cache_entries, max_cache_bytes)
  entry
}

boe_raw_text <- function(raw) {
  rawToChar(raw)
}
