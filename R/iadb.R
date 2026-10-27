.boe_iadb_url <- "https://www.bankofengland.co.uk/boeapps/database/_iadb-fromshowcolumns.asp"

boe_format_date <- function(x, name) {
  if (!inherits(x, "Date")) x <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
  if (length(x) != 1L || is.na(x)) boe_abort(sprintf("`%s` must be a valid date.", name), "config")
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  paste0(format(x, "%d"), "/", months[as.integer(format(x, "%m"))], "/", format(x, "%Y"))
}

boe_parse_number <- function(x) {
  out <- suppressWarnings(as.numeric(gsub(",", "", x, fixed = TRUE)))
  out[x %in% c("..", "", NA_character_)] <- NA_real_
  out
}

boe_parse_iadb_csv_dates <- function(x) {
  x <- trimws(x)
  matched <- regexec("^([0-9]{1,2})[[:space:]]+([A-Za-z]{3})[[:space:]]+([0-9]{4})$", x)
  pieces <- regmatches(x, matched)
  months <- stats::setNames(seq_len(12L), c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  out <- rep(as.Date(NA), length(x))
  for (i in seq_along(pieces)) {
    if (length(pieces[[i]]) != 4L) next
    month <- unname(months[[pieces[[i]][[3L]]]])
    day <- suppressWarnings(as.integer(pieces[[i]][[2L]]))
    year <- suppressWarnings(as.integer(pieces[[i]][[4L]]))
    if (is.null(month) || is.na(day) || is.na(year)) next
    out[[i]] <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", year, month, day)))
  }
  out
}

boe_xml_attr <- function(node, name) {
  out <- xml2::xml_attr(node, name)
  ifelse(is.na(out), NA_character_, out)
}

boe_xml_children_by_attr <- function(node, attr) {
  kids <- xml2::xml_children(node)
  kids[!is.na(xml2::xml_attr(kids, attr))]
}

boe_parse_iadb_xml <- function(body, expected_ids = NULL, max_observations = 2e6, max_series = 300L) {
  boe_check_positive(max_observations, "max_observations", integer = TRUE)
  doc <- tryCatch(xml2::read_xml(body, encoding = "ISO-8859-1"), error = function(e) {
    boe_abort(paste("Could not parse IADB XML:", conditionMessage(e)), "parse")
  })
  series_nodes <- xml2::xml_find_all(doc, "//*[local-name()='Cube'][@SCODE]")
  if (!length(series_nodes)) {
    text <- substr(xml2::xml_text(doc), 1L, 500L)
    boe_abort(paste("IADB XML contains no series nodes.", text), "schema_drift")
  }
  if (length(series_nodes) > max_series) boe_abort("IADB XML exceeded the series bound.", "bounds")
  ids <- toupper(boe_xml_attr(series_nodes, "SCODE"))
  if (anyNA(ids) || anyDuplicated(ids)) boe_abort("IADB XML contains missing or duplicate series identities.", "identity_mismatch")
  if (!is.null(expected_ids)) {
    expected_ids <- unique(boe_validate_ids(expected_ids, max_series))
    missing <- setdiff(expected_ids, ids)
    extra <- setdiff(ids, expected_ids)
    if (length(missing) || length(extra)) {
      boe_abort("Returned IADB series identities do not exactly match the request.", "identity_mismatch", missing = missing, extra = extra)
    }
  }
  metadata <- vector("list", length(series_nodes))
  observations <- vector("list", length(series_nodes))
  for (i in seq_along(series_nodes)) {
    node <- series_nodes[[i]]
    freq <- boe_xml_children_by_attr(node, "FREQ_CODE")
    span <- boe_xml_children_by_attr(node, "FIRST_OBS")
    defs <- boe_xml_children_by_attr(node, "SERIES_DEF")
    cats <- boe_xml_children_by_attr(node, "CAT_NAME")
    comments <- xml2::xml_find_all(node, "./*[local-name()='SERIES_COM']")
    collapse_attr <- function(nodes, attr) paste(boe_xml_attr(nodes, attr), collapse = ";")
    metadata[[i]] <- data.frame(
      series_id = ids[[i]],
      label = boe_xml_attr(node, "DESC"),
      country = boe_xml_attr(node, "COUNTRY"),
      concatenated_label = boe_xml_attr(node, "CONCAT"),
      frequency = if (length(freq)) boe_xml_attr(freq[[1L]], "FREQ_NAME") else NA_character_,
      frequency_code = if (length(freq)) boe_xml_attr(freq[[1L]], "FREQ_CODE") else NA_character_,
      frequency_order = if (length(freq)) suppressWarnings(as.integer(boe_xml_attr(freq[[1L]], "FREQ_ORD"))) else NA_integer_,
      first_observation = if (length(span)) boe_xml_attr(span[[1L]], "FIRST_OBS") else NA_character_,
      last_observation = if (length(span)) boe_xml_attr(span[[1L]], "LAST_OBS") else NA_character_,
      definition = if (length(defs)) collapse_attr(defs, "SERIES_DEF") else NA_character_,
      definition_url = if (length(defs)) collapse_attr(defs, "DEF_LOC") else NA_character_,
      category_name = if (length(cats)) collapse_attr(cats, "CAT_NAME") else NA_character_,
      category_value = if (length(cats)) collapse_attr(cats, "CAT_VAL") else NA_character_,
      category_label = if (length(cats)) collapse_attr(cats, "VAL_DESC") else NA_character_,
      category_order = if (length(cats)) collapse_attr(cats, "CAT_ORD") else NA_character_,
      series_comment = if (length(comments)) paste(xml2::xml_text(comments), collapse = "\n") else NA_character_,
      stringsAsFactors = FALSE
    )
    obs <- xml2::xml_find_all(node, "./*[local-name()='Cube'][@TIME]")
    if (!length(obs)) {
      observations[[i]] <- NULL
      next
    }
    raw_value <- boe_xml_attr(obs, "OBS_VALUE")
    raw_provisional <- boe_xml_attr(obs, "PROV_VALUE")
    foot <- vapply(obs, function(x) {
      y <- xml2::xml_find_all(x, "./*[local-name()='OBS_COM']")
      if (length(y)) paste(xml2::xml_text(y), collapse = "\n") else NA_character_
    }, character(1))
    observations[[i]] <- data.frame(
      series_id = ids[[i]],
      date = as.Date(boe_xml_attr(obs, "TIME")),
      value_raw = raw_value,
      value = boe_parse_number(raw_value),
      provisional_value_raw = raw_provisional,
      provisional_value = boe_parse_number(raw_provisional),
      confidential = boe_xml_attr(obs, "OBS_CONF"),
      last_updated = boe_xml_attr(obs, "LAST_UPDATED"),
      last_revised = boe_xml_attr(obs, "LAST_REVISED"),
      observation_comment = foot,
      source_row = seq_along(obs),
      stringsAsFactors = FALSE
    )
  }
  metadata <- do.call(rbind, metadata)
  observations <- observations[!vapply(observations, is.null, logical(1))]
  observations <- if (length(observations)) do.call(rbind, observations) else data.frame(
    series_id = character(), date = as.Date(character()), value_raw = character(), value = numeric(),
    provisional_value_raw = character(), provisional_value = numeric(), confidential = character(),
    last_updated = character(), last_revised = character(), observation_comment = character(), source_row = integer()
  )
  rownames(metadata) <- rownames(observations) <- NULL
  if (nrow(observations) > max_observations) boe_abort("IADB XML exceeded the observation-row bound.", "bounds")
  if (nrow(observations) && anyNA(observations$date)) boe_abort("IADB XML contains an invalid observation date.", "schema_drift")
  structure(list(metadata = metadata, observations = observations), class = "boe_iadb")
}

boe_parse_iadb_csv <- function(body, expected_ids, max_observations = 2e6) {
  text <- boe_raw_text(body)
  if (grepl("<html|<title>Error", text, ignore.case = TRUE)) boe_abort("IADB returned HTML instead of CSV.", "schema_drift")
  raw <- tryCatch(utils::read.csv(text = text, check.names = FALSE, stringsAsFactors = FALSE, na.strings = character()),
    error = function(e) boe_abort(paste("Could not parse IADB CSV:", conditionMessage(e)), "parse"))
  if (ncol(raw) < 2L) boe_abort("IADB CSV does not contain a date and series column.", "schema_drift")
  ids <- toupper(trimws(names(raw)[-1L]))
  expected_ids <- unique(boe_validate_ids(expected_ids))
  if (!setequal(ids, expected_ids) || anyDuplicated(ids)) {
    boe_abort("IADB CSV series identities do not exactly match the request.", "identity_mismatch", returned = ids, expected = expected_ids)
  }
  dates <- boe_parse_iadb_csv_dates(raw[[1L]])
  if (anyNA(dates)) boe_abort("IADB CSV contains an invalid date.", "schema_drift")
  out <- do.call(rbind, lapply(seq_along(ids), function(i) {
    value_raw <- trimws(as.character(raw[[i + 1L]]))
    data.frame(series_id = ids[[i]], date = dates, value_raw = value_raw,
      value = boe_parse_number(value_raw), suppressed = value_raw == "..", source_row = seq_along(dates),
      stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  if (nrow(out) > max_observations) boe_abort("IADB CSV exceeded the observation-row bound.", "bounds")
  out
}

#' Read observations and metadata from the IADB
#'
#' Retrieves exact series identifiers from the Bank of England Interactive
#' Statistical Database (IADB).  XML is the canonical format and retains
#' metadata, footnotes, revision fields, provisional values, and confidentiality
#' markers.  CSV is a compatibility format with a deliberately smaller schema.
#' Dates are inclusive and identifiers are trimmed, upper-cased, and checked
#' against the returned identity set.
#'
#' @param series_id One or more non-empty official IADB identifiers, up to 300
#'   per request.  Input is trimmed and upper-cased before the request; the
#'   returned identity set must still match the request exactly.
#' @param from Inclusive start date, coercible to one valid `Date`.  The default
#'   is `"1960-01-01"`.
#' @param to Inclusive end date, coercible to one valid `Date`.  The default is
#'   `Sys.Date()` at call time.
#' @param format Character choice: `"xml"` (default, canonical) or `"csv"`
#'   (compatibility).
#' @param provisional Logical; request and retain provisional values in XML
#'   responses (default `TRUE`).  CSV has no separate provisional columns.
#' @param footnotes Logical; request XML observation footnotes (default
#'   `TRUE`).  This argument has no CSV effect.
#' @param vud Character choice `"all"` (default) or `"updated"`; VUD filtering
#'   is available only for XML.
#' @param updated_since Optional explicit date or the string `"latest"` from
#'   which XML updates are selected.  Supplying it for CSV is a configuration
#'   error.
#' @param deadline A caller-owned absolute deadline, one `POSIXct`, positive
#'   number of seconds, or `NULL` for 60 seconds.  It covers retries and parse
#'   work.
#' @param max_bytes Positive response byte bound (default 25 MiB).
#' @param max_observations Positive integer bound on parsed observation rows
#'   (default 2e6).
#' @param cache Whether to read and write the managed cache (default `TRUE`).
#' @param cache_only If `TRUE`, require an exact cache entry and never contact
#'   the network; a miss signals `readboe_cache_miss`.
#' @param refresh If `TRUE`, bypass a cached response.  It cannot be combined
#'   with `cache_only = TRUE`.
#' @param cache_dir Cache directory; defaults to
#'   `tools::R_user_dir("readboe", "cache")`.
#' @return For XML, a list with class `boe_iadb` and exactly two elements:
#'   `metadata` and `observations`, plus a `boe_provenance` attribute.
#'   `metadata` is a data frame with columns `series_id`, `label`, `country`,
#'   `concatenated_label`, `frequency`, `frequency_code`, `frequency_order`,
#'   `first_observation`, `last_observation`, `definition`, `definition_url`,
#'   `category_name`, `category_value`, `category_label`, `category_order`,
#'   and `series_comment`.  `observations` has `series_id`, `date` (`Date`),
#'   `value_raw`, `value`, `provisional_value_raw`, `provisional_value`,
#'   `confidential`, `last_updated`, `last_revised`,
#'   `observation_comment`, and `source_row`.  Raw markers and duplicate rows
#'   are retained.  For CSV, a plain data frame with `series_id`, `date`,
#'   `value_raw`, `value`, `suppressed`, and `source_row` is returned with the
#'   same provenance attribute; XML-only metadata, footnotes, revisions,
#'   provisional fields, and VUD filtering are unavailable.
#' @details Requests use the canonical allowlisted HTTPS endpoint, bounded
#'   retries, redirects, response bytes, observations, and a caller-owned
#'   deadline.  Temporary request state is not written outside the managed
#'   cache.  Invalid dates/identifiers/options, identity mismatch, schema drift,
#'   malformed XML/CSV, transport/rate-limit/timeout failures, cache failures,
#'   and resource-limit breaches signal typed `readboe_*` conditions.  HTML
#'   error pages are rejected and never cached.
#' @seealso [boe_series_metadata()], [boe_series_updates()],
#'   [boe_provenance()], [boe_deadline()], [boe_rate_series()]
#' @examples
#' ## A reversed range is validated locally and therefore works offline.
#' bad <- tryCatch(
#'   read_boe_series("IUDBEDR", from = "2024-02-01", to = "2024-01-01"),
#'   error = function(error) error
#' )
#' stopifnot(inherits(bad, "readboe_config"))
#' ## Set READBOE_LIVE_EXAMPLES=true for a bounded official request.
#' if (identical(Sys.getenv("READBOE_LIVE_EXAMPLES"), "true")) {
#'   x <- read_boe_series("IUDBEDR", from = "2024-01-01", to = "2024-01-02",
#'     deadline = boe_deadline(15), cache = TRUE)
#'   stopifnot(inherits(x, "boe_iadb"), identical(names(x),
#'     c("metadata", "observations")))
#' }
#' @export
read_boe_series <- function(series_id, from = "1960-01-01", to = Sys.Date(),
                            format = c("xml", "csv"), provisional = TRUE, footnotes = TRUE,
                            vud = c("all", "updated"), updated_since = NULL,
                            deadline = NULL, max_bytes = 25 * 1024^2, max_observations = 2e6,
                            cache = TRUE, cache_only = FALSE, refresh = FALSE,
                            cache_dir = boe_default_cache_dir()) {
  ids <- boe_validate_ids(series_id)
  format <- match.arg(format)
  vud <- match.arg(vud)
  from_text <- boe_format_date(from, "from")
  to_text <- boe_format_date(to, "to")
  if (as.Date(from) > as.Date(to)) boe_abort("`from` must not be later than `to`.", "config")
  query <- list(
    Datefrom = from_text, Dateto = to_text, SeriesCodes = paste(ids, collapse = ","),
    VPD = if (provisional) "Y" else "N"
  )
  if (format == "xml") {
    query <- c(list(CodeVer = "new", `xml.x` = "yes"), query, list(VFD = if (footnotes) "Y" else "N"))
    if (!is.null(updated_since) || identical(vud, "updated")) {
      query$VUD <- if (vud == "all") "A" else "B"
      if (!is.null(updated_since)) query$VUDdate <- if (identical(updated_since, "latest")) "latest" else boe_format_date(updated_since, "updated_since")
    }
  } else {
    if (!is.null(updated_since) || identical(vud, "updated")) boe_abort("VUD revision filtering is available only from canonical XML.", "config")
    query <- c(list(`csv.x` = "yes"), query, list(UsingCodes = "Y", CSVF = "TN"))
  }
  entry <- boe_request_raw(.boe_iadb_url, query, deadline = deadline, max_bytes = max_bytes,
    cache = cache, cache_only = cache_only, refresh = refresh, cache_dir = cache_dir,
    validate_response = boe_validate_not_error_html)
  receipt <- boe_receipt_from_entry(entry)
  out <- if (format == "xml") boe_parse_iadb_xml(entry$body, ids, max_observations) else boe_parse_iadb_csv(entry$body, ids, max_observations)
  boe_attach_provenance(out, receipt, list(surface = "iadb", format = format, series_id = ids,
    from = as.character(as.Date(from)), to = as.character(as.Date(to)), vud = vud, updated_since = updated_since),
    transforms = c("XML/CSV syntax parsed", "numeric companion columns added without changing raw values"))
}

#' Read only IADB metadata for exact series identifiers
#'
#' Requests the canonical IADB XML metadata surface without retrieving
#' observations.  The fixed date window used by the endpoint is sufficient for
#' current metadata and is not a caller-facing observation filter.
#'
#' @param series_id One or more non-empty official identifiers, up to 300;
#'   values are trimmed, upper-cased, and identity-checked.
#' @param deadline A caller-owned absolute deadline, `POSIXct`, positive number
#'   of seconds, or `NULL` for 60 seconds.
#' @param max_bytes Positive response byte bound (default 10 MiB).
#' @param cache Whether to read and write the managed cache (default `TRUE`).
#' @param cache_only If `TRUE`, require a matching cache entry and never use
#'   the network.
#' @param refresh If `TRUE`, bypass a cached response.
#' @param cache_dir Cache directory; defaults to
#'   `tools::R_user_dir("readboe", "cache")`.
#' @return A data frame with exactly the metadata columns `series_id`, `label`,
#'   `country`, `concatenated_label`, `frequency`, `frequency_code`,
#'   `frequency_order`, `first_observation`, `last_observation`, `definition`,
#'   `definition_url`, `category_name`, `category_value`, `category_label`,
#'   `category_order`, and `series_comment`, plus a `boe_provenance` attribute.
#'   No observation rows are returned.
#' @details The request is canonical XML over HTTPS with a 3 June 2007 to
#'   `now` metadata window.  Bounds, timeout, cache, identity, schema, and
#'   transport failures signal typed `readboe_*` conditions.  Successful
#'   responses alone enter the managed cache.
#' @seealso [read_boe_series()], [boe_series_updates()], [boe_provenance()]
#' @examples
#' bad <- tryCatch(boe_series_metadata(character()), error = function(error) error)
#' stopifnot(inherits(bad, "readboe_config"))
#' if (identical(Sys.getenv("READBOE_LIVE_EXAMPLES"), "true")) {
#'   metadata <- boe_series_metadata("IUDBEDR", deadline = boe_deadline(15))
#'   stopifnot(is.data.frame(metadata),
#'     all(c("series_id", "label", "frequency") %in% names(metadata)),
#'     !is.null(boe_provenance(metadata)))
#' }
#' @export
boe_series_metadata <- function(series_id, deadline = NULL, max_bytes = 10 * 1024^2,
                                cache = TRUE, cache_only = FALSE, refresh = FALSE,
                                cache_dir = boe_default_cache_dir()) {
  ids <- boe_validate_ids(series_id)
  query <- list(CodeVer = "new", `xml.x` = "yes", Datefrom = "03/Jun/2007", Dateto = "now",
    SeriesCodes = paste(ids, collapse = ","), VPD = "Y", VFD = "Y", Omit = "-G")
  entry <- boe_request_raw(.boe_iadb_url, query, deadline = deadline, max_bytes = max_bytes,
    cache = cache, cache_only = cache_only, refresh = refresh, cache_dir = cache_dir,
    validate_response = boe_validate_not_error_html)
  parsed <- boe_parse_iadb_xml(entry$body, ids, max_observations = 1L)
  boe_attach_provenance(parsed$metadata, boe_receipt_from_entry(entry),
    list(surface = "iadb_metadata", series_id = ids), transforms = "XML syntax parsed; no observation data requested")
}

#' Read IADB new or revised observations
#'
#' Convenience wrapper for [read_boe_series()] with `format = "xml"` and
#' `vud = "updated"`.  It preserves the canonical metadata and observation
#' schema while asking the IADB for new or revised observations in an inclusive
#' date range.
#'
#' @param series_id One or more non-empty official identifiers, up to 300.
#' @param from Required inclusive start date.
#' @param to Required inclusive end date.
#' @param updated_since Explicit update date or `"latest"` (the default).
#'   Dates before the IADB's 3 June 2007 VUD baseline are accepted by the
#'   endpoint but may return no updates.
#' @param ... Additional arguments passed to [read_boe_series()].  The wrapper
#'   fixes `format = "xml"`, `vud = "updated"`, and the supplied
#'   `series_id`/date arguments; passing colliding names in `...` is an error.
#' @return A class `boe_iadb` list with `metadata` and `observations`, using
#'   the same 16 metadata and 11 observation columns and `boe_provenance`
#'   attribute as the XML branch of [read_boe_series()].  `observations`
#'   contains only rows selected by the update filter.
#' @details Network, cache, deadline, bounds, identity, schema, and transport
#'   semantics are inherited from [read_boe_series()].  The wrapper has no
#'   side effects beyond the managed cache.  CSV is not used because VUD is an
#'   XML-only capability.
#' @seealso [read_boe_series()], [boe_series_metadata()], [boe_provenance()]
#' @examples
#' bad <- tryCatch(
#'   boe_series_updates("IUDBEDR", from = "2024-02-01", to = "2024-01-01"),
#'   error = function(error) error
#' )
#' stopifnot(inherits(bad, "readboe_config"))
#' if (identical(Sys.getenv("READBOE_LIVE_EXAMPLES"), "true")) {
#'   updates <- boe_series_updates("IUDBEDR", from = "2024-01-01",
#'     to = "2024-01-02", cache = TRUE, deadline = boe_deadline(15))
#'   stopifnot(inherits(updates, "boe_iadb"))
#' }
#' @export
boe_series_updates <- function(series_id, from, to, updated_since = "latest", ...) {
  read_boe_series(series_id, from = from, to = to, format = "xml", vud = "updated", updated_since = updated_since, ...)
}

#' Print a compact summary of a canonical IADB result
#'
#' @param x An object with class `boe_iadb`, normally returned by the XML
#'   branch of [read_boe_series()] or [boe_series_updates()].
#' @param ... Unused additional arguments, accepted for compatibility with
#'   [`print`][base::print].
#' @return The input `x`, invisibly.  The method writes one line to standard
#'   output in the form `"<boe_iadb> <series> series, <observations>
#'   observations"`; it does not alter `x`.
#' @details Printing performs no network, cache, or filesystem operations.  A
#'   malformed object lacking data-frame `metadata` or `observations` elements
#'   may fail when their row counts are computed.
#' @seealso [read_boe_series()], [boe_series_updates()]
#' @examples
#' x <- structure(list(metadata = data.frame(series_id = "IUDBEDR"),
#'   observations = data.frame(series_id = character())), class = "boe_iadb")
#' printed <- capture.output(result <- print(x))
#' stopifnot(identical(printed, "<boe_iadb> 1 series, 0 observations"),
#'           identical(result, x))
#' @method print boe_iadb
#' @export
print.boe_iadb <- function(x, ...) {
  cat(sprintf("<boe_iadb> %d series, %d observations\n", nrow(x$metadata), nrow(x$observations)))
  invisible(x)
}
