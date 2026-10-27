.boe_mpr_sitemaps <- c(
  "https://www.bankofengland.co.uk/sitemap/inflation-report",
  "https://www.bankofengland.co.uk/sitemap/monetary-policy-report"
)
.boe_mpr_archives <- c(
  "https://www.bankofengland.co.uk/inflation-report/inflation-reports",
  "https://www.bankofengland.co.uk/monetary-policy-report/monetary-policy-report"
)

boe_mpr_html <- function(raw) tryCatch(xml2::read_html(raw), error = function(e)
  boe_abort("The official forecast archive HTML is invalid.", "schema_drift"))

boe_mpr_json <- function(raw) {
  parsed <- tryCatch(httr2::resp_body_json(httr2::response(200L,
    headers = list(`content-type` = "application/json"), body = raw)), error = function(e) NULL)
  if (!is.list(parsed) || !is.character(parsed$Results) || length(parsed$Results) != 1L) {
    boe_abort("The official forecast search response has no Results HTML.", "schema_drift")
  }
  parsed
}

boe_mpr_is_release_url <- function(url) {
  grepl("^https://www[.]bankofengland[.]co[.]uk/(inflation-report|monetary-policy-report)/[0-9]{4}/(january|february|march|april|may|june|july|aug|august|september|october|november|december)-[0-9]{4}$", url) |
    grepl("^https://www[.]bankofengland[.]co[.]uk/report/[0-9]{4}/monetary-policy-report[^/]+-[0-9]{4}$", url)
}

boe_mpr_month_key <- function(text) {
  # Month names are matched explicitly, without changing LC_TIME or assuming a
  # quarterly calendar. Abbreviated official archive labels are also accepted.
  text <- tolower(text)
  month <- paste0("(", paste(c(tolower(month.name), tolower(month.abb)), collapse = "|"), ")")
  hits <- regmatches(text, gregexpr(paste0(month, "[[:space:]_-]+[0-9]{4}"), text, perl = TRUE))[[1L]]
  if (!length(hits)) return(NA_character_)
  keys <- vapply(hits, function(hit) {
    parts <- strsplit(hit, "[[:space:]_-]+")[[1L]]
    m <- match(substr(parts[[1L]], 1L, 3L), tolower(month.abb))
    sprintf("%s-%02d", parts[[2L]], m)
  }, character(1L))
  keys <- unique(keys)
  if (length(keys) == 1L) keys else NA_character_
}

boe_mpr_iso_date <- function(text) {
  text <- trimws(gsub("[[:space:]]+", " ", text))
  match <- regmatches(text, regexpr("[0-9]{1,2} [A-Za-z]+ [0-9]{4}", text))
  if (!length(match) || !nzchar(match)) return(as.Date(NA))
  key <- boe_mpr_month_key(sub("^[0-9]+ ", "", match))
  if (is.na(key)) return(as.Date(NA))
  as.Date(paste0(key, "-", sprintf("%02d", as.integer(sub(" .*", "", match)))))
}

boe_mpr_links <- function(doc, base) {
  nodes <- xml2::xml_find_all(doc, "//a[@href]")
  href <- xml2::xml_attr(nodes, "href")
  url <- xml2::url_absolute(href, base)
  url <- sub("#.*$", "", url)
  data.frame(url = url, label = trimws(gsub("[[:space:]]+", " ", xml2::xml_text(nodes))), stringsAsFactors = FALSE)
}

boe_mpr_config_string <- function(text, key) {
  expr <- paste0("CP[.]BOE[.]", key, "[[:space:]]*=[[:space:]]*\"([^\"]+)\"")
  m <- regexec(expr, text, perl = TRUE)
  found <- regmatches(text, m)[[1L]]
  if (length(found) == 2L) found[[2L]] else NA_character_
}

boe_mpr_config_array <- function(text, key) {
  expr <- paste0("CP[.]BOE[.]", key, "[[:space:]]*=[[:space:]]*\\[([^]]*)\\]")
  found <- regmatches(text, regexec(expr, text, perl = TRUE))[[1L]]
  if (length(found) != 2L) return(NA_character_)
  values <- regmatches(found[[2L]], gregexpr('"[^" ]+"', found[[2L]]))[[1L]]
  if (length(values) != 1L) return(NA_character_)
  gsub('"', "", values, fixed = TRUE)
}

boe_mpr_archive_search <- function(url, deadline, cache, cache_only, refresh, cache_dir, max_pages) {
  entry <- boe_request_raw(url, deadline = deadline, max_bytes = 5 * 1024^2,
    cache = cache, cache_only = cache_only, refresh = refresh, cache_dir = cache_dir)
  html <- rawToChar(entry$body)
  news <- !is.na(boe_mpr_config_string(html, "NewsPageDataSourceID"))
  id <- boe_mpr_config_string(html, if (news) "NewsPageDataSourceID" else "ContentPageSearchDataSourceID")
  types <- boe_mpr_config_array(html, if (news) "NewsTypes" else "ContentPageTypes")
  taxonomy <- boe_mpr_config_array(html, if (news) "NewsTaxonomies" else "ContentPageTaxonomies")
  if (anyNA(c(id, types, taxonomy))) boe_abort("Official forecast archive search configuration changed.", "schema_drift")
  endpoint <- paste0("https://www.bankofengland.co.uk/_api/", if (news) "News/RefreshPagedNewsList" else "Search/RefreshContentPageSearchList")
  records <- list(); receipts <- list(boe_receipt_from_entry(entry))
  seen <- character()
  for (page in seq_len(max_pages)) {
    boe_time_left(deadline)
    form <- list(Id = id, Page = as.character(page), PageSize = "200", OrderBy = "4")
    form[[if (news) "NewsTypesAvailable[]" else "ReleaseTypesAvailable[]"]] <- types
    form[["Taxonomies[]"]] <- taxonomy
    if (news) { form$Grid <- "false"; form$InfiniteScrolling <- "false" }
    raw <- boe_request_raw(endpoint, deadline = deadline, max_bytes = 10 * 1024^2,
      cache = cache, cache_only = cache_only, refresh = refresh, cache_dir = cache_dir,
      method = "POST", form = form, validate_response = function(body, url) { boe_mpr_json(body); TRUE })
    receipts[[length(receipts) + 1L]] <- c(boe_receipt_from_entry(raw), list(request_identity = raw$request_identity))
    response <- boe_mpr_json(raw$body)
    doc <- boe_mpr_html(charToRaw(response$Results))
    links <- xml2::xml_find_all(doc, "//a[contains(concat(' ',normalize-space(@class),' '),' release ')]")
    urls <- xml2::url_absolute(xml2::xml_attr(links, "href"), url)
    if (any(urls %in% seen)) boe_abort("Official forecast archive pagination repeated an entry.", "schema_drift")
    seen <- c(seen, urls)
    keep <- boe_mpr_is_release_url(urls)
    for (i in which(keep)) {
      date <- xml2::xml_attr(xml2::xml_find_first(links[[i]], ".//time[@datetime]"), "datetime")
      records[[length(records) + 1L]] <- data.frame(landing_url = urls[[i]],
        release_date = as.character(as.Date(date)), date_source = endpoint, stringsAsFactors = FALSE)
    }
    total <- suppressWarnings(as.integer(response$NumResults %||% NA_character_))
    if ((!is.na(total) && length(seen) >= total) || length(links) < 200L) break
    if (page == max_pages) boe_abort("The forecast archive exceeds `max_pages`.", "bounds")
  }
  list(records = if (length(records)) do.call(rbind, records) else data.frame(
    landing_url = character(), release_date = character(), date_source = character()), receipts = receipts)
}

#' Discover the full official forecast-report archive
#'
#' Discovers Inflation Reports, Monetary Policy Reports and joint MPR/FSR
#' editions from the official sitemaps and their live archive search interfaces.
#' Future scheduled placeholders are distinct from published editions. No edition
#' allowlist or assumed February/May/August/November calendar is used.
#' @param include_upcoming Include scheduled future editions in the inventory.
#' @param as_of Publication-date cutoff; defaults to the caller's current date.
#' @param max_pages,max_releases Hard discovery bounds; never silent truncation.
#' @inheritParams boe_download_asset
#' @return A data frame with release keys, exact landing URLs, publication dates,
#'   date sources and publication status. Archive receipts are attached.
#' @keywords internal
boe_mpr_discover_live <- function(include_upcoming = FALSE, as_of = Sys.Date(), deadline = NULL,
    max_pages = 10L, max_releases = 1000L, cache = TRUE, cache_only = FALSE,
    refresh = FALSE, cache_dir = boe_default_cache_dir()) {
  deadline <- boe_as_deadline(deadline)
  boe_time_left(deadline)
  boe_assert_scalar(include_upcoming, "include_upcoming", "logical")
  if (!inherits(as_of, "Date") || length(as_of) != 1L || is.na(as_of)) boe_abort("`as_of` must be one Date.", "config")
  boe_check_positive(max_pages, "max_pages", integer = TRUE)
  boe_check_positive(max_releases, "max_releases", integer = TRUE)
  found <- list(); receipts <- list()
  for (url in .boe_mpr_sitemaps) {
    raw <- boe_request_raw(url, deadline = deadline, max_bytes = 10 * 1024^2,
      cache = cache, cache_only = cache_only, refresh = refresh, cache_dir = cache_dir)
    receipts[[length(receipts) + 1L]] <- boe_receipt_from_entry(raw)
    links <- boe_mpr_links(boe_mpr_html(raw$body), url)
    found[[length(found) + 1L]] <- links[boe_mpr_is_release_url(links$url), , drop = FALSE]
  }
  candidates <- unique(do.call(rbind, found))
  candidates <- candidates[!duplicated(candidates$url), , drop = FALSE]
  if (!nrow(candidates) || nrow(candidates) > max_releases) boe_abort("Official forecast release count is empty or over budget.", "bounds")
  dates <- list()
  for (url in .boe_mpr_archives) {
    search <- boe_mpr_archive_search(url, deadline, cache, cache_only, refresh, cache_dir, max_pages)
    dates[[length(dates) + 1L]] <- search$records
    receipts <- c(receipts, search$receipts)
  }
  dates <- do.call(rbind, dates)
  if (anyDuplicated(dates$landing_url)) boe_abort("Official archive report dates are ambiguous.", "schema_drift")
  # Date-search-only reports are retained too, including newly published editions
  # which may appear before the sitemap refreshes.
  all_urls <- unique(c(candidates$url, dates$landing_url))
  if (length(all_urls) > max_releases) boe_abort("Official releases exceed `max_releases`.", "bounds")
  out <- data.frame(landing_url = all_urls, label = candidates$label[match(all_urls, candidates$url)],
    release_date = dates$release_date[match(all_urls, dates$landing_url)],
    date_source = dates$date_source[match(all_urls, dates$landing_url)], stringsAsFactors = FALSE)
  for (i in which(is.na(out$release_date))) {
    raw <- boe_request_raw(out$landing_url[[i]], deadline = deadline, max_bytes = 20 * 1024^2,
      cache = cache, cache_only = cache_only, refresh = refresh, cache_dir = cache_dir)
    receipts[[length(receipts) + 1L]] <- boe_receipt_from_entry(raw)
    doc <- boe_mpr_html(raw$body)
    nodes <- xml2::xml_find_all(doc, "//*[contains(concat(' ',normalize-space(@class),' '),' published-date ')]")
    parsed <- unique(na.omit(as.Date(vapply(xml2::xml_text(nodes), function(x) as.character(boe_mpr_iso_date(x)), character(1L)))))
    if (length(parsed) != 1L) boe_abort("An official report has no unambiguous publication date.", "schema_drift", url = out$landing_url[[i]])
    out$release_date[[i]] <- as.character(parsed)
    out$date_source[[i]] <- out$landing_url[[i]]
  }
  out$release_date <- as.Date(out$release_date)
  out$source_published_date <- out$release_date
  out$release <- vapply(out$landing_url, boe_mpr_month_key, character(1L))
  if (anyNA(out$release) || anyDuplicated(out$release)) boe_abort("Official forecast edition identities are ambiguous.", "schema_drift")
  out$family <- ifelse(grepl("/inflation-report/", out$landing_url, fixed = TRUE), "inflation_report", "monetary_policy_report")
  out$publication_status <- ifelse(out$release_date > as_of, "scheduled", "published")
  out$release_date_precision <- "day"
  # Legacy archive cohorts use the first of every publication month as a
  # placeholder. Such a cohort is conservatively month precision, not four
  # independently established exact publication days. Preserve the raw date.
  for (year in unique(substr(out$release, 1L, 4L))) {
    cohort <- which(substr(out$release, 1L, 4L) == year)
    if (length(cohort) >= 3L && all(format(out$release_date[cohort], "%d") == "01")) {
      out$release_date_precision[cohort] <- "month"
      out$release_date[cohort] <- as.Date(NA)
    }
  }
  out$release_period_start <- as.Date(paste0(out$release, "-01"))
  out <- out[order(out$source_published_date), c("release", "release_date", "release_date_precision",
    "release_period_start", "source_published_date", "family", "publication_status", "landing_url", "date_source", "label")]
  rownames(out) <- NULL
  attr(out, "boe_provenance") <- list(source_identity = list(surface = "forecast_report_archive"),
    receipts = receipts, as_of = as_of, candidates = nrow(out),
    transforms = "Union of official sitemap and archive-search identities; scheduled editions retained separately")
  if (!include_upcoming) out <- out[out$publication_status == "published", , drop = FALSE]
  boe_time_left(deadline)
  out
}
