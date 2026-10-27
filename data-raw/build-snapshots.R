args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) normalizePath(args[[1L]], mustWork = TRUE) else normalizePath(".", mustWork = TRUE)
cache_dir <- if (length(args) >= 2L) args[[2L]] else file.path(tempdir(), "readboe-build-cache")
workers <- as.integer(Sys.getenv("READBOE_CATALOGUE_WORKERS", "3"))
if (is.na(workers) || workers < 1L || workers > 8L) stop("READBOE_CATALOGUE_WORKERS must be 1 through 8")

for (file in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) sys.source(file, envir = globalenv())
dir.create(file.path(root, "inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
deadline <- boe_deadline(4 * 60 * 60)

request <- function(url, query = list(), max_bytes = 25 * 1024^2) {
  validator <- if (grepl("/boeapps/database/", url, fixed = TRUE)) boe_validate_not_error_html else NULL
  boe_request_raw(url, query = query, deadline = deadline, attempt_timeout = 45, max_bytes = max_bytes,
    cache = TRUE, cache_dir = cache_dir, ttl = 365 * 86400, max_cache_entries = 5000L,
    max_cache_bytes = 2 * 1024^3, max_tries = 5L, validate_response = validator)
}

receipt_row <- function(entry, ...) {
  data.frame(..., url = entry$url, final_url = entry$final_url %||% entry$url,
    retrieved_at = format(entry$retrieved_at, tz = "UTC", usetz = TRUE),
    raw_bytes = length(entry$body), raw_sha256 = entry$raw_sha256,
    etag = entry$etag, last_modified = entry$last_modified, status = entry$status,
    stringsAsFactors = FALSE, check.names = FALSE)
}

parse_search_page <- function(entry, partition, page) {
  doc <- xml2::read_html(boe_raw_text(entry$body))
  total_node <- xml2::xml_find_first(doc, "//input[@name='TotalNumResults']")
  total <- suppressWarnings(as.integer(xml2::xml_attr(total_node, "value")))
  rows <- xml2::xml_find_all(doc, "//tr[.//strong]")
  parts <- lapply(rows, function(row) {
    id <- toupper(trimws(xml2::xml_text(xml2::xml_find_first(row, ".//strong"))))
    cells <- xml2::xml_find_all(row, "./td")
    label <- if (length(cells) >= 2L) trimws(gsub("[[:space:]]+", " ", xml2::xml_text(cells[[length(cells)]]))) else NA_character_
    data.frame(series_id = id, search_label = label, partition = partition, search_page = page, stringsAsFactors = FALSE)
  })
  out <- if (length(parts)) do.call(rbind, parts) else data.frame(series_id = character(), search_label = character(), partition = character(), search_page = integer())
  out <- out[grepl("^[A-Z0-9._-]+$", out$series_id), , drop = FALSE]
  list(data = out, total = total)
}

search_url <- "https://www.bankofengland.co.uk/boeapps/database/fromshowcolumns.asp"
partitions <- c(not_seasonally_adjusted = "X,U", seasonally_adjusted = "C,D,Z,G,S,B,A,E")
frequency_values <- c("D", "W3", "W4", "M", "P", "Q", "B", "A", "F")
type_values <- c("M", "L", "V", "A", "B", "T,F", "N", "D", "W", "E", "2", "1", "Y", "Q",
  "3", "6", "I", "J", "0", "R", "5", "U", "P", "G", "9", "X", "4", "K", "Z")
currency_values <- c("A", "3", "B", "N", "CY", "1", "BA", "BB", "D", "M", "U", "BC", "E", "4",
  "C", "F", "X", "G", "7", "&", "BD", "IR", "9", "IS", "I", "Y", "BF", "BE", "@", "MR",
  "BG", "H", "2", "%", "8", "J,Q", "BH", "5", "RR", "R", "_", "BK", "BJ", "#", "SW", "6",
  "Z", "£", "T", "K", "S", "~", "TB", "TL", "$", "O")
# `_` is a wildcard in the legacy search form, not a literal identifier token.
identifier_suffix_values <- c(LETTERS, as.character(0:9), ".", "-")
# These are the complete three-character code prefixes observed in the official
# current Bankstats page, discontinued workbook, and first supported search
# window at the audit date. Cardinality against each unfiltered partition is a
# hard completeness assertion, so a new upstream prefix fails the build.
seed_prefix_values <- c("CFM", "CFQ", "CPM", "CPQ", "DPA", "DPM", "DPQ", "IUA", "IUD", "IUM", "IUQ",
  "LOQ", "LPM", "LPQ", "LPW", "OCA", "OCQ", "PQA", "PQM", "PQQ", "RPA", "RPM", "RPQ", "RPW",
  "VPB", "VPQ", "XUA", "XUD", "XUM", "XUQ", "YOA", "YWA", "YWD", "YWM", "YWQ", "YWW")
base_query_for <- function(partition, prefix = "", frequency = "any", data_type = "any", currency = "any") list(
  Cats = "", SearchType = "Advanced", Travel = "NIxASx",
  SearchText = if (nzchar(prefix)) paste0(prefix, "*") else "", SearchExclude = "",
  SearchTextFields = "TC", Cat1 = partitions[[partition]], Frequency = frequency, Cat8 = currency, Cat2 = data_type)

root_totals <- integer(length(partitions)); names(root_totals) <- names(partitions)
count_receipts <- list(); leaves <- list()

# Discover exact code families from every supported frequency window. The
# legacy search exposes only 16 pages per query, but the union of mutually
# exclusive frequencies is complete and supplies additional family prefixes
# before the exact family crawl below.
discovery_results <- list(); discovery_tasks <- list()
for (partition in names(partitions)) {
  for (frequency in frequency_values) {
    first_entry <- request(search_url, c(base_query_for(partition, frequency = frequency), list(ShadowPage = 1L)))
    first <- parse_search_page(first_entry, partition, 1L)
    if (is.na(first$total)) first$total <- 0L
    count_receipts[[length(count_receipts) + 1L]] <- receipt_row(first_entry,
      role = "frequency_prefix_discovery", partition = partition, prefix = "", frequency = frequency,
      data_type = "any", currency = "any", page = 1L)
    discovery_results[[length(discovery_results) + 1L]] <- list(entry = first_entry, parsed = first,
      partition = partition, frequency = frequency, page = 1L)
    pages <- min(16L, ceiling(first$total / 150L))
    if (pages > 1L) for (page in 2:pages) discovery_tasks[[length(discovery_tasks) + 1L]] <- list(
      partition = partition, frequency = frequency, page = page, total = first$total)
  }
}
discovery_extra <- parallel::mclapply(discovery_tasks, function(task) {
  entry <- request(search_url,
    c(base_query_for(task$partition, frequency = task$frequency), list(ShadowPage = task$page)))
  parsed <- parse_search_page(entry, task$partition, task$page)
  expected <- min(150L, task$total - (task$page - 1L) * 150L)
  if (!identical(parsed$total, task$total) || nrow(parsed$data) != expected || anyDuplicated(parsed$data$series_id)) {
    stop("Frequency prefix-discovery pagination failed: ", task$partition, "/", task$frequency, "/", task$page)
  }
  list(entry = entry, parsed = parsed, partition = task$partition, frequency = task$frequency, page = task$page)
}, mc.cores = workers, mc.preschedule = FALSE)
failed <- vapply(discovery_extra, inherits, logical(1), "try-error")
if (any(failed)) stop("Parallel frequency discovery failed: ",
  paste(unique(vapply(discovery_extra[failed], as.character, character(1))), collapse = " | "))
for (result in discovery_extra) {
  count_receipts[[length(count_receipts) + 1L]] <- receipt_row(result$entry,
    role = "frequency_prefix_discovery", partition = result$partition, prefix = "", frequency = result$frequency,
    data_type = "any", currency = "any", page = result$page)
  discovery_results[[length(discovery_results) + 1L]] <- result
}
discovered_ids <- unique(unlist(lapply(discovery_results, function(x) x$parsed$data$series_id), use.names = FALSE))
prefix_values <- sort(unique(c(seed_prefix_values, substr(discovered_ids[nchar(discovered_ids) >= 3L], 1L, 3L))))
if (!length(prefix_values)) stop("Official frequency windows exposed no series-code prefixes")
message("Discovered ", length(prefix_values), " exact series-code families: ", paste(prefix_values, collapse = ","))

if (FALSE) {
refine_prefix_leaf <- function(partition, prefix, frequency, expected_total, depth = 0L) {
  if (depth > 8L) stop("Series-code prefix refinement exceeded its hard depth bound: ", prefix)
  children <- list()
  child_total <- 0L
  for (suffix in identifier_suffix_values) {
    child_prefix <- paste0(prefix, suffix)
    entry <- request(search_url,
      c(base_query_for(partition, child_prefix, frequency), list(ShadowPage = 1L)))
    child <- parse_search_page(entry, partition, 1L)
    if (is.na(child$total)) child$total <- 0L
    count_receipts[[length(count_receipts) + 1L]] <<- receipt_row(entry,
      role = "series_code_prefix_count", partition = partition, prefix = child_prefix,
      frequency = frequency, data_type = "any", currency = "any", page = 1L)
    child_total <- child_total + child$total
    children[[length(children) + 1L]] <- list(prefix = child_prefix, entry = entry, child = child)
  }
  if (child_total != expected_total) {
    observed_children <- vapply(children, function(x) if (x$child$total > 0L) {
      paste0(x$prefix, "=", x$child$total)
    } else "", character(1))
    stop("Series-code prefix children do not exactly partition leaf: ", partition, "/", prefix, "/",
      frequency, " (", child_total, " of ", expected_total, "; ",
      paste(observed_children[nzchar(observed_children)], collapse = ", "), ")")
  }
  for (node in children) if (node$child$total > 0L) {
    if (node$child$total > 2400L) {
      refine_prefix_leaf(partition, node$prefix, frequency, node$child$total, depth + 1L)
    } else {
      leaves[[length(leaves) + 1L]] <<- list(partition = partition, prefix = node$prefix,
        frequency = frequency, data_type = "any", currency = "any", total = node$child$total,
        first_entry = node$entry, first_data = node$child$data)
    }
  }
  invisible(NULL)
}
for (partition in names(partitions)) {
  root_entry <- request(search_url, c(base_query_for(partition), list(ShadowPage = 1L)))
  root <- parse_search_page(root_entry, partition, 1L)
  if (is.na(root$total) || root$total < 1L) stop("Unfiltered search partition has no stable total: ", partition)
  root_totals[[partition]] <- root$total
  count_receipts[[length(count_receipts) + 1L]] <- receipt_row(root_entry,
    role = "partition_total", partition = partition, prefix = "", frequency = "any", data_type = "any",
    currency = "any", page = 1L)
  for (prefix in prefix_values) {
    prefix_entry <- request(search_url, c(base_query_for(partition, prefix), list(ShadowPage = 1L)))
    prefix_result <- parse_search_page(prefix_entry, partition, 1L)
    count_receipts[[length(count_receipts) + 1L]] <- receipt_row(prefix_entry,
      role = "prefix_count", partition = partition, prefix = prefix, frequency = "any", data_type = "any",
      currency = "any", page = 1L)
    if (is.na(prefix_result$total) || prefix_result$total == 0L) next
    if (prefix_result$total <= 2400L) {
      leaves[[length(leaves) + 1L]] <- list(partition = partition, prefix = prefix,
        frequency = "any", data_type = "any", currency = "any", total = prefix_result$total,
        first_entry = prefix_entry, first_data = prefix_result$data)
    } else {
      refine_prefix_leaf(partition, prefix, "any", prefix_result$total)
    }
  }
}

page_tasks <- list()
for (i in seq_along(leaves)) {
  leaf <- leaves[[i]]
  expected_first <- min(150L, leaf$total)
  if (nrow(leaf$first_data) != expected_first || anyDuplicated(leaf$first_data$series_id) ||
      any(!startsWith(leaf$first_data$series_id, leaf$prefix))) {
    stop("Search leaf first-page identity/count contract failed: ", leaf$partition, "/", leaf$prefix,
      "/", leaf$frequency)
  }
  pages <- ceiling(leaves[[i]]$total / 150L)
  if (pages > 1L) for (page in 2:pages) page_tasks[[length(page_tasks) + 1L]] <- list(leaf = i, page = page)
}
page_results <- parallel::mclapply(page_tasks, function(task) {
  leaf <- leaves[[task$leaf]]
  entry <- request(search_url,
    c(base_query_for(leaf$partition, leaf$prefix, leaf$frequency, leaf$data_type, leaf$currency),
      list(ShadowPage = task$page)))
  parsed <- parse_search_page(entry, leaf$partition, task$page)
  expected <- min(150L, leaf$total - (task$page - 1L) * 150L)
  if (!identical(parsed$total, leaf$total) || nrow(parsed$data) != expected ||
      anyDuplicated(parsed$data$series_id) || any(!startsWith(parsed$data$series_id, leaf$prefix))) {
    stop("Search leaf pagination contract failed: ", leaf$partition, "/", leaf$prefix, "/", leaf$frequency,
      "/", leaf$data_type, "/", leaf$currency, " page ", task$page)
  }
  list(entry = entry, data = parsed$data, leaf = task$leaf, page = task$page)
}, mc.cores = workers, mc.preschedule = FALSE)
failed <- vapply(page_results, inherits, logical(1), "try-error")
if (any(failed)) stop("Parallel search leaves failed: ", paste(unique(vapply(page_results[failed], as.character, character(1))), collapse = " | "))

for (i in seq_along(leaves)) {
  leaf_pages <- page_results[vapply(page_results, function(x) identical(x$leaf, i), logical(1))]
  leaf_ids <- c(leaves[[i]]$first_data$series_id,
    unlist(lapply(leaf_pages, function(x) x$data$series_id), use.names = FALSE))
  observed <- length(unique(leaf_ids))
  if (observed != leaves[[i]]$total) stop("Search leaf cross-page identity contract failed: ",
    leaves[[i]]$partition, "/", leaves[[i]]$prefix, "/", leaves[[i]]$frequency,
    " (", observed, " unique of ", leaves[[i]]$total, ")")
}

search_rows <- lapply(leaves, `[[`, "first_data")
page_rows <- lapply(page_results, `[[`, "data")
search_catalogue <- do.call(rbind, c(search_rows, page_rows))
rownames(search_catalogue) <- NULL
if (anyDuplicated(search_catalogue$series_id)) {
  split_rows <- split(search_catalogue, search_catalogue$series_id)
  search_catalogue <- do.call(rbind, lapply(split_rows, function(x) {
    observed_partitions <- paste(sort(unique(x$partition)), collapse = ";")
    x <- x[1L, , drop = FALSE]
    x$partition <- observed_partitions
    x$search_page <- NA_integer_
    x
  }))
  rownames(search_catalogue) <- NULL
}
for (partition in names(partitions)) {
  observed <- length(unique(search_catalogue$series_id[grepl(partition, search_catalogue$partition, fixed = TRUE)]))
  if (observed != root_totals[[partition]]) stop("Prefix search union is incomplete for ", partition, ": ", observed, " of ", root_totals[[partition]])
}
page_receipts <- lapply(page_results, function(x) {
  leaf <- leaves[[x$leaf]]
  receipt_row(x$entry, role = "leaf_page", partition = leaf$partition, prefix = leaf$prefix,
    frequency = leaf$frequency, data_type = leaf$data_type, currency = leaf$currency, page = x$page)
})
first_receipts <- lapply(leaves, function(leaf) receipt_row(leaf$first_entry,
  role = "leaf_page", partition = leaf$partition, prefix = leaf$prefix, frequency = leaf$frequency,
  data_type = leaf$data_type, currency = leaf$currency, page = 1L))
search_receipts <- do.call(rbind, c(count_receipts, first_receipts, page_receipts))
}

collapse_search_rows <- function(parts) {
  parts <- parts[vapply(parts, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
  if (!length(parts)) return(data.frame(series_id = character(), search_label = character(),
    partition = character(), search_page = integer(), stringsAsFactors = FALSE))
  out <- do.call(rbind, parts)
  out <- out[!duplicated(out$series_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

fetch_bounded_search <- function(first_entry, first, partition, frequency, data_type = "any", currency = "any") {
  if (first$total < 1L) return(first$data[0, , drop = FALSE])
  pages <- ceiling(first$total / 150L)
  if (pages > 16L) stop("Search leaf exceeds the official 16-page window: ", partition, "/",
    frequency, "/", data_type, "/", currency, " (", first$total, ")")
  expected_first <- min(150L, first$total)
  if (nrow(first$data) != expected_first || anyDuplicated(first$data$series_id)) {
    stop("Search leaf first-page contract failed: ", partition, "/", frequency, "/", data_type, "/", currency)
  }
  count_receipts[[length(count_receipts) + 1L]] <<- receipt_row(first_entry,
    role = "catalogue_leaf_page", partition = partition, prefix = "", frequency = frequency,
    data_type = data_type, currency = currency, page = 1L)
  extras <- list()
  if (pages > 1L) {
    tasks <- lapply(2:pages, function(page) list(page = page))
    extras <- parallel::mclapply(tasks, function(task) {
      entry <- request(search_url, c(base_query_for(partition, frequency = frequency,
        data_type = data_type, currency = currency), list(ShadowPage = task$page)))
      parsed <- parse_search_page(entry, partition, task$page)
      expected <- min(150L, first$total - (task$page - 1L) * 150L)
      if (!identical(parsed$total, first$total) || nrow(parsed$data) != expected ||
          anyDuplicated(parsed$data$series_id)) {
        stop("Search leaf pagination contract failed: ", partition, "/", frequency, "/",
          data_type, "/", currency, "/", task$page, " (returned total=", parsed$total,
          ", expected total=", first$total, ", returned rows=", nrow(parsed$data),
          ", expected rows=", expected, ")")
      }
      list(entry = entry, parsed = parsed, page = task$page)
    }, mc.cores = workers, mc.preschedule = FALSE)
    failed <- vapply(extras, inherits, logical(1), "try-error")
    if (any(failed)) stop("Parallel bounded search leaf failed: ",
      paste(unique(vapply(extras[failed], as.character, character(1))), collapse = " | "))
    for (extra in extras) count_receipts[[length(count_receipts) + 1L]] <<- receipt_row(extra$entry,
      role = "catalogue_leaf_page", partition = partition, prefix = "", frequency = frequency,
      data_type = data_type, currency = currency, page = extra$page)
  }
  out <- collapse_search_rows(c(list(first$data), lapply(extras, function(x) x$parsed$data)))
  if (nrow(out) != first$total) stop("Search leaf cross-page identity contract failed: ", partition, "/",
    frequency, "/", data_type, "/", currency, " (", nrow(out), " of ", first$total, ")")
  out
}

root_totals <- integer(length(partitions)); names(root_totals) <- names(partitions)
frequency_catalogues <- list()
for (partition in names(partitions)) {
  root_entry <- request(search_url, c(base_query_for(partition), list(ShadowPage = 1L)))
  root <- parse_search_page(root_entry, partition, 1L)
  if (is.na(root$total) || root$total < 1L) stop("Unfiltered search partition has no stable total: ", partition)
  root_totals[[partition]] <- root$total
  count_receipts[[length(count_receipts) + 1L]] <- receipt_row(root_entry,
    role = "partition_total", partition = partition, prefix = "", frequency = "any",
    data_type = "any", currency = "any", page = 1L)
  partition_parts <- list(); frequency_total <- 0L
  for (frequency in frequency_values) {
    selected <- discovery_results[vapply(discovery_results, function(x) identical(x$partition, partition) &&
      identical(x$frequency, frequency), logical(1))]
    first <- selected[[which(vapply(selected, function(x) identical(x$page, 1L), logical(1)))[[1L]]]]$parsed
    frequency_total <- frequency_total + first$total
    direct <- collapse_search_rows(lapply(selected, function(x) x$parsed$data))
    if (first$total <= 2400L) {
      if (nrow(direct) != first$total) stop("Bounded frequency window is incomplete: ", partition, "/", frequency)
      partition_parts[[length(partition_parts) + 1L]] <- direct
      next
    }
    combined <- list(direct)
    for (data_type in type_values) {
      type_entry <- request(search_url,
        c(base_query_for(partition, frequency = frequency, data_type = data_type), list(ShadowPage = 1L)))
      type_child <- parse_search_page(type_entry, partition, 1L)
      if (is.na(type_child$total)) type_child$total <- 0L
      count_receipts[[length(count_receipts) + 1L]] <- receipt_row(type_entry,
        role = "frequency_type_count", partition = partition, prefix = "", frequency = frequency,
        data_type = data_type, currency = "any", page = 1L)
      if (type_child$total == 0L) next
      if (type_child$total <= 2400L) {
        combined[[length(combined) + 1L]] <- fetch_bounded_search(type_entry, type_child,
          partition, frequency, data_type)
      } else {
        for (currency in currency_values) {
          currency_entry <- request(search_url, c(base_query_for(partition, frequency = frequency,
            data_type = data_type, currency = currency), list(ShadowPage = 1L)))
          currency_child <- parse_search_page(currency_entry, partition, 1L)
          if (is.na(currency_child$total)) currency_child$total <- 0L
          count_receipts[[length(count_receipts) + 1L]] <- receipt_row(currency_entry,
            role = "frequency_type_currency_count", partition = partition, prefix = "", frequency = frequency,
            data_type = data_type, currency = currency, page = 1L)
          if (currency_child$total > 0L) combined[[length(combined) + 1L]] <- fetch_bounded_search(
            currency_entry, currency_child, partition, frequency, data_type, currency)
        }
      }
    }
    complete <- collapse_search_rows(combined)
    if (nrow(complete) != first$total) stop("Type/currency search union is incomplete for ", partition,
      "/", frequency, ": ", nrow(complete), " of ", first$total)
    partition_parts[[length(partition_parts) + 1L]] <- complete
  }
  if (frequency_total != root$total) stop("Frequency totals do not exactly partition official root: ",
    partition, " (", frequency_total, " of ", root$total, ")")
  partition_catalogue <- collapse_search_rows(partition_parts)
  if (nrow(partition_catalogue) != root$total) stop("Frequency catalogue union is incomplete for ",
    partition, ": ", nrow(partition_catalogue), " of ", root$total)
  frequency_catalogues[[partition]] <- partition_catalogue
}
search_catalogue <- collapse_search_rows(frequency_catalogues)
if (nrow(search_catalogue) != sum(root_totals)) stop("Seasonal-adjustment catalogue union is incomplete")
search_receipts <- do.call(rbind, count_receipts)

tables_entry <- request("https://www.bankofengland.co.uk/statistics/tables", max_bytes = 5 * 1024^2)
tables_doc <- xml2::read_html(boe_raw_text(tables_entry$body))
series_links <- xml2::xml_find_all(tables_doc, "//a[contains(@href, 'EC=')]")
bankstats_links <- do.call(rbind, lapply(series_links, function(a) {
  href <- xml2::xml_attr(a, "href")
  code <- sub(".*[?&]EC=([^&]+).*", "\\1", href)
  heading <- xml2::xml_find_first(a, "preceding::h3[1]")
  heading_text <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(heading)))
  section_node <- xml2::xml_find_first(a, "preceding::h2[1]")
  section <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(section_node)))
  table_id <- trimws(sub("[[:space:]]+-.*$", "", heading_text))
  data.frame(series_id = toupper(code), table_id = table_id, table_label = heading_text, section = section,
    displayed_code = trimws(xml2::xml_text(a)), stringsAsFactors = FALSE)
}))
bankstats_links <- bankstats_links[grepl("^[A-Z0-9._-]+$", bankstats_links$series_id), , drop = FALSE]
if (nrow(bankstats_links) < 5000L) stop("Bankstats page exposed fewer than 5000 exact series links")
collapse_bankstats <- function(x) data.frame(series_id = x$series_id[[1L]],
  table_id = paste(sort(unique(x$table_id)), collapse = ";"),
  table_label = paste(sort(unique(x$table_label)), collapse = ";"),
  section = paste(sort(unique(x$section)), collapse = ";"), stringsAsFactors = FALSE)
bankstats_catalogue <- do.call(rbind, lapply(split(bankstats_links, bankstats_links$series_id), collapse_bankstats))
rownames(bankstats_catalogue) <- NULL

discontinued_entry <- request("https://www.bankofengland.co.uk/-/media/BoE/Files/statistics/discontinued-series", max_bytes = 10 * 1024^2)
discontinued_path <- boe_write_raw_temp(discontinued_entry$body, ".xlsx")
on.exit(unlink(discontinued_path), add = TRUE)
discontinued <- suppressMessages(readxl::read_excel(discontinued_path, sheet = "Discontinued Series", col_names = FALSE, .name_repair = "minimal"))
discontinued <- as.data.frame(discontinued, stringsAsFactors = FALSE)
if (ncol(discontinued) < 4L) stop("Discontinued workbook schema changed")
discontinued <- discontinued[-seq_len(3L), seq_len(4L), drop = FALSE]
names(discontinued) <- c("series_id", "latest_period_available", "replacement_series_id", "discontinued_note")
discontinued$series_id <- toupper(trimws(discontinued$series_id))
discontinued <- discontinued[grepl("^[A-Z0-9._-]+$", discontinued$series_id), , drop = FALSE]
if (anyDuplicated(discontinued$series_id) || nrow(discontinued) < 14000L) stop("Discontinued identity/count contract failed")

metadata_ids <- sort(unique(c(search_catalogue$series_id, bankstats_catalogue$series_id)))
batches <- split(metadata_ids, ceiling(seq_along(metadata_ids) / 300L))
metadata_results <- parallel::mclapply(seq_along(batches), function(i) {
  ids <- batches[[i]]
  query <- list(CodeVer = "new", `xml.x` = "yes", Datefrom = "03/Jun/2007", Dateto = "now",
    SeriesCodes = paste(ids, collapse = ","), VPD = "Y", VFD = "Y", Omit = "-G")
  entry <- request(.boe_iadb_url, query, max_bytes = 25 * 1024^2)
  parsed <- boe_parse_iadb_xml(entry$body, expected_ids = ids, max_observations = 1L)
  message("metadata batch ", i, "/", length(batches), ": ", nrow(parsed$metadata))
  list(metadata = parsed$metadata, entry = entry, ids = ids)
}, mc.cores = workers, mc.preschedule = FALSE)
metadata <- do.call(rbind, lapply(metadata_results, `[[`, "metadata"))
rownames(metadata) <- NULL
metadata_receipts <- do.call(rbind, lapply(seq_along(metadata_results), function(i) {
  result <- metadata_results[[i]]
  receipt_row(result$entry, batch = i, requested_series = length(result$ids), returned_series = nrow(result$metadata))
}))

all_ids <- sort(unique(c(metadata_ids, discontinued$series_id)))
catalogue <- data.frame(series_id = all_ids, stringsAsFactors = FALSE)
match_column <- function(table, column) table[[column]][match(catalogue$series_id, table$series_id)]
for (column in setdiff(names(metadata), "series_id")) catalogue[[column]] <- match_column(metadata, column)
search_label <- match_column(search_catalogue, "search_label")
catalogue$label <- ifelse(is.na(catalogue$label) | !nzchar(catalogue$label), search_label, catalogue$label)
catalogue$table_id <- match_column(bankstats_catalogue, "table_id")
catalogue$table_label <- match_column(bankstats_catalogue, "table_label")
catalogue$section <- match_column(bankstats_catalogue, "section")
for (column in setdiff(names(discontinued), "series_id")) catalogue[[column]] <- match_column(discontinued, column)
in_search <- catalogue$series_id %in% search_catalogue$series_id
in_bankstats <- catalogue$series_id %in% bankstats_catalogue$series_id
in_discontinued <- catalogue$series_id %in% discontinued$series_id
catalogue$status <- ifelse(in_bankstats, "bankstats_current", ifelse(in_discontinued, "discontinued", "iadb_search_unclassified"))
catalogue$metadata_status <- ifelse(catalogue$series_id %in% metadata$series_id, "available", "unavailable_discontinued_only")
catalogue$surface_membership <- vapply(seq_len(nrow(catalogue)), function(i) paste(c(
  if (in_search[[i]]) "iadb_advanced_search", if (in_bankstats[[i]]) "bankstats_current",
  if (in_discontinued[[i]]) "discontinued_workbook"), collapse = ";"), character(1))
catalogue <- catalogue[, c("series_id", "label", "status", "metadata_status", "frequency", "frequency_code",
  "frequency_order", "first_observation", "last_observation", "country", "concatenated_label", "definition",
  "definition_url", "category_name", "category_value", "category_label", "category_order", "series_comment",
  "table_id", "table_label", "section", "latest_period_available", "replacement_series_id", "discontinued_note",
  "surface_membership")]
catalogue <- catalogue[order(catalogue$series_id), , drop = FALSE]
rownames(catalogue) <- NULL
if (anyNA(catalogue$series_id) || anyDuplicated(catalogue$series_id)) stop("Final catalogue identity contract failed")

zip_entry <- request("https://www.bankofengland.co.uk/-/media/boe/files/statistics/bankstats-latest-tables.zip", max_bytes = 30 * 1024^2)
zip_path <- boe_write_raw_temp(zip_entry$body, ".zip")
on.exit(unlink(zip_path), add = TRUE)
zip_listing <- boe_safe_zip_members(zip_path, max_members = 100L, max_uncompressed_bytes = 100 * 1024^2)
workbook_members <- zip_listing$Name[grepl("[.]xls$", zip_listing$Name, ignore.case = TRUE)]
member_table_ids <- sub("^.*Tab(.*)[.]xls$", "\\1", workbook_members, ignore.case = TRUE)
bankstats_assets <- data.frame(
  table_id = paste(member_table_ids, collapse = ";"),
  table_label = "Bankstats (Monetary & Financial Statistics) - Latest Tables",
  section = paste(sort(unique(substr(member_table_ids, 1L, 1L))), collapse = ";"),
  url = zip_entry$url, file_type = "zip", archive = FALSE,
  member_count = length(workbook_members), member_names = paste(workbook_members, collapse = ";"),
  source_page_url = tables_entry$url, source_page_sha256 = tables_entry$raw_sha256,
  source_sha256 = zip_entry$raw_sha256, stringsAsFactors = FALSE)
if (length(workbook_members) != 64L) stop("Expected exactly 64 Bankstats workbook members")

sitemap_entry <- request("https://www.bankofengland.co.uk/sitemap/statistics", max_bytes = 20 * 1024^2)
sitemap_doc <- xml2::read_html(boe_raw_text(sitemap_entry$body))
asset_links <- xml2::xml_find_all(sitemap_doc, "//a[@href]")
asset_label <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(asset_links)))
asset_url <- xml2::xml_attr(asset_links, "href")
asset_url <- ifelse(startsWith(asset_url, "/"), paste0("https://www.bankofengland.co.uk", asset_url), asset_url)
is_file <- grepl("[.](xls|xlsx|xlsb|csv|zip)([?#]|$)", asset_url, ignore.case = TRUE)
release_assets <- data.frame(label = asset_label[is_file], url = asset_url[is_file], stringsAsFactors = FALSE)
release_assets <- release_assets[grepl("^https://www[.]bankofengland[.]co[.]uk/", release_assets$url, ignore.case = TRUE), , drop = FALSE]
classify <- function(url, label) {
  text <- tolower(paste(url, label))
  if (grepl("requested-data", text)) return("requested_data")
  if (grepl("gdp.*real.time|real.time.*gdp", text)) return("gdp_realtime")
  if (grepl("research.dataset", text)) return("research_datasets")
  if (grepl("payment|chaps|rtgs|settlement", text)) return("payments")
  if (grepl("gold.*custody", text)) return("gold_custody")
  if (grepl("banknote", text)) return("banknotes")
  if (grepl("international-reserves|liquidity-data", text)) return("international_reserves")
  if (grepl("mortgage-lenders-and-administrators|mlar", text)) return("mlar")
  if (grepl("capital-issuance", text)) return("capital_issuance")
  if (grepl("credit-union", text)) return("credit_unions")
  if (grepl("insurance", text) && grepl("quarter", text)) return("insurance_quarterly")
  if (grepl("insurance", text)) return("insurance_annual")
  if (grepl("external-claims|form-cc|country.*claims", text)) return("external_claims")
  if (grepl("external-business", text)) return("external_business")
  NA_character_
}
release_assets$family <- mapply(classify, release_assets$url, release_assets$label, USE.NAMES = FALSE)
release_assets <- release_assets[!is.na(release_assets$family), , drop = FALSE]
release_assets$file_type <- tolower(tools::file_ext(sub("[?#].*$", "", release_assets$url)))
release_assets$parser <- ifelse(release_assets$file_type %in% c("xls", "xlsx"), "workbook_or_raw", "raw_verified")
release_assets$source_sitemap_url <- sitemap_entry$url
release_assets$source_sitemap_sha256 <- sitemap_entry$raw_sha256
release_assets <- release_assets[!duplicated(release_assets$url), c("family", "label", "url", "file_type", "parser", "source_sitemap_url", "source_sitemap_sha256")]
release_assets <- release_assets[order(release_assets$family, release_assets$label, release_assets$url), , drop = FALSE]
rownames(release_assets) <- NULL
missing_release_families <- setdiff(boe_release_registry()$family, unique(release_assets$family))
if (length(missing_release_families)) stop("Statistics sitemap exposed no identity-stable assets for audited families: ",
  paste(missing_release_families, collapse = ", "))

extdata <- file.path(root, "inst", "extdata")
saveRDS(catalogue, file.path(extdata, "iadb-catalogue.rds"), version = 3)
saveRDS(search_receipts, file.path(extdata, "iadb-catalogue-search-receipts.rds"), version = 3)
saveRDS(metadata_receipts, file.path(extdata, "iadb-catalogue-metadata-receipts.rds"), version = 3)
saveRDS(bankstats_assets, file.path(extdata, "bankstats-assets.rds"), version = 3)
saveRDS(release_assets, file.path(extdata, "release-assets.rds"), version = 3)
release_family_counts <- as.data.frame(table(release_assets$family), stringsAsFactors = FALSE)
names(release_family_counts) <- c("family", "asset_rows")
utils::write.csv(release_family_counts, file.path(extdata, "release-family-counts.csv"),
  row.names = FALSE, na = "")

manifest <- data.frame(
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  catalogue_rows = nrow(catalogue), iadb_search_rows = nrow(search_catalogue),
  iadb_not_seasonally_adjusted_rows = unname(root_totals[["not_seasonally_adjusted"]]),
  iadb_seasonally_adjusted_rows = unname(root_totals[["seasonally_adjusted"]]),
  bankstats_series_rows = nrow(bankstats_catalogue), bankstats_link_rows = nrow(bankstats_links),
  bankstats_only_rows = sum(bankstats_catalogue$series_id %in% setdiff(bankstats_catalogue$series_id, search_catalogue$series_id)),
  discontinued_rows = nrow(discontinued), search_discontinued_overlap = sum(search_catalogue$series_id %in% discontinued$series_id),
  union_discontinued_overlap = sum(metadata_ids %in% discontinued$series_id), metadata_rows = nrow(metadata),
  metadata_unavailable_rows = sum(catalogue$metadata_status != "available"), search_page_rows = nrow(search_receipts),
  metadata_request_rows = nrow(metadata_receipts), bankstats_workbook_members = length(workbook_members),
  release_asset_rows = nrow(release_assets),
  search_source_url = search_url, bankstats_page_url = tables_entry$url,
  discontinued_source_url = discontinued_entry$url, bankstats_bulk_url = zip_entry$url,
  statistics_sitemap_url = sitemap_entry$url, bankstats_page_sha256 = tables_entry$raw_sha256,
  discontinued_sha256 = discontinued_entry$raw_sha256, bankstats_bulk_sha256 = zip_entry$raw_sha256,
  statistics_sitemap_sha256 = sitemap_entry$raw_sha256,
  catalogue_snapshot_sha256 = boe_sha256_file(file.path(extdata, "iadb-catalogue.rds")),
  search_receipts_sha256 = boe_sha256_file(file.path(extdata, "iadb-catalogue-search-receipts.rds")),
  metadata_receipts_sha256 = boe_sha256_file(file.path(extdata, "iadb-catalogue-metadata-receipts.rds")),
  bankstats_assets_sha256 = boe_sha256_file(file.path(extdata, "bankstats-assets.rds")),
  release_assets_sha256 = boe_sha256_file(file.path(extdata, "release-assets.rds")),
  release_family_counts_sha256 = boe_sha256_file(file.path(extdata, "release-family-counts.csv")),
  stringsAsFactors = FALSE)
saveRDS(manifest, file.path(extdata, "iadb-catalogue-manifest.rds"), version = 3)
utils::write.csv(manifest, file.path(extdata, "iadb-catalogue-manifest.csv"), row.names = FALSE, na = "")

cat("Final catalogue:", nrow(catalogue), "\n")
print(manifest)
print(table(catalogue$status, useNA = "ifany"))
print(table(catalogue$metadata_status, useNA = "ifany"))
print(table(release_assets$family))
