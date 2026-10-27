boe_mpr_col <- function(x) vapply(strsplit(sub("[0-9]+$", "", x), ""), function(letters) {
  value <- 0
  for (letter in letters) value <- value * 26 + match(letter, LETTERS)
  value
}, numeric(1L))

boe_mpr_load_workbook <- function(raw, extension, options) {
  path <- boe_write_raw_temp(raw, paste0(".", extension)); on.exit(unlink(path), add = TRUE)
  if (extension == "xls") {
    wb <- boe_read_workbook_raw(raw, ".xls", options$max_sheets, options$max_rows,
      options$max_columns, options$max_cells, deadline = options$deadline, col_types = "text")
    return(wb)
  }
  listing <- boe_safe_zip_members(path, 1000L, options$max_uncompressed_bytes)
  ns <- c(m = "http://schemas.openxmlformats.org/spreadsheetml/2006/main",
    r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
  wb <- boe_mpr_zip_xml(path, "xl/workbook.xml", listing)
  tabs <- xml2::xml_attr(xml2::xml_find_all(wb, ".//m:sheets/m:sheet", ns), "name")
  dims <- boe_mpr_preflight(path, data.frame(source_sheet = tabs), tabs,
    options$deadline, options$max_uncompressed_bytes, options$max_sheets,
    options$max_rows, options$max_columns, options$max_cells, retain_xml = TRUE)
  strings <- character()
  if ("xl/sharedStrings.xml" %in% listing$Name) {
    strings <- xml2::xml_text(xml2::xml_find_all(boe_mpr_zip_xml(path, "xl/sharedStrings.xml", listing), ".//m:si", ns))
  }
  out <- stats::setNames(vector("list", length(tabs)), tabs)
  for (i in seq_along(tabs)) {
    boe_time_left(options$deadline)
    doc <- attr(dims, "xml_parts")[[tabs[[i]]]]
    cells <- xml2::xml_find_all(doc, ".//m:sheetData/m:row/m:c", ns)
    refs <- xml2::xml_attr(cells, "r")
    type <- xml2::xml_attr(cells, "t")
    value <- xml2::xml_text(xml2::xml_find_first(cells, "./m:v", ns))
    shared <- which(!is.na(type) & type == "s")
    index <- suppressWarnings(as.integer(value[shared])) + 1L
    if (anyNA(index) || any(index < 1L | index > length(strings))) boe_abort("Workbook shared-string identities changed.", "schema_drift")
    value[shared] <- strings[index]
    inline <- which(!is.na(type) & type == "inlineStr")
    value[inline] <- xml2::xml_text(xml2::xml_find_first(cells[inline], "./m:is", ns))
    value[is.na(value) | value == ""] <- NA_character_
    matrix <- matrix(NA_character_, dims$rows[[i]], dims$columns[[i]])
    if (length(refs)) matrix[cbind(as.integer(sub("^[A-Z]+", "", refs)), boe_mpr_col(refs))] <- value
    out[[i]] <- as.data.frame(matrix, stringsAsFactors = FALSE)
  }
  structure(out, class = c("boe_workbook", "list"))
}

boe_mpr_slug <- function(x) {
  x <- iconv(enc2utf8(tolower(x)), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

boe_mpr_probability_slug <- function(x) {
  x <- gsub("<=", " less than or equal to ", x, fixed = TRUE)
  x <- gsub(">=", " greater than or equal to ", x, fixed = TRUE)
  x <- gsub("<", " less than ", x, fixed = TRUE)
  x <- gsub(">", " greater than ", x, fixed = TRUE)
  boe_mpr_slug(x)
}

boe_mpr_measure <- function(title) {
  key <- tolower(trimws(sub("^[0-9]+[.] *", "", title)))
  key <- gsub("[\u2010\u2011\u2012\u2013\u2014]", "-", key)
  aliases <- c("cpi inflation" = "cpi_inflation", "cpi forecast" = "cpi_inflation",
    "rpix inflation" = "rpix_inflation", "rpix forecast" = "rpix_inflation",
    "gdp growth" = "gdp_growth", "uk gdp" = "gdp_growth", "gdp level" = "gdp_level",
    "unemployment" = "unemployment", "unemployment rate (q4)" = "unemployment",
    "unemployment rate (annual)" = "unemployment", "bank rate" = "bank_rate",
    "excess supply/excess demand" = "output_gap", "average weekly earnings (awe)" = "average_weekly_earnings",
    "average hours" = "average_weekly_hours_worked",
    "world gdp - ppp weighted" = "world_gdp_ppp", "net trade contribution to gdp growth" = "net_trade_contribution",
    "world gdp (ppp-weighted)" = "world_gdp_ppp", "world gdp (uk-weighted)" = "world_gdp",
    "energy prices \u2013 direct contribution to cpi inflation" = "energy_cpi_contribution",
    "energy prices - direct contribution to cpi inflation" = "energy_cpi_contribution",
    "labour productivity (output per worker)" = "labour_productivity",
    "average weekly earnings (private sector regular pay)" = "average_weekly_earnings",
    "unit wage costs (private sector regular pay based)" = "unit_wage_costs",
    "uk-weighted world export prices" = "world_export_prices")
  if (key %in% names(aliases)) return(unname(aliases[[key]]))
  if (grepl("energy.*contribution.*cpi|energy.*cpi.*contribution", key)) return("energy_cpi_contribution")
  if (grepl("net.trade.*contribution|contribution.*net.trade", key)) return("net_trade_contribution")
  if (grepl("contribution", key)) return(boe_mpr_slug(key))
  if (grepl("^employment \\(", key)) return("employment")
  if (grepl("^working age population", key)) return("working_age_population")
  if (grepl("^participation rate", key)) return("participation_rate")
  if (grepl("cpi.*(inflation|projection|forecast)", key)) return("cpi_inflation")
  if (grepl("rpix.*(inflation|projection|forecast)", key)) return("rpix_inflation")
  if (grepl("gdp.*(growth|forecast|projection)", key) && !grepl("level", key)) return("gdp_growth")
  if (grepl("unemployment", key)) return("unemployment")
  boe_mpr_slug(key)
}

boe_mpr_statistic <- function(measure, frequency, title, definition, units = "") {
  text <- tolower(paste(title, definition, units))
  contribution <- measure %in% c("net_trade_contribution", "energy_cpi_contribution") || grepl("contribution", text)
  growth <- grepl("growth|percentage change|inflation|four.quarter|annual change", text)
  if (frequency == "Q") return(if (contribution) "contribution" else if (growth) "four_quarter_change" else "level")
  if (grepl("q4", text)) return(if (contribution) "q4_contribution" else if (growth) "q4_four_quarter_change" else "q4_level")
  if (contribution) return("annual_contribution")
  if (growth) return("annual_change")
  "annual_average"
}

boe_mpr_scenario <- function(label, release, measure) {
  key <- tolower(trimws(label)); vintage <- boe_mpr_month_key(label)
  kind <- "scenario_projection"; scenario <- boe_mpr_slug(sub("[[:space:]]*scenario[[:space:]]*", "", key))
  if (grepl("scenario [a-z0-9]+$", key)) scenario <- paste0("scenario_", sub(".*scenario ", "", key))
  if (grepl("^(memo: *)?central( indicative)? projection$", key)) {
    scenario <- if (startsWith(key, "memo")) "central_memo" else "central"
    kind <- if (measure %in% c("cpi_inflation", "gdp_growth", "unemployment", "rpix_inflation")) "central_forecast" else "indicative_profile"
  } else if (!is.na(vintage) && vintage != release && grepl("central|indicative|baseline", key)) {
    month <- tolower(month.name[as.integer(substr(vintage, 6L, 7L))])
    scenario <- paste0(month, if (substr(vintage, 1L, 4L) != substr(release, 1L, 4L)) paste0("_", substr(vintage, 1L, 4L)) else "", "_comparator")
    kind <- "comparator"
  } else if (grepl("optimal policy|taylor|policy.rule", key)) kind <- "policy_simulation"
  else if (grepl("conditioning|market.path|market rate", key)) { scenario <- "market"; kind <- "conditioning_assumption" }
  if (is.na(vintage)) vintage <- release
  list(scenario = scenario, scenario_label = label, forecast_vintage = vintage, kind = kind)
}

boe_mpr_source_meta <- function(rel, source_rel, url, member, sheet, layout, title, units, definition) {
  list(release = rel$release, release_date = rel$release_date,
    release_date_precision = rel$release_date_precision, source_edition = source_rel$release,
    source_publication_date = source_rel$release_date, source_url = url, source_member = member,
    source_sheet = sheet, layout = layout, title = title, units = units, definition = definition)
}

boe_mpr_path <- function(meta, periods, values, rows, columns, measure, scenario, statistic,
    frequency, kind, forecast_vintage = meta$release, scenario_label = scenario,
    source_vintage = forecast_vintage, source_vintage_label = source_vintage,
    include_distributions = FALSE, probability_event = NULL) {
  if (!length(periods)) return(data.frame())
  parsed <- boe_mpr_periods(periods, frequency, statistic)
  vals <- boe_mpr_values(values, allow_censored = kind == "forecast_probability", units = meta$units)
  n <- length(periods)
  out <- as.data.frame(lapply(meta, function(value) rep(value, n)), stringsAsFactors = FALSE)
  out$measure <- measure; out$scenario <- scenario; out$scenario_label <- scenario_label
  out$statistic <- statistic; out$frequency <- frequency; out$kind <- kind
  out$forecast_vintage <- forecast_vintage; out$source_vintage <- source_vintage; out$source_vintage_label <- source_vintage_label
  out$series_id <- paste("mpr", meta$release, measure, scenario, statistic, frequency, kind, sep = ":")
  out <- cbind(out, parsed, as.data.frame(vals, stringsAsFactors = FALSE), source_row = rows, source_column = columns)
  if (!is.null(probability_event)) out$probability_event <- probability_event
  out
}

boe_mpr_matrix <- function(cells, meta, alias = NULL) {
  header <- which(!is.na(cells[, 1L]) & trimws(cells[, 1L]) == "Date of publication")
  if (!length(header)) return(data.frame())
  if (length(header) != 1L) boe_abort("Forecast publication headers are ambiguous.", "schema_drift")
  cols <- which(!is.na(cells[header, ]) & grepl("^([0-9]{4})( ?Q[1-4])?$", cells[header, ]))
  if (!length(cols)) boe_abort("Forecast target-period headers are absent.", "schema_drift")
  vintages <- boe_mpr_vintages(cells[, 1L])
  if (anyDuplicated(vintages[!is.na(vintages)])) boe_abort("Forecast vintage rows are duplicate.", "schema_drift")
  key <- meta$release
  row <- which(!is.na(vintages) & vintages == key)
  if (!length(row) && !is.null(alias) && meta$release == meta$source_edition) {
    key <- alias; row <- which(!is.na(vintages) & vintages == key)
  }
  if (!length(row)) return(data.frame())
  values <- cells[row, cols]
  last <- which(!is.na(values) & nzchar(trimws(values)))
  if (!length(last)) return(data.frame())
  # Empty columns beyond an older row's published horizon are structural, not
  # observations. Internal blank cells and explicit terminal NA markers remain.
  cols <- cols[seq_len(max(last))]
  measure <- boe_mpr_measure(meta$title)
  frequency <- if (any(grepl("Q", cells[header, cols]))) "Q" else "A"
  statistic <- boe_mpr_statistic(measure, frequency, meta$title, meta$definition, meta$units)
  kind <- if (measure == "bank_rate") "conditioning_assumption" else if (
    measure %in% c("cpi_inflation", "gdp_growth", "unemployment", "rpix_inflation") && frequency == "Q") "central_forecast" else "indicative_profile"
  boe_mpr_path(meta, cells[header, cols], cells[row, cols], row, cols, measure,
    if (kind == "conditioning_assumption") "market" else "central", statistic, frequency, kind,
    scenario_label = if (kind == "conditioning_assumption") "Market conditioning path" else "Central projection",
    source_vintage = key, source_vintage_label = cells[row, 1L])
}

boe_mpr_scenario_columns <- function(cells, meta) {
  header <- which(!is.na(cells[, 1L]) & trimws(cells[, 1L]) == "Date")
  if (!length(header)) return(data.frame())
  if (length(header) != 1L) boe_abort("Scenario date headers are ambiguous.", "schema_drift")
  rows <- which(seq_len(nrow(cells)) > header & !is.na(cells[, 1L]) & grepl("^[0-9]{4}( ?Q[1-4])?$", cells[, 1L]))
  if (!length(rows)) return(data.frame())
  cols <- which(seq_len(ncol(cells)) > 1L & !is.na(cells[header, ]) & nzchar(cells[header, ]))
  if (!length(cols) || anyDuplicated(cells[header, cols])) boe_abort("Scenario labels are absent or duplicate.", "schema_drift")
  if (!any(grepl("scenario|central|indicative|projection|baseline|memo|market.*path",
      cells[header, cols], ignore.case = TRUE))) return(data.frame())
  unknown_dates <- which(seq_len(nrow(cells)) > header & !is.na(cells[, 1L]) & nzchar(trimws(cells[, 1L])) & !seq_len(nrow(cells)) %in% rows)
  if (length(unknown_dates)) boe_abort("Scenario target periods changed schema.", "schema_drift")
  measure <- boe_mpr_measure(meta$title)
  frequency <- if (any(grepl("Q", cells[rows, 1L]))) "Q" else "A"
  statistic <- boe_mpr_statistic(measure, frequency, meta$title, meta$definition, meta$units)
  boe_mpr_bind(lapply(cols, function(col) {
    s <- boe_mpr_scenario(cells[header, col], meta$release, measure)
    boe_mpr_path(meta, cells[rows, 1L], cells[rows, col], rows, col, measure,
      s$scenario, statistic, frequency, s$kind, s$forecast_vintage, s$scenario_label,
      source_vintage = s$forecast_vintage, source_vintage_label = s$scenario_label)
  }))
}

boe_mpr_fan_rows <- function(cells, meta, include_distributions) {
  headers <- which(apply(cells, 1L, function(x) sum(grepl("^[0-9]{4} ?Q[1-4]$", x), na.rm = TRUE) >= 2L))
  if (!length(headers)) return(data.frame())
  header <- headers[[1L]]
  cols <- which(grepl("^[0-9]{4} ?Q[1-4]$", cells[header, ]))
  labels <- tolower(trimws(cells[, 1L])); measure <- boe_mpr_measure(meta$title)
  if (!measure %in% c("cpi_inflation", "rpix_inflation", "gdp_growth", "unemployment")) return(data.frame())
  rows <- which(labels %in% c("mode", "median", "mean", "four-quarter gdp growth projection", "four-quarter cpi inflation projection", "16+ unemployment rate projection"))
  if (include_distributions) rows <- unique(c(rows, which(labels %in% c("uncertainty", "skewness")), which(grepl("^pr[.]? *\\{", labels))))
  if (!length(rows)) return(data.frame())
  boe_mpr_bind(lapply(rows, function(row) {
    header <- max(headers[headers < row])
    cols <- which(grepl("^[0-9]{4} ?Q[1-4]$", cells[header, ]))
    section <- which(seq_len(nrow(cells)) < row & !is.na(cells[, 1L]) &
      grepl("projections? based|forecast.*based", cells[, 1L], ignore.case = TRUE))
    title <- if (length(section)) cells[max(section), 1L] else meta$title
    scenario <- if (grepl("constant", title, ignore.case = TRUE)) "constant_rate" else if (
      grepl("market.*(rate|expectation)", title, ignore.case = TRUE)) "market_conditioned" else "central"
    label <- labels[[row]]; probability <- grepl("^pr", label)
    kind <- if (label %in% c("mode", "four-quarter gdp growth projection", "four-quarter cpi inflation projection", "16+ unemployment rate projection")) "central_forecast" else if (probability) "forecast_probability" else "distribution_statistic"
    if (kind == "central_forecast" && scenario == "constant_rate") kind <- "conditional_forecast"
    stat <- if (probability) paste0("probability_", boe_mpr_probability_slug(label)) else if (label %in% c("mode", "median", "mean", "uncertainty", "skewness")) label else "published_point"
    m <- meta; m$title <- title; m$units <- if (probability) "fraction" else if (stat == "skewness") "Dimensionless" else "Per cent"
    boe_mpr_path(m, cells[header, cols], cells[row, cols], row, cols, measure,
      scenario, stat, "Q", kind, scenario_label = title,
      probability_event = if (probability) cells[row, 1L] else NULL)
  }))
}

boe_mpr_reconcile_vintage <- function(workbook, rel) {
  # A source anomaly is reconciled only by an independent named central memo,
  # never by picking the closest date. The terminal matrix row must match every
  # overlapping forward value of that edition's memo for at least two measures.
  agreements <- character()
  for (sheet in names(workbook)) {
    x <- as.matrix(workbook[[sheet]])
    if (nrow(x) < 6L || ncol(x) < 3L) next
    h <- which(x[, 1L] == "Date" & !is.na(x[, 1L]))
    if (length(h) != 1L) next
    memo <- which(!is.na(x[h, ]) & grepl("^Memo: central projection$", x[h, ], ignore.case = TRUE))
    if (length(memo) != 1L) next
    measure <- boe_mpr_measure(x[1L, 1L])
    if (!measure %in% c("cpi_inflation", "gdp_growth", "unemployment")) next
    for (other in names(workbook)) {
      y <- as.matrix(workbook[[other]])
      if (nrow(y) < 6L || ncol(y) < 3L || is.na(y[1L, 1L]) || boe_mpr_measure(y[1L, 1L]) != measure) next
      hdr <- which(!is.na(y[, 1L]) & y[, 1L] == "Date of publication")
      if (length(hdr) != 1L) next
      vintages <- boe_mpr_vintages(y[, 1L]); valid <- which(!is.na(vintages))
      valid <- valid[vapply(valid, function(r) any(!is.na(y[r, -1L])), logical(1L))]
      if (!length(valid) || rel$release %in% vintages[valid]) next
      terminal <- tail(valid, 1L); key <- vintages[[terminal]]
      quarters <- which(!is.na(y[hdr, ]) & grepl("^[0-9]{4} ?Q[1-4]$", y[hdr, ]))
      matched <- match(gsub(" ", "", y[hdr, quarters]), gsub(" ", "", x[, 1L]))
      future <- substr(y[hdr, quarters], 1L, 4L) >= substr(rel$release, 1L, 4L)
      keep <- !is.na(matched) & future & !is.na(y[terminal, quarters])
      if (sum(keep) < 8L) next
      a <- suppressWarnings(as.numeric(y[terminal, quarters[keep]])); b <- suppressWarnings(as.numeric(x[matched[keep], memo]))
      if (!anyNA(c(a, b)) && all(abs(a - b) < 1e-9)) agreements[measure] <- key
    }
  }
  if (length(agreements) >= 2L && length(unique(agreements)) == 1L) unique(agreements) else NULL
}

boe_mpr_parse_workbook <- function(workbook, rel, source_rel, url, member, include_distributions, options) {
  parts <- list(); inventory <- list(); alias <- boe_mpr_reconcile_vintage(workbook, source_rel)
  for (sheet in names(workbook)) {
    boe_time_left(options$deadline)
    cells <- as.matrix(workbook[[sheet]])
    if (!length(cells)) next
    title <- cells[1L, 1L]; if (is.na(title)) title <- trimws(sub("^[0-9]+[.] *", "", sheet))
    units <- if (nrow(cells) >= 2L && !is.na(cells[2L, 1L])) cells[2L, 1L] else "Per cent"
    definition <- if (nrow(cells) >= 3L && !is.na(cells[3L, 1L])) cells[3L, 1L] else ""
    layout <- if (any(cells[, 1L] == "Date of publication", na.rm = TRUE)) "vintage_matrix" else if (any(cells[, 1L] == "Date", na.rm = TRUE)) "scenario_columns" else "other"
    distribution_sheet <- grepl("distribution", sheet, ignore.case = TRUE)
    fan_workbook <- grepl("current fan|probability.distribution", paste(member, url), ignore.case = TRUE)
    excluded <- grepl("short.term|policy.rule|other assumptions|conditioning assumptions", sheet, ignore.case = TRUE)
    if (distribution_sheet || fan_workbook) layout <- "fan_scalar_rows"
    if (excluded) layout <- "supporting_material"
    meta <- boe_mpr_source_meta(rel, source_rel, url, member, sheet, layout, title, units, definition)
    same_edition <- rel$release == source_rel$release
    data <- if (layout == "vintage_matrix") boe_mpr_matrix(cells, meta, alias) else if (layout == "scenario_columns" && same_edition) boe_mpr_scenario_columns(cells, meta) else data.frame()
    if (!nrow(data) && grepl("market profiles", member, ignore.case = TRUE)) data <- boe_mpr_market_profiles(cells, meta)
    if (!nrow(data) && same_edition && grepl("projected level", member, ignore.case = TRUE)) data <- boe_mpr_gdp_level_columns(cells, meta)
    if (!nrow(data) && grepl("parameters for MPC.*projection", member, ignore.case = TRUE)) data <- boe_mpr_parameter_groups(cells, meta, include_distributions)
    if (!nrow(data) && layout == "fan_scalar_rows" && same_edition &&
        (include_distributions || !distribution_sheet)) {
      meta$layout <- "fan_scalar_rows"; data <- boe_mpr_fan_rows(cells, meta, include_distributions)
    }
    if (nrow(data)) parts[[length(parts) + 1L]] <- data
    inventory[[length(inventory) + 1L]] <- data.frame(release = rel$release, source_edition = source_rel$release,
      source_member = member, source_sheet = sheet, layout = meta$layout, parsed_rows = nrow(data),
      supported = nrow(data) > 0L, kind = if (grepl("policy|taylor", sheet, ignore.case = TRUE)) "policy_simulation" else "supporting_material")
  }
  list(data = boe_mpr_bind(parts), inventory = boe_mpr_bind(inventory))
}

boe_mpr_parse_bundle <- function(raw, asset, rel, source_rel, options, include_distributions,
    historical_only = FALSE) {
  receipt <- boe_provenance(raw)
  extension <- asset$file_type[[1L]]
  if (extension %in% c("xls", "xlsx")) {
    workbook <- boe_mpr_load_workbook(raw, extension, options)
    parsed <- boe_mpr_parse_workbook(workbook, rel, source_rel, asset$url[[1L]], "", include_distributions, options)
    parsed$receipt <- receipt; parsed$receipt$member_sha256 <- boe_sha256_raw(raw)
    parsed$databank <- any(vapply(workbook, function(x) any(as.matrix(x)[, 1L] == "Date of publication", na.rm = TRUE), logical(1L)))
    return(parsed)
  }
  path <- boe_write_raw_temp(raw, ".zip"); on.exit(unlink(path), add = TRUE)
  listing <- boe_safe_zip_members(path, options$max_archive_members, options$max_uncompressed_bytes)
  workbooks <- listing$Name[grepl("[.]xlsx?$", listing$Name, ignore.case = TRUE, useBytes = TRUE)]
  databank <- workbooks[grepl("projections databank", workbooks, ignore.case = TRUE)]
  if (length(databank) > 1L) boe_abort("The report contains ambiguous Projections Databanks.", "identity_mismatch")
  if (length(databank)) selected <- databank else {
    # Parse named forecast sources rather than general visual-summary outturn
    # chart files or outside-forecaster surveys. Historical parameter files also
    # supply separately labelled constant-rate and backcast variants.
    current <- workbooks[grepl("current fan chart data", workbooks, ignore.case = TRUE)]
    selected <- unique(c(current, workbooks[grepl("market profiles|projected level.*GDP|prospects for inflation", workbooks, ignore.case = TRUE)]))
    selected <- unique(c(selected, workbooks[grepl("parameters for MPC.*projection", workbooks, ignore.case = TRUE)]))
  }
  if (!length(selected)) return(list(data = data.frame(), inventory = data.frame(), receipt = receipt, databank = FALSE))
  paths <- boe_extract_members(path, selected, options$max_archive_members, options$max_uncompressed_bytes)
  on.exit(unlink(attr(paths, "extraction_dir"), recursive = TRUE), add = TRUE)
  data <- list(); inventories <- list(); hashes <- character()
  for (i in seq_along(paths)) {
    bytes <- readBin(paths[[i]], "raw", n = file.info(paths[[i]])$size)
    wb <- boe_mpr_load_workbook(bytes, tolower(tools::file_ext(paths[[i]])), options)
    parsed <- boe_mpr_parse_workbook(wb, rel, source_rel, asset$url[[1L]], selected[[i]], include_distributions, options)
    data[[i]] <- parsed$data; inventories[[i]] <- parsed$inventory
    hashes[[selected[[i]]]] <- boe_sha256_raw(bytes)
  }
  other <- setdiff(workbooks, selected)
  if (length(other)) inventories[[length(inventories) + 1L]] <- data.frame(release = rel$release,
    source_edition = source_rel$release, source_member = other, source_sheet = NA_character_,
    layout = "supporting_chart_workbook", parsed_rows = 0L, supported = FALSE,
    kind = "supporting_material")
  receipt$member_sha256 <- if (length(hashes) == 1L) unname(hashes[[1L]]) else hashes
  combined <- boe_mpr_bind(data)
  alternate <- data.frame()
  if (nrow(combined)) {
    key <- paste(combined$series_id, combined$target_period)
    historical <- combined$layout == "historical_parameter_groups"
    # Current fan files publish the edition's explicit path, often at greater
    # precision than its historical parameter file. Keep both source records,
    # while selecting the original current path for shared observation keys.
    replaced <- historical & key %in% key[!historical]
    alternate <- combined[replaced, , drop = FALSE]
    combined <- combined[!replaced, , drop = FALSE]
  }
  list(data = combined, inventory = boe_mpr_bind(inventories), receipt = receipt,
    alternate_sources = alternate, databank = length(databank) == 1L)
}

boe_mpr_parse_assets <- function(rel, assets, options, include_distributions, source_rel = rel,
    historical_only = FALSE) {
  result <- list(data = data.frame(), inventory = data.frame(), receipts = list(), primary = NULL,
    numeric_table_seen = FALSE, evidence = list(), databank = FALSE)
  numeric <- assets[assets$asset_role %in% c("data_bundle", "numeric_workbook"), , drop = FALSE]
  # A directly published current fan workbook is more precise than unrelated
  # chart spreadsheets in the accompanying bundle. A databank bundle takes
  # precedence when present, as detected from its actual members below.
  if (nrow(numeric)) {
    numeric <- numeric[order(numeric$file_type != "zip"), , drop = FALSE]
    for (i in seq_len(nrow(numeric))) {
      asset <- numeric[i, , drop = FALSE]
      raw <- boe_mpr_download(asset$url[[1L]], options)
      parsed <- boe_mpr_parse_bundle(raw, asset, rel, source_rel, options, include_distributions, historical_only)
      if (!historical_only && !nrow(parsed$data) &&
          (isTRUE(parsed$databank) || asset$asset_role[[1L]] == "numeric_workbook")) result$numeric_table_seen <- TRUE
      result$receipts[[length(result$receipts) + 1L]] <- parsed$receipt
      result$inventory <- boe_mpr_bind(list(result$inventory, parsed$inventory))
      result$alternate_sources <- boe_mpr_bind(list(result$alternate_sources, parsed$alternate_sources))
      if (nrow(parsed$data)) {
        if (is.null(result$primary)) result$primary <- parsed$receipt
        result$data <- boe_mpr_bind(list(result$data, parsed$data))
      }
      if (parsed$databank) { result$databank <- TRUE; break }
    }
  }
  if (!historical_only && !result$databank) {
    pdfs <- assets[assets$asset_role == "report_pdf", , drop = FALSE]
    if (nrow(pdfs) > 1L) {
      # Full-report links may occur more than once with query/fragment aliases.
      pdfs <- pdfs[!duplicated(sub("[?#].*$", "", pdfs$url)), , drop = FALSE]
    }
    if (nrow(pdfs) > 1L) boe_abort("The report PDF identity is ambiguous.", "identity_mismatch", urls = pdfs$url)
    if (nrow(pdfs) == 1L) {
      raw <- boe_mpr_download(pdfs$url[[1L]], options)
      parsed <- boe_mpr_parse_pdf(raw, rel, pdfs$url[[1L]], options, include_distributions)
      result$receipts[[length(result$receipts) + 1L]] <- boe_provenance(raw)
      result$inventory <- boe_mpr_bind(list(result$inventory, parsed$inventory))
      result$numeric_table_seen <- isTRUE(result$numeric_table_seen) || isTRUE(parsed$numeric_table_seen)
      result$available_kinds <- parsed$available_kinds
      result$evidence <- parsed$evidence
      if (nrow(parsed$data)) {
        if (is.null(result$primary)) result$primary <- boe_provenance(raw)
        result$data <- boe_mpr_bind(list(result$data, parsed$data))
      }
    }
  }
  # Aliases with identical observations are not collapsed blindly. Sources
  # yielding overlapping path identities must agree byte-for-value, including
  # missing markers, before the preferred first published source is retained.
  if (nrow(result$data)) {
    key <- paste(result$data$series_id, result$data$target_period)
    dup <- which(duplicated(key))
    for (i in dup) {
      first <- match(key[[i]], key)
      a <- result$data$value[[i]]; b <- result$data$value[[first]]
      if (!identical(is.na(a), is.na(b)) || (!is.na(a) && abs(a - b) > 1e-9)) {
        boe_abort("Published sources disagree for an identical forecast path.", "schema_drift", series_id = result$data$series_id[[i]],
          conflicting_cells = result$data[c(first, i), intersect(c("source_url", "source_member", "source_sheet", "layout", "source_row", "source_column", "value", "target_period"), names(result$data)), drop = FALSE])
      }
    }
    result$data <- result$data[!duplicated(key), , drop = FALSE]
  }
  result
}

boe_mpr_historical_fallback <- function(rel, releases, options, include_distributions) {
  result <- list(data = data.frame(), inventory = data.frame(), receipts = list(), primary = NULL, inspected_editions = character())
  # Search discovered later editions for actual multi-vintage data. A successful
  # vintage match must exist in source rows; current/scenario-only paths are never
  # relabelled as older forecasts. This rule contains no date allowlist.
  later <- releases[releases$source_published_date > rel$source_published_date, , drop = FALSE]
  later <- later[order(later$source_published_date, decreasing = TRUE), , drop = FALSE]
  for (i in seq_len(nrow(later))) {
    boe_time_left(options$deadline)
    source <- later[i, , drop = FALSE]
    assets <- boe_mpr_assets_for_release(source, options)
    numeric <- assets[assets$asset_role == "data_bundle", , drop = FALSE]
    if (!nrow(numeric)) next
    parsed <- boe_mpr_parse_assets(rel, numeric, options, include_distributions,
      source_rel = source, historical_only = TRUE)
    result$inspected_editions <- c(result$inspected_editions, source$release)
    result$receipts <- c(result$receipts, parsed$receipts)
    result$inventory <- boe_mpr_bind(list(result$inventory, parsed$inventory))
    if (nrow(parsed$data)) {
      old_keys <- if (nrow(result$data)) boe_mpr_keys(result$data) else character()
      added <- !boe_mpr_keys(parsed$data) %in% old_keys
      result$data <- boe_mpr_bind(list(result$data, parsed$data[added, , drop = FALSE]))
      if (is.null(result$primary)) result$primary <- parsed$primary
    }
    # Once an actual full databank has been inspected, its source-vintage
    # inventory supplies affirmative absence for earlier rows; do not issue a
    # speculative request for every subsequent calendar month.
    if (parsed$databank) break
  }
  result
}

boe_mpr_market_profiles <- function(cells, meta) {
  location <- which(tolower(trimws(cells)) == "report date", arr.ind = TRUE)
  if (!nrow(location)) return(data.frame())
  if (nrow(location) != 1L) boe_abort("Conditioning-path report-date header is ambiguous.", "schema_drift")
  header <- location[1L, 1L] + 1L
  vintages <- boe_mpr_vintages(cells[header, ])
  cols <- which(!is.na(vintages) & vintages == meta$release)
  if (!length(cols)) return(data.frame())
  if (length(cols) != 1L) boe_abort("Conditioning-path vintage columns are duplicate.", "schema_drift")
  rows <- which(seq_len(nrow(cells)) > header & !is.na(cells[, 1L]) & grepl("^[0-9]{4} ?Q[1-4]$", cells[, 1L]))
  valid <- which(!is.na(cells[rows, cols]))
  if (!length(valid)) return(data.frame())
  rows <- rows[seq_len(max(valid))]
  meta$title <- "Bank Rate"; meta$units <- "Per cent"; meta$definition <- paste(na.omit(cells[seq_len(header - 1L), 1:2]), collapse = " ")
  meta$layout <- "conditioning_vintage_columns"
  boe_mpr_path(meta, cells[rows, 1L], cells[rows, cols], rows, cols, "bank_rate", "market", "level", "Q",
    "conditioning_assumption", source_vintage_label = cells[header, cols], scenario_label = "Market conditioning path")
}

boe_mpr_gdp_level_columns <- function(cells, meta) {
  headers <- which(!is.na(cells) & tolower(trimws(cells)) == "date", arr.ind = TRUE)
  if (!nrow(headers)) return(data.frame())
  for (i in seq_len(nrow(headers))) {
    h <- headers[i, 1L]; date_col <- headers[i, 2L]
    rows <- which(seq_len(nrow(cells)) > h & !is.na(cells[, date_col]) & grepl("^[0-9]{4} ?Q[1-4]$", cells[, date_col]))
    cols <- which(!is.na(cells[h, ]) & grepl("(Ex|Inc)[.]? *Backcast", cells[h, ], ignore.case = TRUE))
    if (!length(rows) || !length(cols)) next
    units_cells <- which(!is.na(cells) & grepl("GDP.*(billion|chained.volume)", cells, ignore.case = TRUE), arr.ind = TRUE)
    units <- if (nrow(units_cells)) cells[units_cells[1L, 1L], units_cells[1L, 2L]] else "Published chained-volume level"
    intro <- paste(na.omit(as.vector(cells[seq_len(h - 1L), , drop = FALSE])), collapse = " ")
    vintage <- boe_mpr_month_key(intro)
    if (is.na(vintage)) vintage <- meta$release
    parts <- lapply(cols, function(col) {
      conditions <- which(!is.na(cells[seq_len(h - 1L), seq_len(col), drop = FALSE]) &
        grepl("GDP( growth)? projections based", cells[seq_len(h - 1L), seq_len(col), drop = FALSE], ignore.case = TRUE), arr.ind = TRUE)
      condition <- if (nrow(conditions)) {
        order <- order(conditions[, 1L], conditions[, 2L], decreasing = TRUE)
        cells[conditions[order[[1L]], 1L], conditions[order[[1L]], 2L]]
      } else "Published GDP-level projection"
      scenario <- if (grepl("market", condition, ignore.case = TRUE)) "market_conditioned" else if (grepl("constant", condition, ignore.case = TRUE)) "constant_rate" else "central"
      kind <- "indicative_profile"
      if (vintage != meta$release) { scenario <- paste0(boe_mpr_slug(vintage), "_comparator_", scenario); kind <- "comparator" }
      m <- meta; m$layout <- "gdp_level_backcast_columns"; m$title <- "GDP level"; m$units <- units; m$definition <- intro
      stat <- if (grepl("^Ex", cells[h, col], ignore.case = TRUE)) "level_excluding_backcast" else "level_including_backcast"
      boe_mpr_path(m, cells[rows, date_col], cells[rows, col], rows, col, "gdp_level", scenario,
        stat, "Q", kind, forecast_vintage = vintage, scenario_label = condition,
        source_vintage = vintage, source_vintage_label = intro)
    })
    return(boe_mpr_bind(parts))
  }
  data.frame()
}

boe_mpr_parameter_groups <- function(cells, meta, include_distributions) {
  headers <- which(apply(cells, 1L, function(x) sum(grepl("^[0-9]{4} ?Q[1-4]$", x), na.rm = TRUE) >= 2L))
  if (!length(headers) || ncol(cells) < 4L) return(data.frame())
  header <- headers[[1L]]; cols <- which(grepl("^[0-9]{4} ?Q[1-4]$", cells[header, ]))
  counts <- vapply(seq_len(min(4L, ncol(cells))), function(col)
    sum(tolower(cells[, col]) %in% c("mode", "market mode", "median", "market median", "mean", "market mean")), integer(1L))
  stat_col <- which.max(counts)
  if (!counts[[stat_col]]) return(data.frame())
  month <- match(substr(tolower(trimws(cells[, 2L])), 1L, 3L), tolower(month.abb))
  anchors <- which(!is.na(month) & seq_len(nrow(cells)) > header)
  year_values <- suppressWarnings(as.integer(cells[, 1L]))
  year_values[is.na(year_values) | year_values < 1000L | year_values > 9999L] <- NA_integer_
  year <- NA_integer_; years <- rep(NA_integer_, nrow(cells))
  for (i in seq_len(nrow(cells))) { if (!is.na(year_values[[i]])) year <- year_values[[i]]; years[[i]] <- year }
  keys <- sprintf("%04d-%02d", years[anchors], month[anchors])
  selected <- which(!is.na(years[anchors]) & keys == meta$release)
  if (!length(selected)) return(data.frame())
  measure <- boe_mpr_measure(meta$title)
  if (!measure %in% c("cpi_inflation", "rpix_inflation", "gdp_growth", "unemployment")) return(data.frame())
  parts <- list()
  for (index in selected) {
    from <- anchors[[index]] - 1L
    to <- if (index < length(anchors)) anchors[[index + 1L]] - 2L else nrow(cells)
    group_labels <- cells[seq.int(from, to), 2L]
    backcast <- group_labels[!is.na(group_labels) & grepl("backcast", group_labels, ignore.case = TRUE)]
    variant <- if (!length(backcast)) "" else if (grepl("^Ex", backcast[[1L]], ignore.case = TRUE)) "excluding_backcast" else if (
      grepl("^Inc", backcast[[1L]], ignore.case = TRUE)) "including_backcast" else ""
    current_market <- FALSE
    for (row in seq.int(from, to)) {
      label <- tolower(trimws(cells[row, stat_col]))
      allowed <- c("mode", "median", "mean", "market mode", "market median", "market mean")
      if (include_distributions) allowed <- c(allowed, "uncertainty", "skew", "skewness")
      if (is.na(label) || !label %in% allowed) next
      values <- cells[row, cols]; valid <- which(!is.na(values) & nzchar(trimws(values)))
      if (!length(valid)) next
      selected_cols <- cols[seq_len(max(valid))]
      if (grepl("^(market )?(mode|median|mean)$", label)) current_market <- startsWith(label, "market")
      market <- current_market
      stat <- sub("^market ", "", label); if (stat == "skew") stat <- "skewness"
      scenario <- if (market) "market_conditioned" else if (stat_col >= 4L) "constant_rate" else "published_conditioning"
      kind <- if (stat %in% c("mean", "median", "uncertainty", "skewness")) "distribution_statistic" else if (
        scenario == "constant_rate") "conditional_forecast" else "central_forecast"
      if (nzchar(variant)) stat <- paste(stat, variant, sep = "_")
      m <- meta; m$layout <- "historical_parameter_groups"; m$units <- if (grepl("^skewness", stat)) "Dimensionless" else "Per cent"
      data <- boe_mpr_path(m, cells[header, selected_cols], cells[row, selected_cols],
        row, selected_cols, measure, scenario, stat, "Q", kind, scenario_label = cells[row, stat_col],
        source_vintage_label = paste(years[anchors[[index]]], cells[anchors[[index]], 2L]))
      data$backcast_variant <- if (nzchar(variant)) variant else NA_character_
      parts[[length(parts) + 1L]] <- data
    }
  }
  boe_mpr_bind(parts)
}
