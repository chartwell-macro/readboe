boe_mpr_pdf_words <- function(raw, options) {
  command <- Sys.which("pdftotext")
  if (!nzchar(command)) boe_abort("Historical numeric report tables require Poppler's `pdftotext` on PATH.",
    "dependency", dependency = "pdftotext", source_capability = "pdf_forecast_tables")
  input <- boe_write_raw_temp(raw, ".pdf"); output <- tempfile(fileext = ".html"); errors <- tempfile()
  on.exit(unlink(c(input, output, errors)), add = TRUE)
  if (length(raw) < 5L || !identical(rawToChar(raw[1:5]), "%PDF-")) boe_abort("The official report is not a PDF payload.", "parse")
  run <- function(args, destination) {
    timeout <- max(1L, as.integer(ceiling(boe_time_left(options$deadline))))
    status <- suppressWarnings(system2(command, c(args, shQuote(input), shQuote(destination)),
      stdout = FALSE, stderr = errors, timeout = timeout))
    boe_time_left(options$deadline)
    if (status != 0L || !file.exists(destination)) boe_abort("The official report PDF could not be decoded.", "parse")
    if (file.info(destination)$size > 30 * 1024^2) boe_abort("PDF text exceeds the bounded extraction size.", "bounds")
  }
  text_path <- tempfile(fileext = ".txt"); on.exit(unlink(text_path), add = TRUE)
  run(c("-layout", "-enc", "UTF-8"), text_path)
  text <- readChar(text_path, file.info(text_path)$size, useBytes = TRUE)
  text_pages <- strsplit(text, "\f", fixed = TRUE)[[1L]]
  if (length(text_pages) && !nzchar(trimws(tail(text_pages, 1L)))) text_pages <- head(text_pages, -1L)
  if (!length(text_pages) || length(text_pages) > 300L) boe_abort("PDF page count is empty or exceeds 300.", "bounds")
  candidates <- which(vapply(text_pages, function(x) grepl(
    "MPC.{0,8}expectations|Bank.{0,8}expectations|Mode[^\\n]*Median[^\\n]*Mean|Table[^\\n]*[Ii]ndicative|[Cc]alendar.year GDP|Q4 CPI inflation|Percentiles of projected|[Ee]xpectations of the Bank|Conditioning path for Bank Rate|implied by market yields",
    x, perl = TRUE), logical(1L)))
  words <- list(); lines <- list()
  for (page in candidates) {
    run(c("-bbox-layout", "-enc", "UTF-8", "-f", as.character(page), "-l", as.character(page)), output)
    doc <- tryCatch(xml2::read_xml(output, options = c("NONET", "NOBLANKS")), error = function(e)
      boe_abort("PDF text geometry is invalid.", "parse"))
    node <- xml2::xml_find_first(doc, "//*[local-name()='page']")
    nodes <- xml2::xml_find_all(node, ".//*[local-name()='word']")
    line <- xml2::xml_find_all(node, ".//*[local-name()='line']")
    index <- length(words) + 1L
    words[[index]] <- data.frame(page = rep(page, length(nodes)), text = xml2::xml_text(nodes),
      x = as.numeric(xml2::xml_attr(nodes, "xMin")), y = as.numeric(xml2::xml_attr(nodes, "yMin")),
      xmax = as.numeric(xml2::xml_attr(nodes, "xMax")), ymax = as.numeric(xml2::xml_attr(nodes, "yMax")),
      width = rep(as.numeric(xml2::xml_attr(node, "width")), length(nodes)), stringsAsFactors = FALSE)
    lines[[index]] <- data.frame(page = rep(page, length(line)), text = vapply(line, function(x) paste(xml2::xml_text(xml2::xml_find_all(x, ".//*[local-name()='word']")), collapse = " "), character(1L)),
      x = as.numeric(xml2::xml_attr(line, "xMin")), y = as.numeric(xml2::xml_attr(line, "yMin")),
      xmax = as.numeric(xml2::xml_attr(line, "xMax")), ymax = as.numeric(xml2::xml_attr(line, "yMax")), stringsAsFactors = FALSE)
  }
  list(words = words, lines = lines, pages = length(text_pages), text_pages = text_pages)
}

boe_mpr_pdf_number <- function(x) {
  # Fractions are printed scalar values, not inferred points on a chart.
  clean <- trimws(gsub("[[:space:]]+", "", x))
  clean <- gsub("\u2212", "-", clean, fixed = TRUE)
  clean <- gsub("\u2013", "-", clean, fixed = TRUE)
  clean <- gsub("\u2011", "-", clean, fixed = TRUE)
  value <- suppressWarnings(as.numeric(clean))
  simple_fraction <- grepl("^[+-]?[13]/[24]$", clean)
  for (i in which(simple_fraction)) {
    pair <- strsplit(clean[[i]], "/", fixed = TRUE)[[1L]]
    value[[i]] <- as.numeric(pair[[1L]]) / as.numeric(pair[[2L]])
  }
  fractions <- c("\u00bc" = 0.25, "\u00bd" = 0.5, "\u00be" = 0.75)
  for (fraction in names(fractions)) {
    hit <- grepl(paste0("^[+-]?[0-9]*", fraction, "$"), clean)
    if (any(hit)) {
      whole <- sub(paste0(fraction, "$"), "", clean[hit])
      sign <- ifelse(startsWith(whole, "-"), -1, 1)
      whole <- suppressWarnings(as.numeric(whole)); whole[is.na(whole)] <- 0
      value[hit] <- whole + sign * fractions[[fraction]]
    }
  }
  hit <- grepl("^[+-]?[0-9]+[13]/[24]$", clean)
  for (i in which(hit)) {
    m <- regmatches(clean[[i]], regexec("^([+-]?[0-9]+)([13])/([24])$", clean[[i]]))[[1L]]
    value[[i]] <- as.numeric(m[[2L]]) + ifelse(startsWith(m[[2L]], "-"), -1, 1) * as.numeric(m[[3L]]) / as.numeric(m[[4L]])
  }
  value
}

boe_mpr_pdf_row_words <- function(words, y, left, right, tolerance = 2) {
  x <- words[abs(words$y - y) <= tolerance & words$x >= left & words$xmax <= right, , drop = FALSE]
  x[order(x$x), , drop = FALSE]
}

boe_mpr_pdf_scalar_table <- function(words, lines, header, rel, url) {
  h <- lines[header, , drop = FALSE]
  estimator_words <- words[abs(words$y - h$y) < 2 & tolower(words$text) %in% c("mode", "median", "mean"), , drop = FALSE]
  estimator_words <- estimator_words[order(estimator_words$x), , drop = FALSE]
  if (nrow(estimator_words) != 3L) return(data.frame())
  above <- lines[lines$y < h$y & lines$y > h$y - 85 & lines$x < min(estimator_words$x), , drop = FALSE]
  captions <- above[grepl("^Table [0-9]+[.][A-Z]", above$text), , drop = FALSE]
  if (!nrow(captions)) return(data.frame())
  caption <- captions[which.max(captions$y), , drop = FALSE]
  title_lines <- above[above$y >= caption$y, , drop = FALSE]
  title <- paste(title_lines$text[order(title_lines$y)], collapse = " ")
  if (grepl("other forecaster|external|survey", title, ignore.case = TRUE)) return(data.frame())
  measure <- if (grepl("GDP", title)) "gdp_growth" else if (grepl("CPI inflation", title)) "cpi_inflation" else NA_character_
  if (is.na(measure)) return(data.frame())
  annual <- measure == "gdp_growth" && grepl("calendar.year", title, ignore.case = TRUE)
  if (!annual && !grepl("Q4 CPI", title, ignore.case = TRUE)) return(data.frame())
  first <- min(estimator_words$x) - 30
  target_words <- words[grepl("^[0-9]{4}(\\([a-z]\\))?$", words$text) & words$x < first & words$x >= caption$x - 2 &
    words$y > h$y & words$y < h$y + 85, , drop = FALSE]
  target_words <- target_words[order(target_words$y), , drop = FALSE]
  if (!nrow(target_words)) return(data.frame())
  target_words <- target_words[abs(target_words$x - target_words$x[[1L]]) < 3, , drop = FALSE]
  terminators <- lines$y[lines$y > h$y & lines$x <= caption$x + 5 &
    grepl("^(The table|The figures|The numbers|Sources?:|\\([a-z]\\))", lines$text)]
  if (length(terminators)) target_words <- target_words[target_words$y < min(terminators), , drop = FALSE]
  bounds <- c(first, head(estimator_words$x, -1) + diff(estimator_words$x) / 2,
    max(estimator_words$xmax) + 35)
  parts <- list()
  for (i in seq_len(nrow(target_words))) {
    target <- sub("\\(.*$", "", target_words$text[[i]])
    if (!annual) target <- paste(target, "Q4")
    for (j in 1:3) {
      cell <- boe_mpr_pdf_row_words(words, target_words$y[[i]], bounds[[j]], bounds[[j + 1L]])
      full_cell <- paste(cell$text, collapse = " ")
      raw <- trimws(sub("\\(.*$", "", full_cell))
      value <- boe_mpr_pdf_number(raw)
      if (is.na(value) && !raw %in% c("", "-", "\u2013")) boe_abort("A published PDF forecast scalar changed format.", "schema_drift", raw_value = raw)
      estimator <- tolower(estimator_words$text[[j]])
      statistic <- paste(estimator, if (annual) "annual_change" else "q4_four_quarter_change", sep = "_")
      meta <- boe_mpr_source_meta(rel, rel, url, "", sub("^(Table [0-9]+[.][A-Z]).*", "\\1", caption$text),
        "pdf_scalar_estimators", title, "Per cent", title)
      out <- boe_mpr_path(meta, target, if (is.na(value)) NA_character_ else as.character(value),
        i, j, measure, "central", statistic, "A", if (estimator == "mode") "central_forecast" else "distribution_statistic")
      out$raw_value <- raw; out$source_page <- words$page[[1L]]
      out$source_cell_text <- full_cell
      out$comparison_raw_value <- if (grepl("\\(", full_cell)) sub(".*\\(([^)]*)\\).*", "\\1", full_cell) else NA_character_
      out$source_x_min <- bounds[[j]]; out$source_y_min <- target_words$y[[i]]
      parts[[length(parts) + 1L]] <- out
    }
  }
  boe_mpr_bind(parts)
}

boe_mpr_pdf_prob_block <- function(words, start, end, left, right, measure, heading, table, rel, url) {
  target_words <- words[words$x >= left & words$x < left + 60 & words$y > start & words$y < end &
    grepl("^[0-9]{4}$", words$text), , drop = FALSE]
  target_words <- target_words[order(target_words$y), , drop = FALSE]
  if (!nrow(target_words)) return(data.frame())
  first <- boe_mpr_pdf_row_words(words, target_words$y[[1L]], left, right)
  nums <- first[first$x > target_words$xmax[[1L]] + 12 &
    grepl("^[<>]?[0-9]{1,3}([.][0-9]+)?%?$", first$text), , drop = FALSE]
  if (nrow(nums) < 2L || nrow(nums) > 12L) return(data.frame())
  centers <- (nums$x + nums$xmax) / 2
  step <- stats::median(diff(centers))
  bounds <- c(centers[[1L]] - step / 2, head(centers, -1) + diff(centers) / 2, tail(centers, 1L) + step / 2)
  events <- vapply(seq_along(centers), function(j) {
    w <- words[words$x >= bounds[[j]] & words$x < bounds[[j + 1L]] & words$y > start &
      words$y < target_words$y[[1L]] - 1 &
      grepl("^(Less|More|less|more|than|to|[<>]=?|[0-9]+([.][0-9]+)?%)$", words$text), , drop = FALSE]
    paste(w$text[order(w$y, w$x)], collapse = " ")
  }, character(1L))
  if (any(!nzchar(events)) || anyDuplicated(events)) return(data.frame())
  scenario <- if (grepl("constant", heading, ignore.case = TRUE)) "constant_rate" else if (
    grepl("market.*interest|market.*rate", heading, ignore.case = TRUE)) "market_conditioned" else "central"
  parts <- list()
  for (i in seq_len(nrow(target_words))) {
    row <- boe_mpr_pdf_row_words(words, target_words$y[[i]], left, right)
    q <- row$text[grepl("^Q[1-4]$", row$text)]
    if (length(q) != 1L) next
    values <- vapply(seq_along(centers), function(j) paste(row$text[row$x >= bounds[[j]] &
      row$x < bounds[[j + 1L]]], collapse = " "), character(1L))
    censored <- grepl("^[<>]=? *[0-9]+([.][0-9]+)?%?$", values)
    numeric <- suppressWarnings(as.numeric(values))
    if (any(is.na(numeric) & !censored) || any(numeric[!censored] < 0 | numeric[!censored] > 100) ||
        (!any(censored) && abs(sum(numeric) - 100) > 3) || sum(numeric[!censored]) > 103) {
      boe_abort("An MPC probability row disagrees with its published table schema.", "schema_drift",
        source_page = words$page[[1L]], raw_values = values, measure = measure)
    }
    for (j in seq_along(centers)) {
      meta <- boe_mpr_source_meta(rel, rel, url, "", table, "pdf_probability_bins", heading, "percent", heading)
      data <- boe_mpr_path(meta, paste(target_words$text[[i]], q), values[[j]], i, j, measure, scenario,
        paste0("probability_", boe_mpr_slug(events[[j]])), "Q", "forecast_probability", probability_event = events[[j]])
      data$source_page <- words$page[[1L]]; data$source_x_min <- bounds[[j]]; data$source_y_min <- target_words$y[[i]]
      data$policy_conditioning_label <- heading
      parts[[length(parts) + 1L]] <- data
    }
  }
  boe_mpr_bind(parts)
}

boe_mpr_pdf_prob_table <- function(words, lines, caption, rel, url) {
  cap <- lines[caption, , drop = FALSE]
  right <- words$width[[1L]] - 25
  prose <- lines[lines$y > cap$y - 5 & lines$y < cap$y + 120 & lines$x > cap$x + 140 & nchar(lines$text) > 35, , drop = FALSE]
  if (nrow(prose)) right <- min(prose$x) - 3
  scope <- lines[lines$x >= cap$x - 2 & lines$x < right & lines$y >= cap$y & lines$y < cap$y + 400, , drop = FALSE]
  next_caption <- scope$y[grepl("^(Table|Chart) ", scope$text) & scope$y > cap$y + 4]
  footnote <- scope$y[grepl("^\\([a-z]\\)", scope$text)]
  end <- min(c(cap$y + 400, next_caption, footnote))
  heading <- paste(scope$text[scope$y < cap$y + 45], collapse = " ")
  if (!grepl("(MPC|Bank).*(expectations|probabilities|projections)|projected probabilities", heading, ignore.case = TRUE) ||
      grepl("other forecaster|external|survey", heading, ignore.case = TRUE)) return(data.frame())
  variable_labels <- c(rpixinflation = "rpix_inflation", cpiinflation = "cpi_inflation",
    gdpgrowth = "gdp_growth", unemployment = "unemployment", unemploymentrate = "unemployment")
  normalized <- gsub("[[:space:]]", "", tolower(scope$text))
  marker <- scope[scope$y > cap$y & scope$y < end & normalized %in% names(variable_labels), , drop = FALSE]
  if (nrow(marker)) marker$text <- unname(variable_labels[gsub("[[:space:]]", "", tolower(marker$text))])
  marker <- marker[order(marker$y), , drop = FALSE]
  if (!nrow(marker)) {
    measure <- if (grepl("RPIX", heading)) "rpix_inflation" else if (grepl("CPI", heading)) "cpi_inflation" else if (grepl("GDP", heading)) "gdp_growth" else NA_character_
    if (is.na(measure)) return(data.frame())
    marker <- data.frame(text = measure, y = cap$y + 25)
  }
  table <- sub("^(Table [0-9]+[.][A-Z]).*", "\\1", cap$text)
  boe_mpr_bind(lapply(seq_len(nrow(marker)), function(i) {
    measure <- if (marker$text[[i]] %in% c("rpix_inflation", "cpi_inflation", "gdp_growth", "unemployment")) marker$text[[i]] else boe_mpr_measure(marker$text[[i]])
    boe_mpr_pdf_prob_block(words, marker$y[[i]], if (i < nrow(marker)) marker$y[[i + 1L]] else end,
      cap$x - 2, right, measure, heading, table, rel, url)
  }))
}

boe_mpr_parse_pdf <- function(raw, rel, url, options, include_distributions) {
  geometry <- boe_mpr_pdf_words(raw, options)
  data <- list(); inventory <- list(); seen <- FALSE; evidence <- character(); available_kinds <- character()
  annual <- boe_mpr_pdf_annual_text(geometry$text_pages, rel, url)
  if (nrow(annual$data)) { data[[length(data) + 1L]] <- annual$data; seen <- TRUE }
  if (nrow(annual$inventory)) inventory[[length(inventory) + 1L]] <- annual$inventory
  for (i in seq_along(geometry$words)) {
    words <- geometry$words[[i]]; lines <- geometry$lines[[i]]
    page_number <- if (nrow(words)) words$page[[1L]] else NA_integer_
    if (!nrow(words)) next
    # PDF producers may emit each estimator heading as a separate line/block.
    # Group only words sharing the same physical baseline.
    estimator_y <- words$y[tolower(words$text) == "mode"]
    for (y in estimator_y) {
      w <- words[abs(words$y - y) < 2 & tolower(words$text) %in% c("mode", "median", "mean"), , drop = FALSE]
      if (nrow(w) == 3L && length(unique(tolower(w$text))) == 3L &&
          !any(abs(lines$y - y) < 2 & grepl("Mode.*Median.*Mean", lines$text))) {
        lines <- rbind(lines, data.frame(page = page_number, text = paste(w$text[order(w$x)], collapse = " "),
          x = min(w$x), y = y, xmax = max(w$xmax), ymax = max(w$ymax)))
      }
    }
    headers <- which(grepl("\\bMode\\b.*\\bMedian\\b.*\\bMean\\b", lines$text))
    for (header in headers) {
      parsed <- boe_mpr_pdf_scalar_table(words, lines, header, rel, url)
      if (nrow(parsed)) data[[length(data) + 1L]] <- parsed
    }
    captions <- which(grepl("^Table [0-9]+[.][A-Z]", lines$text))
    for (caption in captions) {
      quantiles <- boe_mpr_pdf_quantiles(words, lines, caption, rel, url)
      if (nrow(quantiles)) {
        seen <- TRUE; available_kinds <- unique(c(available_kinds, quantiles$kind))
        if (include_distributions) data[[length(data) + 1L]] <- quantiles
      }
      conditioning <- boe_mpr_pdf_conditioning(words, lines, caption, rel, url)
      if (nrow(conditioning)) { data[[length(data) + 1L]] <- conditioning; seen <- TRUE }
      scope <- lines$text[lines$y >= lines$y[[caption]] & lines$y < lines$y[[caption]] + 40 &
        abs(lines$x - lines$x[[caption]]) < 12]
      title <- paste(scope, collapse = " ")
      candidate <- grepl("(MPC|Bank).*(expectations|probabilities)|projected probabilities|Percentiles of projected|calendar.year GDP|Q4 CPI|[Ee]xpectations of the Bank|Conditioning path for Bank Rate", title, ignore.case = TRUE) &&
        !grepl("other forecaster|external|survey", title, ignore.case = TRUE)
      if (candidate) {
        seen <- TRUE
        evidence <- c(evidence, paste("PDF page", page_number, title))
        parsed <- boe_mpr_pdf_prob_table(words, lines, caption, rel, url)
        if (nrow(parsed)) available_kinds <- unique(c(available_kinds, parsed$kind))
        if (include_distributions && nrow(parsed)) data[[length(data) + 1L]] <- parsed
      }
      inventory[[length(inventory) + 1L]] <- data.frame(release = rel$release,
        source_edition = rel$release, source_sheet = lines$text[[caption]], source_page = page_number,
        layout = if (candidate) "own_forecast_table" else "other_report_table", title = title,
        candidate_numeric_forecast = candidate, stringsAsFactors = FALSE)
    }
  }
  parsed <- boe_mpr_bind(data)
  tables <- boe_mpr_bind(inventory)
  if (nrow(tables)) {
    tables$parsed_rows <- vapply(seq_len(nrow(tables)), function(i) {
      if (!nrow(parsed)) return(0L)
      sum(parsed$source_page == tables$source_page[[i]] & parsed$source_sheet == tables$source_sheet[[i]])
    }, integer(1L))
  }
  list(data = parsed, inventory = tables, numeric_table_seen = seen, available_kinds = available_kinds,
    evidence = list(report_url = url, pages_inspected = geometry$pages,
      own_forecast_table_captions = evidence,
      absence_scope = "All report pages inspected; outside-forecaster tables and chart/prose values are not relabelled as Bank scalar forecasts"))
}

boe_mpr_pdf_conditioning <- function(words, lines, caption, rel, url) {
  cap <- lines[caption, , drop = FALSE]
  right <- words$width[[1L]] - 25
  prose <- lines[lines$y > cap$y & lines$y < cap$y + 130 & lines$x > cap$x + 160 & nchar(lines$text) > 50, , drop = FALSE]
  if (nrow(prose)) right <- min(prose$x) - 3
  scope <- lines[lines$x >= cap$x - 2 & lines$x < right & lines$y >= cap$y & lines$y < cap$y + 300, , drop = FALSE]
  heading <- paste(scope$text[scope$y < cap$y + 45 & abs(scope$x - cap$x) < 8], collapse = " ")
  if (!grepl("expectations of the Bank.*official|Conditioning path for Bank Rate", heading, ignore.case = TRUE)) return(data.frame())
  endings <- scope$y[(grepl("^(Table|Chart) ", scope$text) & scope$y > cap$y + 5) | grepl("^\\([a-z]\\)", scope$text)]
  end <- min(c(cap$y + 300, endings))
  quarters <- words[grepl("^Q[1-4]$", words$text) & words$x >= cap$x - 2 & words$xmax < right & words$y > cap$y + 20 & words$y < end, , drop = FALSE]
  if (nrow(quarters) < 2L) return(data.frame())
  # The first block is the report's current market path. Some sources append a
  # previous-report comparison on a second independent year/quarter header.
  y <- min(quarters$y)
  quarters <- quarters[abs(quarters$y - y) < 2, , drop = FALSE]
  quarters <- quarters[order(quarters$x), , drop = FALSE]
  years <- words[grepl("^[0-9]{4}$", words$text) & words$x >= cap$x - 2 & words$x < right & words$y < y & words$y > y - 30, , drop = FALSE]
  if (!nrow(years)) return(data.frame())
  years <- years[abs(years$y - max(years$y)) < 2, , drop = FALSE]
  years <- years[order(years$x), , drop = FALSE]
  q <- as.integer(sub("Q", "", quarters$text))
  # Year headings may be left aligned or centred over their quarter groups.
  # Pair every printed heading with its ordered Q4-to-Q1 (or repeated-quarter)
  # group, rather than deriving later years from the first heading.
  anchors <- cumsum(c(1L, as.integer(diff(q) <= 0L)))
  if (max(anchors) != nrow(years) || anyDuplicated(years$text)) boe_abort("Conditioning quarter/year anchors are ambiguous.", "schema_drift")
  target_year <- as.integer(years$text[anchors])
  if (any(diff(target_year * 4L + q) <= 0L)) boe_abort("Conditioning target periods are unordered or duplicate.", "schema_drift")
  centers <- (quarters$x + quarters$xmax) / 2
  step <- stats::median(diff(centers))
  bounds <- c(centers[[1L]] - step / 2, head(centers, -1L) + diff(centers) / 2, tail(centers, 1L) + step / 2)
  for (i in seq_len(nrow(years))) {
    group <- which(anchors == i)
    center <- (years$x[[i]] + years$xmax[[i]]) / 2
    if (center < bounds[min(group)] - 2 || center > bounds[max(group) + 1L] + 2) {
      boe_abort("A printed year heading lies outside its conditioning quarter group.", "schema_drift")
    }
  }
  numeric <- words[words$y > y + 5 & words$y < min(end, y + 40) & words$x >= bounds[[1L]] & words$x < tail(bounds, 1L) & !is.na(boe_mpr_pdf_number(words$text)), , drop = FALSE]
  if (!nrow(numeric)) return(data.frame())
  numeric <- numeric[abs(numeric$y - min(numeric$y)) < 2, , drop = FALSE]
  values <- vapply(seq_along(centers), function(i) paste(numeric$text[numeric$x >= bounds[[i]] & numeric$x < bounds[[i + 1L]]], collapse = " "), character(1L))
  parsed <- boe_mpr_pdf_number(values)
  if (anyNA(parsed)) boe_abort("A published interest-rate conditioning row changed schema.", "schema_drift", raw_values = values, source_page = words$page[[1L]])
  title <- sub("^(Table [0-9]+[.][A-Z]).*", "\\1", cap$text)
  definition <- paste(scope$text[order(scope$y, scope$x)], collapse = " ")
  meta <- boe_mpr_source_meta(rel, rel, url, "", title, "pdf_market_conditioning", heading, "Per cent", definition)
  out <- boe_mpr_path(meta, paste(target_year, quarters$text), as.character(parsed), 1L, seq_along(parsed),
    "bank_rate", "market", "level", "Q", "conditioning_assumption", scenario_label = heading)
  out$raw_value <- values; out$source_page <- words$page[[1L]]
  out$source_y_min <- min(numeric$y); out$source_x_min <- bounds[-length(bounds)]
  out
}

boe_mpr_quantile_condition <- function(text, vintage) {
  text <- tolower(text)
  month <- tolower(month.name[as.integer(substr(vintage, 6L, 7L))])
  named_constant <- grepl(paste0("constant (nominal )?interest rates? *\\(", month, "\\)"), text)
  named_market <- grepl(paste0("market interest rates? *\\(", month, "\\)"), text)
  if (named_constant && !named_market) return("constant_rate")
  if (named_market && !named_constant) return("market_conditioned")
  constant <- grepl("constant (nominal )?interest rates?", text)
  market <- grepl("market interest rates?", text)
  if (constant && !market) "constant_rate" else if (market && !constant) "market_conditioned" else "published_conditioning"
}

boe_mpr_pdf_quantiles <- function(words, lines, caption, rel, url) {
  cap <- lines[caption, , drop = FALSE]
  title_lines <- lines[lines$y >= cap$y & lines$y < cap$y + 25 & abs(lines$x - cap$x) < 8, , drop = FALSE]
  title <- paste(title_lines$text[order(title_lines$y)], collapse = ' ')
  if (!grepl('Percentiles of projected', title, ignore.case = TRUE)) return(data.frame())
  measure <- if (grepl('CPI', title)) 'cpi_inflation' else if (grepl('RPIX', title)) 'rpix_inflation' else if (
    grepl('GDP', title)) 'gdp_growth' else if (grepl('unemployment', title, ignore.case = TRUE)) 'unemployment' else NA_character_
  if (is.na(measure)) boe_abort('A published percentile table has an unknown variable.', 'schema_drift')
  probs <- words[words$x >= cap$x & words$y > cap$y + 10 & words$y < cap$y + 60 & grepl('^[0-9]+%$', words$text), , drop = FALSE]
  if (nrow(probs) < 2L) boe_abort('Published percentile headings changed schema.', 'schema_drift')
  probs <- probs[abs(probs$y - min(probs$y)) < 2, , drop = FALSE]
  probs <- probs[order(probs$x), , drop = FALSE]
  probability <- as.numeric(sub('%','',probs$text,fixed=TRUE))/100
  if (any(probability<=0|probability>=1) || is.unsorted(probability,strictly=TRUE)) boe_abort('Published percentile probabilities are unordered or invalid.', 'schema_drift')
  centers <- (probs$x+probs$xmax)/2;step<-stats::median(diff(centers))
  # Numeric cells (including their parenthesised comparisons) are right aligned
  # to the percentile headings. Midpoints between heading centres can cut off
  # a signed current value at the left edge of a wide cell.
  bounds <- c(cap$x+35,probs$xmax+2)
  targets <- words[words$x >= cap$x-2 & words$x < bounds[1] & words$y > max(probs$y)+3 & words$y < max(probs$y)+100 & grepl('^[0-9]{4}$',words$text),,drop=FALSE]
  targets <- targets[order(targets$y),,drop=FALSE]
  notes <- lines[lines$x >= cap$x-2 & lines$y > max(probs$y) & lines$y < max(probs$y)+220,,drop=FALSE]
  note_text <- paste(notes$text[order(notes$y,notes$x)],collapse=' ')
  scenario <- boe_mpr_quantile_condition(note_text, rel$release)
  references <- regmatches(note_text, gregexpr(paste0("(",paste(month.name,collapse="|"),") [0-9]{4}"),note_text))[[1L]]
  references <- unique(vapply(references,boe_mpr_month_key,character(1L)))
  previous <- setdiff(references,rel$release)
  previous <- if(length(previous)==1L) previous else NA_character_
  parts<-list()
  for(i in seq_len(nrow(targets))) {
    row<-boe_mpr_pdf_row_words(words,targets$y[[i]],cap$x-2,words$width[[1]]-10)
    quarter<-row$text[grepl('^Q[1-4]$',row$text)]
    if(length(quarter)!=1L) next
    for(j in seq_len(nrow(probs))) {
      cell<-paste(row$text[row$x >= bounds[j]&row$x < bounds[j+1]],collapse=' ')
      raw<-trimws(sub(' *\\(.*$','',cell));value<-boe_mpr_pdf_number(raw)
      if(is.na(value)) boe_abort('A published percentile value changed schema.','schema_drift',raw_value=raw,source_page=words$page[[1]])
      meta<-boe_mpr_source_meta(rel,rel,url,'',sub('^(Table [0-9]+[.][A-Z]).*','\\1',cap$text),
        'pdf_published_quantiles',title,'Per cent',note_text)
      x<-boe_mpr_path(meta,paste(targets$text[[i]],quarter),as.character(value),i,j,measure,scenario,
        paste0('published_quantile_',sub('%','',probs$text[[j]],fixed=TRUE)),'Q','distribution_quantile',scenario_label=title)
      x$raw_value<-raw;x$source_cell_text<-cell;x$quantile_probability<-probability[[j]]
      x$quantile_label<-probs$text[[j]];x$policy_conditioning_label<-note_text
      x$comparison_raw_value<-if(grepl('(',cell,fixed=TRUE))sub('.*\\(([^)]*)\\).*','\\1',cell) else NA_character_
      x$comparison_forecast_vintage<-previous
      x$comparison_scenario<-if(!is.na(previous)) boe_mpr_quantile_condition(note_text,previous) else NA_character_
      x$source_page<-words$page[[1]];x$source_x_min<-bounds[j];x$source_y_min<-targets$y[[i]]
      parts[[length(parts)+1L]]<-x
    }
  }
  boe_mpr_bind(parts)
}
