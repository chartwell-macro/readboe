# Annual profile tables can continue over several physical PDF pages. Poppler's
# layout text preserves printed column gaps while page/line coordinates remain
# available for every selected scalar. No graph values are digitised.
boe_mpr_pdf_annual_text <- function(pages, rel, url) {
  page_lines <- lapply(pages, function(x) strsplit(x, "\n", fixed = TRUE)[[1L]])
  text <- unlist(page_lines, use.names = FALSE)
  page <- rep(seq_along(page_lines), lengths(page_lines))
  line <- unlist(lapply(lengths(page_lines), seq_len), use.names = FALSE)
  captions <- vapply(text, function(x) strsplit(trimws(x), " {3,}")[[1L]][1L] %||% "", character(1L))
  starts <- which(grepl("^Table [0-9]+[.][A-Z][^\n]*(Indicative (paths|projections)|MPC key judgements)", captions, ignore.case = TRUE))
  parts <- list(); inventory <- list()
  for (start in starts) {
    title <- captions[[start]]
    if (grepl("other forecaster|external|survey", title, ignore.case = TRUE)) next
    table <- sub("^ *(Table [0-9]+[.][A-Z]).*", "\\1", text[[start]])
    after <- seq.int(start + 1L, min(length(text), start + 700L))
    boundaries <- after[grepl("^ *(Table [0-9]+[.]|Chart [0-9]+[.]|Box [A-Z]:)", text[after])]
    end <- if (length(boundaries)) boundaries[[1L]] - 1L else tail(after, 1L)
    range <- seq.int(start + 1L, end)
    year_matches <- lapply(text[range], function(x) regmatches(x, gregexpr("(?<![0-9])[0-9]{4}(?![0-9])", x, perl = TRUE))[[1L]])
    header <- which(vapply(year_matches, function(x) sum(as.integer(x) >= as.integer(substr(rel$release, 1L, 4L))) >= 2L, logical(1L)))
    if (!length(header)) next
    header <- range[header[[1L]]]
    # Only standalone year columns count; historical average ranges do not.
    fields <- trimws(strsplit(trimws(text[[header]]), " {2,}")[[1L]])
    years <- fields[grepl("^[0-9]{4}$", fields)]
    future <- which(as.integer(years) >= as.integer(substr(rel$release, 1L, 4L)))
    if (length(future) < 2L || anyDuplicated(years)) next
    year_positions <- gregexpr("(?<![0-9])[0-9]{4}(?![0-9])", text[[header]], perl = TRUE)[[1L]]
    printed_years <- regmatches(text[[header]], list(year_positions))[[1L]]
    declared_year_end <- max(year_positions[match(years, printed_years)] + 3L)
    last_year_end <- tail(year_positions, 1L) + 3L
    remainder <- substring(text[[header]], last_year_end + 1L)
    first_prose <- regexpr("[^ ]", remainder)[[1L]]
    width <- if (first_prose > 0L) last_year_end + first_prose - 1L else Inf
    sources <- range[range > header & grepl("^ *Sources?:", text[range])]
    notes <- range[range > header & grepl("^ *\\([a-z]{1,2}\\) +[A-Za-z]", text[range])]
    data_end <- min(c(end + 1L, sources, notes)) - 1L
    # Two-column reports place prose beside the table, sometimes on baselines
    # with no table text. Its repeated indented prose start establishes the
    # right edge even when the year-heading line itself has no neighbouring text.
    edges <- unlist(lapply(text[seq.int(header, data_end)], function(x) {
      matches <- gregexpr(" {12,}(?=[A-Za-z][A-Za-z ,\u2018\u2019'-]{20,})", x, perl = TRUE)[[1L]]
      if (matches[[1L]] < 0L) return(integer())
      matches + attr(matches, "match.length") - 1L
    }))
    edges <- edges[edges > max(40, 0.45 * max(nchar(text[seq.int(header, data_end)])))]
    if (length(edges)) width <- min(width, min(edges))
    if (is.finite(width)) width <- max(width, declared_year_end + 2L)
    if (is.finite(width)) {
      fields <- trimws(strsplit(trimws(substr(text[[header]], 1L, width)), " {2,}")[[1L]])
      years <- fields[grepl("^[0-9]{4}$", fields)]
      future <- which(as.integer(years) >= as.integer(substr(rel$release, 1L, 4L)))
    }
    scalar <- "[-+\u2212\u2013\u2011]?(?:[0-9]+[13]/[24]|[13]/[24]|[0-9]+(?:[.][0-9]+)?(?: ?[\u00bc\u00bd\u00be])?|[\u00bc\u00bd\u00be])"
    cell_pattern <- paste0("(?<![[:alnum:]_/])(?:", scalar, "|n[.]a[.]|n/a)(?![[:alnum:]_/])(?: *\\((?:", scalar, "|[a-z]{1,2})\\))?")
    rows <- list(); current <- NULL; expected <- NULL; pending <- character(); judgement <- FALSE; label_width <- Inf
    for (i in seq.int(header + 1L, data_end)) {
      raw <- if (is.finite(width)) substr(text[[i]], 1L, width) else text[[i]]; clean <- trimws(raw)
      if (grepl("^Key Judgement", clean)) { pending <- character(); judgement <- TRUE; next }
      if (!nzchar(clean) || grepl("^(Bank of England|Inflation Report|Monetary Policy Report|Page [0-9]+|Projections$)", clean) || grepl("^[0-9]+(\\([a-z]\\))?$", clean)) next
      fields <- trimws(strsplit(clean, " {2,}")[[1L]])
      if (sum(grepl("^[0-9]{4}$", fields)) >= 2L) { pending <- character(); judgement <- FALSE; next }
      if (judgement) next
      positions <- gregexpr(cell_pattern, raw, perl = TRUE)[[1L]]
      values <- if (positions[[1L]] < 0L) character() else regmatches(raw, list(positions))[[1L]]
      if (length(values) >= length(years) && (is.null(expected) || length(values) == expected)) {
        prefix <- trimws(substr(raw, 1L, positions[[1L]] - 1L))
        if (!nzchar(prefix) && !length(pending)) next
        if (!is.null(current)) rows[[length(rows) + 1L]] <- current
        expected <- length(values)
        label_width <- positions[[1L]] - 1L
        label <- c(pending, prefix)
        pending <- character()
        current <- list(label = label, values = values, page = page[[i]], line = line[[i]], raw_line = raw)
      } else if (!grepl("^(Per cent|Percentage|Annual average growth rate|Four.quarter growth rate|[0-9]{4}[\u2013-])", clean)) {
        # Labels can end above, beside or below their numeric baseline. Only
        # the first text column is a label; hanging parenthesised comparison
        # values in later columns must not become part of the variable name.
        fragment <- fields[[1L]]
        comparison <- grepl("^\\([^a-zA-Z]*\\)$|^[0-9]{2,4}\\([a-z]\\)$", fragment)
        if (nzchar(fragment) && !comparison) {
          if (!is.null(current) && !grepl("\\([a-z]{1,2}\\)$", paste(current$label, collapse = " "))) {
            current$label <- c(current$label, fragment)
          } else pending <- c(pending, fragment)
        }
      }
    }
    if (!is.null(current)) rows[[length(rows) + 1L]] <- current
    if (!length(rows)) next
    note_lines <- if (length(notes)) text[seq.int(min(notes), end)] else character()
    if (is.finite(width)) note_lines <- substr(note_lines, 1L, width)
    note_lines <- note_lines[!grepl("^ *(Bank of England|[0-9]+ +Inflation Report|Inflation Report|Monetary Policy Report|Section [0-9]+|[0-9]+ *$)", note_lines)]
    note_starts <- which(grepl("^ *\\([a-z]{1,2}\\) ", note_lines))
    definitions <- character()
    for (i in seq_along(note_starts)) {
      to <- if (i < length(note_starts)) note_starts[[i + 1L]] - 1L else length(note_lines)
      note <- trimws(paste(trimws(note_lines[seq.int(note_starts[[i]], to)]), collapse = " "))
      key <- sub("^\\(([a-z]{1,2})\\).*", "\\1", note)
      definitions[[key]] <- note
    }
    for (i in seq_along(rows)) {
      row <- rows[[i]]; label <- paste(row$label, collapse = " ")
      label <- gsub("([[:alpha:]])- +([[:alpha:]])", "\\1-\\2", label)
      marker <- regmatches(label, regexpr("\\([a-z]{1,2}\\)$", label))
      clean_label <- trimws(sub("\\([a-z]{1,2}\\)$", "", label))
      definition <- if (length(marker) && nzchar(marker)) unname(definitions[substr(marker, 2L, nchar(marker) - 1L)]) else NA_character_
      if (!length(definition) || is.na(definition)) definition <- paste(definitions, collapse = " ")
      measure <- boe_mpr_measure(clean_label)
      illustrative <- grepl("illustrative scenario", title, ignore.case = TRUE)
      scenario <- if (illustrative) "illustrative" else "central"
      kind <- if (measure == "bank_rate") "conditioning_assumption" else if (illustrative &&
        measure %in% c("cpi_inflation", "gdp_growth", "unemployment")) "scenario_projection" else "indicative_profile"
      contribution <- grepl("contribution", clean_label, ignore.case = TRUE)
      level <- measure %in% c("unemployment", "bank_rate", "household_saving_ratio", "output_gap",
        "participation_rate", "average_weekly_hours_worked", "credit_spreads") || grepl("ratio$", clean_label, ignore.case = TRUE)
      q4 <- grepl("Q4|fourth quarter", definition, ignore.case = TRUE)
      statistic <- if (q4) if (level || grepl("level", definition, ignore.case = TRUE)) "q4_level" else if (contribution) "q4_contribution" else "q4_four_quarter_change" else
        if (contribution) "annual_contribution" else if (level || grepl("annual average inflation", definition, ignore.case = TRUE)) "annual_average" else "annual_change"
      selected <- length(row$values) - length(years) + future
      raw_values <- trimws(sub(" *\\(.*$", "", row$values[selected]))
      values <- boe_mpr_pdf_number(raw_values)
      if (any(is.na(values) & !raw_values %in% c("-", "\u2013", "n.a.", "n/a"))) boe_abort("An annual PDF projection cell changed schema.", "schema_drift", raw_values = raw_values, source_page = row$page)
      units <- if (contribution || measure == "credit_spreads") "Percentage points" else if (
        measure == "average_weekly_hours_worked") "Hours" else "Per cent"
      meta <- boe_mpr_source_meta(rel, rel, url, "", table, "pdf_annual_profiles", clean_label, units, definition)
      x <- boe_mpr_path(meta, years[future], ifelse(is.na(values), NA_character_, as.character(values)), i, selected,
        measure, scenario, statistic, "A", kind, scenario_label = title)
      x$source_page <- row$page; x$source_line <- row$line; x$source_text_line <- row$raw_line
      x$raw_value <- raw_values; x$source_cell_text <- row$values[selected]
      parts[[length(parts) + 1L]] <- x
    }
    inventory[[length(inventory) + 1L]] <- data.frame(release = rel$release, source_edition = rel$release,
      source_sheet = table, source_page = page[[start]], layout = "pdf_annual_profiles", title = title,
      candidate_numeric_forecast = TRUE, stringsAsFactors = FALSE)
  }
  list(data = boe_mpr_bind(parts), inventory = boe_mpr_bind(inventory))
}
