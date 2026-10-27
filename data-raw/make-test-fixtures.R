if (!requireNamespace("openxlsx", quietly = TRUE)) stop("openxlsx is required")
root <- normalizePath(if (length(commandArgs(trailingOnly = TRUE))) commandArgs(trailingOnly = TRUE)[[1L]] else ".")
fixtures <- file.path(root, "tests", "testthat", "fixtures")
dir.create(fixtures, recursive = TRUE, showWarnings = FALSE)

yield <- openxlsx::createWorkbook()
openxlsx::addWorksheet(yield, "4. spot curve")
openxlsx::writeData(yield, "4. spot curve", "Fixture yield curve", startRow = 1, startCol = 1, colNames = FALSE)
openxlsx::writeData(yield, "4. spot curve", x = matrix(c(NA, 0.5, 1.0, 2.0, 5.0, 10.0), nrow = 1), startRow = 4, colNames = FALSE)
openxlsx::writeData(yield, "4. spot curve", x = data.frame(
  date = as.Date(c("2024-01-02", "2024-01-02", "2024-01-03")),
  m0.5 = c(1.0, 1.1, NA), m1 = c(1.2, 1.3, 1.4), m2 = c(1.5, 1.6, 1.7),
  m5 = c(1.8, 1.9, 2.0), m10 = c(2.1, 2.2, 2.3)), startRow = 6, colNames = FALSE)
openxlsx::saveWorkbook(yield, file.path(fixtures, "yield.xlsx"), overwrite = TRUE)

rate <- openxlsx::createWorkbook()
openxlsx::addWorksheet(rate, "HISTORICAL SINCE 1694")
openxlsx::writeData(rate, "HISTORICAL SINCE 1694", "Fixture Bank Rate history", startRow = 1, startCol = 1, colNames = FALSE)
openxlsx::writeData(rate, "HISTORICAL SINCE 1694", x = data.frame(
  year = c("1694", NA, "1975", NA), day = c(NA, NA, "20", "27"),
  month = c("Oct", "Nov", "Jan", "Jan"), rate = c("6", "5", "11.25", "11")),
  startRow = 8, colNames = FALSE)
openxlsx::saveWorkbook(rate, file.path(fixtures, "bank-rate.xlsx"), overwrite = TRUE)

book <- openxlsx::createWorkbook()
openxlsx::addWorksheet(book, "Data")
openxlsx::writeData(book, "Data", data.frame(a = c(1, NA), b = c("x", "y")), colNames = FALSE)
openxlsx::saveWorkbook(book, file.path(fixtures, "workbook.xlsx"), overwrite = TRUE)
