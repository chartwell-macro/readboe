#' List official source and reuse notices
#'
#' Returns the official legal, IADB semantics, and yield-curve methodology
#' references that downstream analyses should retain with source provenance.
#' URLs are references only; this function does not fetch or validate them.
#'
#' @return A deterministic three-row data frame with columns `notice`, `url`,
#'   and `instruction`.  The rows cover Bank legal terms, IADB semantics/help,
#'   and the yield-curve disclaimer/methodology.  No provenance attribute,
#'   network request, cache write, or filesystem side effect is produced.
#'   Callers remain responsible for reviewing the linked terms and preserving
#'   any attribution required by the source.
#' @seealso [boe_provenance()], [boe_rate_series()],
#'   [read_boe_yield_curve()]
#' @examples
#' notices <- boe_rights()
#' stopifnot(identical(names(notices), c("notice", "url", "instruction")),
#'           nrow(notices) == 4L,
#'           all(grepl("^https://", notices$url)))
#' @export
boe_rights <- function() {
  data.frame(
    notice = c("Bank of England website legal terms", "IADB help and data semantics", "Yield-curve disclaimer and methodology",
      "Monetary Policy Report databanks"),
    url = c("https://www.bankofengland.co.uk/legal", "https://www.bankofengland.co.uk/boeapps/database/Help.asp?Highlight=Hierarchy&Travel=",
      "https://www.bankofengland.co.uk/statistics/yield-curves/terminology-and-concepts", "https://www.bankofengland.co.uk/legal"),
    instruction = c("Review and retain the Bank's applicable source attribution and terms.",
      "Preserve official identifiers, confidentiality markers, footnotes, provisional values, and revision fields.",
      "Yield estimates are provided for reference and research; retain the official methodology/disclaimer link.",
      "Do not assume Database OGL terms cover MPR workbooks. Establish applicable reuse permission and retain report and third-party notices."),
    stringsAsFactors = FALSE
  )
}
