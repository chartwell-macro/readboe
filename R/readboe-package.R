#' readboe: Reproducible Bank of England statistical data
#'
#' `readboe` helps analysts and data engineers discover exact Bank of England
#' statistical identities, retrieve canonical IADB observations and official
#' bulk assets, parse losslessly within explicit resource bounds, and retain
#' source identity and revision metadata.  Its offline catalogue and static
#' registries make discovery deterministic; remote readers use allowlisted
#' HTTPS, caller-owned deadlines, managed cache replay, and typed conditions.
#'
#' The package covers the IADB XML/CSV/value-update surfaces, the current
#' Bankstats catalogue and bulk workbooks, official Bank Rate history, fitted
#' yield-curve files, an audited registry of statistical release families, and
#' saved Inflation Report/MPR forecasts with explicit calendar-aware updates.
#' It does not aggregate, interpolate, seasonally adjust, fill missing values,
#' silently coerce official markers, or infer identifiers outside the pinned
#' catalogue.  Release-asset browsing remains unavailable in source versions
#' without the verified `release-assets.rds` snapshot.
#'
#' @seealso [boe_catalogue()], [read_boe_series()],
#'   [read_boe_bankstats_series()], [boe_provenance()], [boe_rights()]
#' @author Chartwell Macro
#' @examples
#' rates <- boe_rate_series()
#' stopifnot(is.data.frame(rates), "IUDBEDR" %in% rates$series_id)
#' @importFrom jsonlite toJSON
#' @importFrom stats na.omit
#' @importFrom utils head tail
"_PACKAGE"

utils::globalVariables(c("."))
