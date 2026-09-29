#' Format fiscal year column values as currencies using `gt::fmt_currency()`
#'
#' [fmt_fy_span_currency()] is a wrapper for [gt::fmt_currency()] with a preset
#' column selection using [curr_fy_span()].
#'
#' @inheritParams gt::fmt_currency
#' @inheritDotParams gt::fmt_currency
#' @param year Start year for the default `columns` selection. Defaults to the
#'   `"baltimoreCIP.curr_year"` option. Required if `columns` is not supplied
#'   and the option is not set.
#' @keywords gt internal
#' @export
fmt_fy_span_currency <- function(
  data,
  columns = tidyselect::all_of(curr_fy_span(year)),
  ...,
  year = getOption("baltimoreCIP.curr_year"),
  decimals = 0,
  suffixing = "K"
) {
  check_installed("gt")

  if (missing(columns)) {
    check_option_year(year, option = "baltimoreCIP.curr_year")
  }

  gt::fmt_currency(
    data = data,
    columns = columns,
    suffixing = suffixing,
    decimals = decimals,
    ...
  )
}
