#' Format fiscal year column values as currencies using `gt::fmt_currency()`
#'
#' [fmt_fy_span_currency()] is a wrapper for [gt::fmt_currency()] with a preset
#' column selection using [curr_fy_span()].
#'
#' @inheritParams gt::fmt_currency
#' @inheritDotParams gt::fmt_currency
#' @keywords gt internal
#' @export
fmt_fy_span_currency <- function(
  data,
  columns = tidyselect::all_of(curr_fy_span(year)),
  ...,
  year = getOption("baltimoreCIP.curr_year", 2026),
  decimals = 0,
  suffixing = "K"
) {
  check_installed("gt")

  gt::fmt_currency(
    data = data,
    columns = columns,
    suffixing = suffixing,
    decimals = decimals,
    ...
  )
}
