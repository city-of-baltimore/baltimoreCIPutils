#' Create a character vector of year in a specific time span.
#'
#' @param yr Start year in span.
#' @param prefix Text before years.
#' @param n Number of years in span.
fy_span <- function(yr, prefix = "FY", n = 6) {
  paste0(prefix, seq(yr, yr + n - 1))
}

#' [curr_yr_span()] defaults to using the start year set by the
#' "baltimoreCIP.curr_yr" option.
#'
#' @rdname fy_span
curr_yr_span <- function(yr = getOption("baltimoreCIP.curr_yr", 2026), prefix = "FY", n = 6) {
  fy_span(yr, prefix, n)
}

#' [prior_yr_span()] defaults to using the start year set by the
#' "baltimoreCIP.prior_yr" option.
#'
#' @rdname fy_span
prior_yr_span <- function(yr = getOption("baltimoreCIP.prior_yr", 2025), prefix = "FY", n = 6) {
  fy_span(yr, prefix, n)
}
