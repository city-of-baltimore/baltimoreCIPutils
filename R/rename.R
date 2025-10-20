
#' Rename "FY" or "Year" columns from an input data frame
#'
#' Use [dplyr::rename_with()] to rename an input data frame with columns
#' prefixed by "FY" (absolute year columns) or "Year" (relative year columns).
#'
#' @param .data Input data frame with "Year" or "FY" columns to rename.
#' @inheritParams as_relative_year
#' @examples
#' rename_fy_cols(
#'   data.frame(
#'     "FY2027" = 1,
#'     "FY2028" = 2
#'   )
#' )
#'
#' rename_yr_cols(
#'   data.frame(
#'     "Year1" = 1,
#'     "Year2" = 2
#'   )
#' )
#' @inheritParams as_relative_year
#' @export
rename_fy_cols <- function(
  .data,
  start_year = 2027,
  existing_before = "FY",
  before = "Year"
) {
  # Update FY columns to match request table convention
  dplyr::rename_with(
    .data = .data,
    .cols = tidyselect::starts_with(existing_before),
    \(x) {
      as_relative_year(
        x,
        start_year = start_year,
        before = before,
        existing_before = existing_before
      )
    }
  )
}

#' @rdname rename_fy_cols
#' @export
rename_yr_cols <- function(
  .data,
  start_year = 2027,
  existing_before = "Year",
  before = "FY"
) {
  # Update FY columns to match request table convention
  dplyr::rename_with(
    .data = .data,
    .cols = tidyselect::starts_with(existing_before),
    \(x) {
      paste0(
        before,
        as.integer(stringr::str_remove(x, existing_before)) + start_year - 1
      )
    }
  )
}

#' Convert vector of years as integer or text
#'
#' @param x A vector of integer or character values.
#' @param start_year Year to use as "Year 1"
#' @param existing_before Prefix text for input years. Required if `x`` is a
#' character vector.
#' @param before Prefix to use for output.
#' @examples
#' as_relative_year(2026, start_year = 2027)
#'
#' as_relative_year(c("FY24", "FY25"), start_year = 24)
#'
#' @export
as_relative_year <- function(
  x,
  start_year = 2027,
  existing_before = "FY",
  before = "Year"
) {
  if (is.character(x)) {
    x <- stringr::str_remove(x, existing_before)
  }
  # Convert first year into Year 1
  x <- as.integer(x) - start_year + 1
  before <- before %||% ""
  paste0(before, x)
}
