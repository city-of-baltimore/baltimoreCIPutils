#' Rename "FY" or "Year" columns from an input data frame
#'
#' Use [dplyr::rename_with()] to rename an input data frame with columns
#' prefixed by "FY" (absolute year columns) or "Year" (relative year columns).
#'
#' [rename_with_fy_abb()] abbreviates four-digit fiscal year column names,
#' e.g. "FY2027" to "FY27".
#'
#' @param .data Input data frame with "Year" or "FY" columns to rename.
#' @param existing_before Prefix text for existing column names. For
#'   [rename_with_fy_abb()], a regular expression passed to
#'   [tidyselect::matches()] and [stringr::str_replace()] that matches the text
#'   to replace. Defaults to `"^FY20"`.
#' @examples
#' rename_fy_cols(
#'   data.frame(
#'     "FY2027" = 1,
#'     "FY2028" = 2
#'   ),
#'   start_year = 2027
#' )
#'
#' rename_yr_cols(
#'   data.frame(
#'     "Year1" = 1,
#'     "Year2" = 2
#'   ),
#'   start_year = 2027
#' )
#'
#' rename_with_fy_abb(
#'   data.frame(
#'     "FY1999" = 1,
#'     "FY2000" = 2
#'   ),
#'   existing_before = "^FY(19|20)"
#' )
#'
#' @inheritParams as_relative_year
#' @export
rename_fy_cols <- function(
  .data,
  start_year = getOption("baltimoreCIP.curr_year"),
  existing_before = "FY",
  before = "Year"
) {
  check_option_year(start_year, option = "baltimoreCIP.curr_year")

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
rename_with_fy_abb <- function(
  .data,
  existing_before = "^FY20",
  before = "FY"
) {
  dplyr::rename_with(
    .data,
    \(x) {
      stringr::str_replace(x, existing_before, before)
    },
    tidyselect::matches(existing_before)
  )
}

#' @rdname rename_fy_cols
#' @export
rename_yr_cols <- function(
  .data,
  start_year = getOption("baltimoreCIP.curr_year"),
  existing_before = "Year",
  before = "FY"
) {
  check_option_year(start_year, option = "baltimoreCIP.curr_year")

  # Update FY columns to match request table convention
  dplyr::rename_with(
    .data = .data,
    .cols = tidyselect::starts_with(existing_before),
    \(x) {
      paste0(
        before,
        as.integer(stringr::str_remove(x, existing_before)) +
          as.integer(start_year) -
          1
      )
    }
  )
}

#' Convert vector of years as integer or text
#'
#' @param x A vector of integer or character values.
#' @param start_year Year to use as "Year 1". Defaults to the
#'   `"baltimoreCIP.curr_year"` option. Required if the option is not set.
#' @param existing_before Prefix text for input years. Required if `x` is a
#'   character vector.
#' @param before Prefix to use for output.
#' @examples
#' as_relative_year(2026, start_year = 2027)
#'
#' as_relative_year(c("FY24", "FY25"), start_year = 24)
#'
#' @export
as_relative_year <- function(
  x,
  start_year = getOption("baltimoreCIP.curr_year"),
  existing_before = "FY",
  before = "Year"
) {
  check_option_year(
    start_year,
    option = "baltimoreCIP.curr_year",
    n = NULL,
    digits = NULL
  )

  if (is.character(x)) {
    x <- stringr::str_remove(x, existing_before)
  }

  x <- suppressWarnings(as.integer(x))

  if (anyNA(x)) {
    cli_abort(
      "{.arg x} must be a whole number or a string that can be coerced to a whole number."
    )
  }

  # Convert first year into Year 1
  x <- x - as.integer(start_year) + 1
  before <- before %||% ""
  paste0(before, x)
}
