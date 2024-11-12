#' Convert an object to a fiscal year string or date
#'
#' [fiscal_year()] uses [lubridate::as_date()] to return a fiscal year value
#' based on any input vector coercible to dates.
#'
#' @param type One of "year", "year_prefix", "year_prefix_abb", "date_first", or
#'   "date_last"
#' @inheritDotParams lubridate::as_date
#' @returns An integer (if type = "year"), character, or date (if type is
#'   "date_first" or "date_last").
#' @examples
#' fiscal_year("2021-01-01")
#'
#' fiscal_year("2021-07-01")
#'
#' fiscal_year("2021-01-01", "year_prefix")
#'
#' fiscal_year("2021-01-01", "year_prefix_abb")
#'
#' fiscal_year("2021-01-01", "date_first")
#'
#' fiscal_year("2021-01-01", "date_ldate")
#'
#' @export
fiscal_year <- function(x,
                        type = c("year", "year_prefix", "year_prefix_abb",
                                 "date_first", "date_last"),
                        before = "FY",
                        ...) {
  type <- rlang::arg_match(type)

  if (!inherits(x, "Date")) {
    x <- lubridate::as_date(x, ...)
  }

  fy <- dplyr::if_else(
    lubridate::month(x) < 7,
    lubridate::year(x),
    lubridate::year(x) + 1
  )

  switch(type,
    year = fy,
    year_prefix = dplyr::if_else(!is.na(fy), paste0(before, fy), NA_character_),
    year_prefix_abb = dplyr::if_else(!is.na(fy), paste0(before, substr(fy, 3, 4)), NA_character_),
    date_first = lubridate::as_date(paste0(fy - 1, "-07-01")),
    date_last = lubridate::as_date(paste0(fy, "-06-30"))
  )
}
