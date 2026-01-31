#' Convert an object to a fiscal year string or date
#'
#' [fiscal_year()] uses [lubridate::as_date()] to return a fiscal year value
#' based on any input vector coercible to dates.
#'
#' @inheritParams lubridate::as_date
#' @param type One of "year", "year_prefix", "year_prefix_abb", "date_first", or
#'   "date_last". Defaults to "year".
#' @param before Text to use as prefix if type is "year_prefix"
#'   or "year_prefix_abb". Ignored for other types. Defaults to "FY".
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
#' fiscal_year("2021-01-01", "date_last")
#'
#' @export
fiscal_year <- function(
  x,
  type = c(
    "year",
    "year_prefix",
    "year_prefix_abb",
    "date_first",
    "date_last"
  ),
  before = "FY",
  ...
) {
  type <- rlang::arg_match(type)

  if (!all(inherits(x, "Date"))) {
    x <- lubridate::as_date(x, ...)
  }

  fy <- dplyr::if_else(
    lubridate::month(x) < 7,
    lubridate::year(x),
    lubridate::year(x) + 1
  )

  switch(
    type,
    year = fy,
    year_prefix = dplyr::if_else(!is.na(fy), paste0(before, fy), NA_character_),
    year_prefix_abb = dplyr::if_else(
      !is.na(fy),
      paste0(before, substr(fy, 3, 4)),
      NA_character_
    ),
    date_first = lubridate::as_date(paste0(fy - 1, "-07-01")),
    date_last = lubridate::as_date(paste0(fy, "-06-30"))
  )
}


#' Create a fiscal year vector of specified length from a start year
#'
#' [fy_span()] is a helper function for returning a vector of specified length.
#'
#' [fy_span_label()] is a helper that returns a string serving as a label for
#' the range of the span.
#'
#' @param year Start year in span. Character or numeric coercible to integer.
#'   Required.
#' @param before Text to use as prefix if type is "year_prefix"
#'   or "year_prefix_abb". Ignored for other types. Defaults to "FY" or `c("FY",
#'   "")` for [fy_span_label()].
#' @inheritParams fiscal_year
#' @param n Number of years in span. Defaults to `NULL` (equivalent to `n = 1`)
#'   for [fy_span()] or 6 for [curr_fy_span()] and [prior_fy_span()].
#' @examples
#'
#' fy_span(2023, type = "year_prefix_abb")
#'
#' fy_span(2025, n = 2)
#'
#' fy_span_label(2025, n = 6)
#'
#' fy_span_label(2025, type = "year_prefix", n = 6)
#'
#' curr_fy_span()
#'
#' prior_fy_span()
#'
#' @export
fy_span <- function(year, before = "FY", n = 1, type = "year_prefix") {
  stopifnot(
    has_length(year, 1),
    nchar(year) == 4,
    !is.na(as.integer(year))
  )

  year <- as.integer(year)

  range <- seq(year, year + n - 1)

  fiscal_year(paste0(range, "-01-01"), before = before, type = type)
}

#' @rdname fy_span
#' @param sep Separator between first and last element in label.
#' @param ... Additional parameters passed to `fy_span()` by `fy_span_label()`.
#' @export
fy_span_label <- function(
  year,
  n = 1,
  before = c("FY", ""),
  sep = "-",
  type = "year_prefix_abb",
  ...
) {
  year <- fy_span(year, n = n, before = "", type = type, ...)

  if (!(type %in% c("year_prefix", "year_prefix_abb"))) {
    before <- c("", "")
  }

  if (n == 1) {
    return(paste0(before[1], year))
  }

  if (length(before) < 2) {
    before <- rep(before, 2)
  }

  paste0(
    before[1],
    year[1],
    sep,
    before[2],
    year[length(year)]
  )
}

#' `curr_yr_span()` defaults to using the start year set by the
#' `"baltimoreCIP.curr_yr"` option.
#'
#' @name curr_fy_span
#' @rdname fy_span
#' @export
curr_fy_span <- function(
  year = getOption("baltimoreCIP.curr_year", 2026),
  before = "FY",
  n = 6,
  type = "year_prefix"
) {
  fy_span(year, before, n, type)
}

#' `prior_yr_span()` defaults to using the start year set by the
#' `"baltimoreCIP.prior_yr"` option.
#'
#' @name prior_fy_span
#' @rdname fy_span
#' @export
prior_fy_span <- function(
  year = getOption("baltimoreCIP.prior_year", 2025),
  before = "FY",
  n = 6,
  type = "year_prefix"
) {
  fy_span(year, before, n, type)
}
