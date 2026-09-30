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
#' @param call Passed to [rlang::arg_match()] for error attribution.
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
  ...,
  call = caller_env()
) {
  type <- rlang::arg_match(type, error_call = call)

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
#' @param call Execution environment used in error messages.
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
#' curr_fy_span(2027)
#'
#' prior_fy_span(2026)
#'
#' # Use options to set the default start year
#' op <- options(baltimoreCIP.curr_year = 2027, baltimoreCIP.prior_year = 2026)
#'
#' curr_fy_span()
#'
#' prior_fy_span()
#'
#' options(op)
#'
#' @export
fy_span <- function(
  year,
  before = "FY",
  n = 1,
  type = "year_prefix",
  call = caller_env()
) {
  check_year(year, call = call)

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

#' [curr_fy_span()] defaults to using the start year set by the
#' `"baltimoreCIP.curr_year"` option. An error is returned if `year` is not
#' supplied and the option is not set.
#'
#' @name curr_fy_span
#' @rdname fy_span
#' @export
curr_fy_span <- function(
  year = getOption("baltimoreCIP.curr_year"),
  before = "FY",
  n = 6,
  type = "year_prefix",
  call = caller_env()
) {
  check_option_year(year, option = "baltimoreCIP.curr_year", call = call)
  fy_span(year, before, n, type, call = call)
}

#' [prior_fy_span()] defaults to using the start year set by the
#' `"baltimoreCIP.prior_year"` option. An error is returned if `year` is not
#' supplied and the option is not set.
#'
#' @name prior_fy_span
#' @rdname fy_span
#' @export
prior_fy_span <- function(
  year = getOption("baltimoreCIP.prior_year"),
  before = "FY",
  n = 6,
  type = "year_prefix",
  call = caller_env()
) {
  check_option_year(year, option = "baltimoreCIP.prior_year", call = call)
  fy_span(year, before, n, type, call = call)
}

#' Check that a year is supplied directly or with an option
#'
#' `check_option_year()` errors if `year` is `NULL`, which happens when a
#' function argument defaults to an option (e.g., `"baltimoreCIP.curr_year"`)
#' that is not set. Otherwise, `year` is validated with `check_year()` using
#' `digits` and `n`.
#'
#' @param year A year value or `NULL`.
#' @param option Name of the option used as the default value for `year`.
#' @param arg Argument name used in the error message.
#' @param call Execution environment used in the error message.
#' @returns `year`, invisibly.
#' @inheritParams check_year
#' @keywords internal
#' @noRd
check_option_year <- function(
  year,
  option = "baltimoreCIP.curr_year",
  n = 1,
  digits = 4,
  arg = caller_arg(year),
  call = caller_env()
) {
  if (!is.null(year)) {
    check_year(
      year,
      digits = digits,
      n = n,
      arg = arg,
      call = call
    )
    return(invisible(year))
  }

  cli_abort(
    c(
      "{.arg {arg}} must be supplied if the {.code {option}} option is not set.",
      "i" = "Supply {.arg {arg}} or set the option, e.g., {.code options({option} = 2027)}."
    ),
    call = call
  )
}

#' Check that a value appears to be a year
#'
#' `check_year()` errors if `year` is not a vector of length `n` with
#' non-missing values that have `digits` characters and can be coerced to
#' integers.
#'
#' @param year A year value.
#' @param digits Number of characters each value of `year` must have. Set to
#'   `NULL` to allow any number of digits. Defaults to 4.
#' @param n Required length of `year`. Set to `NULL` to allow any non-zero
#'   length. Defaults to 1.
#' @param arg Argument name used in the error message.
#' @param call Execution environment used in the error message.
#' @returns `year`, invisibly.
#' @keywords internal
#' @noRd
check_year <- function(
  year,
  digits = 4,
  n = 1,
  arg = caller_arg(year),
  call = caller_env()
) {
  if (
    has_length(year, n) &&
      !anyNA(year) &&
      (is.null(digits) || all(nchar(year) == digits)) &&
      !anyNA(suppressWarnings(as.integer(year)))
  ) {
    return(invisible(year))
  }

  expected <- "{.arg {arg}} must be"

  if (is.null(n)) {
    expected <- paste(expected, "a vector of")
  } else if (n == 1) {
    expected <- paste(expected, "a single")
  } else {
    expected <- paste(expected, "a length {n} vector of")
  }

  if (!is.null(digits)) {
    expected <- paste(expected, "{digits}-digit")
  }

  # TODO: Use cli built-in pluralization instead
  expected <- paste(expected, if (identical(n, 1)) "year" else "years")

  if (has_length(year, 1)) {
    message <- paste0(expected, ", not {.val {year}}.")
  } else {
    message <- paste0(expected, ", not {.obj_type_friendly {year}}.")
  }

  cli_abort(message, call = call)
}
