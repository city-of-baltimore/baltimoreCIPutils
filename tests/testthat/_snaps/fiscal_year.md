# check_year error messages describe digits and n

    Code
      check_year(27)
    Condition
      Error:
      ! `27` must be a single 4-digit year, not 27.
    Code
      check_year(c(2027, 2028))
    Condition
      Error:
      ! `c(2027, 2028)` must be a single 4-digit year, not a double vector.
    Code
      check_year(c(2027, 28), n = 2)
    Condition
      Error:
      ! `c(2027, 28)` must be a length 2 vector of 4-digit years, not a double vector.
    Code
      check_year(c(1, NA), n = NULL, digits = NULL)
    Condition
      Error:
      ! `c(1, NA)` must be a vector of years, not a double vector.
    Code
      check_year("x", digits = NULL)
    Condition
      Error:
      ! `"x"` must be a single year, not "x".

# fiscal_year errors on invalid fiscal_start

    Code
      fiscal_year("2019-01-01", fiscal_start = 0)
    Condition
      Error:
      ! `fiscal_start` must be a whole number between 1 and 12, not the number 0.
    Code
      fiscal_year("2019-01-01", fiscal_start = 13)
    Condition
      Error:
      ! `fiscal_start` must be a whole number between 1 and 12, not the number 13.
    Code
      fiscal_year("2019-01-01", fiscal_start = 7.5)
    Condition
      Error:
      ! `fiscal_start` must be a whole number, not the number 7.5.
    Code
      fiscal_year("2019-01-01", fiscal_start = "July")
    Condition
      Error:
      ! `fiscal_start` must be a whole number, not the string "July".

# fiscal_quarter errors on invalid input

    Code
      fiscal_quarter("2019-01-01", type = "foo")
    Condition
      Error:
      ! `type` must be one of "year_prefix", "year_prefix_abb", "year", "quarter_prefix", "quarter", "year.quarter", "date_first", or "date_last", not "foo".
    Code
      fiscal_quarter("2019-01-01", fiscal_start = 0)
    Condition
      Error:
      ! `fiscal_start` must be a whole number between 1 and 12, not the number 0.

