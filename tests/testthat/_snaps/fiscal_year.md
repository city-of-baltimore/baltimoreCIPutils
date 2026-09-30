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

