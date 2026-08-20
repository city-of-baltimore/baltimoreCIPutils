test_that("as_relative_year works", {
  expect_equal(
    as_relative_year(2026, start_year = 2027),
    "Year0"
  )

  expect_equal(
    as_relative_year(c("FY24", "FY25"), start_year = 24),
    c("Year1", "Year2")
  )

  expect_equal(
    as_relative_year(2027, start_year = 2027, before = NULL),
    "1"
  )
})

test_that("rename_fy_cols works", {
  expect_named(
    rename_fy_cols(
      data.frame(
        "FY2027" = 1,
        "FY2028" = 2,
        check.names = FALSE
      )
    ),
    c("Year1", "Year2")
  )

  expect_named(
    rename_fy_cols(
      data.frame(
        "FY2027" = 1,
        "Name" = "x",
        check.names = FALSE
      )
    ),
    c("Year1", "Name")
  )
})

test_that("rename_yr_cols works", {
  expect_named(
    rename_yr_cols(
      data.frame(
        "Year1" = 1,
        "Year2" = 2,
        check.names = FALSE
      )
    ),
    c("FY2027", "FY2028")
  )
})

test_that("rename_fy_cols and rename_yr_cols are inverses", {
  fy_data <- data.frame(
    "FY2027" = 1,
    "FY2028" = 2,
    check.names = FALSE
  )

  expect_named(
    rename_yr_cols(rename_fy_cols(fy_data)),
    names(fy_data)
  )
})
