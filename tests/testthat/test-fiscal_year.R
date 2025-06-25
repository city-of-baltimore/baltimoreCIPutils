test_that("fiscal_year works", {
  expect_equal(
    fiscal_year("2021-01-01"),
    2021
  )

  expect_equal(
    fiscal_year("2021-07-01"),
    2022
  )

  expect_equal(
    fiscal_year("2021-01-01", "year_prefix"),
    "FY2021"
  )

  expect_equal(
    fiscal_year("2021-01-01", "year_prefix_abb"),
    "FY21"
  )

  #' fiscal_year("2021-01-01", "date_first")
  #'
  #' fiscal_year("2021-01-01", "date_last")
})
