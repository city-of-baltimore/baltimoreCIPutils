test_that("fmt_wd_proj_dates works", {
  data <- data.frame(
    "Project Start Date" = c("2021-01-01", "2021-07-01"),
    "Project End Date" = c("2022-01-01", NA),
    check.names = FALSE
  )

  result <- fmt_wd_proj_dates(data)

  expect_equal(
    result[["Project Start FY"]],
    c(2021, 2022)
  )

  expect_equal(
    result[["Project End FY"]],
    c(2022, NA)
  )
})

test_that("fmt_wd_proj_dates works with custom column names", {
  data <- data.frame(
    start = "2021-01-01",
    end = "2021-01-01"
  )

  result <- fmt_wd_proj_dates(
    data,
    start_date_col = "start",
    end_date_col = "end"
  )

  expect_named(
    result,
    c("start", "end", "Project Start FY", "Project End FY")
  )
})
