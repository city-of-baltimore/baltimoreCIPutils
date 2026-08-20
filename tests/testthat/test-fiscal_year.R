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

  expect_equal(
    fiscal_year("2021-01-01", "date_first"),
    as.Date("2020-07-01")
  )

  expect_equal(
    fiscal_year("2021-01-01", "date_last"),
    as.Date("2021-06-30")
  )

  expect_equal(
    fiscal_year("2021-01-01", "year_prefix", before = "Fiscal Year "),
    "Fiscal Year 2021"
  )

  expect_equal(
    fiscal_year(c("2021-01-01", NA)),
    c(2021, NA)
  )

  expect_equal(
    fiscal_year(c("2021-01-01", NA), "year_prefix"),
    c("FY2021", NA)
  )

  expect_error(
    fiscal_year("2021-01-01", "invalid_type")
  )
})

test_that("fy_span works", {
  expect_equal(
    fy_span(2023, type = "year_prefix_abb"),
    "FY23"
  )

  expect_equal(
    fy_span(2025, n = 2),
    c("FY2025", "FY2026")
  )

  expect_equal(
    fy_span(2025, n = 1, type = "year"),
    2025
  )

  expect_error(
    fy_span(20255)
  )

  expect_error(
    fy_span(c(2025, 2026))
  )
})

test_that("fy_span_label works", {
  expect_equal(
    fy_span_label(2025, n = 6),
    "FY25-30"
  )

  expect_equal(
    fy_span_label(2025, type = "year_prefix", n = 6),
    "FY2025-2030"
  )

  expect_equal(
    fy_span_label(2025, n = 1),
    "FY25"
  )

  expect_equal(
    fy_span_label(2025, n = 6, type = "year"),
    "2025-2030"
  )
})

test_that("curr_fy_span works", {
  expect_equal(
    curr_fy_span(),
    fy_span(getOption("baltimoreCIP.curr_year", 2026), before = "FY", n = 6, type = "year_prefix")
  )

  expect_equal(
    curr_fy_span(2020, n = 3),
    c("FY2020", "FY2021", "FY2022")
  )
})

test_that("prior_fy_span works", {
  expect_equal(
    prior_fy_span(),
    fy_span(getOption("baltimoreCIP.prior_year", 2025), before = "FY", n = 6, type = "year_prefix")
  )

  expect_equal(
    prior_fy_span(2019, n = 3),
    c("FY2019", "FY2020", "FY2021")
  )
})
