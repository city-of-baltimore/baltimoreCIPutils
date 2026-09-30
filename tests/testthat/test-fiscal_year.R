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

  expect_error(fy_span("abcd"), class = "rlang_error")
  expect_error(fy_span(NA), class = "rlang_error")
  expect_error(fy_span(NULL), class = "rlang_error")
})

test_that("fy_span errors are attributed to the caller", {
  wrapper <- function(year) fy_span(year, call = rlang::current_env())
  err <- rlang::catch_cnd(wrapper(25), "rlang_error")
  expect_equal(conditionCall(err), quote(wrapper(25)))

  err <- rlang::catch_cnd(fy_span_label(25), "rlang_error")
  expect_equal(conditionCall(err), quote(fy_span_label(25)))
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
  rlang::local_options(baltimoreCIP.curr_year = 2028)

  expect_equal(
    curr_fy_span(),
    c("FY2028", "FY2029", "FY2030", "FY2031", "FY2032", "FY2033")
  )

  expect_equal(
    curr_fy_span(2020, n = 3),
    c("FY2020", "FY2021", "FY2022")
  )
})

test_that("curr_fy_span errors if year is missing", {
  rlang::local_options(baltimoreCIP.curr_year = NULL)

  expect_error(curr_fy_span(), "baltimoreCIP.curr_year")

  expect_equal(curr_fy_span(2020, n = 1), "FY2020")
})

test_that("prior_fy_span works", {
  rlang::local_options(baltimoreCIP.prior_year = 2027)

  expect_equal(
    prior_fy_span(),
    c("FY2027", "FY2028", "FY2029", "FY2030", "FY2031", "FY2032")
  )

  expect_equal(
    prior_fy_span(2019, n = 3),
    c("FY2019", "FY2020", "FY2021")
  )
})

test_that("prior_fy_span errors if year is missing", {
  rlang::local_options(baltimoreCIP.prior_year = NULL)

  expect_error(prior_fy_span(), "baltimoreCIP.prior_year")
})

test_that("curr_fy_span and prior_fy_span errors are attributed to the caller", {
  rlang::local_options(
    baltimoreCIP.curr_year = NULL,
    baltimoreCIP.prior_year = NULL
  )

  curr_wrapper <- function() curr_fy_span()
  err <- rlang::catch_cnd(curr_wrapper(), "rlang_error")
  expect_equal(conditionCall(err), quote(curr_wrapper()))

  prior_wrapper <- function(year) prior_fy_span(year)
  err <- rlang::catch_cnd(prior_wrapper("x"), "rlang_error")
  expect_equal(conditionCall(err), quote(prior_wrapper("x")))
})

test_that("check_year accepts valid years", {
  expect_no_error(check_year(2027))
  expect_no_error(check_year("2027"))
  expect_no_error(check_year(c(2027, 2028), n = 2))
  expect_no_error(check_year(c(2027, 2028, 2029), n = NULL))
  expect_no_error(check_year(27, digits = 2))
  expect_no_error(check_year(1, digits = NULL))
  expect_identical(check_year(2027), 2027)
})

test_that("check_year rejects invalid years", {
  expect_error(check_year(27), class = "rlang_error")
  expect_error(check_year("abcd"), class = "rlang_error")
  expect_error(check_year(NA), class = "rlang_error")
  expect_error(check_year(NULL), class = "rlang_error")
  expect_error(check_year(c(2027, 2028)), class = "rlang_error")
  expect_error(check_year(2027, n = 2), class = "rlang_error")
  expect_error(check_year(c(2027, 28), n = 2), class = "rlang_error")
  expect_error(check_year(c(2027, NA), n = NULL), class = "rlang_error")
  expect_error(check_year(2027, digits = 2), class = "rlang_error")
  expect_error(check_year("x", digits = NULL), class = "rlang_error")
})

test_that("check_year error messages describe digits and n", {
  expect_snapshot(error = TRUE, {
    check_year(27)
    check_year(c(2027, 2028))
    check_year(c(2027, 28), n = 2)
    check_year(c(1, NA), n = NULL, digits = NULL)
    check_year("x", digits = NULL)
  })
})

test_that("check_option_year passes digits and n to check_year", {
  expect_no_error(check_option_year(24, n = NULL, digits = NULL))
  expect_no_error(check_option_year(c(2027, 2028), n = 2))
  expect_error(check_option_year(24), class = "rlang_error")
  expect_error(check_option_year(NULL), "baltimoreCIP.curr_year")
})
