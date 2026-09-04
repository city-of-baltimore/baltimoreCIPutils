test_that("wd_proj_filter works with cost_center", {
  data <- data.frame(
    "Cost Center Code" = c("CAP009197", "CAP009198"),
    "PHierarchy1 Code" = c("PJH1234", "PJH5678"),
    "PHierarchy2 Code" = c(NA, NA),
    check.names = FALSE
  )

  result <- wd_proj_filter(data, cost_center = "CAP009197")

  expect_equal(nrow(result), 1)
  expect_equal(result[["Cost Center Code"]], "CAP009197")
})

test_that("wd_proj_filter works with hierarchy", {
  data <- data.frame(
    "Cost Center Code" = c("CAP009197", "CAP009198"),
    "PHierarchy1 Code" = c("PJH1234", "PJH5678"),
    "PHierarchy2 Code" = c(NA, NA),
    check.names = FALSE
  )

  result <- wd_proj_filter(data, hierarchy = "PJH1234")

  expect_equal(nrow(result), 1)
  expect_equal(result[["PHierarchy1 Code"]], "PJH1234")
})

test_that("wd_proj_filter errors on invalid input", {
  data <- data.frame(
    "Cost Center Code" = "CAP009197",
    check.names = FALSE
  )

  expect_error(wd_proj_filter(data, cost_center = "not-a-code"))
  expect_error(wd_proj_filter(data, hierarchy = "not-a-hierarchy-code"))
})

test_that("as_proj_status works", {
  result <- as_proj_status(c("Design", "Construction", "Bogus"))

  expect_s3_class(result, "factor")
  expect_true(is.ordered(result))
  expect_equal(as.character(result[1:2]), c("Design", "Construction"))
  expect_true(is.na(result[3]))
})

test_that("vec_as_str_list_value works", {
  expect_equal(
    vec_as_str_list_value(c("a", "b", "c")),
    '"a,b,c"'
  )
})

test_that("set_excel_fmt_class works", {
  data <- data.frame(x = 1, y = 2)

  result <- set_excel_fmt_class(data, cols = c("x", "y"), fmt_class = "currency")

  expect_true("currency" %in% class(result[["x"]]))
  expect_true("currency" %in% class(result[["y"]]))
})

test_that("set_excel_fmt_class errors on invalid fmt_class", {
  data <- data.frame(x = 1)

  expect_error(set_excel_fmt_class(data, cols = "x", fmt_class = "invalid"))
})
