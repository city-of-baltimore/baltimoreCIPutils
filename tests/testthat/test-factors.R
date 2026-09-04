test_that("fmt_fct_recode works", {
  data <- data.frame(x = c("a", "b", "a"))

  result <- fmt_fct_recode(
    data,
    col = "x",
    levels = c("A" = "a", "B" = "b")
  )

  expect_s3_class(result[["x"]], "factor")
  expect_equal(levels(result[["x"]]), c("A", "B"))
  expect_equal(as.character(result[["x"]]), c("A", "B", "A"))
})

test_that("fmt_fct_recode returns data unchanged when levels is NULL", {
  data <- data.frame(x = c("a", "b"))

  expect_identical(
    fmt_fct_recode(data, col = "x"),
    data
  )
})

test_that("fmt_wd_proj_importance works", {
  data <- data.frame(
    "Importance Rating" = c("1 - Critical", "2 - Major", "3 - Minor"),
    check.names = FALSE
  )

  result <- fmt_wd_proj_importance(data)

  expect_equal(
    as.character(result[["Importance Rating"]]),
    c("Critical", "Major", "Minor")
  )
})

test_that("fmt_wd_proj_priority works", {
  data <- data.frame(
    "Priority" = c("1-High", "2-Medium", "3-Low"),
    check.names = FALSE
  )

  result <- fmt_wd_proj_priority(data)

  expect_equal(
    as.character(result[["Priority"]]),
    c("High", "Medium", "Low")
  )
})

test_that("fmt_wd_proj_risk works", {
  data <- data.frame(
    "Risk Level" = c("High", "Medium", "Low"),
    check.names = FALSE
  )

  result <- fmt_wd_proj_risk(data)

  expect_true(is.ordered(result[["Risk Level"]]) || is.factor(result[["Risk Level"]]))
  expect_equal(
    as.character(result[["Risk Level"]]),
    c("High", "Medium", "Low")
  )
})
