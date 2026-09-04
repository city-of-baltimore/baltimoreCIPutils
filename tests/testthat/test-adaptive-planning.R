test_that("summarise_timespan works", {
  data <- data.frame(
    group = c("a", "a", "b"),
    FY2025 = c(1, 2, 3),
    FY2026 = c(NA, 4, 5)
  )

  result <- summarise_timespan(
    data,
    timespan_cols = c("FY2025", "FY2026"),
    .by = "group"
  )

  expect_equal(result[["FY2025"]], c(3, 3))
  expect_equal(result[["FY2026"]], c(4, 5))
})

test_that("replace_na_timespan works", {
  data <- data.frame(
    FY2025 = c(1, NA, 3),
    FY2026 = c(NA, NA, 6),
    other = c("a", "b", "c")
  )

  result <- replace_na_timespan(data, timespan_cols = c("FY2025", "FY2026"))

  expect_equal(result[["FY2025"]], c(1, 0, 3))
  expect_equal(result[["FY2026"]], c(0, 0, 6))
  expect_equal(result[["other"]], c("a", "b", "c"))
})

test_that("replace_na_timespan works with custom replacement", {
  data <- data.frame(FY2025 = c(1, NA))

  result <- replace_na_timespan(
    data,
    timespan_cols = "FY2025",
    replacement = -1
  )

  expect_equal(result[["FY2025"]], c(1, -1))
})

test_that("fmt_request_worktags works", {
  data <- data.frame(
    "FGSFund Code" = NA_character_,
    "FGSFund Name" = "1001 General Fund",
    "FGSGrant Code" = NA_character_,
    "FGSGrant Name" = "GRT001234 Some Grant",
    "Revenue Category Code" = "RC1234 Other text",
    check.names = FALSE
  )

  result <- fmt_request_worktags(data)

  expect_equal(result[["FGSFund Code"]], "1001")
  expect_equal(result[["FGSGrant Code"]], "GRT001234")
  expect_equal(result[["Revenue Category Code"]], "RC1234")
})

test_that("fmt_request_worktags errors on missing columns", {
  data <- data.frame(x = 1)

  expect_error(fmt_request_worktags(data))
})
