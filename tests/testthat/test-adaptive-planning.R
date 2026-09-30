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
