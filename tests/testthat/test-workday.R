test_that("fmt_wd_code_name works", {
  data <- data.frame(
    code = "PRJ001145",
    name = "PRJ001145 Race St Box Culvert"
  )

  result <- fmt_wd_code_name(
    data,
    code_col = "code",
    name_col = "name",
    code_pattern = "^PRJ[:digit:]+"
  )

  expect_equal(result[["code"]], "PRJ001145")
  expect_equal(result[["name"]], "Race St Box Culvert")
})

test_that("fmt_wd_code_name works with new column names", {
  data <- data.frame(
    code = "PRJ001145",
    name = "PRJ001145 Race St Box Culvert"
  )

  result <- fmt_wd_code_name(
    data,
    code_col = "code",
    name_col = "name",
    code_pattern = "^PRJ[:digit:]+",
    new_name_col = "name_short"
  )

  expect_named(result, c("code", "name", "name_short"))
  expect_equal(result[["name_short"]], "Race St Box Culvert")
})

test_that("fmt_wd_proj_name works", {
  data <- data.frame(
    "Project Code" = c("PRJ001145", NA),
    "Project Name" = c(
      "PRJ001145 908093 504-004 Race St Box Culvert PRJ001145",
      "PRJ001146 Some other project"
    ),
    check.names = FALSE
  )

  result <- fmt_wd_proj_name(data)

  # Row with NA project code is dropped
  expect_equal(nrow(result), 1)
  expect_equal(result[["Project Name Short"]], "Race St Box Culvert ")
})

test_that("fmt_wd_proj_worktags works", {
  data <- data.frame(
    "FGSFund Code" = "1001 General Fund",
    "FGSFund Name" = "1001 General Fund",
    "Cost Center Code" = "CAP009110 Mayor's Office of Recovery Programs",
    "Cost Center Name" = "CAP009110 Mayor's Office of Recovery Programs",
    check.names = FALSE
  )

  result <- fmt_wd_proj_worktags(data)

  expect_equal(result[["FGSFund Code"]], "1001")
  expect_equal(result[["FGSFund Name"]], "General Fund")
  expect_equal(result[["Cost Center Code"]], "CAP009110")
  expect_equal(
    result[["Cost Center Agency Label"]],
    "Mayor's Office of Recovery Programs"
  )
})

test_that("fmt_wd_proj_worktags errors on missing columns", {
  data <- data.frame(x = 1)

  expect_error(fmt_wd_proj_worktags(data))
})
