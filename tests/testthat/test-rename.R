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

test_that("rename functions use the baltimoreCIP.curr_year option", {
  rlang::local_options(baltimoreCIP.curr_year = 2028)

  expect_named(
    rename_yr_cols(data.frame("Year1" = 1, check.names = FALSE)),
    "FY2028"
  )

  expect_named(
    rename_fy_cols(data.frame("FY2028" = 1, check.names = FALSE)),
    "Year1"
  )

  expect_equal(as_relative_year(2029), "Year2")
})

test_that("rename functions error if start_year is missing", {
  rlang::local_options(baltimoreCIP.curr_year = NULL)

  fy_data <- data.frame("FY2027" = 1, check.names = FALSE)

  expect_error(rename_fy_cols(fy_data), "baltimoreCIP.curr_year")
  expect_error(
    rename_yr_cols(data.frame("Year1" = 1, check.names = FALSE)),
    "baltimoreCIP.curr_year"
  )
  expect_error(as_relative_year(2027), "baltimoreCIP.curr_year")

  expect_named(rename_fy_cols(fy_data, start_year = 2027), "Year1")
})

test_that("rename_with_fy_abb abbreviates four-digit fiscal year columns", {
  fy_data <- data.frame(
    "FY2027" = 1,
    "FY2028" = 2,
    "Project" = "A",
    check.names = FALSE
  )

  result <- rename_with_fy_abb(fy_data)

  expect_named(result, c("FY27", "FY28", "Project"))
  expect_equal(unname(unlist(result[1, ])), unname(unlist(fy_data[1, ])))
})

test_that("rename_with_fy_abb uses existing_before and before", {
  expect_named(
    rename_with_fy_abb(
      data.frame("FY1998" = 1, "FY2001" = 2, check.names = FALSE),
      existing_before = "^FY19"
    ),
    c("FY98", "FY2001")
  )

  expect_named(
    rename_with_fy_abb(
      data.frame("FY2027" = 1, check.names = FALSE),
      before = "Year "
    ),
    "Year 27"
  )
})

test_that("rename_with_fy_abb leaves unmatched columns unchanged", {
  data <- data.frame("Year1" = 1, "Total FY2027" = 2, check.names = FALSE)

  expect_identical(rename_with_fy_abb(data), data)
})
