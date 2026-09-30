test_that("summarise_timespan works", {
  data <- data.frame(
    group = c("a", "a", "b"),
    FY2025 = c(1, 2, 3),
    FY2026 = c(NA, 4, 5)
  )

  expect_no_warning(
    result <- summarise_timespan(
      data,
      timespan_cols = c("FY2025", "FY2026"),
      .by = "group"
    )
  )

  expect_equal(result[["FY2025"]], c(3, 3))
  expect_equal(result[["FY2026"]], c(4, 5))
})

test_that("summarise_timespan supports external character vectors", {
  data <- data.frame(
    group = c("a", "a", "b"),
    FY2025 = c(1, 2, 3),
    FY2026 = c(NA, 4, 5)
  )
  cols <- c("FY2025", "FY2026")
  by <- "group"

  expect_no_warning(
    result <- summarise_timespan(data, timespan_cols = cols, .by = by)
  )

  expect_equal(result[["group"]], c("a", "b"))
  expect_equal(result[["FY2025"]], c(3, 3))
  expect_equal(result[["FY2026"]], c(4, 5))
})

test_that("summarise_timespan supports tidyselect inputs", {
  data <- data.frame(
    group = c("a", "a", "b"),
    FY2025 = c(1, 2, 3),
    FY2026 = c(NA, 4, 5)
  )

  bare <- summarise_timespan(
    data,
    timespan_cols = c(FY2025, FY2026),
    .by = group
  )
  helper <- summarise_timespan(
    data,
    timespan_cols = tidyselect::starts_with("FY"),
    .by = c(group)
  )

  expect_equal(bare[["FY2025"]], c(3, 3))
  expect_equal(bare[["FY2026"]], c(4, 5))
  expect_equal(helper, bare)
})

test_that("summarise_timespan prefers columns over external variables", {
  data <- data.frame(
    group = c("a", "a", "b"),
    FY2025 = c(1, 2, 3)
  )
  group <- "FY2025"

  result <- summarise_timespan(data, timespan_cols = "FY2025", .by = group)

  expect_equal(result[["group"]], c("a", "b"))
  expect_equal(result[["FY2025"]], c(3, 3))
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


test_that("replace_na_timespan supports tidyselect inputs", {
  data <- data.frame(FY2025 = c(1, NA), FY2026 = c(NA, 2), other = c(NA, 1))

  result <- replace_na_timespan(data, tidyselect::starts_with("FY"))

  expect_equal(result[["FY2025"]], c(1, 0))
  expect_equal(result[["FY2026"]], c(0, 2))
  expect_equal(result[["other"]], c(NA, 1))
})

test_that("replace_na_timespan ignores missing character columns", {
  data <- data.frame(FY2025 = c(1, NA))

  expect_no_error(
    result <- replace_na_timespan(data, timespan_cols = c("FY2025", "FY2026"))
  )
  expect_equal(result[["FY2025"]], c(1, 0))
})

test_that("add_timespan_total adds a row-wise total", {
  data <- data.frame(
    Source = c("GO Bonds", "Federal"),
    FY2027 = c(1000, NA),
    FY2028 = c(2000, 3000),
    Notes = c("a", "b")
  )

  result <- add_timespan_total(data, timespan_cols = c("FY2027", "FY2028"))

  expect_equal(result[["Total"]], c(3000, 3000))
  expect_equal(
    names(result),
    c("Source", "FY2027", "FY2028", "Total", "Notes")
  )
})

test_that("add_timespan_total supports tidyselect and .after", {
  data <- data.frame(Source = "a", FY2027 = 1, FY2028 = 2)

  result <- add_timespan_total(
    data,
    tidyselect::starts_with("FY"),
    col = "FY Total",
    .after = Source
  )

  expect_equal(result[["FY Total"]], 3)
  expect_equal(names(result), c("Source", "FY Total", "FY2027", "FY2028"))
})

test_that("add_timespan_total does not double count an existing total", {
  data <- data.frame(FY2027 = 1, FY2028 = 2)

  once <- add_timespan_total(data, tidyselect::starts_with("FY"), col = "FY Total")
  twice <- add_timespan_total(once, tidyselect::starts_with("FY"), col = "FY Total")

  expect_equal(twice[["FY Total"]], 3)
})

test_that("add_timespan_total preserves grouping", {
  data <- dplyr::group_by(data.frame(g = c("a", "b"), FY2027 = 1:2), g)

  result <- add_timespan_total(data, "FY2027")

  expect_equal(dplyr::group_vars(result), "g")
})

test_that("summarise_program_totals works", {
  data <- data.frame(
    ProjectID = c("P1", "P1", "P1", "P2"),
    SplitChildRow = c(FALSE, FALSE, TRUE, FALSE),
    Year1 = c(100, 50, 999, NA),
    Year2 = c(200, 0, 999, 300),
    YearNotes = c("x", "y", "z", "w")
  )

  result <- summarise_program_totals(
    data,
    timespan_cols = c("Year1", "Year2"),
    amount_names = c("Year1ProgramAmount", "TotalProgramAmount")
  )

  expect_equal(
    names(result),
    c("ProjectID", "Year1ProgramAmount", "TotalProgramAmount")
  )
  expect_equal(result[["Year1ProgramAmount"]], c(150, 0))
  expect_equal(result[["TotalProgramAmount"]], c(350, 300))
})

test_that("summarise_program_totals supports multiple .by columns", {
  data <- data.frame(
    ProjectID = c("P1", "P1"),
    ProgramVersion = c("A", "B"),
    SplitChildRow = FALSE,
    Year1 = c(1, 2),
    Year2 = c(3, 4)
  )

  result <- summarise_program_totals(
    data,
    timespan_cols = c("Year1", "Year2"),
    .by = c(ProjectID, ProgramVersion)
  )

  expect_equal(result[["TotalAmount"]], c(4, 6))
})

test_that("summarise_program_totals validates inputs", {
  data <- data.frame(ProjectID = "P1", SplitChildRow = FALSE, Year1 = 1)

  expect_error(
    summarise_program_totals(data, "Year1", amount_names = "Total")
  )
  expect_error(
    summarise_program_totals(data, "Year1", first_col = "Year2")
  )
  expect_error(
    summarise_program_totals(data["Year1"], "Year1", .by = NULL)
  )
})

test_that("sum_fy_total_col is deprecated but keeps its behavior", {
  data <- dplyr::group_by(
    data.frame(Source = c("a", "b"), FY27 = c(1, NA), FY28 = c(2, 3)),
    Source
  )

  lifecycle::expect_deprecated(result <- sum_fy_total_col(data))
  expect_equal(result[["FY Total"]], c(3, 3))
  expect_false(dplyr::is_grouped_df(result))

  rlang::local_options(lifecycle_verbosity = "quiet")
  result <- sum_fy_total_col(data, cols_all_of = "FY28", col = "Total")
  expect_equal(result[["Total"]], c(2, 3))
  expect_equal(names(result), c("Source", "FY27", "FY28", "Total"))
})

test_that("sum_6yr_program_totals is deprecated but keeps its behavior", {
  data <- data.frame(
    ProjectID = c("P1", "P1"),
    SplitChildRow = c(FALSE, TRUE),
    Year1 = c(1, 10),
    Year2 = c(2, 10)
  )

  lifecycle::expect_deprecated(
    result <- sum_6yr_program_totals(data, outer_year_cols = "Year2")
  )
  expect_equal(result[["Budget Request Amount"]], 1)
  expect_equal(result[["Total Request Amount"]], 3)
})
