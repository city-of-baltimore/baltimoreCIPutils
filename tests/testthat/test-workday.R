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

test_that("fmt_submit_project_rating() converts labels to Workday IDs", {
  allowed <- c("1-High" = "High", "2-Medium" = "Medium", "3-Low" = "Low")

  expect_equal(
    fmt_submit_project_rating(c("1-High", "2 - Medium", NA), allowed, "Priority"),
    c("High", "Medium", NA)
  )
  expect_error(
    fmt_submit_project_rating(c("1-High", "4-Urgent"), allowed, "Priority"),
    "4-Urgent"
  )
})

# Fixtures for fmt_submit_project_sheet_data() ----

submit_project_test_dict <- function() {
  tibble::tibble(
    Sheet = "Submit Project",
    Fields = c(
      "Project",
      "Project Description",
      "Risk Level",
      "Priority",
      "Importance Rating",
      "Objective",
      "Submit"
    ),
    `Source Type` = c(rep("Workday", 6), "Default"),
    `Workday Report Name` = c(
      "Project ID",
      "Project Description",
      "Risk Level",
      "Priority",
      "Importance Rating",
      "Objective",
      NA
    ),
    `Default Value` = c(rep(NA, 6), "Y")
  )
}

submit_project_test_proj_list <- function() {
  tibble::tibble(
    `Project ID` = c("PRJ1", "PRJ2", "PRJ3"),
    Project = c("PRJ1 Alley", "PRJ2 Bridge", "PRJ3 Park"),
    Inactive = c("No", "No", "Yes"),
    Billable = "No",
    `Capital Project` = "Yes",
    `Project Status` = "Active",
    Priority = c("1-High", "2-Medium", "3-Low"),
    `Importance Rating` = c("1 - Critical", "2 - Major", "3 - Minor"),
    `Project Description` = c("WD desc 1", "WD desc 2", "WD desc 3"),
    `Risk Level` = c("Low", "Medium", "High"),
    `Problem Statement` = NA_character_,
    Objective = c("WD obj 1", "WD obj 2", "WD obj 3"),
    `In Scope` = NA_character_,
    `Out of Scope` = NA_character_,
    `Measures of Success` = NA_character_,
    `Project Overview` = NA_character_,
    `Balancing Worktag` = "1001 Fund",
    `Fund ID` = "1001",
    `Cost Center ID` = NA_character_,
    `Grant ID` = NA_character_,
    `Project Resource Plan` = NA_character_,
    `Project Groups` = NA_character_,
    `Primary Project Hierarchy` = NA_character_,
    `Optional Project Hierarchies` = NA_character_
  )
}

submit_project_test_app_info <- function(...) {
  tibble::tibble(
    ProjectID = c("PRJ1", "PRJ2"),
    ProjectName = c("Alley Upgrade", NA),
    ProjectDescription = c("App desc 1", NA),
    RiskLevel = NA_character_,
    PriorityRating = c("3-Low", NA),
    ImportanceRating = NA_character_,
    ProblemStatement = NA_character_,
    Objective = c(NA, "App obj 2"),
    InScope = NA_character_,
    OutOfScope = NA_character_,
    SuccessMeasures = NA_character_,
    ProjectOverview = NA_character_,
    ...
  )
}

fmt_submit_project_test_sheet <- function(app_info, proj_list) {
  ref_ids <- tibble::tibble(
    `Business Object Instance` = "none",
    `Reference ID Value` = "none"
  )

  fmt_submit_project_sheet_data(
    proj_list = proj_list,
    eib_dict = submit_project_test_dict(),
    proj_app_info = app_info,
    proj_resource_plan_ids = ref_ids,
    proj_hierarchy_ids = ref_ids,
    proj_group_ids = ref_ids
  )
}

test_that("fmt_submit_project_sheet_data() only updates non-missing app values", {
  out <- fmt_submit_project_test_sheet(
    submit_project_test_app_info(),
    submit_project_test_proj_list()
  )

  # The inactive project is dropped
  expect_equal(out[["Workday Project ID"]], c("PRJ1", "PRJ2"))
  # Missing app values keep the Workday value
  expect_equal(out[["Project Description"]], c("App desc 1", "WD desc 2"))
  expect_equal(out[["Objective"]], c("WD obj 1", "App obj 2"))
  expect_equal(out[["Risk Level"]], c("Low", "Medium"))
  # Labels are converted to Workday IDs
  expect_equal(out[["Priority"]], c("Low", "Medium"))
  expect_equal(out[["Importance Rating"]], c("Critical", "Major"))
})

test_that("fmt_submit_project_sheet_data() skips inactive projects and rejects unknown ones", {
  app_info <- submit_project_test_app_info()
  app_info$ProjectID[2] <- "PRJ3"

  # PRJ3 is inactive so its app values are skipped without an error
  out <- fmt_submit_project_test_sheet(app_info, submit_project_test_proj_list())
  expect_equal(nrow(out), 2)

  app_info$ProjectID[2] <- "PRJ9"
  expect_error(
    fmt_submit_project_test_sheet(app_info, submit_project_test_proj_list()),
    "PRJ9"
  )
})

test_that("fmt_submit_project_sheet_data() rejects a Priority that is not allowed", {
  proj_list <- submit_project_test_proj_list()
  proj_list$Priority[2] <- "4-Urgent"

  expect_error(
    fmt_submit_project_test_sheet(submit_project_test_app_info(), proj_list),
    "4-Urgent"
  )
})
