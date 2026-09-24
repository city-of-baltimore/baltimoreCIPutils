test_that("replace_lead_row_value works", {
  result <- replace_lead_row_value(mtcars[, 1:2], "cyl")

  expect_equal(
    result[["cyl"]][1:3],
    c("6", "\"", "4")
  )
})

test_that("replace_lead_row_value keeps unrepeated values", {
  data <- data.frame(col = c(1, 2, 2, 3, 3, 3))

  result <- replace_lead_row_value(data, "col", replacement = "same")

  expect_equal(
    result[["col"]],
    c("1", "2", "same", "3", "same", "same")
  )
})

test_that("filter_program_data filters by a single version", {
  data <- data.frame(
    ProgramVersion = c("Planning", "Planning", "Adopted"),
    Amount = c(100, 250, 150)
  )

  result <- filter_program_data(data, "Planning")

  expect_equal(result[["ProgramVersion"]], c("Planning", "Planning"))
  expect_named(result, names(data))
})

test_that("filter_program_data supports multiple versions and extra filters", {
  data <- data.frame(
    ProgramVersion = c("Planning", "Planning", "Adopted", "Draft"),
    Amount = c(100, 250, 150, 300)
  )

  result <- filter_program_data(
    data,
    c("Planning", "Adopted"),
    multiple = TRUE,
    Amount > 100
  )

  expect_equal(result[["ProgramVersion"]], c("Planning", "Adopted"))
  expect_equal(result[["Amount"]], c(250, 150))
})

test_that("filter_program_data requires a single version unless multiple = TRUE", {
  data <- data.frame(ProgramVersion = c("Planning", "Adopted"))

  expect_snapshot(error = TRUE, {
    filter_program_data(data, c("Adopted", "Planning"))
  })
})

test_that("filter_program_data errors informatively", {
  data <- data.frame(ProgramVersion = c("Planning", "Adopted", "Draft"))

  expect_snapshot(error = TRUE, {
    filter_program_data(data, "Plannng")
    filter_program_data(data, c("Planning", "Adopted"))
    filter_program_data(data.frame(x = 1), "Planning")
  })
})
