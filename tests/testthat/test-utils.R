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
