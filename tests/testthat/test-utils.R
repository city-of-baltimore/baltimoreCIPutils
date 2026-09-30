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

local_child_file <- function(lines, env = parent.frame()) {
  path <- tempfile(fileext = ".Rmd")
  writeLines(lines, path)
  do.call(on.exit, list(call("unlink", path), add = TRUE), envir = env)
  path
}

test_that("knit_children knits a child for each row with .params", {
  child <- local_child_file("- Project `r project_id` (`r program_version`)")

  output <- capture.output(
    knit_children(
      data.frame(project_id = c("PRJ001", "PRJ002")),
      .input = child,
      .params = list(program_version = "FY2027")
    )
  )

  expect_contains(
    output,
    c("- Project PRJ001 (FY2027)", "- Project PRJ002 (FY2027)")
  )
})

test_that("knit_children uses .params over columns with the same name", {
  child <- local_child_file("`r version`")

  output <- capture.output(
    knit_children(
      data.frame(version = "column"),
      .input = child,
      .params = list(version = "param")
    )
  )

  expect_contains(output, "param")
  expect_false(any(grepl("column", output)))
})

test_that("knit_children returns no output for empty input", {
  child <- local_child_file("`r project_id`")

  output <- capture.output(
    knit_children(data.frame(project_id = character()), .input = child)
  )

  expect_false(any(nzchar(output)))
})

test_that("knit_children validates inputs", {
  child <- local_child_file("text")
  data <- data.frame(x = 1)

  expect_error(knit_children("x", .input = child))
  expect_error(knit_children(data, .input = c(child, child)))
  expect_error(knit_children(data, .input = tempfile(fileext = ".Rmd")))
  expect_error(knit_children(data, .input = child, .params = list(1)))
  expect_error(knit_children(data, .input = child, .quiet = NA))
  expect_error(knit_children(list(1), .input = child))
  expect_error(knit_children(data, .input = child, .envir = "x"))
})

test_that("knit_children uses the calling environment as the parent", {
  child <- local_child_file("`r env` / `r exists('.input')`")
  # `env` would be masked by rlang::env() if children inherited from the
  # package namespace
  env <- "caller value"

  output <- capture.output(
    knit_children(data.frame(x = 1), .input = child)
  )

  expect_contains(output, "caller value / FALSE")
})

test_that("knit_children supports a custom .envir", {
  child <- local_child_file("`r x` / `r y`")
  envir <- rlang::env(y = "from envir")

  output <- capture.output(
    knit_children(data.frame(x = "from row"), .input = child, .envir = envir)
  )

  expect_contains(output, "from row / from envir")
})

test_that("nested knit_children calls can use objects from the calling child", {
  inner <- local_child_file("`r agency` / `r project_id` / `r outer_obj`")
  outer <- local_child_file(c(
    "```{r}",
    "#| results: asis",
    "#| echo: false",
    "outer_obj <- paste('created in', agency)",
    "knit_children(data.frame(project_id = 'P1'), .input = inner)",
    "```"
  ))

  output <- capture.output(
    knit_children(data.frame(agency = "DPW"), .input = outer)
  )

  expect_contains(output, "DPW / P1 / created in DPW")
})
