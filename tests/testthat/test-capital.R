test_that("wd_proj_join_hierarchy_labels works", {
  data <- data.frame(
    "PHierarchy1 Code" = "PJH6700",
    "PHierarchy2 Code" = "PJHCIP9510",
    check.names = FALSE
  )

  result <- wd_proj_join_hierarchy_labels(data)

  expect_equal(
    result[["PHierarchy1 Label"]],
    "Parking Authority of Baltimore City"
  )

  expect_equal(
    result[["PHierarchy2 Label"]],
    "DOT Street Lighting Group"
  )
})

test_that("wd_proj_join_hierarchy_labels handles unmatched codes", {
  data <- data.frame(
    "PHierarchy1 Code" = "PJH0000000",
    "PHierarchy2 Code" = "PJH0000000",
    check.names = FALSE
  )

  result <- wd_proj_join_hierarchy_labels(data)

  expect_true(is.na(result[["PHierarchy1 Label"]]))
  expect_true(is.na(result[["PHierarchy2 Label"]]))
})

test_that("wd_proj_join_cost_center_labels works", {
  data <- data.frame(
    "Cost Center Code" = "CAP009110",
    check.names = FALSE
  )

  result <- wd_proj_join_cost_center_labels(data)

  expect_equal(
    result[["Cost Center Agency Label"]],
    "Mayor's Office of Recovery Programs"
  )
})
