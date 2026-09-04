test_that("filter_cip_empty_rows works", {
  data <- data.frame(
    id = c(1, 2, 3),
    FY2025 = c(0, NA, 100),
    FY2026 = c(0, NA, 0)
  )

  result <- filter_cip_empty_rows(data, timespan_cols = c("FY2025", "FY2026"))

  expect_equal(result[["id"]], 3)
})
