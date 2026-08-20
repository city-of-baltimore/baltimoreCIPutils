test_that("str_extract_project_code works", {
  expect_equal(
    str_extract_project_code("PRJ001145 908093 SWC 7764 Race St Box Culvert"),
    "PRJ001145"
  )

  expect_equal(
    str_extract_project_code("PRJ-001145 908093 SWC 7764 Race St Box Culvert"),
    "PRJ001145"
  )

  expect_equal(
    str_extract_project_code("no match here"),
    NA_character_
  )
})

test_that("str_extract_all_project_codes works", {
  expect_equal(
    str_extract_all_project_codes(
      c(
        "PRJ002404 PRJ002550",
        "PRJ001599 913117 Shake and Bake Recreation Center"
      )
    ),
    list("PRJ002404", "PRJ001599")
  )
})

test_that("str_extract_cip_num works", {
  expect_equal(
    str_extract_cip_num("PRJ002550 943004 504-004 ADA Infrastructure Upgrades-111"),
    "504-004"
  )

  expect_equal(
    str_extract_cip_num("no match here"),
    NA_character_
  )
})

test_that("str_extract_contract_num works", {
  expect_equal(
    str_extract_contract_num("PRJ001145 908093 SWC 7764 Race St Box Culvert"),
    "SWC7764"
  )

  expect_equal(
    str_extract_contract_num("no match here"),
    NA_character_
  )

  lifecycle::expect_deprecated(
    result <- str_extract_contract_num(
      "SWC 7764 and SWC 7765",
      extract_all = TRUE
    )
  )

  expect_equal(
    result,
    list(c("SWC7764", "SWC7765"))
  )
})

test_that("str_extract_all_contract_num works", {
  expect_equal(
    str_extract_all_contract_num(
      c("SWC 7764 and SWC 7765", "no match here")
    ),
    list(c("SWC7764", "SWC7765"), character(0))
  )
})

test_that("str_extract_dgs_asset_id works", {
  expect_equal(
    str_extract_dgs_asset_id("1 West Pratt St. (B06033)"),
    "B06033"
  )

  expect_equal(
    str_extract_dgs_asset_id("1 West Pratt St. B06033"),
    NA_character_
  )
})

test_that("str_extract_revenue_category_code works", {
  expect_equal(
    str_extract_revenue_category_code("RC1234 Some other text"),
    "RC1234"
  )

  expect_equal(
    str_extract_revenue_category_code("no match here"),
    NA_character_
  )
})
