test_that("join_cap_agency_labels works with AgencyID only", {
  data <- data.frame(
    AgencyID = "AGC2600",
    CostCenterID = NA_character_
  )

  result <- join_cap_agency_labels(data)

  expect_equal(result[["AgencyName"]], "Department of General Services")
  expect_equal(result[["AgencyLabel"]], "DGS")
})

test_that("join_cap_agency_labels combines DGS agencies by default", {
  data <- data.frame(
    AgencyID = "AGC4361",
    CostCenterID = NA_character_
  )

  result <- join_cap_agency_labels(data)

  expect_equal(result[["AgencyName"]], "Department of General Services")
})

test_that("join_cap_agency_labels can keep DGS agencies separate", {
  data <- data.frame(
    AgencyID = "AGC4361",
    CostCenterID = NA_character_
  )

  result <- join_cap_agency_labels(data, combine_dgs_agencies = FALSE)

  expect_equal(result[["AgencyName"]], "Baltimore City Convention Center")
})

test_that("join_cap_agency_labels resolves CostCenterID overrides", {
  data <- data.frame(
    AgencyID = "AGC7000",
    CostCenterID = "CAP009580"
  )

  result <- join_cap_agency_labels(data)

  expect_equal(result[["AgencyName"]], "Parking Authority of Baltimore City")
  expect_equal(result[["AgencyLabel"]], "Parking")
})

test_that("join_cap_agency_labels errors on missing or conflicting columns", {
  expect_error(join_cap_agency_labels(data.frame(x = 1)))

  expect_error(
    join_cap_agency_labels(
      data.frame(AgencyID = "AGC2600", CostCenterID = NA, AgencyName = "x")
    )
  )
})
