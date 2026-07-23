#' Join Capital Agency labels and names based on AgencyID and CostCenterID columns
#'
#' [join_cap_agency_labels()] was created to support agency name assignments for
#' projects starting in the FY27-32 CIP process based on an AgencyID and
#' CostCenterID column. The function is similar to
#' [wd_proj_join_cost_center_labels()].
#'
#'
#' @param data Input data was with AgencyID and CostCenterID columns and no
#' existing columns named AgencyName, AgencyLabel, or AgencyWorktag.
#' @param ... Ignored.
#' @param combine_dgs_agencies If `TRUE`, set the AgencyName value for the
#' Convention Center and Enoch Pratt Free Library to the Department of General
#' Services.
#' @export
join_cap_agency_labels <- function(
  data,
  ...,
  combine_dgs_agencies = TRUE
) {
  stopifnot(
    rlang::has_name(data, c("AgencyID", "CostCenterID")),
    !any(rlang::has_name(data, c("AgencyName", "AgencyLabel", "AgencyWorktag")))
  )

  # fmt:skip
  cap_agencies <- tibble::tribble(
    ~AgencyName,                                       ~AgencyLabel,     ~AgencyWorktag,                                            ~AgencyID, ~CostCenter,                                             ~CostCenterID,
    "Baltimore City Information Technology",           "BCIT",           "AGC4303 M-R Office of Information and Technology", "AGC4303", NA,                                                      NA,
    "Baltimore City Public Schools",                   "BCPSS",          "AGC4371 M-R Baltimore City Public Schools",        "AGC4371", NA,                                                      NA,
    "Department of Recreation and Parks",              "BCRP",           "AGC6300 Recreation and Parks",                     "AGC6300", NA,                                                      NA,
    "Department of General Services",                  "DGS",            "AGC2600 General Services",                         "AGC2600", NA,                                                      NA,
    "Baltimore City Convention Center",                "DGS-BCC",        "AGC4361 M-R Convention Complex",                   "AGC4361", NA,                                                      NA,
    "Enoch Pratt Free Library",                        "DGS-Library",    "AGC3900 Enoch Pratt Free Library",                 "AGC3900", NA,                                                      NA,
    "Department of Housing and Community Development", "DHCD",           "AGC3100 Housing and Community Development",        "AGC3100", NA,                                                      NA,
    "Department of Finance",                           "DOF",            "AGC2300 Finance",                                  "AGC2300", NA,                                                      NA,
    "Department of Planning",                          "DOP",            "AGC5700 Planning",                                 "AGC5700", NA,                                                      NA,
    "Department of Transportation",                    "DOT",            "AGC7000 Transportation",                           "AGC7000", NA,                                                      NA,
    "Department of Public Works",                      "DPW",            "AGC6100 Public Works",                             "AGC6100", NA,                                                      NA,
    "Baltimore City Mayor's Office",                   "Mayoralty",      "AGC4301 Mayoralty",                                "AGC4301", NA,                                                      NA,
    "Mayor's Office of Recovery Programs",             "MORP",           "AGC4392 M-R Office of Recovery Programs",          "AGC4392", NA,                                                      NA,
    "Parking Authority of Baltimore City",             "Parking",        "AGC7000 Transportation",                           "AGC7000", "CAP009580 CAP Transportation",                          "CAP009580",
    "Baltimore Development Corporation",               "BDC",            "AGC3100 Housing and Community Development",        "AGC3100", "CAP009601 CAP Baltimore Development Corporation (BDC)", "CAP009601",
    "Baltimore City Police Department",                "BPD",            "AGC5900 Police",                                   "AGC5900", NA,                                                      NA,
    "Baltimore City Fire Department",                  "BCFD",           "AGC2500 Fire",                                     "AGC2500", NA,                                                      NA,
    "Baltimore City Health Department",                "BCHD",           "AGC2700 Health",                                   "AGC2700", NA,                                                      NA,
    "Baltimore City Comptroller's Office",             "Comptroller",    "AGC1200 Comptroller",                              "AGC1200", NA,                                                      NA
  )

  data <- data |>
    dplyr::left_join(
      cap_agencies |>
        dplyr::filter(is.na(CostCenterID)) |>
        dplyr::select(
          AgencyName,
          AgencyLabel,
          AgencyID,
          AgencyWorktag
        ),
      by = dplyr::join_by(AgencyID)
    ) |>
    dplyr::left_join(
      cap_agencies |>
        dplyr::filter(!is.na(CostCenterID)) |>
        dplyr::select(
          AgencyName,
          AgencyLabel,
          AgencyID,
          AgencyWorktag,
          CostCenterID
        ),
      by = dplyr::join_by(AgencyID, CostCenterID),
      suffix = c("", "Corrected")
    ) |>
    dplyr::mutate(
      AgencyName = dplyr::coalesce(
        AgencyNameCorrected,
        AgencyName
      ),
      AgencyLabel = dplyr::coalesce(
        AgencyLabelCorrected,
        AgencyLabel
      )
    ) |>
    dplyr::select(
      !c(AgencyNameCorrected, AgencyLabelCorrected, AgencyWorktagCorrected)
    ) |>
    dplyr::relocate(
      AgencyName,
      .before = AgencyID
    )

  # TODO: Determine if the BPD, BCFD, and BCHD projects should be classed as DGS projects
  if (combine_dgs_agencies) {
    data <- data |>
      dplyr::mutate(
        AgencyName = dplyr::if_else(
          AgencyName %in%
            c(
              "Baltimore City Convention Center",
              "Enoch Pratt Free Library"
            ),
          "Department of General Services",
          AgencyName
        )
      )
  }

  data
}
