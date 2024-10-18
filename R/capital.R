#' Format Adaptive Planing Six-Year Capital Improvement Program Data
#'
#' @export
fmt_adapt_capital_program <- function(data,
                                      current_year = 2026,
                                      timespan = c(2026:2031),
                                      drop_cols = c(
                                        "PCode Code", "PCode Name",
                                        "Fund, Grant, Special Purpose Code",
                                        # TODO: Check if either of the Fund, Grant, Special Purpose Code columns
                                        # should be retained
                                        "Fund, Grant, Special Purpose Name",
                                        "RObject Code", "Grant_Detail Code"
                                      )) {
  out_years <- setdiff(timespan, current_year)

  data |>
    fmt_wd_proj_worktags() |>
    fmt_wd_code_name(
      "Revenue Category Code",
      "Revenue Category Name",
      "^RC[:digit:]+"
    ) |>
    fmt_wd_code_name(
      "RAccount Code",
      "RAccount Name",
      "^([:digit:]|:)+"
    ) |>
    dplyr::mutate(
      "RAccount Code" := stringr::str_remove(.data[["RAccount Code"]], ":$")
    ) |>
    dplyr::select(!any_of(drop_cols))
}

#' Format Workday Project Hierarchy columns
#'
#' [fmt_wd_proj_hierarchy()] formats the PHierarchy1 Code and Name and
#' PHierarchy2 Code and Name columns and then joins related labels from the
#' `wd_proj_hierarchy_xwalk` reference data.
fmt_wd_proj_hierarchy <- function(data) {
  data |>
    fmt_wd_code_name(
      "PHierarchy1 Code",
      "PHierarchy1 Name",
      "^PJH[:digit:]+"
    ) |>
    fmt_wd_code_name(
      "PHierarchy2 Code",
      "PHierarchy2 Name",
      "^(PJH|CIP|PJHCIP)[:digit:]+"
    ) |>
    join_wd_proj_hierarchy_labels()
}


#' Join PHierarchy1 Label and PHierarchy2 Label columns based on
#' `wd_proj_hierarchy_xwalk` reference data
#'
#' @keywords internal
join_wd_proj_hierarchy_labels <- function(data) {
  stopifnot(
    all(has_name(data, c("PHierarchy1 Code", "PHierarchy2 Code")))
  )

  hierarchy_1_xwalk <- baltimoreCIPutils::wd_proj_hierarchy_xwalk |>
    dplyr::filter(!is.na(.data[["PHierarchy1 Code"]])) |>
    dplyr::select(
      all_of(
        c("PHierarchy1 Code",
          "PHierarchy1 Label" = "entity"
        )
      )
    )

  hierarchy_2_xwalk <- baltimoreCIPutils::wd_proj_hierarchy_xwalk |>
    dplyr::filter(!is.na(.data[["PHierarchy2 Code"]])) |>
    dplyr::select(
      all_of(
        c("PHierarchy2 Code",
          "PHierarchy2 Label" = "entity"
        )
      )
    )

  data |>
    dplyr::left_join(
      hierarchy_1_xwalk,
      by = "PHierarchy1 Code"
    ) |>
    dplyr::left_join(
      hierarchy_2_xwalk,
      by = "PHierarchy2 Code"
    ) |>
    dplyr::relocate(
      all_of(
        c("PHierarchy1 Label", "PHierarchy2 Label")
      ),
      .after = starts_with("PHierarchy")
    )
}

#' Format Adaptive Planning Capital Projects data
#'
#' @export
fmt_adapt_capital_projects <- function(data,
                                       # current_year,
                                       # timespan,
                                       date_cols = c(
                                         "Date Beg",
                                         "Date End",
                                         "Design Start Date",
                                         "Construction Start Date"
                                       ),
                                       drop_cols = c(
                                         "PStatus Code", "PDescription Code",
                                         "PPercentComplete Code",
                                         "PCode Code", "PCode Name",
                                         "Fund, Grant, Special Purpose Code",
                                         # TODO: Check if either of the Fund,
                                         # Grant, Special Purpose Code columns
                                         # should be retained
                                         "Fund, Grant, Special Purpose Name",
                                         "RObject Code", "Grant_Detail Code",
                                         # TODO: Check when/how these columns
                                         # changed in Workday
                                         "PManager Code", "PProjectOwner Code"
                                       )) {
  # out_years <- setdiff(timespan, current_year)

  data |>
    dplyr::select(
      !any_of(drop_cols)
    ) |>
    fmt_wd_proj_worktags() |>
    fmt_wd_proj_name() |>
    fmt_wd_proj_hierarchy() |>
    # Format date columns
    dplyr::mutate(
      dplyr::across(
        all_of(date_cols),
        \(x) {
          lubridate::parse_date_time(x, "m/d/y")
        }
      )
    ) |>
    # Replace invalid values with explicit NAs
    dplyr::mutate(
      dplyr::across(
        all_of(
          c("Year of Impact", "Related Plan")
        ),
        \(x) {
          dplyr::if_else(
            x %in% c("N/A", "NA", "None", "TBD"),
            NA_character_,
            x
          )
        }
      )
    )
  # TODO: Add leveled formatting for leveled columns
}

#' Join Asset ID values based on `wd_proj_asset_xwalk` reference data
#'
#' By default, [join_wd_proj_asset_id()] stores the asset ID values in a nested
#' data frame list column to allow handling of projects with multiple matching
#' assets.
#'
#' @keywords internal
join_wd_proj_asset_id <- function(data,
                                  project_code_col = "Project Code",
                                  asset_id_col = "asset_id",
                                  multiple = "nested") {
  asset_xwalk <- baltimoreCIPutils::wd_proj_asset_xwalk |>
    dplyr::select(
      all_of(c(project_code_col, asset_id_col))
    )

  if (multiple == "nested") {
    asset_xwalk <- asset_xwalk |>
      dplyr::group_by(.data[[project_code_col]]) |>
      dplyr::nest_by(
        .key = asset_id_col,
        .keep = TRUE
      )

    multiple <- "any"
  }

  data |>
    dplyr::left_join(
      asset_xwalk,
      multiple = multiple,
      by = project_code_col
    )
}

# TODO: Add a function to join related plan data
# join_wd_proj_related_plan <- function(data) {
#
# }
