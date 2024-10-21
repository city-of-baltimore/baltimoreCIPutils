#' Format Adaptive Planning Capital Projects data
#'
#' [fmt_adapt_proj_details()] supports formatting for the Project Details
#' Sheet from Adaptive Planning and can be adapted for use with Capital Projects
#' data reports from Workday.
#'
#' @param date_cols Names of date columns to parse with
#'   [lubridate::parse_date_time()].
#' @param drop_cols Columns to drop from input data frame. By default these are
#'   all duplicative of retained columns or fully empty.
#' @export
fmt_adapt_proj_details <- function(data,
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

#' Format Adaptive Planing Six-Year Capital Improvement Program Data
#'
#' [fmt_adapt_6yr_program()] formats data from the Six-Year Sheet exported from
#' Adaptive Planning.
#'
#' @inheritParams fmt_wd_proj_worktags
#' @param timespan_cols Timespan columns to format using default "accounting"
#'   formatting. Passed to cols argument of [set_excel_fmt_class()]. Defaults to
#'   [curr_yr_span()].
#' @export
fmt_adapt_6yr_program <- function(data,
                                  timespan_cols = curr_yr_span(),
                                  drop_cols = c(
                                    "PCode Code", "PCode Name",
                                    "Fund, Grant, Special Purpose Code",
                                    # TODO: Check if either of the Fund,
                                    # Grant, Special Purpose Code columns
                                    # should be retained
                                    "Fund, Grant, Special Purpose Name",
                                    "RObject Code", "Grant_Detail Code"
                                  ),
                                  fund_cols = c(
                                    "FGSFund Code",
                                    "FGSFund Name"
                                  ),
                                  cost_center_cols = c(
                                    "Cost Center Code",
                                    "Cost Center Name"
                                  ),
                                  revenue_category_cols = c(
                                    "Revenue Category Code",
                                    "Revenue Category Name"
                                  )) {
  # timespan <- timespan %||% seq(current_year, current_year + 5)
  # out_years <- setdiff(timespan, current_year)

  data |>
    fmt_wd_proj_worktags(
      fund_cols = fund_cols,
      cost_center_cols = cost_center_cols
    ) |>
    fmt_wd_code_name(
      revenue_category_cols[[1]],
      revenue_category_cols[[2]],
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
    dplyr::select(!any_of(drop_cols)) |>
    set_excel_fmt_class(
      cols = timespan_cols,
      fmt_class = "accounting"
    )
}

#' Format Workday Project Hierarchy columns
#'
#' [fmt_wd_proj_hierarchy()] formats the PHierarchy1 Code and Name and
#' PHierarchy2 Code and Name columns and then joins related labels from the
#' `wd_proj_hierarchy_xwalk` reference data.
#'
#' @keywords enrichment
fmt_wd_proj_hierarchy <- function(data) {
  phierarhcy1_cols <- c(
    "PHierarchy1 Code",
    "PHierarchy1 Name"
  )

  phierarhcy2_cols <- c(
    "PHierarchy2 Code",
    "PHierarchy2 Name"
  )

  stopifnot(
    all(has_name(data, c(phierarhcy1_cols, phierarhcy2_cols)))
  )

  data |>
    fmt_wd_code_name(
      phierarhcy1_cols[[1]],
      phierarhcy1_cols[[2]],
      "PHierarchy1 Name",
      "^PJH[:digit:]+"
    ) |>
    fmt_wd_code_name(
      phierarhcy2_cols[[1]],
      phierarhcy2_cols[[2]],
      "^(PJH|CIP|PJHCIP)[:digit:]+"
    ) |>
    join_wd_proj_hierarchy_labels()
}


#' Join PHierarchy1 Label and PHierarchy2 Label columns based on
#' `wd_proj_hierarchy_xwalk` reference data
#'
#' [join_wd_proj_hierarchy_labels()] joins the internal
#' [baltimoreCIPutils::wd_proj_hierarchy_xwalk] data to the input data frame
#' using "PHierarchy1 Code" and "PHierarchy2 Code" as join columns.
#'
#' @returns A data frame with added columns "PHierarchy1 Label" and "PHierarchy2
#'   Label"
#' @keywords enrichment
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


# TODO: Add a function to join related plan data
# join_wd_proj_related_plan <- function(data) {
#
# }
