#' Format Adaptive Planning Capital Projects data
#'
#' [fmt_adapt_proj_details()] supports formatting for the Project Details
#' Sheet from Adaptive Planning and can be adapted for use with Capital Projects
#' data reports from Workday.
#'
#' @param data A data frame with columns matching `date_cols` and additional
#'   columns ("Year of Impact" and "Related Plan").
#' @param date_cols Names of date columns to parse with
#'   [lubridate::parse_date_time()].
#' @param drop_cols Columns to drop from input data frame. By default these are
#'   all duplicative of retained columns or fully empty.
#' @export
fmt_adapt_proj_details <- function(
  data,
  date_cols = c(
    "Date Beg",
    "Date End",
    "Design Start Date",
    "Construction Start Date"
  ),
  drop_cols = c(
    "PStatus Code",
    "PDescription Code",
    "PPercentComplete Code",
    "PCode Code",
    "PCode Name",
    "Fund, Grant, Special Purpose Code",
    # TODO: Check if either of the Fund,
    # Grant, Special Purpose Code columns
    # should be retained
    "Fund, Grant, Special Purpose Name",
    "RObject Code",
    "Grant_Detail Code",
    # TODO: Check when/how these columns
    # changed in Workday
    "PManager Code",
    "PProjectOwner Code"
  )
) {
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
#' @param timespan_cols Time span columns to format using default "accounting"
#'   formatting. Passed to cols argument of [set_excel_fmt_class()]. Defaults to
#'   [curr_fy_span()].
#' @param phase_col Name of column with phase name.
#' @param phase_levels Ordered character vector of phases.
#' @param na_phase_level Value to use as replacement for NA values in phase name column.
#' @export
fmt_adapt_6yr_program <- function(
  data,
  timespan_cols = curr_fy_span(),
  drop_cols = c(
    "PCode Code",
    "PCode Name",
    "Fund, Grant, Special Purpose Code",
    # TODO: Check if either of the Fund,
    # Grant, Special Purpose Code columns
    # should be retained
    "Fund, Grant, Special Purpose Name",
    "RObject Code",
    "Grant_Detail Code"
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
  ),
  phase_col = "Phase Name",
  phase_levels = c(
    "<multiple>",
    "Planning/Design",
    "Construction",
    "Post-construction",
    "Information Technology",
    na_phase_level
  ),
  na_phase_level = "Unspecified"
) {
  # timespan <- timespan %||% seq(current_year, current_year + 5)
  # out_years <- setdiff(timespan, current_year)

  data <- data |>
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
    dplyr::select(!any_of(drop_cols))

  if (has_name(data, phase_col)) {
    data <- data |>
      dplyr::mutate(
        "{phase_col}" := dplyr::if_else(
          is.na(.data[[phase_col]]) & !is.null(na_phase_level),
          na_phase_level,
          .data[[phase_col]]
        ),
        # TODO: Add a similar conversion to factor for 1Phase Code
        "{phase_col}" := factor(
          .data[[phase_col]],
          levels = phase_levels
        )
      )
  }

  data |>
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
      code_pattern = "^PJH[:digit:]+"
    ) |>
    fmt_wd_code_name(
      phierarhcy2_cols[[1]],
      phierarhcy2_cols[[2]],
      code_pattern = "^(PJH|CIP|PJHCIP)[:digit:]+"
    ) |>
    wd_proj_join_hierarchy_labels()
}


#' Join PHierarchy1 Label and PHierarchy2 Label columns based on
#' `wd_proj_hierarchy_xwalk` reference data
#'
#' [wd_proj_join_hierarchy_labels()] joins the internal
#' [baltimoreCIPutils::wd_proj_hierarchy_xwalk] data to the input data frame
#' using "PHierarchy1 Code" and "PHierarchy2 Code" as join columns.
#' [wd_proj_join_cost_center_labels()] is a variant that joins the "Cost Center
#' Agency Label" column based on the "Cost Center Code" join column.
#'
#' @param data A data frame with columns matching `hierarchy1_col` and
#'   `hierarchy2_col` arguments.
#' @param hierarchy1_col,hierarchy2_col Column names to join values from
#'   `baltimoreCIPutils::wd_proj_hierarchy_xwalk`
#' @returns A data frame with added columns "PHierarchy1 Label" and "PHierarchy2
#'   Label"
#' @export
wd_proj_join_hierarchy_labels <- function(
  data,
  hierarchy1_col = "PHierarchy1 Code",
  hierarchy2_col = "PHierarchy2 Code"
) {
  stopifnot(
    has_name(data, hierarchy1_col) || is.null(hierarchy1_col),
    has_name(data, hierarchy2_col) || is.null(hierarchy2_col)
  )

  if (!is.null(hierarchy1_col)) {
    hierarchy_1_xwalk <- baltimoreCIPutils::wd_proj_hierarchy_xwalk |>
      dplyr::filter(!is.na(.data[["PHierarchy1 Code"]])) |>
      dplyr::select(
        all_of(
          c("PHierarchy1 Code", "PHierarchy1 Label" = "entity")
        )
      )

    data <- data |>
      dplyr::left_join(
        hierarchy_1_xwalk,
        by = hierarchy1_col
      )
  }

  if (!is.null(hierarchy2_col)) {
    hierarchy_2_xwalk <- baltimoreCIPutils::wd_proj_hierarchy_xwalk |>
      dplyr::filter(!is.na(.data[["PHierarchy2 Code"]])) |>
      dplyr::select(
        all_of(
          c("PHierarchy2 Code", "PHierarchy2 Label" = "entity")
        )
      )

    data <- data |>
      dplyr::left_join(
        hierarchy_2_xwalk,
        by = hierarchy2_col
      )
  }

  data |>
    dplyr::relocate(
      any_of(
        c("PHierarchy1 Label", "PHierarchy2 Label")
      ),
      .after = starts_with("PHierarchy")
    )
}

#' @rdname wd_proj_join_hierarchy_labels
#' @param cost_center_col Cost Center Code column to join on.
#' @export
wd_proj_join_cost_center_labels <- function(
  data,
  cost_center_col = "Cost Center Code"
) {
  cost_center_xwalk <- baltimoreCIPutils::wd_proj_hierarchy_xwalk |>
    dplyr::filter(!is.na(.data[["Cost Center Code"]])) |>
    dplyr::select(
      all_of(
        c("Cost Center Code", "Cost Center Agency Label" = "entity")
      )
    )

  data <- data |>
    dplyr::left_join(
      cost_center_xwalk,
      by = cost_center_col
    )

  data |>
    dplyr::relocate(
      any_of(
        c("Cost Center Agency Label")
      ),
      .after = starts_with("Cost Center")
    )
}

# TODO: Add a function to join related plan data
# join_wd_proj_related_plan <- function(data) {
#
# }
