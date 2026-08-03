#' Compare current year and prior year CIP plan data
#'
#' Using data from the Adaptive Planning Six-Year CIP Sheet, compare the first
#' five timespan columns of the current year to the last five timespan columns
#' of the prior year.
#'
#' @param cip_data_cy Current year plan data
#' @param cip_data_py Prior year plan data
#' @param project_data Project data with a Project Code and Agency Name column
#' @param cy_timespan_cols Current year timespan columns, defaults to [curr_fy_span()]
#' @param py_timespan_cols Prior year timespan columns, defaults to [prior_fy_span()]
#' @export
compare_adapt_6yr_program <- function(
  cip_data_cy,
  cip_data_py,
  project_data,
  cy_timespan_cols = curr_fy_span(),
  py_timespan_cols = prior_fy_span()
) {
  last_cy_timespan_col <- utils::tail(cy_timespan_cols, 1)
  comparison_timespan_cols <- setdiff(cy_timespan_cols, last_cy_timespan_col)

  capital_plan_changes <- cip_data_cy |>
    dplyr::select(
      `Project Code`,
      `Project Name`,
      `Change Type` = `Change Type Name`,
      `Change Justification`
    ) |>
    dplyr::distinct() #|>
  # FIXME: Check if this is still needed as a check
  # add_count(`Project Code`)

  capital_plan_comparison <- cip_data_cy |>
    # Remove split child rows for initial comparison
    dplyr::filter(
      `Is Split Child Row` == "No"
    ) |>
    summarise_timespan(
      timespan_cols = cy_timespan_cols,
      .by = c(
        "Cost Center Code",
        "Cost Center Name",
        "Project Code",
        "Project Name",
        "Revenue Category Code",
        "Revenue Category Name"
      )
    ) |>
    replace_na_timespan(
      timespan_cols = cy_timespan_cols
    ) |>
    dplyr::select(
      # Drop last of the current year timespan columns
      !tidyselect::any_of(last_cy_timespan_col)
    ) |>
    # FIXME: Address how this is dropping the change justification from the summary
    filter_cip_empty_rows(
      timespan_cols = comparison_timespan_cols
    ) |>
    dplyr::full_join(
      cip_data_py |>
        replace_na_timespan(
          timespan_cols = py_timespan_cols
        ) |>
        summarise_timespan(
          timespan_cols = py_timespan_cols,
          .by = c(
            "Cost Center Code",
            "Cost Center Name",
            "Project Code",
            "Project Name",
            "Revenue Category Code",
            "Revenue Category Name"
          )
        ) |>
        dplyr::select(
          "Project Code",
          "Project Name",
          "Cost Center Code",
          "Cost Center Name",
          "Revenue Category Code",
          starts_with("FY")
        ) |>
        # Drop first year of the prior year timespan columns
        dplyr::select(
          !tidyselect::any_of(setdiff(
            py_timespan_cols,
            comparison_timespan_cols
          ))
        ) |>
        # Remove projects that are empty for FY26-30
        filter_cip_empty_rows(
          timespan_cols = comparison_timespan_cols
        ),
      by = c(
        "Project Code",
        "Cost Center Code",
        "Cost Center Name",
        "Revenue Category Code"
      ),
      na_matches = "never",
      suffix = c("", "_PY")
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("FY"),
      names_to = "year",
      values_to = "amount"
    ) |>
    dplyr::mutate(
      amount_type = dplyr::case_when(
        stringr::str_detect(year, "PY$") ~ "py_amount",
        .default = "cy_amount"
      ),
      year = paste0(
        "FY",
        stringr::str_extract(year, "[:digit:]+")
      )
    ) |>
    tidyr::pivot_wider(
      values_from = "amount",
      names_from = "amount_type"
    ) |>
    dplyr::mutate(
      `Project Name` = dplyr::coalesce(
        `Project Name`,
        `Project Name_PY`
      ),
      amount_diff = dplyr::case_when(
        is.na(py_amount) ~ cy_amount - 0,
        is.na(cy_amount) ~ 0 - py_amount,
        .default = cy_amount - py_amount
      ),
      diff_note = dplyr::case_when(
        is.na(py_amount) ~ "Added for CY CIP",
        is.na(cy_amount) ~ "Removed from PY CIP",
        amount_diff > 0 ~ "Increase from PY CIP",
        amount_diff < 0 ~ "Decrease from PY CIP",
        amount_diff == 0 ~ "No change from PY CIP"
      )
    ) |>
    dplyr::select(!`Project Name_PY`)

  capital_plan_add <- capital_plan_comparison |>
    dplyr::filter(
      diff_note %in% c("Added for CY CIP")
    ) |>
    dplyr::select(
      !c(cy_amount, py_amount, diff_note)
    ) |>
    tidyr::pivot_wider(
      values_from = amount_diff,
      names_from = year
    ) |>
    dplyr::mutate(
      `Project Change Type` = "Added for CY CIP"
    )

  capital_plan_remove <- capital_plan_comparison |>
    dplyr::filter(
      diff_note %in% c("Removed from PY CIP")
    ) |>
    dplyr::select(
      !c(cy_amount, py_amount, diff_note)
    ) |>
    tidyr::pivot_wider(
      values_from = amount_diff,
      names_from = year
    ) |>
    dplyr::mutate(
      `Project Change Type` = "Removed from PY CIP"
    )

  capital_plan_comparison |>
    dplyr::select(
      !c(cy_amount, py_amount, diff_note)
    ) |>
    dplyr::filter(
      !(`Project Code` %in%
        c(
          capital_plan_add[["Project Code"]],
          capital_plan_remove[["Project Code"]]
        ))
    ) |>
    tidyr::pivot_wider(
      values_from = amount_diff,
      names_from = year
    ) |>
    dplyr::mutate(
      `Project Change Type` = "Modification from PY CIP"
    ) |>
    dplyr::bind_rows(
      capital_plan_add
    ) |>
    dplyr::bind_rows(
      capital_plan_remove
    ) |>
    dplyr::mutate(
      `FY Total Diff` = sum(
        dplyr::c_across(tidyselect::starts_with("FY")),
        na.rm = TRUE
      ),
      .by = c("Project Code", "Revenue Category Code")
    ) |>
    dplyr::left_join(
      capital_plan_changes
    ) |>
    dplyr::mutate(
      `FY Total Diff Type` = dplyr::case_when(
        `FY Total Diff` > 0 ~ "Increase from PY CIP",
        `FY Total Diff` < 0 ~ "Decrease from PY CIP",
        `FY Total Diff` == 0 ~ "No change from PY CIP"
      )
    ) |>
    replace_na_timespan(
      timespan_cols = comparison_timespan_cols
    ) |>
    dplyr::left_join(
      project_data |>
        dplyr::select(
          `Project Code`,
          `Agency Name`
        )
    ) |>
    dplyr::relocate(
      `Project Change Type`,
      `Agency Name`,
      .before = tidyselect::everything()
    ) |>
    # FIXME: Add filter for things where there is no real change and no need for a
    # justification
    dplyr::filter(
      dplyr::if_any(
        tidyselect::any_of(comparison_timespan_cols),
        \(x) {
          x != 0
        }
      )
    ) |>
    dplyr::arrange(
      `Agency Name`,
      `Cost Center Code`,
      `Project Code`,
      `Revenue Category Code`
    )
}

#' Filter Six-Year CIP data to rows where one or more timespan columns are not
#' NA and not equal to 0
#' @noRd
filter_cip_empty_rows <- function(
  .data,
  ...,
  timespan_cols
) {
  dplyr::filter(
    .data,
    dplyr::if_any(
      tidyselect::all_of(timespan_cols),
      \(x) {
        !is.na(x) & x != 0
      }
    )
  )
}
