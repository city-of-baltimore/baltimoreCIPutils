#' Format a pair of code and name columns from a Workday or Adaptive Planning
#' report
#'
#' [fmt_wd_code_name()] is a helper for handling a common pattern where the code
#' value is included as a prefix in a name value column. This function separates
#' the two, optionally creating a new name or code column (leaving the original
#' value unchanged).
#'
#' @export
fmt_wd_code_name <- function(data,
                             code_col,
                             name_col,
                             code_pattern = NULL,
                             new_code_col = code_col,
                             new_name_col = name_col) {
  data |>
    dplyr::mutate(
      "{new_code_col}" := stringr::str_extract(.data[[code_col]], code_pattern),
      "{new_name_col}" := stringr::str_trim(
        stringr::str_remove(
          .data[[name_col]],
          paste0("^", .data[[new_code_col]])
        )
      )
    )
}

#' Format Capital data from Adaptive Planning by dropping select columns and
#' formatting code/name column pairs
#'
#' @name fmt_wd_proj
NULL

#' [fmt_wd_proj_worktags()] drops duplicative columns then formats the Fund
#' and Cost Center columns.
#'
#' @param fund_cols,cost_center_cols Fund and Cost Center column name pairs.
#'   Must be provided with the code column first and the name column second. One
#'   but not both may be set to `NULL`.
#' @rdname fmt_wd_proj
#' @export
fmt_wd_proj_worktags <- function(
    data,
    fund_cols = c(
      "FGSFund Code",
      "FGSFund Name"
    ),
    fund_pattern = "^[:digit:]+",
    cost_center_cols = c(
      "Cost Center Code",
      "Cost Center Name"
    ),
    cost_center_pattern = cap_patterns[["cost_center"]]) {
  stopifnot(
    is.character(c(fund_cols, cost_center_cols)),
    any(has_name(data, c(fund_cols, cost_center_cols)))
  )

  if (!is.null(fund_cols)) {
    data <- data |>
      fmt_wd_code_name(
        fund_cols[[1]],
        fund_cols[[2]],
        fund_pattern
      )
  }

  if (!is.null(cost_center_cols)) {
    data <- data |>
      fmt_wd_code_name(
        cost_center_cols[[1]],
        cost_center_cols[[2]],
        cost_center_pattern
      )
  }

  data
}

#' [fmt_wd_proj_name()] creates a new column named "Project Name Short" that
#' strips the Project Code from the Project Name, removes any leading or
#' trailing CIP numbers, and removes the six-digit legacy Capital account
#' number.
#' @rdname fmt_wd_proj
#' @export
fmt_wd_proj_name <- function(data,
                             project_code_col = "Project Code",
                             project_name_col = "Project Name") {
  new_project_name_col <- paste0(project_name_col, " Short")

  data |>
    dplyr::filter(!is.na(.data[[project_code_col]])) |>
    fmt_wd_code_name(
      project_code_col,
      project_name_col,
      "^PRJ[:digit:]+",
      new_name_col = new_project_name_col
    ) |>
    dplyr::mutate(
      # Strip leading legacy account number
      "{new_project_name_col}" := stringr::str_remove(
        .data[[new_project_name_col]], "^[:digit:]{6} "
      ),
      # Strip leading CIP Number
      "{new_project_name_col}" := stringr::str_remove(
        .data[[new_project_name_col]], "^[:digit:]{3}-[:digit:]{3} "
      ),
      # Strip trailing Project Code
      "{new_project_name_col}" := stringr::str_remove(
        .data[[new_project_name_col]], paste0(.data[[project_code_col]], "$")
      )
    )
}
