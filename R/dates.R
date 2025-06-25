#' Format Workday Project Dates
#'
#' [fmt_wd_proj_dates()] is used to derive new "Project Start FY" and "Project
#' End FY" columns based on existing project start and end date columns.
#'
#' @param data Data frame (typically originated as a report from Workday) with
#'   column names matching `start_date_col` and `end_date_col` values.
#' @param start_date_col,end_date_col Project start and end date column names.
#' @export
fmt_wd_proj_dates <- function(
  data,
  start_date_col = "Project Start Date",
  end_date_col = "Project End Date"
) {
  data |>
    dplyr::mutate(
      `Project Start FY` = fiscal_year(.data[[start_date_col]]),
      `Project End FY` = fiscal_year(.data[[end_date_col]])
    )
}
