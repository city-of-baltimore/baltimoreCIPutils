#' Read Workday report exported in an Excel or CSV format
#'
#' [wd_read_report()] reads an exported Workday report from a XLSX or CSV file
#' format. For Excel file exports, based on the typical report structure, the
#' title of the report is read and retained as an attribute. The filename is
#' retained as an attribute for both CSV and XLSX input files.
#'
#' @inheritParams openxlsx2::read_xlsx
#' @inheritParams readr::read_csv
#' @export
wd_read_report <- function(file, start_row = 4, ..., name_repair = "unique") {
  fileext <- fs::path_ext(file)

  if (fileext == "csv") {
    check_installed("readr")
    data <- readr::read_csv(
      file,
      ...,
      skip = (start_row - 1),
      name_repair = name_repair
    )
  } else {
    data <- openxlsx2::read_xlsx(file, start_row = start_row, ...)

    data <- rlang::set_names(
      data,
      vctrs::vec_as_names(names(data), repair = name_repair)
    )

    if (start_row > 2) {
      info <- openxlsx2::read_xlsx(
        file,
        start_row = 1,
        cols = seq(2),
        rows = seq(start_row - 1)
      )

      attr(data, "report_title") <- info[1, ][[1]]
    }
  }

  attr(data, "filename") <- basename(file)
  # TODO: Add in more attributes similar to adapt_sheet_info attributes created
  # by `adapt_read_sheet()`
  # attr(data, "birth_time") <- fs::file_info(file)[["birth_time"]]

  data
}

#' Read Workday reports for the Baltimore City Capital Improvement Program
#'
#' Workday reports use different column names and may be structured differently
#' than similar reports from Adaptive Planning. These helper functions are
#' designed to work with common reports for the Baltimore City Capital
#' Improvement Program. Changes to the naming conventions or structure of these
#' reports may break these functions.
#'
#' @details
#' - [wd_read_proj_plan()] reads the "Capital Projects With Plan Info" Workday report.
#' - [wd_read_proj_ltd()] reads the "COB Extract Projects With LTD Balances" Workday report.
#' - [wd_read_extract_cost_center()] reads the "COB Extract Cost
#' Centers" Workday report for capital cost centers.
#' - [wd_read_extract_revenue_cat()] reads the Revenue Category
#' Workday report for all Revenue Categories (dropping revenue categories with no code).
#' @inheritParams wd_read_report
#' @name wd_read_proj
NULL

#' @rdname wd_read_proj
#' @export
wd_read_proj_plan <- function(file, start_row = 4, ...) {
  if (!grepl("Capital_Projects_With_Plan_Info", file)) {
    cli::cli_alert_warning(
      "{.f wd_read_proj_plan} is designed for use with the {.str Capital Projects With Plan Info} report."
    )
  }

  wd_read_report(file = file, start_row = start_row, ...) |>
    fmt_wd_proj_name(
      project_code_col = "Project ID",
      project_name_col = "Project"
    )
}

#' @rdname wd_read_proj
#' @export
wd_read_proj_ltd <- function(file, start_row = 2, ...) {
  if (!grepl("COB_Extract_Projects_With_LTD_Balances", file)) {
    cli::cli_alert_warning(
      "{.f wd_read_proj_ltd} is designed for use with the {.str COB_Extract_Projects_With_LTD_Balances} report."
    )
  }

  wd_read_report(file = file, start_row = start_row, ...) |>
    fmt_wd_proj_name(
      project_code_col = "Project ID",
      project_name_col = "Project"
    )
}

#' @rdname wd_read_proj
#' @param drop_cols Character vector with names of columns from report to drop.
#' @export
wd_read_extract_proj <- function(
  file,
  start_row = 7,
  ...,
  drop_cols = c(
    "Include Project ID in Name",
    "Inactive - Current",
    "Billable",
    "Project Currency",
    "Customer",
    "Time Cost - Approved (Project Currency)",
    "Total Task Estimated Hours",
    "Total Hours Worked",
    "Total Task Hours Remaining"
  )
) {
  data <- wd_read_report(
    file = file,
    start_row = start_row,
    ...
  ) |>
    fmt_wd_proj_name(
      project_code_col = "Project ID",
      project_name_col = "Project"
    ) |>
    fmt_wd_proj_importance() |>
    fmt_wd_proj_risk() |>
    fmt_wd_proj_priority() |>
    dplyr::mutate(
      `Project Code` = `Project ID`
    ) |>
    dplyr::select(
      !tidyselect::any_of(drop_cols)
    )

  data
}

#' @rdname wd_read_proj
#' @export
wd_read_extract_cost_center <- function(file, start_row = 1, ...) {
  wd_read_report(file, start_row = start_row) |>
    dplyr::select(
      `Cost Center Code` = Code,
      `Cost Center Name` = `Cost Center Name`,
      `Cost Center`,
      Agency,
      Service
    )
}

#' @rdname wd_read_proj
#' @export
wd_read_extract_revenue_cat <- function(file, start_row = 2, ...) {
  wd_read_report(file, start_row = start_row) |>
    dplyr::mutate(
      `Revenue Category Code` = str_extract_revenue_category_code(
        `Revenue Category Name`
      )
    ) |>
    dplyr::filter(
      !is.na(`Revenue Category Code`)
    ) |>
    dplyr::select(
      `Revenue Category Code`,
      `Revenue Category`,
      Fund,
      `Cost Center`
    )
}
