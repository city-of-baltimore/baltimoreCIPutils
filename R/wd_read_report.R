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
    data <- readr::read_csv(file, ..., skip = (start_row - 1), name_repair = name_repair)
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
#' @inheritParams wd_read_report
#' @name wd_read_report_proj
NULL

#' @rdname wd_read_report_proj
#' @export
wd_read_report_proj_plan <- function(file, start_row = 4, ...) {
  if (!grepl("^Capital_Projects_With_Plan_Info", file)) {
    cli::cli_alert_warning(
      "{.f wd_read_report_proj_plan} is designed for use with the {.str Capital Projects With Plan Info} report."
    )
  }

  wd_read_report(file = file, start_row = start_row, ...) |>
    fmt_wd_proj_name(
      project_code_col = "Project ID",
      project_name_col = "Project"
    )
}

#' @rdname wd_read_report_proj
#' @export
wd_read_report_proj_ltd <- function(file, start_row = 2, ...) {
  if (!grepl("^COB_Extract_Projects_With_LTD_Balances", file)) {
    cli::cli_alert_warning(
      "{.f wd_read_report_proj_ltd} is designed for use with the {.str COB_Extract_Projects_With_LTD_Balances} report."
    )
  }

  wd_read_report(file = file, start_row = start_row, ...) |>
    fmt_wd_proj_name(
      project_code_col = "Project ID",
      project_name_col = "Project"
    )
}

#' @rdname wd_read_report_proj
#' @export
wd_read_report_extract_proj <- function(file, start_row = 8, ...) {
  wd_read_report(
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
    fmt_wd_proj_priority()
  # TODO: Add formatting for "Cost Center" column
}
