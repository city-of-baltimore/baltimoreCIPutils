#' Read Workday Report files
#'
#'
#' @inheritParams wd_read_report
#' @keywords internal
#' @name wd_budget_load
NULL

#' `read_proj_list` is a minimal wrapper for `wd_read_report()` to read the
#' internal "DOP CIP - List Projects" Workday report.
#' @rdname wd_budget_load
#' @param fy_start_date not used
#' @export
read_proj_list <- function(
  file,
  fy_start_date = lubridate::date("2025-07-01"),
  start_row = 7
) {
  wd_read_report(
    file,
    start_row = start_row
  ) # |>
  # validate_project_dates
  # TODO: Check if this is all still appropriate and what to use it for
  # validate_project_dates(
  #   curr_fy_start_date = fy_start_date
  # )
}

#' `read_proj_plans` is a minimal wrapper for `wd_read_report()` to read the
#' internal "DOP CIP - List Capital Project Budget Plans" Workday report.
#' @rdname wd_budget_load
#' @export
read_proj_plans <- function(file, ..., start_row = 7) {
  proj_plans <- wd_read_report(
    file,
    start_row = start_row
  )

  # fmt_proj_plans(proj_plans)
  proj_plans
}

#' `fmt_proj_plans()` formats project plans data frame for use with EIB
#' @rdname wd_budget_load
#' @param .data Input data frame created by `read_proj_plans()`.
#' @export
fmt_proj_plans <- function(.data) {
  check_installed("chk")
  chk::check_names(
    .data,
    c("Project ID", "Plan", "Plan Date From", "Plan Status")
  )

  .data |>
    # Check initial Plan count per project
    dplyr::add_count(
      `Project ID`,
      name = "Plan Count"
    ) |>
    dplyr::mutate(
      # Flag Plans with DNU in name
      DNU = stringr::str_detect(
        Plan,
        "DNU|Do Not Use|DO NOT USE"
      ),
      # Flag Plans with Corrected in name at end
      `Corrected` = stringr::str_detect(
        Plan,
        "corrected|Corrected$"
      ),
      `Fiscal Year*` = as.integer(
        baltimoreCIPutils::fiscal_year(
          `Plan Date From`
        )
      ),
      `Fiscal Time Interval*` = month.abb[lubridate::month(
        `Plan Date From`
      )],
      # Flag DNU plans, plans w/ no balance, and plans that aren't the
      # "corrected" version
      `Valid Plan` = !DNU & (`Plan Count` < 2 | `Corrected`)
    ) |>
    dplyr::mutate(
      put = is.na(`Plan Status`), # put_budget_template
      # TODO: Document how the plan status is assigned
      new = is.na(`Plan Status`) | `Plan Status` %in% c("Draft", "In Progress"), # import_budget
      amend = `Valid Plan` & (`Plan Status` %in% c("Available")) # amend_budget
    )
}

#' `split_proj_list()` is a utility function to split the project list into a
#' named list with three elements based on the `put`, `new`, and `amend` columns
#' created by `fmt_proj_plans()`. Each element corresponds to a difference EIB
#' file loaded as part of the annual budget load process.
#' @rdname wd_budget_load
#' @export
split_proj_list <- function(
  proj_list,
  proj_plans
) {
  proj_put_budget_template <- dplyr::filter(proj_plans, put)
  proj_import_budget <- dplyr::filter(proj_plans, new)
  proj_amend_budget <- dplyr::filter(proj_plans, amend)

  list(
    put_budget_template = proj_list |>
      dplyr::filter(
        `Project ID` %in% proj_put_budget_template[["Project ID"]]
      ),
    import_budget = proj_list |>
      dplyr::filter(
        `Project ID` %in% proj_import_budget[["Project ID"]]
      ),
    amend_budget = proj_list |>
      dplyr::filter(
        `Project ID` %in% proj_amend_budget[["Project ID"]]
      )
  )
}
