#' Format Submit Project EIB Worktag columns
#'
#' @param data
#' @noRd
fmt_submit_project_worktags_data <- function(.data) {
  check_installed("chk")
  chk::check_names(
    .data,
    c(
      "Fund ID",
      "Cost Center ID",
      "Grant ID"
    )
  )

  .data |>
    dplyr::select(
      tidyselect::all_of(c(
        "Project ID",
        "Fund ID",
        "Cost Center ID",
        "Grant ID"
      ))
    ) |>
    tidyr::pivot_longer(
      cols = !`Project ID`,
      names_to = "Worktag Type",
      values_to = "ID Value",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      `Worktag Type` = stringr::str_remove(
        stringr::str_replace_all(
          `Worktag Type`,
          "[:space:]",
          "_"
        ),
        "_ID$"
      ),
      `Worktag Type` = factor(
        stringr::str_to_upper(`Worktag Type`),
        c("FUND", "COST_CENTER", "GRANT")
      ),
      `ID Type` = dplyr::case_when(
        `Worktag Type` == "FUND" ~ "Fund_ID",
        `Worktag Type` == "COST_CENTER" ~ "Organization_Reference_ID",
        # FIXME: Check if Grant ID is a Organization_Reference_ID or a Grant_ID ID Type
        `Worktag Type` == "GRANT" ~ "Organization_Reference_ID"
      ),
      `Required On Transaction` = dplyr::case_when(
        `Worktag Type` == "FUND" ~ "Y",
        .default = "N"
      ),
      `Required On Transaction For Validation` = dplyr::case_when(
        `Worktag Type` == "FUND" ~ "Y",
        .default = "N"
      )
      # NOTE: Balancing Worktag should come from the List Projects report
      # `Balancing Worktag` = dplyr::case_when(
      #   `Worktag Type` == "FUND" ~ `ID Value`,
      #   .default = NA_character_
      # ),
    ) |>
    dplyr::arrange(`Project ID`, `Worktag Type`) |>
    dplyr::mutate(
      `Row ID*` = dplyr::row_number(),
      .by = `Project ID`
    )
}

#' Build the Submit Project EIB
#'
#' @param proj_list Data frame with project information from "DOP CIP = List
#' Projects" Workday Report.
#' @param eib_dict EIB dictionary with Source Type and Workday Report Name
#' columns.
#' @param proj_app_info Data frame with project information from the SharePoint
#' list.
#' @param eib_wb Workbook object created by loading Excel template file for
#' Submit Project EIB.
#' @keywords internal
#' @export
build_submit_project_wb <- function(
  proj_list,
  eib_dict,
  proj_app_info,
  eib_wb = NULL
) {
  check_data_frame(eib_dict)
  check_installed("chk")
  chk::check_names(
    eib_dict,
    c(
      "Source Type",
      "Workday Report Name"
    )
  )

  submit_proj_sheet_name <- "Submit Project"

  submit_proj_fields <- pull_dict_fields(eib_dict, submit_proj_sheet_name)

  wd_eib_dict <- eib_dict |>
    dplyr::filter(
      `Source Type` == "Workday",
      !is.na(`Workday Report Name`)
    )

  proj_app_cols <- c(
    "ProjectID",
    "ProjectName",
    "ProjectDescription",
    "RiskLevel",
    "PriorityRating",
    "ImportanceRating",
    "ProblemStatement",
    "Objective",
    "InScope",
    "OutOfScope",
    "SuccessMeasures",
    "ProjectOverview"
  )

  # NOTE: sharepointr is not Suggested by {baltimoreCIPutils} so should not be
  # used here
  # proj_app_info <- proj_app_info %||%
  #   sharepointr::get_sp_list_items(
  #     # TODO: Set Capital Project list name based on version in list input config
  #     list_name = "CapitalProject_20250917",
  #     site_url = "https://bmore.sharepoint.com/sites/DOP-CIP/SitePages/CollabHome.aspx",
  #     select = proj_app_cols
  #   )

  # Check type and names for proj_app_info
  rlang::check_data_frame(proj_app_info)
  chk::check_names(
    proj_app_info,
    proj_app_cols
  )

  proj_app_info <- proj_app_info |>
    dplyr::mutate(
      Project = paste0(
        ProjectID,
        " ",
        ProjectName
      ),
      # Remove leading and trailing new lines
      dplyr::across(
        tidyselect::where(is.character),
        \(x) {
          stringr::str_remove_all(
            x,
            # stringr::str_remove_all(x, "^[:blank:]|[:blank:]$"),
            "^\\n|\\n$"
          )
        }
      )
    ) |>
    dplyr::select(
      tidyselect::all_of(
        c(
          "Project ID" = "ProjectID",
          "Project",
          "Project Description" = "ProjectDescription",
          "Risk Level" = "RiskLevel",
          "Priority" = "PriorityRating",
          "Importance Rating" = "ImportanceRating",
          "Problem Statement" = "ProblemStatement",
          "Objective",
          "In Scope" = "InScope",
          "Out of Scope" = "OutOfScope",
          "Measures of Success" = "SuccessMeasures",
          "Project Overview" = "ProjectOverview"
        )
      )
    )

  rlang::check_data_frame(proj_list)
  chk::check_names(
    proj_list,
    c("Project ID", "Inactive", "Billable", "Capital Project")
  )

  proj_list <- proj_list |>
    dplyr::rows_update(
      proj_app_info,
      by = "Project ID"
    )

  # TODO: Add trimming of whitespace at start and end

  submit_proj_data <- proj_list |>
    # Reformat Y/N columns
    dplyr::mutate(
      dplyr::across(
        c(Inactive, Billable, `Capital Project`),
        \(x) {
          dplyr::case_when(
            x == "No" ~ "N",
            x == "Yes" ~ "Y"
          )
        }
      )
    ) |>
    # Rename columns to match dictionary
    dplyr::select(
      tidyselect::any_of(
        wd_eib_dict |>
          dplyr::select(Fields, `Workday Report Name`) |>
          tibble::deframe()
      )
    ) |>
    dplyr::mutate(
      # NOTE: This is necessary because Project ID is a duplicate
      `Workday Project ID` = Project
    ) |>
    dplyr::left_join(
      # Format and join Project Name and Workday Project ID columns
      proj_list |>
        dplyr::mutate(
          `Project Name*` = stringr::str_remove(
            Project,
            paste0("^", `Project ID`)
          ),
          `Project Name*` = dplyr::if_else(
            Inactive == "N",
            `Project Name*`,
            stringr::str_remove(
              `Project Name*`,
              "(Inactive)$"
            )
          ),
          `Project Name*` = stringr::str_trim(
            `Project Name*`
          ),
          `Workday Project ID` = `Project ID`,
          .keep = "none"
        ),
      by = dplyr::join_by(
        `Workday Project ID`
      )
    ) |>
    # Bind default values from dictionary
    cbind_defaults(
      get_dict_defaults(eib_dict, submit_proj_sheet_name)
    ) |>
    # Set Spreadsheet Key
    dplyr::arrange(`Workday Project ID`) |>
    dplyr::mutate(
      `Spreadsheet Key*` = dplyr::row_number()
    )

  default_worktag_data <- proj_list |>
    fmt_submit_project_worktags_data() |>
    dplyr::left_join(
      submit_proj_data |>
        dplyr::select(
          `Project ID` = `Workday Project ID`,
          `Spreadsheet Key*`
        ),
      by = dplyr::join_by(`Project ID`)
    ) |>
    dplyr::select(!`Project ID`)

  submit_proj_sheet <- default_worktag_data |>
    dplyr::left_join(
      submit_proj_data,
      relationship = "many-to-one",
      by = dplyr::join_by(`Spreadsheet Key*`)
    ) |>
    dplyr::mutate(
      # Drop all but the first row of values for each group
      dplyr::across(
        tidyselect::all_of(
          setdiff(
            names(submit_proj_data),
            "Spreadsheet Key*"
          )
        ),
        \(x) {
          dplyr::if_else(
            dplyr::row_number() == 1,
            x,
            NA
          )
        }
      ),
      .by = `Spreadsheet Key*`
    )

  reduce_wb_data_fields(
    data = submit_proj_sheet,
    fields = submit_proj_fields,
    sheet = submit_proj_sheet_name,
    na = "",
    .init = eib_wb
  )
}
