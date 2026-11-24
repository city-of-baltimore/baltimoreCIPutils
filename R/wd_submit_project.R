#' Format Submit Project EIB Worktag columns
#'
#' @param data Input data frame with Fund ID, Cost Center ID, and Grant ID columns.
#' @noRd
fmt_submit_project_worktags_data <- function(.data) {
  check_has_name(
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
        `Worktag Type` == "GRANT" ~ "Grant_ID"
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

#' Convert a Priority or Importance Rating label to its Workday ID
#'
#' Labels such as "1-High" or "1 - High" (spacing is ignored) are converted to
#' the ID Workday expects ("High"). Missing values stay missing.
#'
#' @param x Character vector of labels.
#' @param allowed Named character vector of IDs named by the allowed label
#'   (without spaces).
#' @param arg Column name used in the error message.
#' @noRd
fmt_submit_project_rating <- function(x, allowed, arg, call = caller_env()) {
  out <- dplyr::recode_values(
    stringr::str_remove_all(x, "[:space:]"),
    from = names(allowed),
    to = unname(allowed)
  )

  invalid <- unique(x[is.na(out) & !is.na(x)])

  if (length(invalid) > 0) {
    cli::cli_abort(
      c(
        "{.field {arg}} has {length(invalid)} value{?s} that {?is/are} not allowed: {.val {invalid}}.",
        "i" = "Allowed values: {.val {names(allowed)}}."
      ),
      call = call
    )
  }

  out
}

#' Format the Submit Project EIB sheet data
#'
#' Updates `proj_list` with the non-missing values in `proj_app_info`, drops
#' inactive projects, and formats the rows of the "Submit Project" sheet
#' (including the worktag rows). Separate from [build_submit_project_wb()] so
#' the data can be checked without an EIB template.
#'
#' @inheritParams build_submit_project_wb
#' @param sheet_name Name of the Submit Project sheet in `eib_dict`.
#' @returns A data frame with one row per project worktag.
#' @noRd
fmt_submit_project_sheet_data <- function(
  proj_list,
  eib_dict,
  proj_app_info,
  proj_resource_plan_ids,
  proj_hierarchy_ids,
  proj_group_ids,
  sheet_name = "Submit Project"
) {
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

  # Check type and names for proj_app_info
  check_data_cols(proj_app_info, proj_app_cols)

  proj_app_info <- proj_app_info |>
    dplyr::mutate(
      # Leave `Project` blank (not "PRJ000001 NA") when there is no name so it
      # is not used to update the Workday value
      Project = dplyr::if_else(
        is.na(ProjectName),
        NA_character_,
        paste0(ProjectID, " ", ProjectName)
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

  # Also check the columns that `proj_app_info` updates
  check_data_cols(
    proj_list,
    unique(c(
      "Project ID",
      "Inactive",
      "Billable",
      "Capital Project",
      "Project Status",
      "Priority",
      "Importance Rating",
      names(proj_app_info)
    ))
  )

  # Drop inactive projects before updating so they are not updated
  i_inactive <- proj_list[["Inactive"]] %in% "Yes"

  if (any(i_inactive)) {
    cli::cli_alert_info(
      "Dropping {sum(i_inactive)} inactive projects from the input `proj_list`"
    )
  }

  # Projects missing from `proj_list` are an error; inactive ones are skipped
  missing_proj <- setdiff(proj_app_info[["Project ID"]], proj_list[["Project ID"]])
  if (length(missing_proj) > 0) {
    cli::cli_abort(
      "{length(missing_proj)} project{?s} in {.arg proj_app_info} not in {.arg proj_list}: {.val {missing_proj}}."
    )
  }

  proj_list <- proj_list[!i_inactive, ]
  proj_app_info <- dplyr::semi_join(
    proj_app_info,
    proj_list,
    by = "Project ID"
  )

  # Update proj_list based on the non-missing values in `proj_app_info`:
  # fill missing app values with the Workday value so they are left unchanged.
  # The newline cleanup above only applies to the app values.
  proj_list <- proj_list |>
    dplyr::rows_update(
      proj_app_info |>
        dplyr::rows_patch(
          dplyr::select(proj_list, tidyselect::all_of(names(proj_app_info))),
          by = "Project ID",
          unmatched = "ignore"
        ),
      by = "Project ID"
    )

  check_data_cols(
    proj_resource_plan_ids,
    c(
      "Business Object Instance",
      "Reference ID Value"
    )
  )

  check_data_cols(
    proj_group_ids,
    c(
      "Business Object Instance",
      "Reference ID Value"
    )
  )

  check_data_cols(
    proj_hierarchy_ids,
    c(
      "Business Object Instance",
      "Reference ID Value"
    )
  )

  # TODO: Add check for contractor ID values in Project Owner column

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
      ),
      `Project Status` = stringr::str_replace(
        `Project Status`,
        "[:space:]",
        "_"
      ),
      # Convert Priority and Importance Rating labels to Workday IDs
      Priority = fmt_submit_project_rating(
        Priority,
        c("1-High" = "High", "2-Medium" = "Medium", "3-Low" = "Low"),
        arg = "Priority"
      ),
      `Importance Rating` = fmt_submit_project_rating(
        `Importance Rating`,
        c("1-Critical" = "Critical", "2-Major" = "Major", "3-Minor" = "Minor"),
        arg = "Importance Rating"
      ),
      # Extract Fund ID from start of Balancing Worktag
      `Balancing Worktag` = stringr::str_extract(
        `Balancing Worktag`,
        "^[:digit:]+"
      ),
      # Replace Project Resource Plan values based on View Reference ID report
      `Project Resource Plan` = dplyr::replace_values(
        `Project Resource Plan`,
        from = proj_resource_plan_ids$`Business Object Instance`,
        to = proj_resource_plan_ids$`Reference ID Value`
      ),
      # Use blank values if Project Resource Plan is inherited from the project hierarhcy
      `Project Resource Plan` = dplyr::if_else(
        !stringr::str_detect(`Project Resource Plan`, "PROJECT_RESOURCE"),
        NA_character_,
        `Project Resource Plan`
      ),
      # Replace Project Groups values based on View Reference ID report
      `Project Groups` = dplyr::replace_values(
        `Project Groups`,
        from = proj_group_ids$`Business Object Instance`,
        to = proj_group_ids$`Reference ID Value`
      ),
      # Replace Primary Project Hierarchy values based on View Reference ID report
      `Primary Project Hierarchy` = dplyr::replace_values(
        `Primary Project Hierarchy`,
        from = proj_hierarchy_ids$`Business Object Instance`,
        to = proj_hierarchy_ids$`Reference ID Value`
      ),
      # Replace Optional Project Hierarchies values based on View Reference ID report
      `Optional Project Hierarchies` = dplyr::replace_values(
        `Optional Project Hierarchies`,
        from = proj_hierarchy_ids$`Business Object Instance`,
        to = proj_hierarchy_ids$`Reference ID Value`
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
      get_dict_defaults(eib_dict, sheet_name)
    ) |>
    # Set Spreadsheet Key
    dplyr::arrange(`Workday Project ID`) |>
    dplyr::mutate(
      `Spreadsheet Key*` = dplyr::row_number(),
      # Apply YYYY-MM-DD formatting to date columns
      dplyr::across(
        tidyselect::where(
          \(x) {
            inherits(x, "Date")
          }
        ),
        \(x) {
          openxlsx2::apply_numfmt(
            x,
            format_code = "yyyy-mm-dd"
          )
        }
      )
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
    ) |>
    # Make sure spreadsheet key and row ID values are character
    # NOTE: This may be unecessary - it was added while trouble-shooting an
    # unrelated issue
    dplyr::mutate(
      `Spreadsheet Key*` = as.character(`Spreadsheet Key*`),
      `Row ID*` = as.character(`Row ID*`)
    )

  submit_proj_sheet
}

#' Build the Submit Project EIB
#'
#' @param proj_list Data frame with project information from "DOP CIP = List
#' Projects" Workday Report.
#' @param eib_dict EIB dictionary with Source Type and Workday Report Name
#' columns.
#' @param proj_app_info Data frame with project information from the SharePoint
#' list. Only non-missing values replace the Workday value in `proj_list`, so
#' a project can be limited to the fields that should be updated by setting the
#' other fields to `NA`. A missing app value never blanks a Workday value.
#' @param proj_resource_plan_ids,proj_hierarchy_ids,proj_group_ids Data frames
#' from the Workday "View Reference ID" report with Business Object Instance
#' and Reference ID Value columns.
#' @param eib_wb Workbook object created by loading Excel template file for
#' Submit Project EIB.
#' @keywords internal
#' @export
build_submit_project_wb <- function(
  proj_list,
  eib_dict,
  proj_app_info,
  proj_resource_plan_ids,
  proj_hierarchy_ids,
  proj_group_ids,
  eib_wb
) {
  check_data_cols(
    eib_dict,
    c(
      "Source Type",
      "Workday Report Name"
    )
  )

  submit_proj_sheet_name <- "Submit Project"

  submit_proj_fields <- pull_dict_fields(eib_dict, submit_proj_sheet_name)

  submit_proj_sheet <- fmt_submit_project_sheet_data(
    proj_list = proj_list,
    eib_dict = eib_dict,
    proj_app_info = proj_app_info,
    proj_resource_plan_ids = proj_resource_plan_ids,
    proj_hierarchy_ids = proj_hierarchy_ids,
    proj_group_ids = proj_group_ids,
    sheet_name = submit_proj_sheet_name
  )

  reduce_wb_data_fields(
    data = submit_proj_sheet,
    fields = submit_proj_fields,
    sheet = submit_proj_sheet_name,
    na = "",
    .init = eib_wb
  )
}
