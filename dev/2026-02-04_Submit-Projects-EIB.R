eib_dict <- openxlsx2::read_xlsx(
  "C:/Users/Eli.Pousson/OneDrive - City Of Baltimore/Projects/baltimoreCIP-Project-List/files/COB_Submit_Project SBX and PRD Template_Data-Dictionary.xlsx"
)

proj_list <- read_proj_list(
  "C:/Users/Eli.Pousson/OneDrive - City Of Baltimore/Projects/baltimoreCIP-Project-List/files/DOP_CIP_-_List_Projects.xlsx"
)

eib_wb <- openxlsx2::wb_load(
  "C:/Users/Eli.Pousson/OneDrive - City Of Baltimore/Projects/baltimoreCIP-Project-List/files/COB_Submit_Project SBX and PRD Template.xlsx"
)

fmt_submit_project_worktags_data <- function(data) {
  data |>
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

build_submit_project_wb <- function(
  proj_list,
  eib_dict,
  proj_app_info = NULL,
  eib_wb = NULL
) {
  submit_proj_sheet_name <- "Submit Project"

  submit_proj_fields <- pull_dict_fields(eib_dict, submit_proj_sheet_name)

  app_eib_dict <- eib_dict |>
    dplyr::filter(
      `Source Type` == "App",
      !is.na(`App Name`)
    )

  wd_eib_dict <- eib_dict |>
    dplyr::filter(
      `Source Type` == "Workday",
      !is.na(`Workday Report Name`)
    )

  proj_app_info <- proj_app_info %||%
    sharepointr::get_sp_list_items(
      # TODO: Set Capital Project list name based on version in list input config
      list_name = "CapitalProject_20250917",
      site_url = "https://bmore.sharepoint.com/sites/DOP-CIP/SitePages/CollabHome.aspx",
      select = c(
        "ProjectID",
        app_eib_dict |>
          dplyr::pull("App Name")
      )
    )

  submit_proj_data <- proj_list |>
    dplyr::select(
      tidyselect::all_of(
        wd_eib_dict |>
          dplyr::select(Fields, `Workday Report Name`) |>
          tibble::deframe()
      )
    ) |>
    dplyr::left_join(
      proj_app_info |>
        dplyr::select(
          ProjectID,
          tidyselect::all_of(
            app_eib_dict |>
              dplyr::select(Fields, `App Name`) |>
              tibble::deframe()
          )
        ),
      by = dplyr::join_by(`Workday Project ID` == ProjectID)
    ) |>
    # Bind default values from dictionary
    cbind_defaults(
      get_dict_defaults(eib_dict, submit_proj_sheet_name)
    ) |>
    # Set Spreadsheet Key
    dplyr::arrange(`Workday Project ID`) |>
    dplyr::mutate(
      `Spreadsheet Key*` = dplyr::row_number()
      # .by = `Workday Project ID`
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
    .init = eib_wb
  )
}


eib_out_wb <- build_submit_project_wb(
    proj_list = proj_list,
    eib_dict = eib_dict,
    proj_app_info = proj_app_info,
    eib_wb = eib_wb
  )
