library(tidyverse)
library(targets)
# library(baltimoreCIPutils)
options(openxlsx2.na.strings = "")

file_list <- list_input_files(
  dir = "C:/Users/Eli.Pousson/City Of Baltimore/DOP-POLICY&DATA - Documents/CIP/FY 2026-2031/Budget Load/files"
)

# Load the template and dictionary files
file_list$templates <- map(
  file_list$templates,
  \(x) {
    c(
      x,
      list(
        wb = openxlsx2::wb_load(x[["path"]])
      )
    )
  }
)

file_list$dictionaries <- map(
  file_list$dictionaries,
  \(x) {
    c(
      x,
      list(
        dict = openxlsx2::read_xlsx(x[["path"]])
      )
    )
  }
)

# targets::tar_source(
#   "/Users/elipousson/Projects/00_dop/baltimoreCIP-Workday-EIB/R"
# )

# Read project plans
proj_plans <- read_proj_plans(
  file_list$sources$project_plans$path
) |>
  # Drop select Plans
  dplyr::filter(
    !(`Plan Workday ID` %in%
      c(
        "52bf431a6a8410015fd9808c41920000",
        "8b8ac394e3151000c082ce1436a50000"
      ))
  ) |>
  fmt_proj_plans()


# proj_plans |>
#   filter(
#     `Fiscal Year*` == 2016,
#     `Valid Plan`
#   ) |>
#   glimpse()

proj_budget_ids <- openxlsx2::read_xlsx(
  file_list$sources$project_budget_ids$path,
  start_row = 3
) |>
  select(
    `Plan Name` = `Business Object Instance`,
    `Project Budget ID` = `Reference ID Value`
  )


# Read DOP CIP - List Projects report
proj_list <- read_proj_list(
  file_list$sources$project_list$path
)


# FIXME: Read Project Plan ID values from file
# proj_list <- read_proj_list(
#   "C:/Users/Eli.Pousson/Downloads/DOP_CIP_-_List_Projects.xlsx"
# )
# proj_list$`Company ID` |> unique()

# Read the 6-Year CIP sheet from Adaptive Planning
cip_6yr <- baltimoreCIPutils::adapt_read_sheet(
  file_list$sources$cip_6yr$path
) |>
  # Drop Child Rows
  dplyr::filter(
    `Is Split Child Row` == "No"
  ) |>
  fmt_adapt_6yr_program() |>
  # Rename, derive, and join columns to match new DOP CIP Workday Reports
  dplyr::rename(
    `Project` = `Project Name`,
    `Project ID` = `Project Code`,
    `Fund ID` = `FGSFund Code`,
    `Revenue Category ID` = `Revenue Category Code`,
    `Cost Center ID` = `Cost Center Code`,
    `Grant ID` = `FGSGrant Code`
  ) |>
  dplyr::mutate(
    `Revenue Category` = paste(
      `Revenue Category ID`,
      `Revenue Category Name`
    )
  ) |>
  dplyr::left_join(
    proj_list |>
      dplyr::select(
        `Cost Center`,
        `Project ID`
      ),
    by = dplyr::join_by(`Project ID`)
  ) |>
  mutate(
    across(
      all_of(fy_span(2026, n = 6, type = "year_prefix")),
      as.numeric
    )
  )


budget_year_projects <- cip_6yr |>
  filter(
    !is.na(FY2026) & (FY2026 != 0)
  )


eib_projects <- split_proj_list(
  proj_list,
  proj_plans
)

# Check the number of projects by type
map(
  eib_projects,
  nrow
)

# Check if any invalid plans are included
proj_plans |>
  dplyr::filter(
    `Project ID` %in% budget_year_projects$`Project ID`,
    !`Valid Plan`
  ) |>
  glimpse()

# Join the Plans data to the CIP data for the EIB input budget data
budget_data <- cip_6yr |>
  dplyr::left_join(
    proj_plans |>
      dplyr::filter(
        `Valid Plan`
      ) |>
      dplyr::select(
        `Project ID`,
        Fund,
        `Budget Name` = Plan,
        # Fiscal Year and Fiscal Time Interval columns may use different names
        # in different EIBs
        `Fiscal Year*`,
        `Fiscal Time Interval*`
      ),
    by = dplyr::join_by(`Project ID`)
  ) |>
  dplyr::left_join(
    proj_budget_ids |>
      dplyr::select(
        `Budget Name` = `Plan Name`,
        `Project Budget ID`
      ),
    by = dplyr::join_by(`Budget Name`)
  ) |>
  dplyr::mutate(
    # NOTE: This is a placeholder to fill in the project name as the budget name
    # for projects w/ no existing budget
    `Budget Name` = dplyr::coalesce(
      `Project Budget ID`,
      `Budget Name`,
      Project
    )
  ) |>
  dplyr::select(
    -c(Project, `Cost Center`, Fund, `Revenue Category`)
  ) |>
  dplyr::rename(
    # Project = `Project ID`,
    Fund = `Fund ID`,
    `Cost Center` = `Cost Center ID`,
    Grant = `Grant ID`,
    `Revenue Category` = `Revenue Category ID`
  ) |>
  dplyr::mutate(
    `Project` = `Project ID`
  ) |>
  dplyr::filter(
  !(
    Project %in%
    c(
      "PRJ003540",
      "PRJ003504",
      "PRJ001876",
      "PRJ002143"
    )
  )
)

post_fy_start_dates <- proj_list |>
  dplyr::filter(
   as.Date(`Project Start Date`) > as.Date("2025-07-01"),
   `Project ID` %in% budget_year_projects$`Project ID`
  ) |>
  left_join(
    proj_plans
  )

  budget_data |>
    dplyr::filter(
      FY2026 != 0,
    )

budget_data_split <- split_proj_list(
  budget_data,
  proj_plans
)

# today_text <- janitor::make_clean_names(paste0("_", lubridate::today()))

# "put_budget_template"
now_txt <- as.character(lubridate::date(lubridate::now()))

budget_data_split$put_budget_template |>
  build_put_budget_template_wb(
    file_list$dictionaries$put_budget_template$dict,
    file_list$templates$put_budget_template$wb
  ) |>
  openxlsx2::wb_save(
    file = glue::glue(
      "Put_Budget_Template_{now_txt}_Final2.xlsx"
    )
  )

# "import_budget"
# COB Import Project Active Budget
budget_data_split$import_budget |>
  dplyr::filter(
    !is.na(FY2026) & (FY2026 != 0)
  ) |>
  dplyr::mutate(
    `Year` = `Fiscal Year*`
  ) |>
  build_import_budget_wb(
    file_list$dictionaries$import_budget$dict,
    file_list$templates$import_budget$wb,
    amount_col = "FY2026",
    description = "FY-26 Capital Budget allocation for new project",
    memo = "FY2026 Capital Budget"
  ) |>
  openxlsx2::wb_save(
    file = glue::glue(
      "COB_Project_Budget_Import_{now_txt}_Final2.xlsx"
    )
  )


  # budget_data_split$amend_budget |>
#   dplyr::filter(
#     `Project ID` %in% "PRJ001978"
#   ) |>
#   View()

# "amend_budget"
budget_data_split$amend_budget |>
  dplyr::filter(
    !is.na(FY2026) & (FY2026 != 0)
  ) |>
  build_amend_budget_wb(
    file_list$dictionaries$amend_budget$dict,
    file_list$templates$amend_budget$wb,
    amount_col = "FY2026",
    description = "FY-26 Capital Budget allocation for existing project",
    memo = "FY2026 Capital Budget"
  ) |>
  openxlsx2::wb_save(
    file = glue::glue(
      "9953_Data_load_{now_txt}_Final.xlsx"
    )
  )

  cip_6yr |>
    baltimoreCIPutils::summarise_timespan()

    cip_6yr |>
      dplyr::filter(
        `Revenue Category ID` %in% c("RC0603", "RC0602"),
        is.na(`Grant ID`),
        # !is.na(FY2026),
        FY2026 > 0
      ) |>
      openxlsx2Extras::as_wb() |>
      openxlsx2::wb_save(
        "2025-07-01_FY2026_Projects-Missing-Grant-Worktags.xlsx"
      )


    cip_6yr |>
      dplyr::filter(
        !(`Project ID` %in%
          c(
            "PRJ001876",
            "PRJ002143",
            "PRJ003540",
            "PRJ003504"
          ))
      ) |>
      baltimoreCIPutils::summarise_timespan(
      )
