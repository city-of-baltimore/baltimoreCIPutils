# pkg_install_cip_utils <- function(
#   pkg = "/Users/elipousson/Projects/00_dop/baltimoreCIPutils",
#   reinstall = TRUE,
#   ask = FALSE,
#   dependencies = TRUE
# ) {
#   # pak::cache_delete(package = "baltimoreCIPutils")

#   if (reinstall) {
#     pkg <- paste0(pkg, "?reinstall=true")
#   }

#   if (fs::file_exists(pkg)) {
#     pak::local_install(pkg, ask = ask, dependencies = dependencies)
#   } else {
#     pak::pkg_install(pkg, ask = ask, dependencies = dependencies)
#   }
# }

read_proj_list <- function(
  file,
  fy_start_date = lubridate::date("2025-07-01"),
  start_row = 7
) {
  baltimoreCIPutils::wd_read_report(
    file,
    start_row = start_row
  ) # |>
  # validate_project_dates
  # TODO: Check if this is all still appropriate and what to use it for
  # validate_project_dates(
  #   curr_fy_start_date = fy_start_date
  # )
}

#' Read and format project plans Workday report
read_proj_plans <- function(file, ..., start_row = 7) {
  proj_plans <- baltimoreCIPutils::wd_read_report(
    file,
    start_row = start_row
  )

  # fmt_proj_plans(proj_plans)
  proj_plans
}

#' Format project plans for use with EIB
fmt_proj_plans <- function(.data) {
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

list_input_files <- function(
  input = "_files.yml",
  type = NULL,
  dir = here::here()
) {
  input_reference <- yaml::yaml.load_file(fs::path(dir, input))

  for (nm in names(input_reference)) {
    type_reference <- input_reference[[nm]]

    type_reference <- rlang::set_names(
      type_reference,
      purrr::map_chr(
        type_reference,
        "id"
      )
    )

    if (fs::is_dir(dir)) {
      type_reference <- purrr::map(
        type_reference,
        \(x) {
          x[["path"]] <- fs::path(dir, x[["filename"]])

          x
        }
      )
    }

    input_reference[[nm]] <- type_reference
  }

  if (!is.null(type)) {
    return(input_reference[[type]])
  }

  input_reference
}

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
