#' Load Workday Project Hierarchy-Baltimore City Public Entity Crosswalk from Airtable
load_wd_proj_hierarchy_xwalk <- function(
    url = "https://airtable.com/app1lcJCwi0mpQGqZ/tbl81zsVzjBxVZePB/viwlrhbxPrDasYqzp?blocks=hide",
    cell_format = "string",
    ...) {
  # pak::pkg_install("matthewjrogers/rairtable@dev")
  xwalks <- rairtable::list_records(
    url = url,
    filter = "REGEX_MATCH(source, 'Adaptive Planning')",
    cell_format = cell_format,
    ...
  )

  xwalks |>
    dplyr::select(source, id, name, entity, use, notes, createdTime) |>
    dplyr::mutate(
      `PHierarchy1 Code` = dplyr::if_else(
        stringr::str_detect(source, "1"),
        id,
        NA_character_
      ),
      `PHierarchy2 Code` = dplyr::if_else(
        stringr::str_detect(source, "2"),
        id,
        NA_character_
      )
    )
}

wd_proj_hierarchy_xwalk <- load_wd_proj_hierarchy_xwalk()

usethis::use_data(wd_proj_hierarchy_xwalk, overwrite = TRUE)

#' Load Asset ID crosswalk from Workday Capital Project Crosswalks Google Sheet
load_wd_proj_asset_xwalk <- function(
    url = "https://docs.google.com/spreadsheets/d/1jEgFt1-8a96IxzSyUU2J_tqoII5qL4utWiK6v4OBIBw/edit?usp=sharing",
    cell_format = "string",
    ...) {
  wd_proj_asset_xwalk <- googlesheets4::read_sheet(
    ss = url,
    sheet = "asset_id_xwalk"
  )

  wd_proj_asset_xwalk |>
    dplyr::select(
      !contains("name")
    ) |>
    dplyr::rename(
      `Project Code` = project_code
    )
}

wd_proj_asset_xwalk <- load_wd_proj_asset_xwalk()

usethis::use_data(wd_proj_asset_xwalk, overwrite = TRUE)


#' Load Related Plan crosswalk from Workday Capital Project Crosswalks Google Sheet
load_wd_proj_related_plan_xwalk <- function(
    url = "https://docs.google.com/spreadsheets/d/1jEgFt1-8a96IxzSyUU2J_tqoII5qL4utWiK6v4OBIBw/edit?usp=sharing",
    cell_format = "string",
    ...) {
  wd_proj_related_plan_xwalk <- googlesheets4::read_sheet(
    ss = url,
    sheet = "related_plan_xwalk"
  )

  wd_proj_related_plan_xwalk |>
    dplyr::filter(
      !is.na(airtable_record_id)
    ) |>
    dplyr::rename(
      `Project Code` = project_code
    )
}

wd_proj_related_plan_xwalk <- load_wd_proj_related_plan_xwalk()

usethis::use_data(wd_proj_related_plan_xwalk, overwrite = TRUE)
