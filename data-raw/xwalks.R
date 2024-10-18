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

cap_patterns <- list(
  "contract_num" = "(TR |Tr |TR|TR-|SWC|SWC |SWC-|WC|WC-|WC |SDC|SDC |SDC-|SC |SC|SC-|ER |ER-|ER)[:digit:]+",
  "cost_center" = "^(CAP|RES|zzDNU_CAP|DNU_CAP)[:digit:]+",
  "cip_num" = "[:digit:]{3}\\-[:digit:]{3}"
)

usethis::use_data(cap_patterns, overwrite = TRUE)

project_details <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing",
  sheet = "project_detail_updates"
)

wd_proj_detail_updates <- project_details |>
  filter(!is.na(project_name_updated) | !is.na(project_desc_updated)) |>
  select(
    project_code, project_name_updated, name_justification,
    project_desc_updated, desc_justification
  ) |>
  set_names(
    \(x) {
      stringr::str_to_title(stringr::str_replace_all(x, "_", " "))
    }
  )

usethis::use_data(wd_proj_detail_updates, overwrite = TRUE)
