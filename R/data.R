#' Workday Project to Project Hierarchy 1 and 2 Crosswalk
#'
#' Reference data to join standard agency, bureau, division, or other entity
#' names to Workday capital project data based on "PHierarchy1 Code" and
#' "PHierarchy2 Code" columns. Last updated 2024-10-16.
#'
#' @format A data frame with 58 rows and 9 variables:
#' \describe{
#'   \item{`source`}{Source description}
#'   \item{`id`}{Source ID value}
#'   \item{`name`}{Source name value}
#'   \item{`entity`}{Matching entity name}
#'   \item{`use`}{Source ID or name usage (active or inactive)}
#'   \item{`notes`}{Notes}
#'   \item{`createdTime`}{Created date/time for record}
#'   \item{`PHierarchy1 Code`}{Join key for formatted "PHierarchy1 Code" column}
#'   \item{`PHierarchy2 Code`}{Join key for formatted "PHierarchy2 Code" column}
#' }
#' @details <https://airtable.com/app1lcJCwi0mpQGqZ/tbl81zsVzjBxVZePB/viwlrhbxPrDasYqzp?blocks=hide>
"wd_proj_hierarchy_xwalk"

#' Workday Project to Asset ID Crosswalk
#'
#' Reference data to join asset ID values to Workday Projects. Last updated
#' 2024-11-19.
#'
#' @format A data frame with 796 rows and 6 variables:
#' \describe{
#'   \item{`asset_id`}{Asset ID value, primarily DGS Building ID numbers}
#'   \item{`Project Code`}{Join key for Project Code}
#'   \item{`type_match`}{Type of match (parent asset or same asset)}
#'   \item{`match`}{Match certainty or precision}
#'   \item{`fy_scope`}{Flag for matches that are specific to a FY range}
#'   \item{`notes`}{Notes}
#' }
#' @details <https://docs.google.com/spreadsheets/d/1jEgFt1-8a96IxzSyUU2J_tqoII5qL4utWiK6v4OBIBw/edit?usp=sharing>
"wd_proj_asset_xwalk"

#' Workday Project to Related Plan Crosswalk
#'
#' See `baltimoredata::baltimore_plans` for data with matching Airtable record
#' ID values.
#'
#' @format A data frame with 81 rows and 8 variables:
#' \describe{
#'   \item{`Project Code`}{Join key for Project Code}
#'   \item{`plan`}{Related plan name (from Adaptive Planning Project Details sheet)}
#'   \item{`title`}{Related plan title (from reference Airtable base)}
#'   \item{`year_complete`}{Year completed (from reference Airtable base)}
#'   \item{`document_url`}{Document URL (from reference Airtable base)}
#'   \item{`site_url`}{Related website URL (from reference Airtable base)}
#'   \item{`airtable_record_id`}{Airtable record ID (from reference Airtable base)}
#'   \item{`notes`}{Notes}
#' }
#' @details <https://docs.google.com/spreadsheets/d/1jEgFt1-8a96IxzSyUU2J_tqoII5qL4utWiK6v4OBIBw/edit?usp=sharing>
"wd_proj_related_plan_xwalk"

#' String patterns for capital projects
#'
#' A named list of string patterns for use with this package. Used for string
#' extraction functions.
#'
#' @format A length `r length(cap_patterns)` list with named patterns:
#' `r knitr::combine_words(names(cap_patterns))`
"cap_patterns"

#' Updated Names and Descriptions for Workday Projects
#'
#' `wd_proj_detail_updates`
#'
#' @format A data frame with 462 rows and 5 variables:
#' \describe{
#'   \item{`Project Code`}{Workday Project Code}
#'   \item{`Project Name Updated`}{Updated project name}
#'   \item{`Name Justification`}{Justification for name update}
#'   \item{`Project Desc Updated`}{Updated project description}
#'   \item{`Desc Justification`}{Justification for description update}
#' }
#' @details <https://docs.google.com/spreadsheets/d/1LFjKUq_OgrrvZeXC5rZ9jgqZtqltnG8NLG9NDplvMtg/edit?usp=sharing>
"wd_proj_detail_updates"

#' CIP Number to Workday Project Crosswalk
#'
#' `wd_proj_cip_num_xwalk` is a cross-reference to support the integration of
#' legacy data on projects referenced by CIP Number with new data using Workday
#' Project Codes. The data may include inaccurate cross-references. Last updated
#' 2024-11-23.
#'
#' @format A data frame with 640 rows and 4 variables:
#' \describe{
#'   \item{`cip_number`}{CIP Number}
#'   \item{`project_code`}{Project Codes}
#'   \item{`accuracy_notes`}{Notes on match accuracy}
#'   \item{`source_notes`}{Notes on match source}
#'}
#' @details <https://docs.google.com/spreadsheets/d/1hZY-O_jO9VXvTQ_mol0VXwF4EUQJjZijHpVAZssuNLk/edit?usp=sharing>
"wd_proj_cip_num_xwalk"
