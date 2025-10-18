#' Workday Project to Project Hierarchy 1 and 2 Crosswalk
#'
#' Reference data to join standard agency, bureau, division, or other entity
#' names to Workday capital project data based on "PHierarchy1 Code",
#' "PHierarchy2 Code", or "Cost Center Code" columns. Last updated 2024-12-19.
#'
#' @format A data frame with 128 rows and 10 variables:
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
#'   \item{`Cost Center Code`}{Join key for formatted "Cost Center Code" column}
#' }
#' @details <https://airtable.com/app1lcJCwi0mpQGqZ/tbl81zsVzjBxVZePB/viwlrhbxPrDasYqzp?blocks=hide>
"wd_proj_hierarchy_xwalk"

#' Agency Worktag Reference
#'
#' Reference data on capital agencies. Last updated 2025-10-18.
#'
#' @format A data frame with 17 rows and 6 variables:
#' \describe{
#'   \item{`Agency Name`}{Agency name}
#'   \item{`Agency Label`}{Short agency label}
#'   \item{`Agency`}{Agency worktag}
#'   \item{`Agency ID`}{Agency ID}
#'   \item{`Cost Center`}{Cost Center worktag}
#'   \item{`Cost Center ID`}{Cost Center ID}
#'}
#' @details From Citywide CIP Contact List file on DOP-CIP SharePoint site.
"wd_agencies"

#' Workday Project to Asset ID Crosswalk
#'
#' Reference data to join asset ID values to Workday Projects. Last updated
#' 2025-09-24.
#'
#' @format A data frame with 817 rows and 7 variables:
#' \describe{
#'   \item{`asset_id`}{Asset ID value, primarily DGS Building ID numbers}
#'   \item{`Project Code`}{Join key for Project Code}
#'   \item{`agency`}{Agency name (incomplete)}
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
#' 2025-04-21.
#'
#' @format A data frame with 747 rows and 4 variables:
#' \describe{
#'   \item{`cip_number`}{CIP Number}
#'   \item{`project_code`}{Project Codes}
#'   \item{`accuracy_notes`}{Notes on match accuracy}
#'   \item{`source_notes`}{Notes on match source}
#' }
#' @details <https://docs.google.com/spreadsheets/d/1hZY-O_jO9VXvTQ_mol0VXwF4EUQJjZijHpVAZssuNLk/edit?usp=sharing>
"wd_proj_cip_num_xwalk"

#' Workday Revenue Category Label Crosswalk
#'
#' `wd_revenue_category_xwalk` is a data frame with labels and short names for
#' revenue categories. Last updated 2025-03-13.
#'
#' @format A data frame with 43 rows and 11 variables:
#' \describe{
#'   \item{`Effective Year`}{Effective year (GO Bonds only)}
#'   \item{`Revenue Category`}{Revenue Category code and name combined}
#'   \item{`Revenue Category Code`}{Revenue Category code}
#'   \item{`Revenue Category Name`}{Revenue Category name}
#'   \item{`Prior Revenue Category Code`}{Prior equivalent revenue category code (GO Bonds only)}
#'   \item{`Prior Revenue Category Name`}{Prior equivalent revenue category name (GO Bonds only)}
#'   \item{`Revenue Category Label`}{Revenue Category label}
#'   \item{`Revenue Category Label Short`}{Revenue Category short label}
#'   \item{`Revenue Category Group`}{Revenue Category group}
#'   \item{`Request Category`}{Request Category}
#'   \item{`Request Category Pos`}{Request Category position (overall sort order)}
#' }
#' @details <https://docs.google.com/spreadsheets/d/1jEgFt1-8a96IxzSyUU2J_tqoII5qL4utWiK6v4OBIBw/edit?usp=sharing>
"wd_revenue_category_xwalk"
