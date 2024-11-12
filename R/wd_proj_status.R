#' Format project status information as a project status crosswalk
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' [fmt_wd_proj_lifecycle()] takes project status information and creates a
#' crosswalk of projects based on planned transfers or consolidations.
#'
#' @param data Data frame with status information. By default must have columns
#'   named "Milestone Name" and "Milestone Name".
#' @param status_cols Named vector with existing and new column names including
#'   status value and explanation.
#' @export
#' @importFrom dplyr bind_rows
fmt_wd_proj_lifecycle <- function(data,
                                  status_cols = c(
                                    "Status" = "Milestone Name",
                                    "Status Explanation" = "Milestone Explanation"
                                  )) {
  check_installed("tidyr")

  related_project_data <- data |>
    select(
      `Project Code`,
      `Project Name`,
      all_of(status_cols)
    ) |>
    filter(str_detect(`Status Explanation`, "PRJ")) |>
    mutate(
      `Related Project Code` = str_extract_all_project_codes(
        `Status Explanation`
      ),
      Relationship = case_when(
        str_detect(
          `Status Explanation`,
          "Project canceled|Transfer to|Transferring to|MOVE BALANCE|Move Balance|Transferring fund to|TRANSFER RESERVE AMOUNT|Transferring Funds to") ~ "Transfer balance to another project",
        str_detect(
          `Status Explanation`,
          "Waiting transfers|MAKE PRIMARY ACCOUNT|INTO THIS ACCOUNT") ~ "Transfer balance from other projects",
        str_detect(
          `Status Explanation`,
          "Reserve for|Reserve project for") ~ "Hold in reserve for another project",
        str_detect(
          `Status Explanation`,
          "Refer to") ~ "Superseded by another project"
      )
    ) |>
    tidyr::unnest_longer(
      col = `Related Project Code`
    ) |>
    mutate(
      lifecycle = case_match(
        Relationship,
        "Transfer balance to another project" ~ "retired",
        "Superseded by another project" ~ "superseded",
        "Hold in reserve for another project" ~ "reserve",
        .default = "active"
      )
    ) |>
    add_count(
      `Project Code`,
      name = "Related Project Count"
    )

  data |>
    select(
      `Project Code`,
      `Project Name`,
      all_of(status_cols)
    ) |>
    filter(!str_detect(`Status Explanation`, "PRJ")) |>
    mutate(
      lifecycle = case_when(
        Status == "Canceled" ~ "retired",
        .default = "active"
      )
    ) |>
    bind_rows(
      related_project_data
    )
}

