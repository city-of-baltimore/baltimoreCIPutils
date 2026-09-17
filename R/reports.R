# Functions below prepare data for the annual static CIP reports and expect
# the newer SharePoint-list-derived schema (e.g. `ProjectID`, `SplitChildRow`,
# `RevenueCategoryID`, `Year1`..`Year6`), not the spaced Adaptive Planning
# export column names (e.g. "Project Code", "Is Split Child Row") used
# elsewhere in the package (see capital.R, comparison.R, adaptive-planning.R).

#' Add a row-wise identity key column
#'
#' Concatenates `key_cols` into a single text column, one value per row, so
#' rows that share the same combination of `key_cols` values can be detected
#' (see [remove_split_parent_rows()]). `key_cols` are cast to character in
#' temporary columns before concatenating, because `dplyr::c_across()`
#' combines columns with `vctrs::vec_c()`, which -- unlike `paste()` -- errors
#' when mixing character and numeric columns without an explicit cast; using
#' temporary columns also keeps `key_cols`'s original types unchanged in the
#' returned data.
#'
#' @param data A data frame.
#' @param key_cols Character vector of column names in `data` to concatenate
#'   into the identity key.
#' @param key_name Name to use for the new key column.
#' @returns `data` with an additional column named `key_name` containing the
#'   concatenated key for each row.
#' @keywords internal
add_group_key <- function(data, key_cols, key_name = ".group_key") {
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(key_cols),
        as.character,
        .names = ".key_{.col}"
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      "{key_name}" := paste(
        dplyr::c_across(dplyr::starts_with(".key_")),
        collapse = "||"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::starts_with(".key_"))
}

#' Remove parent rows from split budget requests in program data
#'
#' `program_data` can include, for a single budget request, both a "parent"
#' row that aggregates the request across phases and one "child" row per
#' phase (flagged `SplitChildRow == TRUE`); the parent's amount in every
#' `Year1`..`Year6` column is the sum of its children's amounts in that same
#' column. This function removes each such parent row -- identified as a
#' `SplitChildRow == FALSE` row that shares every value in `key_cols` with at
#' least one `SplitChildRow == TRUE` row -- so downstream totals aren't
#' double-counted.
#'
#' Unlike matching on `Phase == "<multiple>"` (a placeholder set upstream
#' when a split's children span more than one phase), this identifies parent
#' rows directly from `SplitChildRow`, so it isn't fooled by a split whose
#' children happen to all share a single phase.
#'
#' `key_cols` is not guaranteed to be a unique row identity: two unrelated,
#' independently entered rows can legitimately share every `key_cols` value
#' (e.g. the same request logged for two different phases without being a
#' split). That's harmless on its own, but if such a row ever coincided with
#' a real split under the same key, there would be no way to tell which
#' non-child row is the true parent to drop. Rather than guess, this function
#' aborts with [cli::cli_abort()] when a `key_cols` group has both more than
#' one `SplitChildRow == FALSE` row and at least one `SplitChildRow == TRUE`
#' row, so the duplicate can be resolved upstream instead.
#'
#' @param program_data Program data with a logical `SplitChildRow` column and
#'   the columns named in `key_cols`.
#' @param key_cols Character vector of column names that, together, identify
#'   a single split budget request: a parent row and its `SplitChildRow ==
#'   TRUE` children share every one of these values, differing only in
#'   `Phase` and the `Year1`..`Year6` amounts.
#' @returns `program_data` with parent rows of split budget requests removed.
#' @seealso [add_group_key()]
#' @keywords internal
remove_split_parent_rows <- function(
  program_data,
  key_cols = c(
    "ProjectID",
    "CostCenterID",
    "FundID",
    "GrantID",
    "RevenueCategoryID",
    "Account",
    "RequestType",
    "ChangeType",
    "FiscalYear",
    "ProgramVersion",
    "AgencyID"
  )
) {
  program_data <- program_data |>
    add_group_key(key_cols)

  ambiguous <- program_data |>
    dplyr::summarise(
      n_parent = sum(!SplitChildRow),
      n_child = sum(SplitChildRow),
      .by = .group_key
    ) |>
    dplyr::filter(n_parent > 1, n_child > 0)

  if (nrow(ambiguous) > 0) {
    cli::cli_abort(c(
      "Found {nrow(ambiguous)} split group{?s} with more than one non-child row.",
      "i" = "Can't tell which non-child row is the true parent to drop -- resolve the duplicate(s) upstream."
    ))
  }

  program_data |>
    dplyr::filter(
      !(!SplitChildRow & any(SplitChildRow)),
      .by = .group_key
    ) |>
    dplyr::select(-.group_key)
}

#' Format Capital Program data for the project details report
#'
#' Prepares `program_data` (the Six-Year CIP program data) for inclusion in
#' the project details report: joins `ProjectName` from `project_data`,
#' ensures a `ChangeType` column exists, removes split parent rows (see
#' [remove_split_parent_rows()]), labels revenue categories, recodes `Phase`
#' and `RequestType` into report-ready factors, and sorts by agency, cost
#' center, project, request type, revenue category, and phase.
#'
#' @param program_data Program data from the Six-Year CIP sheet, including
#'   `ProjectID`, `SplitChildRow`, `Phase`, `RequestType`, `RevenueCategoryID`,
#'   and the `Year1`..`Year6` amount columns.
#' @param project_data Project data including `ProjectID` and `ProjectName`,
#'   joined onto `program_data` to add project names.
#' @returns `program_data` formatted for the project details report, with
#'   split parent rows removed and `Phase`/`RequestType` recoded as factors.
#' @export
fmt_report_program_data <- function(
  program_data,
  project_data
) {
  program_data <- program_data |>
    # Join ProjectName from project_data
    dplyr::left_join(
      project_data |>
        dplyr::select(ProjectID, ProjectName),
      by = dplyr::join_by(ProjectID)
    )

  # Ensure ChangeType column is available
  # NOTE: This was required when reading program_data from a SharePoint list source using sharepointr from prior to changes 2026-09
  if (!rlang::has_name(program_data, "ChangeType")) {
    program_data <- program_data |>
      dplyr::mutate(
        ChangeType = NA_character_
      )
  }

  program_data |>
    dplyr::relocate(
      AgencyName,
      Project,
      Fund,
      CostCenter,
      Grant,
      RevenueCategory,
      Phase,
      RequestType,
      ChangeType,
      ChangeJustification,
      Year1,
      Year2,
      Year3,
      Year4,
      Year5,
      Year6,
      .before = tidyselect::everything()
    ) |>
    # Remove parent rows
    remove_split_parent_rows() |>
    # Add `Revenue Category Code` column for compatibility w/
    # wd_revenue_category_label
    # TODO: Adjust baltimoreCIPutils::wd_revenue_category_label to support new column naming conventions
    dplyr::mutate(`Revenue Category Code` = RevenueCategoryID) |>
    baltimoreCIPutils::wd_revenue_category_label() |>
    dplyr::mutate(
      Phase = dplyr::case_when(
        # Planning/Pre-Design introduced in FY2027 and should be removed
        Phase == "Not Applicable" ~ NA_character_,
        # Planning/Pre-Design introduced in FY2027 but not used?
        Phase == "Planning/Pre-Design" ~ "Planning",
        # Planning/Design Phase used in FY2026 only
        Phase == "Planning/Design" ~ "Design",
        Phase == "Information Technology" ~ "IT",
        .default = Phase
      ),
      # Replace NA RequestType values with Base Budget
      RequestType = dplyr::coalesce(RequestType, "Base Budget")
    ) |>
    fmt_fct_recode(
      col = "Phase",
      levels = c(
        "Planning",
        "Design",
        "Construction",
        "Post-construction",
        "IT"
      )
    ) |>
    fmt_fct_recode(
      col = "RequestType",
      levels = c("Base Budget", "Special Project", "Additional Priority")
    ) |>
    dplyr::arrange(
      AgencyName,
      CostCenter,
      ProjectID,
      dplyr::desc(RequestType),
      RevenueCategory,
      Phase
    )
}

#' Recode project category, purpose, and type labels for the report
#'
#' Shortens the display labels for `ProjectProgramType`,
#' `ProjectProgramPurpose`, and `ProjectCategory` (e.g. "Public Safety
#' Technology" becomes "Public Safety Tech."). Each `*_col` argument names
#' the column to write the recoded values to; by default these match the
#' source column names, so the source columns are overwritten in place. Pass
#' a different name to write the recoded values into a new column instead
#' and leave the source column untouched.
#'
#' @param project_data Project data with `ProjectProgramType`,
#'   `ProjectProgramPurpose`, and `ProjectCategory` columns.
#' @param project_program_type_col,project_program_purpose_col,project_category_col
#'   Names of the columns to write the recoded `ProjectProgramType`,
#'   `ProjectProgramPurpose`, and `ProjectCategory` values to. Default to the
#'   source column names (overwriting them in place).
#' @returns `project_data` with the recoded label columns added or replaced.
#' @keywords internal
recode_project_details <- function(
  project_data,
  project_program_type_col = "ProjectProgramType",
  project_program_purpose_col = "ProjectProgramPurpose",
  project_category_col = "ProjectCategory"
) {
  project_data |>
    dplyr::mutate(
      "{project_program_type_col}" := dplyr::recode_values(
        ProjectProgramType,
        "Grant and Loan Program" ~ "Grant/Loan Program",
        default = ProjectProgramType
      ),
      "{project_program_purpose_col}" := dplyr::recode_values(
        ProjectProgramPurpose,
        "Community or Economic Development" ~ "Community/Economic Development",
        default = ProjectProgramPurpose
      ),
      "{project_category_col}" := dplyr::recode_values(
        ProjectCategory,
        "Public Safety Technology" ~ "Public Safety Tech.",
        "Streets and Highways" ~ "Streets/Highways",
        "Alleys and Footways" ~ "Alleys/Footways",
        default = ProjectCategory
      )
    )
}

#' Clean up project location names for display
#'
#' Trims whitespace from `LocationName` and recodes a few placeholder values
#' ("City-wide", "TBD", "N/A") to their report display text.
#'
#' @param project_data Project data with a `LocationName` column.
#' @returns `project_data` with `LocationName` trimmed and recoded.
#' @keywords internal
clean_project_location_name <- function(project_data) {
  project_data |>
    dplyr::mutate(
      LocationName = stringr::str_trim(LocationName),
      LocationName = dplyr::case_when(
        LocationName == "City-wide" ~ "Citywide",
        LocationName == "TBD" ~ "To be determined",
        LocationName == "N/A" ~ NA_character_,
        .default = LocationName
      )
    )
}

#' Add a project's report status based on program request type
#'
#' Flags each project as `"Base Budget"` if it has a `program_data` row with
#' `RequestType` of `"Base Budget"` or `"Special Project"`, or
#' `"Additional Priority"` if it has a row with `RequestType` of
#' `"Additional Priority"`, else `NA`.
#'
#' @param project_data Project data with a `ProjectID` column.
#' @param program_data Program data with `ProjectID` and `RequestType`
#'   columns, used to classify each project's current-year request type.
#' @returns `project_data` with an added `ReportStatus` column.
#' @keywords internal
add_report_status <- function(project_data, program_data) {
  curr_yr_program <- program_data |>
    dplyr::filter(
      RequestType %in% c("Base Budget", "Special Project")
    )

  curr_yr_additional_priorities <- program_data |>
    dplyr::filter(
      RequestType == "Additional Priority"
    )

  project_data |>
    dplyr::mutate(
      ReportStatus = dplyr::case_when(
        ProjectID %in% unique(curr_yr_program$ProjectID) ~ "Base Budget",
        ProjectID %in%
          unique(
            curr_yr_additional_priorities$ProjectID
          ) ~ "Additional Priority",
        .default = NA_character_
      )
    )
}

#' Format columns as currency display text
#'
#' Formats `cols` as currency text with [gt::vec_fmt_currency()], keeping
#' `NA` values as `NA` rather than a formatted string. Generic and not tied
#' to any particular report field, so unrelated currency columns (e.g.
#' `OperatingBudgetImpactAmount` and `TargetProgramFundingLevel`) can share
#' the same formatting without being coupled to each other.
#'
#' @param data A data frame.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to format
#'   as currency text. Required; there is no default.
#' @param suffixing Passed to [gt::vec_fmt_currency()].
#' @param decimals Passed to [gt::vec_fmt_currency()].
#' @param output_format Passed to [gt::vec_fmt_currency()]'s `output`
#'   argument.
#' @returns `data` with `cols` formatted as currency text.
#' @keywords internal
fmt_currency_columns <- function(
  data,
  cols,
  suffixing = "K",
  decimals = 0,
  output_format = "latex"
) {
  check_installed("gt")

  data |>
    dplyr::mutate(
      dplyr::across(
        {{ cols }},
        \(x) {
          dplyr::if_else(
            is.na(x),
            NA_character_,
            gt::vec_fmt_currency(
              x,
              suffixing = suffixing,
              decimals = decimals,
              output = output_format
            )
          )
        }
      )
    )
}

#' Add a combined operating budget impact description
#'
#' Builds `OperatingBudgetImpactDesc`, combining the (already
#' currency-formatted) `OperatingBudgetImpactAmount` with a year/frequency
#' phrase into one display-ready string, e.g. `"$500K annually"` or `"$500K
#' expected in 2028"`. Must run after `OperatingBudgetImpactAmount` has been
#' formatted as currency text (see [fmt_currency_columns()]), since it
#' prefixes that formatted text directly. `OperatingBudgetImpactYear` itself
#' is left unmodified.
#'
#' @param project_data Project data with `OperatingBudgetImpact`,
#'   `OperatingBudgetImpactYear`, and a currency-formatted
#'   `OperatingBudgetImpactAmount` column.
#' @returns `project_data` with an added `OperatingBudgetImpactDesc` column.
#' @seealso [fmt_currency_columns()]
#' @keywords internal
add_operating_budget_impact_desc <- function(project_data) {
  project_data |>
    dplyr::mutate(
      OperatingBudgetImpactDesc = dplyr::case_when(
        OperatingBudgetImpact == "Expected operating impact" &
          is.na(OperatingBudgetImpactYear) &
          !is.na(OperatingBudgetImpactAmount) ~ paste0(
          OperatingBudgetImpactAmount,
          " annually"
        ),
        OperatingBudgetImpact == "Expected operating impact" &
          !is.na(OperatingBudgetImpactYear) &
          !is.na(OperatingBudgetImpactAmount) ~ paste0(
          OperatingBudgetImpactAmount,
          " expected in ",
          OperatingBudgetImpactYear
        ),
        .default = as.character(OperatingBudgetImpactYear)
      )
    )
}

#' Join active cost estimate totals onto project data
#'
#' @param project_data Project data with a `ProjectID` column.
#' @param estimate_data Cost estimate data, passed to
#'   [fmt_active_estimate_data()] to compute `DesignCost`, `ConstructionCost`,
#'   and `OtherCost` for each project.
#' @param active_submission_status Passed to
#'   [fmt_active_estimate_data()]'s `active_submission_status` argument.
#' @returns `project_data` joined with `DesignCost`, `ConstructionCost`, and
#'   `OtherCost` for each project.
#' @keywords internal
join_estimate_costs <- function(
  project_data,
  estimate_data,
  active_submission_status = c("Submitted", "Draft")
) {
  project_data |>
    dplyr::left_join(
      estimate_data |>
        fmt_active_estimate_data(
          active_submission_status = active_submission_status
        ) |>
        dplyr::select(
          ProjectID,
          DesignCost,
          ConstructionCost,
          OtherCost
        ),
      by = dplyr::join_by(ProjectID)
    )
}

#' Format project details for inclusion in the details report
#'
#' Joins `project_data` with 6-year program totals (from
#' [sum_6yr_program_totals()]) and active cost estimate totals (from
#' [join_estimate_costs()]), drops projects with a zero total program
#' amount, recodes several category, purpose, and type labels for the
#' report (see [recode_project_details()]), and formats location, report
#' status, and operating budget impact fields for display (see
#' [clean_project_location_name()], [add_report_status()],
#' [fmt_currency_columns()], and [add_operating_budget_impact_desc()]).
#'
#' @param project_data Project data with columns including `ProjectID`,
#'   `ProjectProgramType`, `ProjectProgramPurpose`, `ProjectCategory`,
#'   `LocationName`, `OperatingBudgetImpact`, `OperatingBudgetImpactAmount`,
#'   `OperatingBudgetImpactYear`, and `TargetProgramFundingLevel`.
#' @param program_data Program data from the Six-Year CIP sheet, passed to
#'   [sum_6yr_program_totals()] and [add_report_status()]; must include
#'   `ProjectID` and `RequestType`.
#' @param estimate_data Cost estimate data, passed to
#'   [join_estimate_costs()].
#' @param output_format Passed to [fmt_currency_columns()]'s `output_format`
#'   argument.
#' @param active_submission_status Passed to [join_estimate_costs()]'s
#'   `active_submission_status` argument.
#' @returns `project_data` joined with program totals and estimate costs,
#'   filtered to projects with a non-zero total program amount, with
#'   currency and label columns formatted for the report.
#' @export
fmt_report_data <- function(
  project_data,
  program_data,
  estimate_data,
  output_format = "latex",
  active_submission_status = c("Submitted", "Draft")
) {
  project_data |>
    # Join Year1ProgramAmount and TotalProgramAmount columns
    # Assumes that program_data is only a single version
    # FIXME: Add a check to confirm that program_data only includes a single version
    dplyr::left_join(
      sum_6yr_program_totals(
        program_data,
        start_year_sum_col = "Year1ProgramAmount",
        total_sum_col = "TotalProgramAmount"
      ),
      by = dplyr::join_by(ProjectID)
    ) |>
    # Drop projects with no program amount
    dplyr::filter(
      TotalProgramAmount != 0
    ) |>
    dplyr::arrange(
      AgencyName,
      CostCenter,
      ProjectID
    ) |>
    # TODO: Don't overwrite the existing fields - create variants w/ a Abb suffix
    recode_project_details() |>
    clean_project_location_name() |>
    # FIXME: Remove ReportStatus if not used
    add_report_status(program_data) |>
    fmt_currency_columns(
      cols = c(OperatingBudgetImpactAmount, TargetProgramFundingLevel),
      output_format = output_format
    ) |>
    add_operating_budget_impact_desc() |>
    join_estimate_costs(
      estimate_data,
      active_submission_status = active_submission_status
    )
}

#' Filter estimate data to active estimates and subset n per project
#'
#' Drops estimates with `SubmissionStatus` of `"Archived"` or `"New"`,
#' computes `TotalCost` as the sum of the planning, design, construction,
#' contingency, and management cost columns, and keeps the first `n`
#' remaining estimates per project, preferring `"Submitted"` over `"Draft"`
#' status. Warns (via [cli::cli_inform()]) if any project has more than one
#' remaining draft or submitted estimate.
#'
#' @param estimate_data Cost estimate data with columns `ProjectID`,
#'   `SubmissionStatus`, `PlanningCost`, `DesignCost`, `ConstructionCost`,
#'   `ContingencyCost`, and `ManagementCost`.
#' @param n Number of active estimates to keep per project.
#' @param active_submission_status One or more SubmissionStatus values
#' ("Submitted", "Draft", "New", or "Archived") used to subset `estimate_data`
#' to active estimate/schedule information. Defaults to `c("Submitted", "Draft")`.
#' @returns `estimate_data` filtered to active (non-archived, non-new)
#'   estimates, with a `TotalCost` column added, subset to the first `n`
#'   estimates per `ProjectID`.
#' @keywords internal
fmt_active_estimate_data <- function(
  estimate_data,
  n = 1,
  active_submission_status = c("Submitted", "Draft")
) {
  # Check exclude_status input against submission_levels
  submission_levels <- c("Submitted", "Draft", "New", "Archived")

  active_submission_status <- rlang::arg_match(
    active_submission_status,
    submission_levels,
    multiple = TRUE
  )

  active_estimate_data <- estimate_data |>
    dplyr::filter(
      SubmissionStatus %in% active_submission_status
    ) |>
    dplyr::mutate(
      SubmissionStatus = factor(
        SubmissionStatus,
        levels = submission_levels
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      TotalCost = sum(
        PlanningCost,
        DesignCost,
        ConstructionCost,
        ContingencyCost,
        ManagementCost,
        na.rm = TRUE
      ),
      OtherCost = sum(
        PlanningCost,
        ContingencyCost,
        ManagementCost,
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup()

  active_estimate_project_count <- active_estimate_data |>
    dplyr::count(ProjectID, name = "ProjectIDCount")

  if (any(active_estimate_project_count[["ProjectIDCount"]] > 1)) {
    cli::cli_inform(
      c(
        "!" = "{.arg estimate_data} includes more than one active estimate per project.",
        "i" = "For reporting, only {n} estimate{?s} can be used for each project."
      )
    )
  }

  # Pull the first active estimate per project
  active_estimate_data |>
    dplyr::arrange(SubmissionStatus) |>
    dplyr::slice_head(
      n = n,
      by = ProjectID
    )
}


#' Summarise program data
#'
#' @param program_data Program data from the Six-Year CIP sheet formatted for
#' upload to the 'Capital Request' SharePoint list.
#' @param start_year_col,outer_year_cols Names of budget year and outer year
#' columns.
#' @param amounts_start_with Pattern passed to `tidyselect::starts_with` used to
#' select both budget and outer year columns.
#' @param start_year_sum_col,total_sum_col Defaults to "Budget Request Amount" and "Total Request Amount"
#' @returns A data frame with columns "ProjectID" and matching `start_year_sum_col` and `total_sum_col`.
#' @keywords internal
sum_6yr_program_totals <- function(
  program_data,
  start_year_col = "Year1",
  outer_year_cols = paste0("Year", 2:6),
  amounts_start_with = "Year",
  start_year_sum_col = "Budget Request Amount",
  total_sum_col = "Total Request Amount"
) {
  stopifnot(
    all(
      rlang::has_name(
        program_data,
        c(
          "ProjectID",
          "SplitChildRow",
          start_year_col,
          outer_year_cols
        )
      )
    )
  )
  program_data |>
    # SplitChildRow values must be removed
    dplyr::filter(
      !SplitChildRow
    ) |>
    dplyr::summarise(
      dplyr::across(
        # FIXME: Replace to drop `amounts_start_with` parameter
        # tidyselect::all_of(c(start_year_col, outer_year_cols))
        tidyselect::starts_with(amounts_start_with),
        \(x) {
          sum(x, na.rm = TRUE)
        }
      ),
      .by = ProjectID
    ) |>
    dplyr::mutate(
      "{start_year_sum_col}" := .data[[start_year_col]],
      "{total_sum_col}" := rowSums(
        dplyr::pick(
          tidyselect::all_of(c(
            start_year_col,
            outer_year_cols
          ))
        )
      )
    ) |>
    dplyr::select(
      ProjectID,
      tidyselect::all_of(
        c(start_year_sum_col, total_sum_col)
      )
    )
}
