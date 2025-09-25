#' Format Budget Debit Amount and Budget Credit Amount columns for AllProjectBudgetRevenues entries
#' @noRd
fmt_budget_revenue_entries <- function(
  data,
  ...,
  method = "auto",
  default_col = "Budget Credit Amount",
  amount_col = "Amount",
  debit_flag_value = NULL,
  debit_flag_col = NULL
) {
  data <- data |>
    dplyr::filter(
      !is.na(.data[[amount_col]]) & .data[[amount_col]] != 0
    )

  if (method == "auto") {
    data <- data |>
      # Create new columns
      dplyr::mutate(
        `Budget Debit Amount` = dplyr::if_else(
          .data[[amount_col]] < 0,
          .data[[amount_col]] * -1,
          NA_real_
        ),
        `Budget Credit Amount` = dplyr::if_else(
          .data[[amount_col]] > 0,
          .data[[amount_col]],
          NA_real_
        )
      )
  } else if (!is.null(debit_flag_col)) {
    # TODO: This should be triggered by a `method` option - not by the presence
    # of `debit_flag_col`
    data <- data |>
      # Create new columns
      dplyr::mutate(
        `Budget Debit Amount` = dplyr::if_else(
          .data[[debit_flag_col]] == debit_flag_value,
          .data[[amount_col]],
          NA_real_
        ),
        `Budget Credit Amount` = dplyr::if_else(
          .data[[debit_flag_col]] != debit_flag_value,
          .data[[amount_col]],
          NA_real_
        )
      )
  } else {
    # FIXME: This option isn't being used
    col_values <- c("Budget Debit Amount", "Budget Credit Amount")
    default_col <- rlang::arg_match(
      default_col,
      col_values
    )

    non_default_col = col_values[default_col != col_values]

    data <- data |>
      # Create new columns
      dplyr::mutate(
        "{default_col}" := .data[[amount_col]],
        "{non_default_col}" := NA_real_
      )
  }

  data
}

#' Bind rows with the AllBudgetExpenses entries
#'
#' Must be run on data that has already been formatted with [fmt_budget_revenue_entries()]
#' @noRd
rbind_budget_expense_entries <- function(data, ...) {
  ledger_fields <- c(
    # Used by import_budget only
    "Ledger Account or Ledger Account Summary",
    # Used by amend_budget only
    "Ledger Account Summary"
  )

  any_fields <- c(
    "Memo",
    "Budget Memo",

    # "Budget",
    "Budget Name",
    "Budget Name*",

    # Other fields that are not in both EIBs
    "Budget Currency",
    # "Grant",
    "Year",
    "Fiscal Time Interval",
    "Fiscal Time Interval*"
  )

  all_fields <- c(
    "Header Key",
    # Project may need to move to any_of
    "Project",
    # TODO: Keep Description* if needed as key field
    # "Description*",
    "Project",
    "Fund",
    "Cost Center",
    "Account Set",
    "Budget Debit Amount",
    "Budget Credit Amount"
  )

  budget_expense_entries <- data |>
    dplyr::filter(
      !is.na(`Budget Credit Amount`) |
        !is.na(`Budget Debit Amount`)
    ) |>
    dplyr::select(
      tidyselect::all_of(all_fields),
      tidyselect::any_of(c(any_fields, ledger_fields))
    ) |>
    dplyr::summarise(
      dplyr::across(
        # Avoiding tidyselect::all_of to allow variant column names across EIB files
        c(
          tidyselect::all_of(
            setdiff(
              all_fields,
              c(
                "Budget Credit Amount",
                "Budget Debit Amount",
                "Memo",
                "Header Key"
              )
            )
          ),
          tidyselect::any_of(any_fields)
        ),
        .fns = dplyr::first
      ),
      # Only one must be used - apply the net amount only and leave the other column blank
      # If net is positive it goes in debit, if negative go in credit
      `Budget Debit Amount Update` = dplyr::if_else(
        any(!is.na(`Budget Credit Amount`)),
        sum(`Budget Credit Amount`, na.rm = TRUE),
        NA_real_
      ),
      `Budget Credit Amount Update` = dplyr::if_else(
        any(!is.na(`Budget Debit Amount`)),
        sum(`Budget Debit Amount`, na.rm = TRUE),
        NA_real_
      ),
      Memo = dplyr::if_else(
        any(!is.na(Memo)),
        as.character(
          knitr::combine_words(
            unique(Memo)
          )
        ),
        NA_character_
      ),
      # .by = Project
      .by = `Header Key`
    ) |>
    # Handle rare cases with a value for both the Budget Debit Amount and Budget Credit Amount
    dplyr::mutate(
      `Net Expenses` = if_else(
        !is.na(`Budget Debit Amount Update`) &
          !is.na(`Budget Credit Amount Update`),
        `Budget Credit Amount Update` - `Budget Debit Amount Update`,
        NA_real_
      ),
      `Budget Credit Amount Update` = dplyr::case_when(
        is.na(`Net Expenses`) ~ `Budget Credit Amount Update`,
        !is.na(`Net Expenses`) & `Net Expenses` > 0 ~ `Net Expenses`,
        .default = NA_real_
      ),
      `Budget Debit Amount Update` = dplyr::case_when(
        is.na(`Net Expenses`) ~ `Budget Debit Amount Update`,
        !is.na(`Net Expenses`) & `Net Expenses` <= 0 ~ `Net Expenses` * -1,
        .default = NA_real_
      )
    ) |>
    dplyr::rename(
      `Budget Debit Amount` = `Budget Debit Amount Update`,
      `Budget Credit Amount` = `Budget Credit Amount Update`
    ) |>
    dplyr::mutate(
      `Ledger Account or Ledger Account Summary` = "AllBudgetExpenses",
      `Ledger Account Summary` = "AllBudgetExpenses"
    )

  # FIXME: Is this actually needed here - I think maybe not
  if (all(has_name(data, c("Budget Memo", "Memo")))) {
    data <- dplyr::mutate(
      data,
      `Budget Memo` = Memo
    )
  }

  purrr::list_rbind(
    list(
      data,
      budget_expense_entries
    )
  )
}

build_import_amendment_sheet <- function(
  data,
  eib_dict,
  ...,
  sheet = "Import Budget Amendment",
  description = NULL,
  amendment_date = lubridate::today()
) {
  sheet_defaults <- eib_dict |>
    get_dict_defaults(sheet) |>
    # Set the fiscal year and amendment date columns
    dplyr::mutate(
      `Amendment Date*` = amendment_date
    )

  sheet_fields <- eib_dict |>
    pull_dict_fields(sheet)

  # Build the Import Budget Amendment sheet
  sheet_data <- data |>
    # NOTE: Keep unique Project values
    # FIXME: This may not be allowed
    dplyr::distinct(Project, ..., .keep_all = TRUE) |>
    cbind_defaults(
      sheet_defaults
    ) |>
    dplyr::arrange(
      `Cost Center`,
      `Project`
    ) |>
    # Assign header keys
    dplyr::mutate(
      `Header Key*` = dplyr::row_number(),
      `Description*` = description %||% NA_character_
      # Apply corrections to budget names
      # `Budget Name` = correct_budget_plan_names(
      #   `Budget Name`
      # )
    ) |>
    dplyr::select(
      tidyselect::all_of(as.character(sheet_fields))
    )

  sheet_data
}

#' @noRd
build_amendment_entry_sheet <- function(
  data,
  eib_dict,
  amendment_sheet,
  ...,
  amount_col = "Amount",
  memo = NULL,
  sheet = "Amendment Entry Data"
) {
  sheet_defaults <- eib_dict |>
    get_dict_defaults(sheet)

  sheet_fields <- eib_dict |>
    pull_dict_fields(sheet)

  # Build "Amendment Entry Data" sheet
  entry_sheet <- data |>
    # Fill default values
    cbind_defaults(
      sheet_defaults
    ) |>
    # mutate(
    #   # Apply corrections to budget names
    #   `Budget Name` = correct_budget_plan_names(`Budget Name`)
    # ) |>
    fmt_budget_revenue_entries(
      amount_col = amount_col
    ) |>
    # Join Header Key from `amendment_sheet` (renamed)
    dplyr::left_join(
      # TODO: Add a way to handle amendments involving multiple entries for the same project budget
      amendment_sheet |>
        dplyr::select(
          `Budget Name`,
          `Header Key` = `Header Key*`
        ),
      na_matches = "never",
      by = dplyr::join_by(`Budget Name`)
    ) |>
    dplyr::mutate(
      Memo = memo %||% NA_character_
    ) |>
    rbind_budget_expense_entries() |>
    # Set Line Key by Header Key
    dplyr::mutate(
      `Line Key` = dplyr::row_number(),
      .by = `Header Key`
    ) |>
    dplyr::arrange(`Header Key`)

  entry_sheet
}

#' Build an import budget amendment EIB
#'
#' @export
build_amend_budget_wb <- function(
  data,
  eib_dict,
  eib_wb,
  ...,
  description = NULL,
  memo = NULL,
  amount_col = "Amount",
  amendment_sheet_name = "Import Budget Amendment",
  entry_sheet_name = "Amendment Entry Data"
) {
  data <- data |>
    dplyr::filter(
      !is.na(.data[[amount_col]]) & .data[[amount_col]] != 0
    )

  amendment_sheet <- build_import_amendment_sheet(
    data,
    eib_dict = eib_dict,
    description = description,
    sheet = amendment_sheet_name
  )

  entry_sheet <- build_amendment_entry_sheet(
    data,
    eib_dict = eib_dict,
    amendment_sheet = amendment_sheet,
    memo = memo,
    amount_col = amount_col,
    sheet = entry_sheet_name
  )

  amendment_fields <- eib_dict |>
    pull_dict_fields(amendment_sheet_name)

  eib_out_wb <- reduce_wb_data_fields(
    data = amendment_sheet,
    fields = amendment_fields,
    sheet = amendment_sheet_name,
    .init = eib_wb
  )

  entry_fields <- eib_dict |>
    pull_dict_fields(entry_sheet_name)

  eib_out_wb <- reduce_wb_data_fields(
    data = entry_sheet,
    fields = entry_fields,
    sheet = entry_sheet_name,
    .init = eib_out_wb
  )

  eib_out_wb
}

#' Build a Put Budget Template EIB
#' @export
build_put_budget_template_wb <- function(
  data,
  eib_dict,
  eib_wb,
  sheet_name = "Budget Template"
) {
  sheet_fields <- eib_dict |>
    pull_dict_fields(sheet_name)

  sheet_defaults <- eib_dict |>
    get_dict_defaults(sheet_name)

  # Create Plan Name column from Budget Name colum  if present
  if (!has_name(data, "Plan Name") & has_name(data, c("Budget Name"))) {
    data <- dplyr::mutate(
      data,
      `Plan Name` = `Budget Name`
    )
  }

  # Build sheet using defaults
  put_budget_template_sheet <- data |>
    dplyr::distinct(
      `Plan Name`,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(`Plan Name`) |>
    dplyr::mutate(
      `Spreadsheet Key*` = dplyr::row_number()
    ) |>
    cbind_defaults(sheet_defaults) |>
    dplyr::select(all_of(as.character(sheet_fields)))

  eib_wb_out <- reduce_wb_data_fields(
    data = put_budget_template_sheet,
    fields = sheet_fields,
    sheet = sheet_name,
    .init = eib_wb
  )

  eib_wb_out
}

#' Build a Put Budget Template EIB
#' @export
build_import_budget_wb <- function(
  data,
  eib_dict,
  eib_wb,
  memo = NA_character_,
  description = NA_character_,
  year = NA_integer_,
  amount_col = "Amount",
  budget_sheet_name = "Import Budget High Volume",
  lines_sheet_name = "Budget Lines Data"
) {
  budget_fields <- eib_dict |>
    pull_dict_fields(budget_sheet_name)

  budget_defaults <- eib_dict |>
    get_dict_defaults(budget_sheet_name)

  if (!has_name(data, "Budget Name*") && has_name(data, "Budget Name")) {
    data <- data |>
      dplyr::mutate(
        `Budget Name*` = `Budget Name`
      )
  }

  if (
    has_name(data, "Fiscal Time Interval*") &
      !has_name(data, "Fiscal Time Interval")
  ) {
    data <- data |>
      dplyr::mutate(
        `Fiscal Time Interval` = `Fiscal Time Interval*`
      )
  }

  if (
    has_name(data, "Fiscal Year*") &
      !has_name(data, "Year")
  ) {
    data <- data |>
      dplyr::mutate(
        `Year` = `Fiscal Year*`
      )
  }

  budget_sheet_init <- data |>
    dplyr::filter(
      !is.na(.data[[amount_col]]) & .data[[amount_col]] != 0
    ) |>
    dplyr::distinct(`Budget Name*`, .keep_all = TRUE) |>
    dplyr::mutate(
      `Header Key*` = dplyr::row_number(),
      `Budget Memo` = description %||% NA_character_
    ) |>
    cbind_defaults(budget_defaults)

  budget_sheet <- budget_sheet_init |>
    dplyr::select(tidyselect::all_of(as.character(budget_fields)))

  lines_fields <- eib_dict |>
    pull_dict_fields(lines_sheet_name)

  lines_defaults <- eib_dict |>
    get_dict_defaults(lines_sheet_name)

  lines_sheet <- data |>
    dplyr::left_join(
      budget_sheet_init |>
        dplyr::select(
          Project,
          `Header Key` = `Header Key*`
        ),
      by = dplyr::join_by(Project)
      # by = dplyr::join_by(`Budget Name*`)
    ) |>
    dplyr::mutate(
      `Ledger Account or Ledger Account Summary` = "AllProjectBudgetRevenues",
      `Memo` = memo %||% NA_character_
    ) |>
    cbind_defaults(lines_defaults) |>
    fmt_budget_revenue_entries(
      amount_col = amount_col
    ) |>
    rbind_budget_expense_entries() |>
    dplyr::mutate(
      `Line Key` = dplyr::row_number(),
      `Line Order` = dplyr::row_number(),
      .by = `Header Key`
    ) |>
    dplyr::arrange(`Header Key`, `Line Key`) |>
    dplyr::select(all_of(as.character(lines_fields)))

  eib_wb_out <- reduce_wb_data_fields(
    data = budget_sheet,
    fields = budget_fields,
    sheet = budget_sheet_name,
    .init = eib_wb
  )

  eib_wb_out <- reduce_wb_data_fields(
    data = lines_sheet,
    fields = lines_fields,
    sheet = lines_sheet_name,
    .init = eib_wb_out
  )

  eib_wb_out
}
