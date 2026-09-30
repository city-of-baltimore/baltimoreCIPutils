#' Summarise a data frame by timespan columns
#'
#' [summarise_timespan()] uses [dplyr::across()] and [dplyr::summarise()]
#' to combine fiscal year amount columns grouped by some other variables.
#'
#' @param .data Input data frame.
#' @param timespan_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Required.
#'   Defaults to [curr_fy_span()]. Passed to `.cols` argument of
#'   [dplyr::across()]. Supports tidyselect expressions or a character vector
#'   of column names.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional selection
#'   of columns to group by for just this operation. Supports tidyselect
#'   expressions or a character vector of column names. See
#'   [dplyr::summarise()] for details.
#' @inheritParams dplyr::across
#' @inheritParams dplyr::summarise
#' @seealso [add_timespan_total()], [summarise_program_totals()]
#' @export
summarise_timespan <- function(
  .data,
  timespan_cols = curr_fy_span(),
  .fns = \(x) {
    sum(x, na.rm = TRUE)
  },
  .by = NULL,
  .names = NULL,
  .unpack = FALSE,
  .groups = NULL
) {
  timespan_cols <- as_select_quo(enquo(timespan_cols), .data)
  .by <- as_select_quo(enquo(.by), .data)

  dplyr::summarise(
    .data,
    dplyr::across(
      .cols = !!timespan_cols,
      .fns = .fns,
      .names = .names,
      .unpack = .unpack
    ),
    .by = !!.by,
    .groups = .groups
  )
}

#' Add a row-wise total column for timespan columns
#'
#' [add_timespan_total()] adds a column with the row-wise sum of a set of
#' fiscal year amount columns, ignoring missing values. Use it after
#' [summarise_timespan()] to add a total for the full timespan (e.g., a
#' six-year `"Total"` column for a report table).
#'
#' @inheritParams summarise_timespan
#' @param timespan_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to
#'   sum. Defaults to [curr_fy_span()]. Supports tidyselect expressions or a
#'   character vector of column names. The `col` column is always excluded
#'   from the selection so an existing total is never included in the sum.
#' @param col Name of the total column. Defaults to `"Total"`. If a column
#'   with this name already exists, it is replaced.
#' @param .after <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional
#'   position for the new column. Defaults to after the last column in
#'   `timespan_cols`.
#' @returns `.data` with the total column added. Grouping is preserved.
#' @seealso [summarise_timespan()]
#' @examples
#' data <- data.frame(
#'   Source = c("GO Bonds", "Federal"),
#'   FY2027 = c(1000, NA),
#'   FY2028 = c(2000, 3000)
#' )
#'
#' add_timespan_total(data, timespan_cols = c("FY2027", "FY2028"))
#'
#' add_timespan_total(data, tidyselect::starts_with("FY"), col = "FY Total")
#' @export
add_timespan_total <- function(
  .data,
  timespan_cols = curr_fy_span(),
  col = "Total",
  .after = NULL
) {
  check_string(col)

  timespan_cols <- as_select_quo(enquo(timespan_cols), .data)
  cols <- names(tidyselect::eval_select(timespan_cols, .data))
  cols <- setdiff(cols, col)

  .after <- enquo(.after)
  if (quo_is_null(.after) && length(cols) > 0) {
    .after <- utils::tail(cols, 1)
  }

  dplyr::mutate(
    .data,
    "{col}" := rowSums(
      dplyr::pick(tidyselect::all_of(cols)),
      na.rm = TRUE
    ),
    .after = !!.after
  )
}

#' Summarise first year and total amounts for program data
#'
#' [summarise_program_totals()] drops split child rows, sums the timespan
#' columns by project (by default) or other columns such as `AgencyName` or
#' `ProgramVersion` (with [summarise_timespan()]), and returns the first year
#' amount and the total amount for the full timespan (with
#' [add_timespan_total()]). Use it to join request or program totals to
#' project data.
#'
#' @param program_data Program data from the Six-Year CIP sheet, e.g., data
#'   formatted for upload to the 'Capital Request' SharePoint list. Request
#'   data is a type of program data.
#' @param timespan_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to
#'   sum. Defaults to `paste0("Year", 1:6)`.
#' @param amount_names Length 2 character vector with the names of the first
#'   year amount column and the total amount column in that order. Defaults
#'   to `c("Year1Amount", "TotalAmount")`.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to group by.
#'   Defaults to `"ProjectID"`.
#' @param first_col Name of the first year column. Defaults to the first
#'   column in `timespan_cols`.
#' @param drop_split_child If `TRUE` (default), drop rows where
#'   `SplitChildRow` is `TRUE` before summarising. Split child rows duplicate
#'   the amounts of their parent row.
#' @returns A data frame with the `.by` columns and the two `amount_names`
#'   columns.
#' @seealso [summarise_timespan()], [add_timespan_total()]
#' @examples
#' data <- data.frame(
#'   ProjectID = c("P1", "P1", "P2"),
#'   SplitChildRow = c(FALSE, FALSE, FALSE),
#'   Year1 = c(100, 50, NA),
#'   Year2 = c(200, 0, 300)
#' )
#'
#' summarise_program_totals(
#'   data,
#'   timespan_cols = c("Year1", "Year2"),
#'   amount_names = c("Year1ProgramAmount", "TotalProgramAmount")
#' )
#'
#' # Summarise by another column instead of ProjectID
#' data$AgencyName <- c("DOT", "DOT", "DGS")
#'
#' summarise_program_totals(
#'   data,
#'   timespan_cols = c("Year1", "Year2"),
#'   .by = "AgencyName"
#' )
#' @export
summarise_program_totals <- function(
  program_data,
  timespan_cols = paste0("Year", 1:6),
  amount_names = c("Year1Amount", "TotalAmount"),
  .by = "ProjectID",
  first_col = NULL,
  drop_split_child = TRUE
) {
  check_character(amount_names)
  if (length(amount_names) != 2) {
    cli::cli_abort(
      "{.arg amount_names} must be length 2, not length {length(amount_names)}."
    )
  }
  check_bool(drop_split_child)

  timespan_cols <- as_select_quo(enquo(timespan_cols), program_data)
  .by <- as_select_quo(enquo(.by), program_data)
  cols <- names(tidyselect::eval_select(timespan_cols, program_data))
  by_cols <- names(tidyselect::eval_select(.by, program_data))

  first_col <- first_col %||% cols[1]
  check_string(first_col)
  if (!first_col %in% cols) {
    cli::cli_abort(
      "{.arg first_col} must be one of the {.arg timespan_cols} columns: {.val {cols}}."
    )
  }

  if (drop_split_child) {
    check_data_cols(program_data, "SplitChildRow")
    program_data <- dplyr::filter(program_data, !SplitChildRow)
  }

  program_data |>
    summarise_timespan(timespan_cols = !!cols, .by = !!by_cols) |>
    add_timespan_total(timespan_cols = !!cols, col = amount_names[[2]]) |>
    dplyr::mutate(
      "{amount_names[[1]]}" := .data[[first_col]]
    ) |>
    dplyr::select(
      tidyselect::all_of(c(by_cols, amount_names))
    )
}

#' Replace  `NA` values in numeric timespan columns with replacement value
#'
#' [replace_na_timespan()] replaces all `NA` values in timespan_cols with a
#' replacement value (0 by default).
#'
#' @seealso [summarise_timespan()]
#' @param .data Input data frame with any of columns identified in
#' `timespan_cols`.
#' @param timespan_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Required.
#'   Defaults to [curr_fy_span()]. Supports tidyselect expressions or a
#'   character vector of column names. Columns in a character vector that are
#'   missing from `.data` are ignored (as with [tidyselect::any_of()]).
#' @param replacement Replacement value for `NA` values.
#' @export
replace_na_timespan <- function(
  .data,
  timespan_cols = curr_fy_span(),
  replacement = 0
) {
  timespan_cols <- as_select_quo(enquo(timespan_cols), .data, strict = FALSE)

  dplyr::mutate(
    .data,
    dplyr::across(
      .cols = !!timespan_cols,
      .fns = \(x) {
        dplyr::if_else(
          is.na(x),
          replacement,
          as.numeric(x)
        )
      }
    )
  )
}

#' Deprecated timespan summary functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' - `sum_fy_total_col()` is deprecated in favor of [add_timespan_total()].
#' - `sum_6yr_program_totals()` is deprecated in favor of
#'   [summarise_program_totals()].
#'
#' @param data,program_data Input data frame.
#' @param col_starts_with Prefix used to select the columns to sum with
#'   [tidyselect::starts_with()] if `cols_all_of` is `NULL`. Also used to
#'   position the new column.
#' @param cols_all_of Optional character vector of column names to sum.
#' @param col Name of the total column.
#' @param start_year_col,outer_year_cols Names of first year and outer year
#'   columns.
#' @param amounts_start_with Ignored.
#' @param start_year_sum_col,total_sum_col Names of the first year and total
#'   amount columns.
#' @keywords internal
#' @name deprecated-timespan
NULL

#' @rdname deprecated-timespan
#' @export
sum_fy_total_col <- function(
  data,
  col_starts_with = "FY",
  cols_all_of = NULL,
  col = "FY Total"
) {
  lifecycle::deprecate_soft(
    "0.1.0",
    "sum_fy_total_col()",
    "add_timespan_total()"
  )

  if (is.null(cols_all_of)) {
    timespan_cols <- quo(tidyselect::starts_with(!!col_starts_with))
  } else {
    timespan_cols <- quo(tidyselect::all_of(!!cols_all_of))
  }

  data |>
    add_timespan_total(
      timespan_cols = !!timespan_cols,
      col = col,
      .after = tidyselect::starts_with(!!col_starts_with)
    ) |>
    dplyr::ungroup()
}

#' @rdname deprecated-timespan
#' @export
sum_6yr_program_totals <- function(
  program_data,
  start_year_col = "Year1",
  outer_year_cols = paste0("Year", 2:6),
  amounts_start_with = deprecated(),
  start_year_sum_col = "Budget Request Amount",
  total_sum_col = "Total Request Amount"
) {
  lifecycle::deprecate_soft(
    "0.1.0",
    "sum_6yr_program_totals()",
    "summarise_program_totals()"
  )

  summarise_program_totals(
    program_data,
    timespan_cols = c(start_year_col, outer_year_cols),
    amount_names = c(start_year_sum_col, total_sum_col),
    first_col = start_year_col
  )
}
