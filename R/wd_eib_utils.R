#' Helper functions to support the creation of EIB files
#' @name wd_eib_utils
NULL

#' [cbind_defaults()] column binds a list or data frame of default values to a data frame
#' @rdname wd_eib_utils
#' @param data A data frame to combine with values from `defaults`.
#' @param defaults A named list or data frame with default values to column bind
#' to `data`.
#' @inheritParams purrr::list_cbind
#' @export
cbind_defaults <- function(
  data,
  defaults,
  name_repair = c("unique", "universal", "check_unique"),
  size = NULL
) {
  if (!is.data.frame(defaults)) {
    defaults <- as.data.frame(defaults)
  }

  purrr::list_cbind(
    list(
      data,
      defaults
    ),
    name_repair = name_repair,
    size = size
  )
}

#' [pull_dict_fields()] extracts a vector of fields from a dictionary data frame
#'
#' @param dict A data frame with columns named "Sheet", "Usage", "Fields", and
#' "Column".
#' @param sheet_name Value to filter for based on "Sheet" column.
#' @param usage If `TRUE`, exclude rows from `dict` data frame with `NA` values
#' by filtering a column named "Usage". Typically, the "Usage" column should be
#' "Y" or blank.
#' @param call Passed to [cli::cli_abort()] for error attribution.
#' @rdname wd_eib_utils
#' @export
pull_dict_fields <- function(dict, sheet_name, usage = TRUE, call = caller_env()) {
  check_has_name(dict, c("Sheet", "Usage", "Fields", "Column"), call = call)
  check_string(sheet_name, call = call)

  if (isTRUE(usage)) {
    dict <- dict |>
      dplyr::filter(
        !is.na(`Usage`)
      )
  }

  fields <- dict |>
    dplyr::filter(
      Sheet == sheet_name
    ) |>
    dplyr::pull(Fields, name = Column)

  if (length(fields) == 0) {
    cli::cli_abort(
      c(
        "{.arg dict} is not specifying any fields.",
        "i" = 'Check the "Usage" column of your `xlsx` dictionary file.'
      ),
      call = call
    )
  }

  fields |>
    # Trim leading/trailing space
    stringr::str_trim() |>
    # Remove zero-width space characters
    stringr::str_remove_all("\\u200b") |>
    # Restor vector names (Column values)
    rlang::set_names(names(fields))
}

#' Helper to get default values as data frame from dictionary data frame
#' @param call Passed to `check_has_name()` and `check_string()` for error
#'   attribution.
#' @rdname wd_eib_utils
#' @export
get_dict_defaults <- function(dict, sheet_name, call = caller_env()) {
  check_installed("tidyr")
  check_has_name(dict, c("Sheet", "Default Value", "Fields"), call = call)
  check_string(sheet_name, call = call)

  dict |>
    dplyr::filter(
      Sheet == sheet_name,
      !is.na(`Default Value`)
    ) |>
    dplyr::select(Fields, `Default Value`) |>
    tidyr::pivot_wider(
      names_from = Fields,
      values_from = `Default Value`
    )
}

#' Create a Workbook using a named vector of fields
#'
#' [reduce_wb_data_fields()] uses a named list of fields to insert select
#' columns from `data` into a workbook.
#'
#' @param data Data frame with columns to add to workbook.
#' @param fields A named vector of unique values where names correspond to
#' column position (used in combination with `start_row` to specify starting
#' cell location).
#' @param .init A `wbWorkbook` to add data to.
#' @param ... Additional arguments passed to [openxlsx2::wb_add_data()].
#' @inheritParams openxlsx2::wb_add_data
#' @param call Passed to `check_has_name()` and `check_inherits_all()` for
#'   error attribution.
#' @returns A `wbWorkbook`
#' @export
reduce_wb_data_fields <- function(
  data,
  fields,
  sheet,
  .init,
  ...,
  start_row = 6,
  na = "",
  col_names = FALSE,
  call = caller_env()
) {
  if (!rlang::is_named(fields)) {
    cli_abort("{.arg fields} must be named.", call = call)
  }
  check_has_name(data, fields, call = call)
  check_inherits_all(.init, "wbWorkbook", call = call)
  # TODO: Check that sheet is avilalbe for .init workbook

  purrr::reduce(
    fields,
    \(x, y) {
      openxlsx2::wb_add_data(
        wb = x,
        # column names from fields must be unique
        x = data[, y],
        sheet = sheet,
        dims = paste0(
          # names must correspond to column position
          names(fields)[fields == y],
          start_row
        ),
        ...,
        na = na,
        col_names = col_names
      )
    },
    # .init must be a Workbook
    .init = .init
  )
}
