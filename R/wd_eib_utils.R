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
#' @rdname wd_eib_utils
#' @export
pull_dict_fields <- function(dict, sheet_name, usage = TRUE) {
  check_installed("chk")
  chk::check_names(dict, c("Sheet", "Usage", "Fields", "Column"))
  chk::chk_string(sheet_name)

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
      )
    )
  }

  fields |>
    # Trim leading/trailing space
    stringr::str_trim() |>
    # FIXME: Address the following error on check
    #
    # Portable packages must use only ASCII characters in their R code
    # and
    # NAMESPACE directives, except perhaps in comments. Use \uxxxx escapes for
    # other characters. Function ‘tools::showNonASCIIfile’ can help in finding
    # non-ASCII characters in files.

    # Remove zero-width space characters
    stringr::str_remove_all("​") |>
    # Restor vector names (Column values)
    rlang::set_names(names(fields))
}

#' Helper to get default values as data frame from dictionary data frame
#' @rdname wd_eib_utils
#' @export
get_dict_defaults <- function(dict, sheet_name) {
  check_installed(c("tidyr", "chk"))
  chk::check_names(dict, c("Sheet", "Default Value", "Fields"))
  chk::chk_string(sheet_name)

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
  col_names = FALSE
) {
  check_installed("chk")
  chk::chk_named(fields)
  chk::check_names(data, fields)
  chk::chk_s3_class(.init, "wbWorkbook")
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
