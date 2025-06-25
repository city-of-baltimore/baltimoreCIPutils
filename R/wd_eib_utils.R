#' Helper functions to support the creation of EIB files
#' @name wd_eib_utils
NULL

#' [cbind_defaults()] column binds a list or data frame of default values to a data frame
#' @rdname wd_eib_utils
#' @export
cbind_defaults <- function(data, defaults, ...) {
  if (!is.data.frame(defaults)) {
    defaults <- as.data.frame(defaults)
  }

  purrr::list_cbind(
    list(
      data,
      defaults
    ),
    ...
  )
}

#' [pull_dict_fields()] extracts a vector of fields from a dictionary data frame
#' @rdname wd_eib_utils
#' @export
pull_dict_fields <- function(dict, sheet_name, usage = TRUE) {
  stopifnot(
    all(has_name(dict, c("Sheet", "Usage", "Fields", "Column")))
  )

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
  check_installed("tidyr")

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
#' [reduce_wb_data_fields()] uses a named list of fields to insert select columns from `data` into a workbook.
#'
#' @param fields A named vector of unique values where correspond to column position using in combination with `start_row`.
#' @param .init A `wbWorkbook` to add data to.
#' @returns A `wbWorkbook`
#' @export
reduce_wb_data_fields <- function(
  data,
  fields,
  sheet,
  .init,
  ...,
  start_row = 6,
  col_names = FALSE
) {
  stopifnot(
    rlang::is_named(fields)
  )

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
        col_names = col_names
      )
    },
    # .init must be a Workbook
    .init = .init
  )
}
