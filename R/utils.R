#' List input files based on specified YAML index
#'
#' `list_input_files()` reads a YAML file index that provides a list of file
#' names and use the specified `dir` argument to build a full path.
#'
#' @param input Filename for YAML file located at `dir`.
#' @param dir Base directory where YAML file is located and any files specified
#' in the filename key in the YAML file.
#' @param type A string to use if only a subset of the named elements in the
#' input reference file are needed.
#' @keywords internal
#' @export
list_input_files <- function(
  input = "_files.yml",
  type = NULL,
  dir = here::here()
) {
  rlang::check_installed("yaml12")

  input_reference <- yaml12::read_yaml(fs::path(dir, input))

  for (nm in names(input_reference)) {
    type_reference <- input_reference[[nm]]

    type_reference <- rlang::set_names(
      type_reference,
      purrr::map_chr(
        type_reference,
        "id"
      )
    )

    if (fs::is_dir(dir)) {
      type_reference <- purrr::map(
        type_reference,
        \(x) {
          x[["path"]] <- fs::path(dir, x[["filename"]])

          x
        }
      )
    }

    input_reference[[nm]] <- type_reference
  }

  if (!is.null(type)) {
    return(input_reference[[type]])
  }

  input_reference
}

#' Check that a data frame has required column names
#'
#' `check_data_cols()` combines [rlang::check_data_frame()] and
#' `check_has_name()` to validate that `data` is a data frame with all of
#' `cols` present, in a single call.
#'
#' @param data A data frame to check.
#' @param cols Character vector of required column names.
#' @param allow_any If `TRUE`, only require at least one of `cols` (rather
#'   than all of them) to be present.
#' @param arg,call Passed to `check_has_name()` for error attribution.
#' @keywords internal
check_data_cols <- function(
  data,
  cols,
  ...,
  allow_any = FALSE,
  arg = caller_arg(data),
  call = caller_env()
) {
  check_data_frame(data, arg = arg, call = call)
  check_has_name(data, cols, ..., allow_any = allow_any, arg = arg, call = call)
}

#' Check that a data frame does not already have given column names
#'
#' `check_new_col_names()` errors if `data` already has any column named in
#' `cols`, for use before adding new columns that are not expected to already
#' be present (e.g. to avoid silently overwriting an existing column or
#' creating a confusing `.x`/`.y` suffix after a join).
#'
#' @param data A data frame to check.
#' @param cols Character vector of column names that must not already exist
#'   in `data`.
#' @param arg,call Passed to [cli::cli_abort()] for error attribution.
#' @keywords internal
check_new_col_names <- function(
  data,
  cols,
  ...,
  arg = caller_arg(data),
  call = caller_env()
) {
  existing <- intersect(cols, names(data))

  if (length(existing) > 0) {
    cli_abort(
      "{.arg {arg}} already has column{?s} {.val {existing}}.",
      ...,
      call = call
    )
  }
}

#' Replace value in leading row if repeated in following row
#'
#' @inheritParams dplyr::mutate
#' @param col Column name to check for repeated values in the next row.
#' @param replacement Value to replace repeated values in the next row.
#' Defaults to `'"'`
#' @keywords internal
#' @examples
#' replace_lead_row_value(mtcars[, 1:2], "cyl")
#' @export
replace_lead_row_value <- function(.data, col, replacement = '"') {
  dplyr::mutate(
    .data = .data,
    "{col}" := dplyr::if_else(
      !is.na(dplyr::lag(.data[[col]])) &
        .data[[col]] == dplyr::lag(.data[[col]]),
      replacement,
      as.character(.data[[col]])
    )
  )
}
