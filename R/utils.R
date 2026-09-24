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

#' Filter program data by program version
#'
#' `filter_program_data()` is a convenience function for Capital Improvement
#' Program (CIP) reporting that filters program data to one or more values of
#' the `ProgramVersion` column. `program_version` is matched against the values
#' present in `program_data`, so a misspelled or missing version results in an
#' informative error rather than an empty data frame.
#'
#' @param program_data A data frame of program data with a `ProgramVersion`
#'   column, such as program data from the Six-Year CIP sheet.
#' @param program_version A string (or, if `multiple = TRUE`, a character
#'   vector) of program versions to keep. Must match values in the
#'   `ProgramVersion` column of `program_data`.
#' @param multiple If `TRUE`, allow `program_version` to match more than one
#'   version. Passed to [rlang::arg_match()]. Defaults to `FALSE`.
#' @param ... Additional filter expressions passed to [dplyr::filter()] and
#'   combined with the `ProgramVersion` filter.
#' @returns A filtered data frame with the same columns as `program_data`.
#' @examples
#' program_data <- data.frame(
#'   ProgramVersion = c("Planning", "Planning", "Adopted"),
#'   ProjectID = c("100-001", "100-002", "100-001"),
#'   Amount = c(100, 250, 150)
#' )
#'
#' filter_program_data(program_data, "Planning")
#'
#' filter_program_data(
#'   program_data,
#'   c("Planning", "Adopted"),
#'   multiple = TRUE,
#'   Amount > 100
#' )
#' @export
filter_program_data <- function(
  program_data,
  program_version,
  multiple = FALSE,
  ...
) {
  check_data_cols(program_data, cols = "ProgramVersion")
  check_bool(multiple)

  if (!multiple) {
    check_string(program_version)
  }

  program_version <- rlang::arg_match(
    program_version,
    values = unique(program_data[["ProgramVersion"]]),
    multiple = multiple
  )

  dplyr::filter(
    program_data,
    ProgramVersion %in% program_version,
    ...
  )
}
