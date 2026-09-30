#' Knit a child document once for each row of a data frame
#'
#' `knit_children()` knits the child document `.input` once for each row of
#' `.l` with [knitr::knit_child()] and prints the combined output with [cat()].
#' Each child is knitted in a new environment containing the values from that
#' row, named with the column names of `.l`, and the objects in `.params`.
#'
#' The reports use this to build nested sections in a Quarto report, e.g.
#' knitting an agency report child once per agency, and each agency report knits
#' a project report child once per project.
#'
#' Call `knit_children()` from a code chunk with the `output: asis` option so
#' the knitted Markdown is included in the parent document as-is.
#'
#' @param .l A data frame (or list of equal-length vectors) passed to
#'   [purrr::pmap()]. Each row produces one knitted child, and each column is
#'   available in the child environment as an object with the same name, e.g.,
#'   a `project_id` column is available as `project_id`.
#' @param ... Ignored. Kept so existing calls that pass a column name before
#'   `.input` (e.g., `knit_children(data, project_id, .input = ...)`) continue
#'   to work. Every column in `.l` is always available to the child document,
#'   whether or not it is named here, and these arguments are never evaluated.
#' @param .input Path to the child document, passed to the `input` argument of
#'   [knitr::knit_child()]. Required. The file must exist.
#' @param .params A named list of additional objects to make available to every
#'   child document, e.g., a database connection or report parameters. If a
#'   name matches a column in `.l`, the value from `.params` is used.
#' @param .quiet Passed to the `quiet` argument of [knitr::knit_child()].
#'   Defaults to `TRUE`.
#' @param .envir Parent environment for the child environments. Defaults to
#'   the environment `knit_children()` is called from.
#' @returns `NULL`, invisibly. Called for the side effect of printing the
#'   knitted output of each child document, separated by line breaks.
#' @details
#' Each child environment is a new environment with `.envir` as its parent, so
#' child documents can use any object available where `knit_children()` is
#' called, e.g., objects created in the setup chunk of the parent document or,
#' for a nested call, objects created by the calling child document. Values
#' from `.l` and `.params` take precedence over objects with the same name in
#' `.envir`.
#'
#' Unexported functions from this package are not available to child documents
#' unless called with `baltimoreCIPutils:::`.
#' @examples
#' child <- tempfile(fileext = ".Rmd")
#' writeLines("- Project `r project_id` (`r program_version`)", child)
#'
#' knit_children(
#'   data.frame(project_id = c("PRJ001", "PRJ002")),
#'   .input = child,
#'   .params = list(program_version = "FY2027 CAP Requests")
#' )
#' @export
knit_children <- function(
  .l,
  ...,
  .input,
  .params = rlang::list2(),
  .quiet = TRUE,
  .envir = parent.frame()
) {
  check_list(.l, allow_empty = TRUE)

  if (!is_empty(.l) && !is_named(.l)) {
    cli_abort("All elements of {.arg .l} must be named.")
  }

  check_string(.input, allow_empty = FALSE)

  if (!file.exists(.input)) {
    cli_abort("{.arg .input} must be an existing file, not {.file {.input}}.")
  }

  check_list(.params, allow_empty = TRUE)

  if (!is_empty(.params) && !is_named(.params)) {
    cli_abort("All elements of {.arg .params} must be named.")
  }

  check_bool(.quiet)
  check_environment(.envir)

  res <- purrr::pmap(
    .l,
    \(...) {
      knitr::knit_child(
        input = .input,
        envir = rlang::env(
          .envir,
          ...,
          !!!.params
        ),
        quiet = .quiet
      )
    }
  )

  cat(unlist(res), sep = "\n")
}

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

#' Convert a quosure to a tidyselect-safe selection
#'
#' Wraps character vectors in [tidyselect::all_of()] so selections can be
#' supplied as character vectors (including external variables) without
#' triggering the tidyselect deprecation warning, while still supporting bare
#' column names and tidyselect helpers.
#'
#' @param quo A quosure created with [rlang::enquo()].
#' @param data Data frame the selection will be applied to.
#' @param strict If `FALSE`, wrap character vectors in [tidyselect::any_of()]
#'   instead of [tidyselect::all_of()] so missing columns are ignored.
#' @noRd
as_select_quo <- function(quo, data, strict = TRUE) {
  if (quo_is_null(quo) || quo_is_missing(quo)) {
    return(quo)
  }

  # A bare symbol matching a column is always a column reference
  if (quo_is_symbol(quo) && as_string(quo_get_expr(quo)) %in% names(data)) {
    return(quo)
  }

  # Character vectors (from literals, variables, or calls like curr_fy_span())
  # are wrapped in all_of() to avoid the tidyselect deprecation warning
  # Selection helpers (e.g. all_of()) error or warn outside a selecting
  # function; treat either as a signal to pass the quosure through unchanged
  val <- tryCatch(
    eval_tidy(quo),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.character(val)) {
    if (!strict) {
      return(quo(tidyselect::any_of(!!val)))
    }
    return(quo(tidyselect::all_of(!!val)))
  }

  quo
}
