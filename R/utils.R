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
