#' Read Adaptive Planning Sheet from exported Excel file
#'
#' [adapt_read_sheet()] reads an exported sheet from an exported Excel file,
#' optionally removing summary data and footer rows, or sub-setting to child and
#' parent rows if sheet allows row splitting.
#'
#' @inheritParams openxlsx2::wb_load
#' @param col_names Names to use for returned data frame.
#' @inheritDotParams openxlsx2::wb_to_df -col_names -start_row
#' @param keep Values from input file to keep. "values" keeps all values.
#'   "child" and "parent" are used if the input data uses row-splitting and are
#'   not yet supported. "all" retains any trailing "Total" row.
#' @param name_repair Passed to repair argument of [vctrs::vec_as_names()].
#' @export
#' @importFrom openxlsx2 wb_load wb_get_sheet_names wb_to_df
#' @importFrom cli cli_inform cli_alert_warning
#' @importFrom fs path_file file_info
#' @importFrom vctrs vec_as_names
#' @importFrom dplyr cumany
adapt_read_sheet <- function(file,
                             types = NULL,
                             col_names = NULL,
                             ...,
                             start_row = 4,
                             keep = c("values", "child", "parent", "all"),
                             name_repair = "minimal") {
  adapt_wb <- openxlsx2::wb_load(file)
  adapt_wb_sheets <- openxlsx2::wb_get_sheet_names(adapt_wb)
  keep <- arg_match(keep)

  if (!is.null(types)) {
    adapt_sheet_data <- openxlsx2::wb_to_df(
      adapt_wb,
      sheet = adapt_wb_sheets[[1]],
      start_row = start_row,
      types = types,
      col_names = TRUE,
      ...
    )
  } else {
    adapt_sheet_data <- openxlsx2::wb_to_df(
      adapt_wb,
      sheet = adapt_wb_sheets[[1]],
      start_row = start_row,
      col_names = TRUE,
      ...
    )
  }

  info_sheet <- "Information about this Sheet"

  if (info_sheet %in% adapt_wb_sheets) {
    adapt_sheet_info <- openxlsx2::wb_to_df(
      adapt_wb,
      sheet = info_sheet,
      start_row = 1,
      col_names = FALSE,
      skip_empty_rows = TRUE
    )

    adapt_sheet_info <- set_names(
      c(adapt_sheet_info[1, 1], adapt_sheet_info[2:start_row, 2]),
      c("Source", adapt_sheet_info[2:start_row, 1])
    )

    adapt_sheet_info_msg <- set_names(
      paste0(
        "{.field ", names(adapt_sheet_info), "}: ",
        "{.str ", adapt_sheet_info, "}"
      ),
      rep("*", 4)
    )

    cli::cli_inform(
      c(
        "v" = "Reading {.path {file}}",
        adapt_sheet_info_msg
      )
    )

    attributes(adapt_sheet_data)[["adapt_sheet_info"]] <- c(
      c(
        "filename" = fs::path_file(file),
        "path" = file,
        "birth_time" = fs::file_info(file)[["birth_time"]]
      ),
      adapt_sheet_info
    )
  } else {
    cli::cli_alert_warning(
      "Expected sheet {.field {info_sheet}} can't be found in {.file {file}}"
    )
  }

  col_names <- col_names %||% names(adapt_sheet_data)

  adapt_sheet_data <- set_names(
    adapt_sheet_data,
    vctrs::vec_as_names(col_names, repair = name_repair)
  )

  # TODO: Add handling for "children" and "parent" options
  if (keep == "all") {
    return(adapt_sheet_data)
  }

  # adapt_index <- adapt_sheet_data[, 1]
  value_rows <- !dplyr::cumany(adapt_sheet_data[, 1] == "Total")

  # TODO: Consider if there is value in pulling the export date from the footer
  # footer_rows <- !is.na(adapt_index) &
  #   (adapt_index %in% c("Total", "Notes:", "Confidential Information. Do not distribute without permission.")) &
  #   !value_rows
  # export_date <- adapt_sheet_data[footer_rows, ]

  adapt_sheet_data[value_rows, ]
}

#' Summarise a data frame by timespan columns
#'
#' [summarise_timespan()] uses [dplyr::across()] and [dplyr::summarise()]
#' to combine fiscal year amount columns grouped by some other variables.
#'
#' @param data Input data frame.
#' @param timespan_cols Required. Defaults to `curr_yr_span()`. Passed to `.cols`
#'   argument of [dplyr::across()]
#' @inheritParams dplyr::across
#' @inheritParams dplyr::summarise
#' @export
summarise_timespan <- function(
    data,
    timespan_cols = curr_yr_span(),
    .fns = \(x) {
      sum(x, na.rm = TRUE)
    },
    .by = NULL,
    .names = NULL,
    .unpack = FALSE,
    .groups = NULL) {
  data |>
    dplyr::summarise(
      dplyr::across(
        .cols = timespan_cols,
        .fns = .fns,
        .names = .names,
        .unpack = .unpack
      ),
      .by = .by,
      .groups = .groups
    )
}
