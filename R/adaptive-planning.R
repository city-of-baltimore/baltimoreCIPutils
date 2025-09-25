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
adapt_read_sheet <- function(
  file,
  types = NULL,
  col_names = NULL,
  ...,
  start_row = 4,
  keep = c("values", "child", "parent", "all"),
  name_repair = "minimal"
) {
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
        "{.field ",
        names(adapt_sheet_info),
        "}: ",
        "{.str ",
        adapt_sheet_info,
        "}"
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
#' @param timespan_cols Required. Defaults to [curr_fy_span()]. Passed to `.cols`
#'   argument of [dplyr::across()]
#' @inheritParams dplyr::across
#' @inheritParams dplyr::summarise
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
  dplyr::summarise(
    .data,
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

#' Replace  `NA` values in numeric timespan columns with replacement value
#'
#' [replace_na_timespan()] replaces all `NA` values in timespan_cols with a
#' replacement value (0 by default).
#'
#' @seealso [summarise_timespan()]
#' @param timespan_cols Required. Defaults to [curr_fy_span()]. Passed to `.cols`
#'   argument of [dplyr::across()] (wrapped by [tidyselect::any_of()]).
#' @export
replace_na_timespan <- function(
  .data,
  timespan_cols = curr_fy_span(),
  replacement = 0
) {
  dplyr::mutate(
    .data,
    dplyr::across(
      .cols = tidyselect::any_of(timespan_cols),
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

#' Format request items optionally adding a following cycle based on the
#' existing values
#' @param program_data Program data from Six-Year CIP Sheet in Adaptive Planning.
#' @param first_year Whole number for first year of six year range in fiscal year columns.
#' @param  program_version_name,program_version_date Program version name and date.
#' @param remove_na If `TRUE`, remove rows where all fiscal year column values are `NA` or 0.
#' @keywords internal
#' @export
fmt_request_items <- function(
  program_data,
  first_year = 2027,
  program_version_name = NULL,
  program_version_date = NULL,
  remove_na = TRUE
) {
  adapt_sheet_info <- attr(
    program_data,
    "adapt_sheet_info"
  )

  if (!is.null(adapt_sheet_info)) {
    program_version_name <- program_version_name %||%
      adapt_sheet_info[["Version"]]

    program_version_date <- program_version_date %||%
      as.POSIXct(
        as.numeric(adapt_sheet_info[["birth_time"]])
      )
  }

  request_items <- program_data |>
    fmt_request_worktags()

  if (remove_na) {
    request_items <- request_items |>
      dplyr::filter(
        !dplyr::if_all(
          tidyselect::starts_with("FY"),
          \(x) {
            is.na(x) | x == 0
          }
        )
      )
  }

  request_items |>
    dplyr::mutate(
      `First Fiscal Year` = first_year,
      `Request Version` = program_version_name,
      `Request Version Date` = as.Date(program_version_date)
    ) |>
    # Update FY columns to match request table convention
    dplyr::rename_with(
      .cols = tidyselect::starts_with("FY"),
      \(x) {
        # Convert first year into Year 1
        yr <- as.integer(stringr::str_remove(x, "^FY")) - first_year + 1
        paste0("Year", yr)
      }
    )
}

#' Format Fund ID, Grant ID, and Revenue Category ID columns
#' @rdname fmt_request_items
#' @export
fmt_request_worktags <- function(
  program_data
) {
  assertthat::assert_that(
    assertthat::has_name(
      program_data,
      c(
        "FGSFund Code",
        "FGSFund Name",
        "FGSGrant Code",
        "FGSGrant Name",
        "Revenue Category Code"
      )
    )
  )

  dplyr::mutate(
    .data = program_data,
    `FGSFund Code` = stringr::str_extract(
      `FGSFund Name`,
      baltimoreCIPutils::cap_patterns$fund
    ),
    `FGSGrant Code` = stringr::str_extract(
      `FGSGrant Name`,
      baltimoreCIPutils::cap_patterns$grant
    ),
    `Revenue Category Code` = stringr::str_extract(
      `Revenue Category Code`,
      baltimoreCIPutils::cap_patterns$revenue_category
    )
  )
}
