#' Filter Workday Projects by Cost Center or Hierarchy
#'
#' [wd_proj_filter()] is a convenience function for filtering project data by
#' Cost Center Code and/or PHierarchy1 Code and PHierarchy2 Code.
#'
#' @param data A data frame to filter. Expected to have columns named
#'   "PHierarchy1 Code" and "PHierarchy2 Code" (if `hierarchy` is supplied) or
#'   "Cost Center Code" (if `cost_center` is supplied).
#' @param hierarchy,cost_center Optional, PHierarchy1 Code. PHierarchy2 Code, or
#'   Cost Center Code values to filter by. Default: `NULL`
#' @keywords internal
#' @export
wd_proj_filter <- function(data, cost_center = NULL, hierarchy = NULL) {
  stopifnot(
    is.null(hierarchy) || all(stringr::str_detect(hierarchy, "^PJH")),
    is.null(cost_center) ||
      all(stringr::str_detect(
        cost_center,
        baltimoreCIPutils::cap_patterns[["cost_center"]]
      ))
  )

  if (!is.null(hierarchy)) {
    data <- data |>
      dplyr::filter(
        .data[["PHierarchy1 Code"]] %in%
          hierarchy |
          .data[["PHierarchy2 Code"]] %in% hierarchy
      )
  }

  if (!is.null(cost_center)) {
    data <- data |>
      dplyr::filter(
        .data[["Cost Center Code"]] %in% cost_center
      )
  }

  data
}

#' Vector of project status levels (also known as milestones)
proj_status_levels <- c(
  "Project Initiation",
  "Design",
  "Construction",
  "Warranty",
  "Close Out",
  "Maintenance",
  "On Hold"
)

wb_add_proj_status_validation <- function(wb, x, cols) {
  openxlsx2::wb_add_data_validation(
    wb,
    type = "list",
    dims = wb_dims(
      x = x,
      cols = cols,
      select = "data"
    ),
    value = vec_as_str_list_value(proj_status_levels)
  )
}

#' Convert vector to factor with project status levels
#' @inheritParams base::factor
as_proj_status <- function(x, ordered = TRUE) {
  factor(x, levels = proj_status_levels, ordered = ordered)
}

#' Convert vector to value argument for openxlsx2 package functions
#'
#' @param x A vector to collapse into a single strng separated with commas ",".
vec_as_str_list_value <- function(x) {
  stopifnot(rlang::is_vector(x))
  paste0('"', paste0(x, collapse = ","), '"')
}

#' Add currency formatting to a workbook
#'
#' @inheritParams openxlsx2::wb_add_numfmt
#' @inheritParams gt::fmt_currency
wb_add_currencyfmt <- function(
  wb,
  sheet = openxlsx2::current_sheet(),
  dims = "A1",
  currency = NULL,
  use_subunits = TRUE,
  decimals = NULL,
  locale = NULL,
  accounting = FALSE,
  force_sign = FALSE,
  sep_mark = ","
) {
  check_installed("gt")

  get_locale_currency_code <- utils::getFromNamespace(
    "get_locale_currency_code",
    "gt"
  )

  validate_currency <- utils::getFromNamespace(
    "validate_currency",
    "gt"
  )

  get_currency_decimals <- utils::getFromNamespace(
    "get_currency_decimals",
    "gt"
  )

  currency <- currency %||%
    get_locale_currency_code(currency)

  validate_currency(currency = currency)

  decimals <- get_currency_decimals(
    currency = currency,
    decimals = decimals,
    use_subunits = use_subunits
  )

  # "3"	"#,##0"
  # "4"	"#,##0.00"
  # "37"	"#,##0 ;(#,##0)"
  # "38"	"#,##0 ;[Red](#,##0)"
  # "39"	"#,##0.00;(#,##0.00)"
  # "40"	"#,##0.00;[Red](#,##0.00)"

  numfmt <- paste0("#", sep_mark, "##0")

  if (decimals > 0) {
    numfmt <- paste0(numfmt, ".", paste0(rep(0, decimals), collapse = ""))
  }

  if (accounting) {
    currencies <- utils::getFromNamespace(
      "currencies",
      "gt"
    )

    currency_sym <- paste0(
      "[$",
      currencies[currencies[["curr_code"]] == currency, ][["symbol"]],
      "]"
    )

    numfmt <- paste0(currency_sym, numfmt, ";(", currency_sym, numfmt, ")")
  }

  openxlsx2::wb_add_numfmt(
    wb,
    sheet = sheet,
    dims = dims,
    numfmt = numfmt
  )
}

#' Save a workboook object to file while filling file name from assigned
#' workbook title
#'
#' `r lifecycle::badge("experimental")`
#'
#' [wb_save_ext()] is a helper function that fills in the file name when saving
#' based on the XSLX title. This function is not stable and may change in the
#' future.
#'
#' @keywords internal
wb_save_ext <- function(wb, file = NULL, ...) {
  if (is.null(file)) {
    core_props <- wb |>
      openxlsx2::wb_get_properties()

    file <- fs::path_ext_set(
      core_props[["title"]],
      "xlsx"
    )
  }

  openxlsx2::wb_save(
    wb = wb,
    file = file,
    ...
  )
}

#' Create a project status reporting workbook based on an existing project
#' workbook
#'
#' [wb_wd_proj_status()] creates a new workbook object that can be exported as
#' an Excel file for use in reporting capital project status information.
#'
#' @param project_wb Passed to [openxlsx2::read_xlsx()]
#' @inheritParams wd_proj_filter
#' @param status_wb Optional, If not supplied, project_wb is assumed to be an
#'   Adaptive Planning Project Details Sheet export with columns including
#'   "Milestone Name" and "Milestone Explanation".  Default:  `NULL`
#' @param status_table_style Style specification to use for the "Current status" sheet, Default: 'TableStyleLight1'
#' @param proj_table_style Style specification to use for the protected "Project details" sheet, Default: 'TableStyleLight2'
#' @param na.strings PARAM_DESCRIPTION, Default: ''
#' @param ... Additional arguments
#' @returns A workbook class object.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   project_wb <- "path to Workday/Adaptive Planning Project details XLSX report"
#'
#'   wb_wd_proj_status(project_wb, cost_center = "CAP009197")
#' }
#' }
#' @rdname wb_wd_proj_status
#' @export
#' @importFrom openxlsx2 read_xlsx wb_workbook wb_add_worksheet wb_add_data_table wb_freeze_pane wb_add_data_validation wb_set_col_widths wb_set_cell_style create_cell_style wb_protect_worksheet wb_set_active_sheet
#' @importFrom fs path_file file_info
#' @importFrom epoxy epoxy
#' @importFrom dplyr select arrange desc mutate if_else case_when left_join pick
#' @importFrom stringr str_sub
wb_wd_proj_status <- function(
  project_wb,
  hierarchy = NULL,
  cost_center = NULL,
  status_wb = NULL,
  # FIXME: Unsure if it makes sense to expose these
  # as arguments
  status_table_style = "TableStyleLight1",
  proj_table_style = "TableStyleLight2",
  na.strings = getOption("openxlsx2.na.strings", ""),
  ...
) {
  # Load project data ----
  wd_proj_data <- openxlsx2::read_xlsx(project_wb) |>
    fmt_wd_proj_worktags() |>
    fmt_wd_proj_name() |>
    fmt_wd_proj_hierarchy()

  wb_filename <- project_wb

  if (inherits(project_wb, "wbWorkbook")) {
    wb_filename <- fs::path_file(wb_filename[["path"]])
  }

  hierarchy_label <- hierarchy %||% "all Project Hierarchies"
  cost_center_label <- cost_center %||% "all Cost Centers"

  about_this_wb <- epoxy::epoxy(
    "This workbook was created on {.date Sys.Date()}",
    "using project data from {.dquote wb_filename}.",
    "Data includes {.bold hierarchy_label} and {.bold cost_center_label}.",
    .collapse = " "
  )

  # Optionally filter by Project Hierarchy and/or Cost Center ----
  wd_proj_data <- wd_proj_filter(
    wd_proj_data,
    hierarchy = hierarchy,
    cost_center = cost_center
  )

  currency_cols <- c("Budgets", "Actuals", "Commitments", "Obligations")

  proj_details_sheet <- wd_proj_data |>
    dplyr::select(
      all_of(c("Project Code", "Project Name Short")),
      starts_with("Cost Center"),
      all_of(currency_cols),
      starts_with(c("Project", "PHierarchy"))
    ) |>
    # TODO: Consider adding these cols to drop_cols in `fmt_adapt_proj_details()`
    dplyr::select(
      !any_of(
        c("Project Type Code", "Project or Program Code")
      )
    ) |>
    # Sort by Project Code
    dplyr::arrange(
      dplyr::desc(.data[["Project Code"]])
    )

  file_date <- as.Date(fs::file_info(project_wb$path)[["modification_time"]])

  # FIXME: Status info is likely to live in a separate spreadsheet - not the
  # main project report
  if (!is.null(status_wb)) {
    wd_proj_status_data <- openxlsx2::read_xlsx(status_wb) |>
      fmt_wd_proj_worktags() |>
      fmt_wd_proj_name() |>
      fmt_wd_proj_hierarchy()
  } else {
    wd_proj_status_data <- wd_proj_data
  }

  prior_wd_proj_status_data <- wd_proj_status_data |>
    dplyr::select(
      any_of(
        c(
          "Project Code",
          # FIXME: Replace "Milestone Name" with "Status" and "Milestone
          # Explanation" with "Status Explanation" when working with updated
          # input data
          "Last Reported Status" = "Milestone Name",
          "Last Reported Date" = "Status Date",
          "Last Reported Explanation" = "Milestone Explanation"
          # "Last Reported Status" = "Status",
          # "Last Reported Date" = "Status Date"
        )
      )
    ) |>
    dplyr::mutate(
      "Last Reported Status" := as_proj_status(.data[["Last Reported Status"]]),
      "Last Reported Date" := dplyr::if_else(
        !is.na(.data[["Last Reported Status"]]),
        # FIXME: Replace this with a valid date for the status report when possible
        as.character(file_date),
        NA_character_
      ),
      # FIXME: Correct this when the new framework is implemented
      "Last Reported Explanation" := NA_character_,
      # TODO: Document how the default value for this column is set
      "No Change" := dplyr::case_when(
        is.na(.data[["Last Reported Status"]]) ~ NA_character_,
        .data[["Last Reported Status"]] %in%
          c("Close Out", "Maintenance", "On Hold") ~
          "Y",
        .default = "N"
      )
    )

  curr_proj_status_sheet <- wd_proj_data |>
    dplyr::mutate(
      "Status" := NA_character_,
      "Status Explanation" := NA_character_,
      "Status Date" = NA_complex_
    ) |>
    dplyr::left_join(
      prior_wd_proj_status_data,
      by = "Project Code"
    ) |>
    dplyr::select(
      all_of(
        c(
          "Project Code",
          "Project Name",
          "Cost Center Code",
          "Cost Center Name",
          "Last Reported Date",
          "Last Reported Status",
          "Status",
          "Last Reported Explanation",
          "Status Explanation",
          "No Change",
          "Status Date"
        )
      )
    ) |>
    # Sort by Last Reported Status
    dplyr::arrange(dplyr::pick(all_of("Last Reported Status")))

  protected_proj_status_cols <- c(
    "Project Code",
    "Project Name",
    "Cost Center Code",
    "Cost Center Name",
    "Last Reported Date",
    "Last Reported Status"
  )

  proj_status_value_cols <- c("Last Reported Status", "Status")

  # Create workbook with a title based on the input hierarchy and cost center
  openxlsx2::wb_workbook(
    title = paste0(
      c(
        "Capital Project Status Updates",
        hierarchy,
        cost_center,
        stringr::str_sub(Sys.Date(), end = 7)
      ),
      collapse = " - "
    ),
    company = "City of Baltimore",
    category = "CIP",
    ...
  ) |>
    # Instructions Sheet ----

    # TODO: Restore instructions tab when formatting can be figured out
    # Create sheet for instructions
    # openxlsx2::wb_add_worksheet(
    #   "Instructions",
    #   tab_color = wb_color("orange")
    # ) |>
    # openxlsx2::wb_add_data(
    #   x = c(
    #     "About" = about_this_wb
    #   )
    # ) |>
    # openxlsx2::wb_add_cell_style(
    #   wrap_text = TRUE
    # ) |>
    # openxlsx2::wb_protect_worksheet("Instructions") |>
    # Current Status Sheet ----

    # Create sheet for data entry
    openxlsx2::wb_add_worksheet(
      "Current status",
      tab_color = wb_color("green")
    ) |>
    openxlsx2::wb_add_data_table(
      x = curr_proj_status_sheet,
      table_style = status_table_style,
      first_column = TRUE,
      with_filter = TRUE,
      banded_rows = TRUE,
      na.strings = na.strings
    ) |>
    openxlsx2::wb_freeze_pane(
      first_col = TRUE
    ) |>
    wb_add_proj_status_validation(
      x = curr_proj_status_sheet,
      cols = proj_status_value_cols
    ) |>
    # FIXME: wb_add_form_control works with only one cell at a time
    openxlsx2::wb_add_data_validation(
      type = "list",
      dims = wb_dims(
        x = curr_proj_status_sheet,
        cols = "No Change",
        select = "data"
      ),
      value = vec_as_str_list_value(c("Y", "N"))
    ) |>
    # Protect cost center, project code, and last reported status columns
    openxlsx2::wb_set_col_widths(
      cols = seq_along(proj_details_sheet),
      widths = "auto",
      hidden = FALSE
    ) |>
    openxlsx2::wb_set_cell_style(
      dims = wb_dims(
        x = curr_proj_status_sheet,
        # FIXME: This required a numeric column specification and I don't know why
        cols = match(protected_proj_status_cols, names(curr_proj_status_sheet)),
        select = "data"
      ),
      style = openxlsx2::create_cell_style(locked = TRUE)
    ) |>
    # Project Details Sheet ----

    # Create sheet for project details
    openxlsx2::wb_add_worksheet(
      "Project details",
      tab_color = wb_color("darkturquoise")
    ) |>
    # Add project data including balance information
    openxlsx2::wb_add_data_table(
      x = proj_details_sheet,
      first_column = TRUE,
      with_filter = TRUE,
      banded_rows = TRUE,
      table_style = proj_table_style,
      na.strings = na.strings
    ) |>
    openxlsx2::wb_freeze_pane(
      first_col = TRUE
    ) |>
    openxlsx2::wb_set_col_widths(
      cols = seq_along(proj_details_sheet),
      widths = "auto"
    ) |>
    # Format currency columns
    wb_add_currencyfmt(
      dims = wb_dims(
        x = proj_details_sheet,
        cols = currency_cols,
        select = "data"
      ),
      accounting = TRUE,
      decimals = 0
    ) |>
    # Protect "Project details" sheet
    openxlsx2::wb_protect_worksheet("Project details") |>
    # Set "Current status" to active sheet
    openxlsx2::wb_set_active_sheet("Current status")
}


#' Prepare a data frame with Excel style class values for formatting by
#' openxlsx2
#'
#' `r lifecycle::badge("experimental")`
#'
#' [set_excel_fmt_class()] applies a style to each specified column.
#'
#' @param data Input data frame with columns named in `cols`.
#' @param cols Columns to apply Excel style class to.
#' @param fmt_class Excel style class, one of: c("currency", "accounting",
#'   "hyperlink", "percentage", "scientific", "formula").
#' <https://janmarvin.github.io/openxlsx2/articles/openxlsx2_style_manual.html#numfmts2>
#' @inheritParams rlang::arg_match
set_excel_fmt_class <- function(
  data,
  cols,
  fmt_class = "currency",
  multiple = TRUE
) {
  fmt_class <- arg_match(
    fmt_class,
    c(
      "currency",
      "accounting",
      "hyperlink",
      "percentage",
      "scientific",
      "formula"
    ),
    multiple = multiple
  )

  fmt_class <- vctrs::vec_recycle(fmt_class, size = length(cols))

  for (i in seq_along(cols)) {
    col <- cols[[i]]
    class(data[[col]]) <- c(fmt_class[[i]], class(data[[col]]))
  }

  data
}
