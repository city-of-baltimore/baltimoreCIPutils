#' Create a sparkline for a set of fiscal year columns
#'
#' @inheritParams create_sparkline_group
#' @export
create_timespan_sparklines <- function(
    wb,
    timespan_cols = curr_fy_span(),
    sheet = NULL,
    ...) {
  create_sparkline_group(
    wb = wb,
    cols = timespan_cols,
    sheet = sheet,
    ...
  )
}

#' Create a sparkline group using the same set of cols and settings for each
#'
#' [create_sparkline_group()] creates a character vector of sparklines for use
#' with [openxlsx2::wb_add_sparklines()].
#'
#' @param wb Required to get active sheet and dimensions
#' @inheritParams openxlsx2::create_sparklines
#' @export
create_sparkline_group <- function(
    wb,
    cols,
    sheet = NULL,
    sqref = NULL,
    display_empty_cells_as = "zero",
    type = "column",
    first = TRUE,
    ...) {
  data <- wb_data(wb, sheet = sheet)

  sheet <- sheet %||% openxlsx2::wb_get_active_sheet(wb)

  rows <- seq(nrow(data))

  group_dims <- vapply(
    rows,
    \(x) {
      wb_dims(
        x = data,
        cols = cols,
        rows = x
      )
    },
    character(1)
  )

  group_sqref <- sqref %||% vapply(
    rows,
    \(x) {
      openxlsx2::wb_dims(
        x = data,
        cols = ncol(data),
        rows = x,
        from_col = 2
      )
    },
    character(1)
  )

  vapply(
    rows,
    \(i) {
      openxlsx2::create_sparklines(
        sheet = sheet,
        dims = group_dims[[i]],
        sqref = group_sqref[[i]],
        display_empty_cells_as = display_empty_cells_as,
        type = type,
        first = first,
        ...
      )
    },
    character(1)
  )
}
