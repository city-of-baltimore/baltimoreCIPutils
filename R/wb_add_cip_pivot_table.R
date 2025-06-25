#' Add a pivot table using data from the Six-Year CIP Sheet
#'
#' [wb_add_cip_pivot_table()] uses [openxlsx2::wb_add_pivot_table()] to add a
#' pivot table based on the Cost Center, Project, and Revenue Category for
#' Capital Improvement Program data exported from Adaptive Planning.
#'
#' @param rows Defaults to c("Cost Center Name", "Project Name", "Revenue Category Name")
#' @param data Defaults to curr_fy_span().
#' @inheritParams openxlsx2::wb_add_pivot_table
#' @keywords internal
#' @export
wb_add_cip_pivot_table <- function(
  wb,
  sheet = current_sheet(),
  dims = "A1",
  rows = c("Cost Center Name", "Project Name", "Revenue Category Name"),
  data = curr_fy_span(),
  params = list(
    table_style = "TableStyleLight16",
    apply_number_formats = TRUE,
    first_column = TRUE,
    row_grand_totals = FALSE,
    col_grand_totals = FALSE
  )
) {
  # FIXME: This doesn't work - which requires that the input data be prepared
  # using a similar approach
  # x <- set_excel_fmt_class(
  #   wb_data(wb, sheet = sheet),
  #   cols = data,
  #   fmt_class = "currency"
  # )

  openxlsx2::wb_add_pivot_table(
    wb = wb,
    x = wb_data(wb, sheet = sheet),
    # x = x,
    dims = dims,
    rows = rows,
    data = data,
    params = params,
    fun = "SUM"
  )
}
