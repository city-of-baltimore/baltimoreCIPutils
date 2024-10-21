#' Add a pivot table using data from the Six-Year CIP Sheet
#'
wb_add_cip_pivot_table <- function(wb,
                                   sheet = current_sheet(),
                                   dims = "A1",
                                   rows = c("Cost Center Name", "Project Name", "Revenue Category Name"),
                                   data = curr_yr_span(),
                                   params = list(
                                     table_style = "TableStyleLight16",
                                     apply_number_formats = TRUE,
                                     first_column = TRUE,
                                     with_filter = TRUE,
                                     banded_rows = TRUE # ,
                                     # numfmt = c(formatCode = rep("##0.0", 6))
                                   ),
                                   ...) {
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
