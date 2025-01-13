#' Label Workday Revenue Category Columns
#'
#' [wd_revenue_category_label()] uses the prepared crosswalk
#' `baltimoreCIPutils::wd_revenue_category_xwalk` to join labels and short names
#' to a data frame with a revenue category column.
#'
#' @param by Column name to join on defaults to `"Revenue Category Code"`.
#' @param cols Additional columns to keep from
#'   `baltimoreCIPutils::wd_revenue_category_xwalk` (together with column
#'   specified in `by`).
#' @export
wd_revenue_category_label <- function(
    data,
    by = "Revenue Category Code",
    cols = c(
      "Revenue Category Label",
      "Revenue Category Name Short"
    )
  ) {
  stopifnot(
    is.character(keep),
    is.character(by)
  )

  wd_revenue_category_rs <- baltimoreCIPutils::wd_revenue_category_xwalk |>
    dplyr::rename_with(
      \(x) {
        stringr::str_to_title(stringr::str_replace_all(x, "_", " "))
      }
    ) |>
    dplyr::select(
      tidyselect::all_of(c(by, cols))
    )

  data |>
    dplyr::left_join(
      wd_revenue_category_rs,
      by = by,
      na_matches = "never"
    )
}
