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
      "Revenue Category Label Short"
    )) {
  stopifnot(
    is.character(cols),
    is.character(by)
  )

  wd_revenue_category_rs <- baltimoreCIPutils::wd_revenue_category_xwalk |>
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

#' Replace Workday Revenue Category Values with Prior Equivalents
#'
#' [wd_revenue_category_update()] uses the prepared crosswalk
#' `baltimoreCIPutils::wd_revenue_category_xwalk` to replace values for Revenue Category Code, Revenue Category Name, and Revenue Category (assumed to be the combination of the prior two values) with the prior effective equivalent. This is
#'
#' @param effective_year Year to use as replacement values.
#' @param cols Columns to replace for input data.
#' @export
wd_revenue_category_update <- function(
    .data,
    cols = c(
      "Revenue Category Code", "Revenue Category Name"
    ),
    effective_year = 2026,
    ...) {
  effective_year_xwalk <- baltimoreCIPutils::wd_revenue_category_xwalk |>
    dplyr::filter(
      !is.na(`Effective Year`),
      `Effective Year` == effective_year
    ) |>
    dplyr::mutate(
      `Prior Revenue Category` = paste(
        `Prior Revenue Category Code`,
        `Prior Revenue Category Name`
      )
    )

  # TODO: This pattern only supports the replacement of current year values with
  # prior year values
  pattern <- c(
    dplyr::pull(
      effective_year_xwalk,
      `Prior Revenue Category`,
      `Revenue Category`
    ),
    dplyr::pull(
      effective_year_xwalk,
      `Prior Revenue Category Name`,
      `Revenue Category Name`
    ),
    dplyr::pull(
      effective_year_xwalk,
      `Prior Revenue Category Code`,
      `Revenue Category Code`
    )
  )

  dplyr::mutate(
    .data,
    dplyr::across(
      .cols = tidyselect::all_of(cols),
      \(x) {
        stringr::str_replace_all(
          x,
          pattern = pattern
        )
      }
    )
  )
}
