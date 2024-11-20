#' Join Asset ID values based on `wd_proj_asset_xwalk` reference data
#'
#' By default, [wd_proj_join_asset_id()] uses `multiple = "nested"` to store the
#' asset ID values in a nested data frame list column to allow handling of
#' projects with multiple matching assets.
#'
#' @param data A data frame with column name matching value in
#'   `project_code_col`.
#' @param project_code_col Project code column name in data.
#' @param asset_id_col Asset ID column name.
#' @param multiple Defaults to "nested" which nests the data by project code
#'   column and then sets `multiple = "any"`. Passed to [dplyr::left_join()]
#' @export
wd_proj_join_asset_id <- function(data,
                                  project_code_col = "Project Code",
                                  asset_id_col = "asset_id",
                                  .key = asset_id_col,
                                  multiple = "nested") {
  asset_xwalk_cols <- set_names(
    c("Project Code", asset_id_col),
    c(project_code_col, asset_id_col)
  )

  asset_xwalk <- baltimoreCIPutils::wd_proj_asset_xwalk |>
    dplyr::select(all_of(asset_xwalk_cols))

  if (multiple == "nested") {
    asset_xwalk <- asset_xwalk |>
      dplyr::group_by(.data[[project_code_col]]) |>
      dplyr::nest_by(
        .key = .key,
        .keep = TRUE
      )

    multiple <- "any"
  }

  data |>
    dplyr::left_join(
      asset_xwalk,
      multiple = multiple,
      by = project_code_col,
      na_matches = "never"
    )
}
