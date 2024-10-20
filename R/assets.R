#' Join Asset ID values based on `wd_proj_asset_xwalk` reference data
#'
#' By default, [wd_proj_join_asset_id()] stores the asset ID values in a nested
#' data frame list column to allow handling of projects with multiple matching
#' assets.
#'
#' @keywords internal
wd_proj_join_asset_id <- function(data,
                                  project_code_col = "Project Code",
                                  asset_id_col = "asset_id",
                                  multiple = "nested") {
  asset_xwalk <- baltimoreCIPutils::wd_proj_asset_xwalk |>
    dplyr::select(
      all_of(c(project_code_col, asset_id_col))
    )

  if (multiple == "nested") {
    asset_xwalk <- asset_xwalk |>
      dplyr::group_by(.data[[project_code_col]]) |>
      dplyr::nest_by(
        .key = asset_id_col,
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
