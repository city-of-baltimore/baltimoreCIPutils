#' Format Workday Project rating and level columns as factors
#'
#'
#'
#' @name fmt_wd_proj_fct
#' @inheritParams fmt_fct_recode
NULL

#' @rdname fmt_wd_proj_fct
#' @export
fmt_wd_proj_importance <- function(data,
                                   col = "Importance Rating",
                                   levels = c(
                                     "Critical" = "1 - Critical",
                                     "Major" = "2 - Major",
                                     "Minor" = "3 - Minor"
                                   )) {
  data |>
    fmt_fct_recode(
      col = col,
      levels = levels
    )
}

#' @rdname fmt_wd_proj_fct
#' @export
fmt_wd_proj_priority <- function(data,
                                 col = "Priority",
                                 levels = c(
                                   "High" = "1-High",
                                   "Medium" = "2-Medium",
                                   "Low" = "3-Low"
                                 )) {
  data |>
    fmt_fct_recode(
      col = col,
      levels = levels
    )
}

#' @rdname fmt_wd_proj_fct
#' @export
fmt_wd_proj_risk <- function(data,
                             col = "Risk Level",
                             levels = c(
                               "High",
                               "Medium",
                               "Low"
                             )) {
  data |>
    fmt_fct_recode(
      col = col,
      levels = levels
    )
}

#' Change factor levels
#'
#' @param data A data frame containing a column name matching `col`.
#' @param col Column name containing column to convert to a factor. Required.
#' @inheritParams base::factor
#' @param new_col New column name to use for factor instead of overwriting existing column named in `col`.
#' @param new_levels New levels to use instead of levels. Ignored if levels is a named vector.
#' @keywords internal
#' @export
fmt_fct_recode <- function(data,
                           col,
                           levels = NULL,
                           new_col = col,
                           new_levels = NULL) {
  if (is.null(levels)) {
    return(data)
  }

  check_installed("forcats")

  if (!rlang::is_named(levels)) {
    levels <- rlang::set_names(levels, new_levels %||% levels)
  }

  dplyr::mutate(
    data,
    "{new_col}" := factor(.data[[new_col]], as.character(levels)),
    "{new_col}" := forcats::fct_recode(.data[[new_col]], !!!levels)
  )
}
