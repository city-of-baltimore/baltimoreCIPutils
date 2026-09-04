load_test_cip_data <- function(
  rows = 2,
  nm = curr_fy_span(),
  possible_values = c(1000000:500000000)
) {
  cols <- length(nm)

  cip_matrix <- matrix(
    sample(possible_values, size = rows * cols),
    nrow = rows,
    ncol = cols
  )

  set_names(
    as.data.frame(cip_matrix),
    nm
  )
}
