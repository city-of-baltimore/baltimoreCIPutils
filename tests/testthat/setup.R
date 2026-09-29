# Set the fiscal year options used as defaults by curr_fy_span(),
# prior_fy_span(), and the rename functions for the full test run
rlang::local_options(
  baltimoreCIP.curr_year = 2027,
  baltimoreCIP.prior_year = 2026,
  .frame = testthat::teardown_env()
)
