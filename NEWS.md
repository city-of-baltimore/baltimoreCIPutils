# baltimoreCIPutils 0.1.0

* NEWS set up to track changes 2026-02-03.
* Package created 2024-10-02.
* New `add_timespan_total()` adds a row-wise total of timespan columns. It
  takes the same tidyselect `timespan_cols` argument as `summarise_timespan()`,
  preserves grouping, and never includes an existing total column in the sum.
* New `summarise_program_totals()` drops split child rows and summarises
  first year and total amounts for program data by project (or other `.by`
  columns such as `AgencyName` or `ProgramVersion`). The output
  column names are set together with `amount_names`.
* `sum_fy_total_col()` and `sum_6yr_program_totals()` are deprecated in favor
  of `add_timespan_total()` and `summarise_program_totals()`.
* `replace_na_timespan()` now supports tidyselect expressions for
  `timespan_cols`.
* Timespan helpers now live together in `R/timespan.R`.
* `filter_program_data()` filters program data to one or more values of the
  `ProgramVersion` column.
* `fiscal_quarter()` returns the fiscal quarter as a string with or without
  the fiscal year (e.g. "FY2019 Q1" or "Q1"), a number, or the first or last
  date of the quarter.
* `fiscal_year()`, `fiscal_quarter()`, `fy_span()`, `curr_fy_span()`,
  `prior_fy_span()`, and `fmt_wd_proj_dates()` gain a `fiscal_start` argument
  to set the month the fiscal year starts. It defaults to the new
  `"baltimoreCIP.fiscal_start"` option or 7 (July).
