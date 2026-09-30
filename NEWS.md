# baltimoreCIPutils 0.1.0

* NEWS set up to track changes 2026-02-03.
* Package created 2024-10-02.
* `filter_program_data()` filters program data to one or more values of the
  `ProgramVersion` column.
* `fiscal_quarter()` returns the fiscal quarter as a string with or without
  the fiscal year (e.g. "FY2019 Q1" or "Q1"), a number, or the first or last
  date of the quarter.
* `fiscal_year()`, `fiscal_quarter()`, `fy_span()`, `curr_fy_span()`,
  `prior_fy_span()`, and `fmt_wd_proj_dates()` gain a `fiscal_start` argument
  to set the month the fiscal year starts. It defaults to the new
  `"baltimoreCIP.fiscal_start"` option or 7 (July).
