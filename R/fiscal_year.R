fiscal_year <- function(x,
                        type = c("year", "year_prefix", "year_abb",
                                 "date_first", "date_last"),
                        before = "FY") {
  type <- rlang::arg_match(type)

  if (!inherits(x, "Date")) {
    x <- lubridate::as_date(x)
  }

  fy <- dplyr::if_else(
    lubridate::month(x) < 7,
    lubridate::year(x),
    lubridate::year(x) + 1
  )

  switch(type,
    year = fy,
    year_prefix = paste0(before, fy),
    year_abb = paste0(before, substr(fy, 3, 4)),
    date_first = lubridate::as_date(paste0(fy - 1, "-07-01")),
    date_last = lubridate::as_date(paste0(fy, "-06-30"))
  )
}
