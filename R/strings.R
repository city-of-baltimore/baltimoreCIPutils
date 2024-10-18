#' Helper string functions for Baltimore City Capital Project data
#' @name str_capital
NULL

#' [str_extract_project_code()] uses [stringr::str_extract()] to extract a
#' Project Code.
#' @rdname str_capital
#' @name str_extract_project_code
str_extract_project_code <- function(string) {
  stringr::str_extract(
    string,
    "^PRJ[:digit:]{6}"
  )
}

#' [str_extract_all_project_codes()] uses [stringr::str_extract_all()] to
#' extract Project Codes.
#' @rdname str_capital
#' @name str_extract_all_project_codes
str_extract_all_project_codes <- function(string,
                                          simplify = FALSE) {
  stringr::str_extract_all(
    stringr::str_remove(toupper(string), "-"),
    "^PRJ[:digit:]{6}",
    simplify = simplify
  )
}

#' [str_extract_contract_num()] extract one or more Baltimore City agency
#' contract numbers from a character vector. If `extract_all = TRUE`, the
#' function returns a list instead of a character vector.
#' @param pattern Pattern used to extract
#' @param extract_all If `TRUE`, use [stringr::str_extract_all()] and return a
#'   list of extracted identifiers. This supports multiple identifiers per
#'   string.
#' @rdname str_capital
#' @importFrom stringr str_extract str_extract_all
str_extract_contract_num <- function(
    string,
    pattern = cap_patterns[["contract_num"]],
    extract_all = FALSE,
    ...) {
  fn <- stringr::str_extract

  # FIXME: Implement separate str_extract_all_contract_nums function
  if (extract_all) {
    fn <- stringr::str_extract_all
  }

  string <- fn(
    string,
    pattern,
    ...
  )

  remove_pattern <- "[:space:]|[:punct:]"

  if (!is.list(string)) {
    string <- stringr::str_remove_all(toupper(string), pattern = remove_pattern)
    return(string)
  }

  lapply(
    string,
    \(x) {
      stringr::str_remove_all(toupper(x), pattern = remove_pattern)
    }
  )
}

#' [str_extract_cip_num()] uses [stringr::str_extract()] to extract a legacy CIP
#' number.
#' @rdname str_capital
#' @name str_extract_cip_num
str_extract_cip_num <- function(string) {
  stringr::str_extract(
    string,
    cap_patterns[["cip_num"]]
  )
}
