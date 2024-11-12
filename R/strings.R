#' Extract matching identifiers for Baltimore City Capital Project data
#'
#' A collection of helper functions using [stringr::str_extract()] and
#' [stringr::str_extract_all()] to extract identifiers from Workday Project
#' names or other text. Project codes and contract numbers are always converted
#' to upper case. When using these functions with unstructured text, the results
#' should be reviewed and validated to flag problems.
#'
#' @name str_capital
#' @param pattern Pattern used to extract.
#' @param remove_pattern Pattern of character to remove from returned strings.
#'   Defaults to `"-"` (dashes that occasionally appear between "PRJ" and
#'   following numbers) or `"[:space:]|[:punct:]"` (all spaces and punctuation).
#' @inheritParams stringr::str_extract_all
#' @examples
#'
#' str_extract_project_code("PRJ001145 908093 SWC 7764 Race St Box Culvert")
#'
#' str_extract_all_project_codes(c("PRJ002404 PRJ002550", "PRJ001599 913117 Shake and Bake Recreation Center"))
#'
#' str_extract_cip_num("PRJ002550 943004 504-004 ADA Infrastructure Upgrades-111")
#'
#' str_extract_contract_num("PRJ001145 908093 SWC 7764 Race St Box Culvert")
#'
NULL

#' [str_extract_project_code()] extracts a Workday Project Code at the start of a string.
#' @rdname str_capital
#' @name str_extract_project_code
#' @export
str_extract_project_code <- function(string,
                                     pattern =  "^PRJ[:digit:]{6}",
                                     remove_pattern = "-",
                                     simplify = FALSE) {
  stringr::str_extract(
    stringr::str_remove(toupper(string), remove_pattern),
    pattern
  )
}

#' [str_extract_all_project_codes()] extracts a list of Workday Project Code.
#' @rdname str_capital
#' @name str_extract_all_project_codes
#' @export
str_extract_all_project_codes <- function(string,
                                          pattern = "PRJ[:digit:]{6}",
                                          remove_pattern = "-",
                                          simplify = FALSE) {
  stringr::str_extract_all(
    stringr::str_remove(toupper(string), remove_pattern),
    pattern,
    simplify = simplify
  )
}

#' [str_extract_contract_num()] extract a Baltimore City agency contract number
#' using a pattern from `baltimoreCIPutils::cap_patterns`
#' @param extract_all Deprecated as of 2024-10-23. If `TRUE`, use
#'   [stringr::str_extract_all()] and return a list of extracted identifiers.
#'   This supports multiple identifiers per string.
#' @rdname str_capital
#' @importFrom stringr str_extract str_extract_all
#' @export
str_extract_contract_num <- function(
    string,
    pattern = baltimoreCIPutils::cap_patterns[["contract_num"]],
    extract_all = FALSE,
    remove_pattern = "[:space:]|[:punct:]",
    ...) {
  fn <- stringr::str_extract

  # FIXME: Implement separate str_extract_all_contract_nums function
  if (extract_all) {
    lifecycle::deprecate_soft(
      "0.1.0",
      "str_extract_contract_num(extract_all)",
      "str_extract_all_contract_num()"
    )
    fn <- stringr::str_extract_all
  }

  string <- fn(
    string,
    pattern,
    ...
  )

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

#' [str_extract_all_contract_num()] extracts a list of contract numbers.
#' @rdname str_capital
#' @name str_extract_all_contract_num
#' @export
str_extract_all_contract_num <- function(string,
                                         pattern = baltimoreCIPutils::cap_patterns[["contract_num"]],
                                         remove_pattern = "[:space:]|[:punct:]",
                                         ...) {
  string <- stringr::str_extract_all(string, pattern, ...)

  lapply(
    string,
    \(x) {
      stringr::str_remove_all(toupper(x), pattern = remove_pattern)
    }
  )
}

#' [str_extract_cip_num()] extract a legacy CIP number.
#' @rdname str_capital
#' @name str_extract_cip_num
#' @export
str_extract_cip_num <- function(string) {
  stringr::str_extract(
    string,
    baltimoreCIPutils::cap_patterns[["cip_num"]]
  )
}
