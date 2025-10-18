library(tidyverse)
library(openxlsx2)

eib_path <- "/Users/elipousson/Downloads/COB_Submit_Project_v38.1.xlsx"
eib_path <- "/Users/elipousson/Downloads/9953 Data load EIB template.xlsx"
eib_path <- "/Users/elipousson/Downloads/Put_Budget_Template_v38.2.xlsx"
eib_path <- "/Users/elipousson/Downloads/COB_Import_Capital_Budget_-_Financial_v37.0.xlsx"

# eib_path <- "C:/Users/Eli.Pousson/Downloads/COB_Project_Budget_Import_v37.0.xlsx"
# max_cols <- c("K", "Q")
# sheet_names <- c("Import Budget High Volume", "Budget Lines Data")

eib_path <- "C:/Users/Eli.Pousson/Downloads/Conv_Project_Plans_v36.1.xlsx"
max_cols <- c("AG")
sheet_names <- c("Project Plan")

eib_path <- "/Users/Eli.Pousson/City Of Baltimore/DOP-POLICY&DATA - Documents/CIP/FY 2026-2031/Budget Load/files/COB_Submit_Project SBX and PRD Template.xlsx"
max_cols <- c("BZ")
sheet_names <- c("Submit Project")


eib_wb <- openxlsx2::wb_load(eib_path)

# sheet_name <- "Import Budget Amendment"
# sheet_name <- "Amendment Entry Data"
# sheet_name <- "Budget Template"
# sheet_name <- "Import Budget High Volume"
# sheet_name <- "Budget Lines Data"

eib_dict_src <- purrr::map2(
  max_cols,
  sheet_names,
  \(max_col, sheet_name) {
    comments_src <- eib_wb |>
      openxlsx2::wb_get_comment(
        sheet = sheet_name,
        dims = paste0("B4:", max_col, "4") # "B4:AA4" #"B4:BS4"
      )

    if (!is_empty(comments_src)) {
      format_comments <- comments_src |>
        dplyr::mutate(
          col_order = openxlsx2::dims_to_rowcol(ref, as_integer = TRUE)[[
            "col"
          ]],
          `Format Comment` = comment,
          .keep = "none"
        )
    } else {
      format_comments <- data.frame(col_order = NA)
    }

    fields_src <- eib_wb |>
      openxlsx2::wb_get_comment(
        sheet = sheet_name,
        dims = paste0(
          "B5:",
          max_col,
          "5"
        ) # "B5:AQ5" #"B5:AA5" # "B5:BS5"
      )

    if (!is_empty(comments_src)) {
      fields_comments <- fields_src |>
        dplyr::mutate(
          col_order = openxlsx2::dims_to_rowcol(ref, as_integer = TRUE)[[
            "col"
          ]],
          `Field Comment` = comment,
          .keep = "none"
        )
    } else {
      fields_comments <- data.frame(col_order = NA)
    }

    xlsx_columns <- eib_wb |>
      openxlsx2::read_xlsx(
        start_row = 5,
        sheet = sheet_name,
        convert = FALSE,
        col_names = FALSE
      ) |>
      as.matrix() |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      janitor::remove_empty("cols") |>
      dplyr::mutate(
        col_order = row_number()
      ) |>
      dplyr::select(
        col_order,
        Column = rowname
      )

    # Use this to prep the EIB dictionary
    eib_dict_src <- openxlsx2::read_xlsx(
      eib_wb,
      sheet = sheet_name,
      rows = c(2:5),
      fill_merged_cells = TRUE,
      col_names = FALSE
    ) |>
      as.matrix() |>
      t() |>
      janitor::row_to_names(row_number = 1) |>
      as_tibble() |>
      dplyr::mutate(
        col_order = row_number() + 1
      ) |>
      left_join(
        format_comments
      ) |>
      left_join(
        fields_comments
      ) |>
      left_join(
        xlsx_columns
      )

    eib_dict_src
  }
) |>
  set_names(
    sheet_names
  ) |>
  list_rbind(names_to = "Sheet")

eib_dict_src |>
  openxlsx2Extras::as_wb() |>
  openxlsx2::wb_save(
    "COB_Submit_Project SBX and PRD Template_Data-Dictionary.xlsx"
  )
