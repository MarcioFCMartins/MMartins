#' Create MetaData Sheet
#'
#' This function adds a data.frame to an excel file, formatting the header. Also allows to select columns where rows with equal values are merged.
#' @param excel_file Location of excel file where data.frame will be added
#' @param df Data.frame to be added to file
#' @param merge_cols Character vector with names of columns.
#' @param nested_merge Should merging be done with columns on the right being nested inside the ones on the left?
#' @examples
#' df_to_xlsx(excel_file = "./excel.xlsx", sheet_name = "information", df = info_df, merge_cols = c("Continent", "Country"))

# Structure of metadata page:
# Title -------
# Authors
# Date

# Box structure ----
# meta_box(header = "File contents", # Single cell
#          color  = "red",    # several character options, for which a dark and light colors have been picked for header and body
#          row1 = c("Sheet", "Variable", "Explanation"), # One cell per element in vector
#          row2 = c("Folha1", "Coisas que medi", "Faz muitas coisas buéda giras e tal"),
#          row3 = c("Folha1", "Outra coisa que medi", "Faz coisas girissimas"))

# First step: get the function to place the text (easy part)
# Second step: get the function to create boxes with the colors to make this look good

