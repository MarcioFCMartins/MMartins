#' Add Dataframe to Excel File
#'
#' This function adds a data.frame to an excel file, formatting the header. Also allows to select columns where rows with equal values are merged.
#' @param excel_file Location of excel file where data.frame will be added
#' @param sheet_name Name of the sheet inside the file -will replace sheets with same name-
#' @param df Data.frame to be added to file
#' @param merge_cols Character vector with names of columns. Categories on assumed to be nested (breaks from columns on the left will be carried on to columns on the right)
#' @examples
#' df_to_xlsx(excel_file = "./excel.xlsx", sheet_name = "information", df = info_df, merge_cols = c("Continent", "Country"))

df_to_xlsx <- function(excel_file, sheet_name, df, merge_cols = NULL){
  # Set minimum column width for when auto column sizing is done
  options("openxlsx.minWidth" = 6)
  wb <- openxlsx::loadWorkbook(excel_file)

  # Style for column header - bold, centered, wrapped, yellow background
  # and thick border at the bottom
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border         = c("top", "bottom"),
    borderStyle    = c("thin", "thick"),
    borderColour   = c("black", "black"),
    fgFill         = c("#ffe699"),
    wrapText       = TRUE,
    halign         = "center")


  # Check if sheet name provided exists - remove it if it does
  if(sheet_name %in% names(wb)){
    openxlsx::removeWorksheet(
      wb,
      sheet = sheet_name)

    message("Your workbook already had a sheet named ", sheet_name,
            ".\nThis sheet has been replaced!")
  }

  # Create a new sheet in the workbook
  openxlsx::addWorksheet(wb, sheetName = sheet_name)

  # Write data.frame to created sheet - with header style
  openxlsx::writeData(wb    = wb,
                      sheet = sheet_name,
                      x     = df,
                      headerStyle = header_style)

  # Auto size columns to fit all the text
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         cols  = 1:ncol(df),
                         widths = "auto")

  # Merge cells of specified columns when they are equal
  if(!is.null(merge_cols)) {
    # Matrix where break points are represented as 1's
    index_matrix <- matrix(nrow = nrow(df),
                           ncol = length(merge_cols))
    index_matrix[] <- 0
    index_matrix[nrow(index_matrix),] <- 1
    index_matrix[1,] <- 1
    # Loop over columns - create break points for each individual column
    for (col_index in 1:length(merge_cols)) {

      col_name <- merge_cols[col_index]
      # Get vector of elements in column
      elements  <- df[[col_name]]

      # Initiate indices
      first <- 1    # First element in group

      # Loop over elements in column
      for (i in 1:length(elements)) {
        if (elements[i] != elements[first]) {
          # If 2 sequential elements are diferent, label them as break points
          index_matrix[i-1,col_index] <- 1
          index_matrix[i , col_index] <- 1
          # Update start of next group
          first <- i
        }
      }
    }

    # Propagate breaks from previous columns into next one
    for (col_index in 2:ncol(index_matrix)){
      for(row_index in 1:nrow(index_matrix)){
        index_matrix[row_index, col_index] <- sum(index_matrix[row_index,1:col_index])
      }
    }

    index_matrix[index_matrix > 0] <- 1

    # Convert matrix into a list and apply merge function
    for (i in 1:ncol(index_matrix)){
      col_index <- which(colnames(df) == merge_cols[i])
      indices <- which(index_matrix[,i] == 1)+1
      indices <- split(indices, ceiling(seq_along(indices)/2))

      # Apply merge function to row indices stored in list
      lapply(X = indices,
             FUN = function(x) mergeCells(wb = wb,
                                          sheet = sheet_name,
                                          cols = col_index,
                                          rows = x))
    }
  } # End of column mergind section

  #Save final excel workbook
  openxlsx::saveWorkbook(wb, excel_file, overwrite = TRUE)
}
