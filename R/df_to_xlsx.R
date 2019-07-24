#' Add Dataframe to Excel File
#'
#' This function adds a data.frame to an excel file, formatting the header. Also allows to select columns where rows with equal values are merged.
#' @param excel_file Location of excel file where data.frame will be added
#' @param sheet_name Name of the sheet inside the file -will replace sheets with same name-
#' @param df Data.frame to be added to file
#' @param merge_cols Character vector with names of columns.
#' @param nest_merge Should merging be done with columns on the right being nested inside the ones on the left?
#' @examples
#' df_to_xlsx(excel_file = "./excel.xlsx", sheet_name = "information", df = info_df, merge_cols = c("Continent", "Country"))

df_to_xlsx <- function(excel_file, sheet_name, df, merge_cols = NULL){
  wb <- openxlsx::loadWorkbook(excel_file)

  # Check some basic conditions before running function
  stopifnot(is.character(sheet_name),# Was a sheet name provided
            exists(deparse(substitute(df))), # Was an object provided to df
            merge_cols %in% colnames(df)) # Are merge columns present in df

  # Set minimum column width for when auto column sizing is done
  options("openxlsx.minWidth" = 6)
  print(1)

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


  # Check if provided sheet name exists - remove it if it does
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
    break_matrix <- matrix(nrow = nrow(df)+1,
                           ncol = length(merge_cols))
    break_matrix[] <- 0
    # Loop over columns - create break points for each individual column
    for (col_index in 1:length(merge_cols)) {
      col_name <- merge_cols[col_index]
      # Get vector of elements in column
      elements  <- df[[col_name]]

      # Initiate indices
      first <- 1    # First element in group
      # Loop over elements in column
      for (row_index in 1:length(elements)) {
        if (elements[row_index] != elements[first]) {
          # If 2 sequential elements are diferent, label them as break points
          break_matrix[row_index, col_index] <- 1
          # Update start of next group
          first <- row_index
        }
      }
    }

    # Propagate breaks from previous columns into next one
    for (col_index in 2:ncol(break_matrix)){
      for(row_index in 1:nrow(break_matrix)){
        break_matrix[row_index, col_index] <- sum(break_matrix[row_index,1:col_index])
      }
    }

    # Make first and last breaks always breaks
    break_matrix[1,] <- 1
    break_matrix[nrow(break_matrix),] <- 1

    break_matrix[break_matrix > 0] <- 1


    # Convert matrix into a list and apply merge function
    for (i in 1:ncol(break_matrix)){
      col_index <- which(colnames(df) == merge_cols[i])
      break_indices <- (which(break_matrix[,col_index] == 1))+1
      merge_matrix <- matrix(nrow = length(break_indices)-1,
                             ncol = 2)
      for (break_i in 1:(length(break_indices)-1)){
        merge_matrix[break_i,1] <- break_indices[break_i]
        merge_matrix[break_i,2] <- break_indices[break_i+1] -1
      }
      merge_matrix <- merge_matrix[(merge_matrix[,2] - merge_matrix[,1]) > 0,]
      merge_matrix <- as.vector(t(merge_matrix))
      indices <- split(merge_matrix, ceiling(seq_along(merge_matrix)/2))

      lapply(X = indices,
             FUN = function(x) openxlsx::mergeCells(wb = wb,
                                          sheet = sheet_name,
                                          cols = col_index,
                                          rows = x))
    }

  }

  #Save final excel workbook
  openxlsx::saveWorkbook(wb, excel_file, overwrite = TRUE)
}
