#' Add Dataframe to Excel File
#'
#' This function adds a data.frame to an excel file, formatting the header. Also allows to select columns where rows with equal values are merged.
#' @param excel_file Location of excel file where data.frame will be added
#' @param sheet_name Names of the sheet inside the file -will replace sheets with same name-
#' @param df List of data.frame to be added to file
#' @param auto_size_cols Should column width be changed based on their contents? TRUE or FALSE, defaults to TRUE
#' @param merge_cols Names of columns that should be merged vertically when they have the same value. Default is no merging, expects a character vector. Currently only works if you pass a single df
#' @param nested_merge Should merging be done with columns on the right being nested inside the ones on the left? TRUE or FALSE
#' @return Excel file with data.frame added
#'
#' @examples
#' df_to_xlsx(excel_file = "./excel.xlsx", sheet_name = "information", df = info_df, merge_cols = c("Continent", "Country"))
#' @export

df_to_xlsx <- function(excel_file, sheet_names, dfs, auto_size_cols = TRUE, merge_cols = NULL, nested_merge = TRUE) {
  if (file.exists(excel_file)) {
    wb <- openxlsx2::wb_load(excel_file) # Load excel file if it exists
  } else {
    input <- readline("This file does not exist, create a new one? (y/n)")

    if (tolower(input) == "y") {
      wb <- openxlsx2::wb_workbook()
      message("New excel file was created")
    } else {
      return(message("Please provide the correct filename"))
    }
  }


  # Check some basic conditions before running function
  stopifnot(
    is.character(sheet_names),        # Was a sheet name provided
    !missing(dfs), # Was an object provided to df
    merge_cols %in% colnames(dfs),     # Are merge columns present in df
    is.logical(auto_size_cols),      # Is auto_size_cols a logical
    length(sheet_names) == length(dfs) # Are the number of sheet names and dataframes the same
  )

  # Set minimum column width for when auto column sizing is done
  options("openxlsx2.minWidth" = 6)

  # Replace sheets that already exist in the workbook
  wb <- .replace_sheets(wb, sheet_names = sheet_names)
  # Add sheets that do not exist in the workbook
  wb <- .add_sheets(wb, sheet_names = sheet_names)
  # Add dataframes to the workbook and apply formatting as required
  wb <- .add_data(wb, sheet_names, dfs, auto_size_cols, merge_cols, nested_merge)
  # Save final excel workbook
  openxlsx2::wb_save(wb, excel_file, overwrite = TRUE)
}

.replace_sheets <- function(wb, sheet_names) {
    if (any(sheet_names %in% openxlsx2::wb_get_sheet_names(wb))) {
        sheets_to_remove <- sheet_names[sheet_names %in% openxlsx2::wb_get_sheet_names(wb)]

        for (sheet in sheets_to_remove) {
            wb <- openxlsx2::wb_clean_sheet(wb, sheet = sheet)
        }

        message(
            "The following sheets were replaced:\n", paste(sheets_to_remove, collapse = "\n")
        )
    }

    return(wb)
}

.add_sheets <- function(wb, sheet_names) {
    if(any(!sheet_names %in% openxlsx2::wb_get_sheet_names(wb))) {
        sheets_to_add <- sheet_names[!sheet_names %in% openxlsx2::wb_get_sheet_names(wb)]

        for (sheet in sheets_to_add) {
            wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet)
        }
    }
    return(wb)
}

.add_data <- function(wb, sheet_names, dfs, auto_size_cols, merge_cols, nested_merge) {
    for(i in 1:length(sheet_names)){
        df <- dfs[[i]]
        sheet_name <- sheet_names[i]

        # Add data.frame to excel sheet
        wb <- openxlsx2::wb_add_data(
            wb = wb,
            x = df,
            sheet = sheet_name
        )

        # Auto size columns to fit all the text
        if (auto_size_cols) {
            wb <- openxlsx2::wb_set_col_widths(
                wb,
                sheet = sheet_name,
                cols = 1:ncol(df),
                widths = "auto"
            )
        }


        # Style for column header - bold, centered, wrapped, yellow background
        # and thick border at the bottom
        header_dims <- openxlsx2::wb_dims(x = df, select = "col_names")

        wb <- wb |>
            openxlsx2::wb_add_border(
                dims = header_dims,
                top_color = openxlsx2::wb_color(hex = "#FF000000"),
                top_style = "thin",
                bottom_color = openxlsx2::wb_color(hex = "#FF000000"),
                bottom_style = "thick")|>
            openxlsx2::wb_add_fill(dims = header_dims, color = openxlsx2::wb_color(hex = "#ffbb70")) |>
            openxlsx2::wb_add_font(dims = header_dims, bold = TRUE) |>
            openxlsx2::wb_add_cell_style(
                dims = header_dims, horizontal = "center", vertical = "center")



        # Merge cells of specified columns when they are equal
        if (!is.null(merge_cols)) {
            merge_ranges <- list()
            # Identify breakpoints for all columns
            for (col_index in 1:length(merge_cols)) {
                col_name <- merge_cols[col_index]
                # Get vector of elements in column
                elements <- df[[col_name]]
                # Get run lengths of elements
                run_lengths <- cumsum(rle(elements)$lengths)
                # First merge group starts at 2. Then, each group starts at the end
                # of the previous one, shifted by +1 added because of the header row and
                # +1 again to start after the last group
                start_merge_indices <- c(2, run_lengths[-length(run_lengths)] + 2)
                # End of merge groups, shifted by +1 for header
                end_merge_indices <- c(run_lengths + 1)

                if (isTRUE(nested_merge) & col_index > 1) {
                    # If nested merge is TRUE, merge ranges from previous column are
                    # added to this column
                    prev_col_ranges <- merge_ranges[[col_index - 1]]
                    # Combine start merge ranges and remove duplicates
                    start_merge_indices <- unique(
                        c(start_merge_indices, prev_col_ranges[, 1])
                    )
                    start_merge_indices <- start_merge_indices[order(start_merge_indices)]
                    # Combine end merge ranges and remove duplicates
                    end_merge_indices <- unique(
                        c(end_merge_indices, prev_col_ranges[, 2])
                    )
                    end_merge_indices <- end_merge_indices[order(end_merge_indices)]
                }

                merge_ranges[[col_index]] <- cbind(start_merge_indices, end_merge_indices)
            }

            # Apply merge function to worksheet based on merge ranges
            for (col_index in 1:length(merge_cols)) {
                indices <- merge_ranges[[col_index]]
                col_name <- merge_cols[col_index]
                col_pos <- which(names(df) == col_name)

                for(i in 1:nrow(indices)){
                    index <- indices[i, ]
                    wb <- openxlsx2::wb_merge_cells(
                        wb = wb,
                        sheet = sheet_name,
                        dims = openxlsx2::wb_dims(
                            cols = col_pos,
                            rows = c(index[1]:index[2]))
                    )
                }
            }
        }
    }

    return(wb)
}

