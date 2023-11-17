#' Format data according to linked template
#'
#'
#' @param x Table to format (data frame or tibble)
#' @param file Location where to save the formatted xlsx
#' @param sheet_name Name of the sheet to save the data to
#' @param template_name Name of the template to use as a header
#' @param all_columns Should all columns from the template be included in the
#' output file, even if not present in data? (empty columns will be filled in 'NA')
#' @param allow_new_cols Should columns NOT present in the template
#' be allowed?
#' @return Nothing, this function is used for its side effects - saving a file
#' @export

create_excel_from_template <- function(
    x,
    file,
    sheet_name = "data",
    template_name = NULL,
    all_columns = FALSE,
    allow_new_cols = FALSE) {
    # Download template
    template_loc <- tempfile()
    download.file(
        "https://ualg365-my.sharepoint.com/:x:/g/personal/mfcmartins_ualg_pt/EeSI73WtkIhAvIda1uHKpzcBT6lyLHCFvm5C6MM7VtrKsg?download=1",
        template_loc
    )

    # Read template ---------------------------------------------------------
    template <- openxlsx2::wb_load(template_loc)
    if (is.null(template_name)) {
        available_sheets <- openxlsx2::wb_get_sheet_names(template)
        stop(
            "Please choose a template to use. Available templates are:",
            paste0("\n - ", available_sheets)
        )
    } else {
        template_header <- openxlsx2::read_xlsx(
            template,
            template_name,
            col_names = FALSE,
            skip_empty_rows = TRUE,
            skip_empty_cols = TRUE
        )

        template_cols <- c(
            unlist(
                template_header[nrow(template_header), ],
                use.names = FALSE
            )
        )

        names(template_header) <- template_cols

        template <- wb_clone_worksheet(template, old = template_name, new = "template")
        sheets_to_remove <- openxlsx2::wb_get_sheet_names(template)[openxlsx2::wb_get_sheet_names(template) != "template"]
        for(r_sheet in sheets_to_remove) {
            template <- openxlsx2::wb_remove_worksheet(template, sheet = r_sheet)
        }
    }

    # Check if all columns are present in template ---------------------------
    data_cols <- colnames(x)
    # If no new columns are allowed and some names do not match, stop
    # and tell the user which are the most probable fixes for their column names
    if (!all(data_cols %in% template_cols) & !allow_new_cols) {
        # Calculate closes matches for each problematic column
        problematic_cols <- data_cols[!(data_cols %in% template_cols)]
        distances_from_template <- utils::adist(
            problematic_cols,
            template_cols,
        )

        # Return indices for closest matches based on minimum letter changes required.
        # The top 2 distances are returned, but only if less than 50% of the letters need to be changed
        for (i in 1:nrow(distances_from_template)) {
            max_changes <- ceiling(nchar(problematic_cols[i]) / 2)
            # If more than 50% of the letters needs to be changed, change dist to NA to disqualify it
            distances_from_template[i, ][distances_from_template[i, ] > max_changes] <- NA
        }
        closest_matches <- apply(
            distances_from_template,
            1,
            function(x) {
                if (all(is.na(x))) {
                    return(NA)
                }
                # For each problematic column, calculate the second highest
                # distance and return possible matches for that difference, or lower
                max_diff <- max(unique(sort(na.omit(x)))[1:2], na.rm = T)
                return(which(x <= max_diff))
            },
            simplify = FALSE
        )

        # Create vector of name change suggestions for error message
        name_fix_suggestions <- character()
        for (i in 1:length(closest_matches)) {
            original_name <- problematic_cols[i]

            if (all(is.na(closest_matches[[i]]))) {
                possible_names <- "No suggestions"
            } else {
                possible_names <- template_cols[closest_matches[[i]]]
            }

            name_fix_suggestions <- c(
                name_fix_suggestions,
                paste0(
                    original_name,
                    " -> ",
                    paste0(possible_names, collapse = ", "),
                    "\n"
                )
            )
        }

        stop(
            "Not all columns are present in the template. Please verify and correct your column names.\n",
            "Here are the most probable fixes:\n",
            name_fix_suggestions
        )

    } else if (!all(data_cols %in% template_cols) & allow_new_cols) {

        warning(
            "You are adding new columns, which were not present in the template.\n",
            "Verify that this is what you want to do"
        )

    }

    # Prepare data to export ----------------------------------------------
    if (all_columns) {
        # Add all columns from template to the data
        x[ , template_cols[!template_cols %in% data_cols]] <- NA
        # Reorder columns to match template
        final_col_order <- c(template_cols, data_cols[!data_cols %in% template_cols])
        x <- x[ , final_col_order]

        # Now add the header - add empty columns to match data
        template_header <- cbind(
            template_header,
            matrix(
                NA,
                nrow = nrow(template_header),
                ncol = ncol(x) - ncol(template_header)
            )
        )
        names(template_header) <- names(x)

        x <- rbind(template_header, x)

    } else {
        # Reorder columns to match template
        final_col_order <- c(template_cols[template_cols %in% data_cols], data_cols[!data_cols %in% template_cols])
        x <- x[ , final_col_order]

        template_header[data_cols[!data_cols %in% template_cols]] <- NA
        template_header <- template_header[ , final_col_order]
        x <- rbind(template_header, x)
    }
    # Make the fourth row the machine readable names
    x[4, ] <- colnames(x)

    # Write data to file ---------------------------------------------------
    # Add our data to a new sheet in the template workbook
    wb_data <- template |>
        openxlsx2::wb_add_worksheet(sheet_name) |>
        openxlsx2::wb_add_data(sheet_name, x = x, col_names = FALSE)

    # Match column styles to styles present in template
    template_col_match <- match(final_col_order, template_cols)
    for(i in 1:length(final_col_order)) {
        if(is.na(template_col_match[i])) {
            next
        }
        header_col_style <- openxlsx2::wb_get_cell_style(
            wb_data,
            "template",
            dims = openxlsx2::wb_dims(rows = 1:4, cols = template_col_match[i])
        )

        wb_data <- openxlsx2::wb_set_cell_style(
            wb_data,
            sheet_name,
            dims = openxlsx2::wb_dims(rows = 1:4, cols = i),
            style = header_col_style
        )
    }

    # Match the column widths to the template widths
    template_sheet_index <- which(openxlsx2::wb_get_sheet_names(template) == "template")
    data_sheet_index <- which(openxlsx2::wb_get_sheet_names(wb_data) == sheet_name)
    for(i in 1:length(final_col_order)) {
        if(is.na(template_col_match[i])) {
            # If column was not in template, default width is used
            wb_data[["worksheets"]][[data_sheet_index]][["cols_attr"]][i] <- NA
        } else {
            # Else, copy column width from template
            wb_data[["worksheets"]][[data_sheet_index]][["cols_attr"]][i] <-
            template[["worksheets"]][[template_sheet_index]][["cols_attr"]][template_col_match[i]]
        }
    }

    # Match the row heights to the template heights
    for(i in 1:nrow(template_header)) {
        wb_data[["worksheets"]][[data_sheet_index]]$sheet_data$row_attr$ht[i] <-
            wb_data[["worksheets"]][[template_sheet_index]]$sheet_data$row_attr$ht[i]
    }


    # Remove all worksheets except for the data one, and rename it to user selected name
    sheets_to_remove <- openxlsx2::wb_get_sheet_names(wb_data)[openxlsx2::wb_get_sheet_names(wb_data) != sheet_name]
    for(r_sheet in sheets_to_remove) {
        wb_data <- openxlsx2::wb_remove_worksheet(wb_data, sheet = r_sheet)
    }

    # Save to file
    wb_save(wb_data, file = file, overwrite = TRUE)
}
