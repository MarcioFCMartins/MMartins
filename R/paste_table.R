#' Copy a table from R to clipboard or paste a table from clipboard into R.
#'
#' Allows the user to easily and quickly copy or paste tables from or to R
#' in a format that is compatible with Microsoft Excel.
#'
#' It really shines as a quick hacky tool for exploration, should never be used
#' in a script meant to be reproducible, for obvious reasons.
#'
#' @param obj Object that should be copied to clipboard
#'
#' @source "Using the Windows Clipboard, or Passing Data Quickly From Excel to R and Back Again"
#' \url{https://www.r-bloggers.com/using-the-windows-clipboard-or-passing-data-quickly-from-excel-to-r-and-back-again/}
#' @source "Stackoverflow"
#' \url{https://stackoverflow.com/questions/10959521/how-to-write-to-clipboard-on-ubuntu-linux-in-r}
#'
#' @examples
#' # Import table in clipboard as an object
#' # Do not pass any argument
#' df <- paste_table()
#'
#' # Copy a table to clipboard
#' copy_table(df)
#'
#' @export
paste_table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

#' @rdname paste_table
#' @export
copy_table <- function(obj, size = 4096) {
  os <- Sys.info()[['sysname']]

  if(os == "Windows"){
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)
  } else if(os == "Linux"){
    con <- pipe("xclip -selection c", open="w")
    on.exit(close(con))
    write.table(obj, con, sep="\t", row.names=FALSE)
  } else {
    message("Function not implemented for this operating system")
  }
}

