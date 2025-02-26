#' Combine two vectors into a single vector with a separator
#'
#' Used to create tables for publication, where values are presented with their errors
#' The function will also deal with NAs, omitting the sep and error if error is NA
#' or returning a blank string the whole output if value is NA
#'
#' @param value Numerical vector of value of a variable
#' @param error Numerical vector of error assocciated with variable
#' @param sep Separator for value and error values. Defaults to ±
#' @param digits Number of digits to numbers to. Defaults to 2
#'
#' @examples
#' # This table makes to sense, but it shows how the function works
#' library(dplyr)
#' mutate(cars, example = paste_with_error(speed, dist))
#'
paste_with_error <- function(value, error, sep = " ± ", digits = 2) {

    values <- matrix(
        c(value, error),
        ncol = 2)

    apply(
        values,
        1,
        function(x){
            value_ <- x[1]
            error_ <- x[2]
            if(is.na(value_)) return("")

            if(is.na(error_)) return(round(value_, digits))

            return(paste0(
                round(value_, digits),
                sep,
                round(error_, digits)
            ))
        }
    )

}
