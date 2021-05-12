#' Add row numbers to raw results
#'
#' Takes the output of \code{read_results} and adds row numbers to it
#'
#' @param text output from \code{read_results}
#' @return returns a data frame with event names and row numbers to eventually be recombined with swimming results inside \code{swim_parse}
#'
#' @seealso \code{add_row_numbers} is a helper function inside \code{\link{swim_parse}}


add_row_numbers <- function(text) {
  row_numbs <- seq(1, length(text), 1)
  text <-
    paste(text, row_numbs, sep = "       ") # increased spacing 12/21

  return(text)
}
