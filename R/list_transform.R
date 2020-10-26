#' Transform list of lists into dataframe
#'
#' Converts list of lists, with all sub-lists having the same number of elements into a dataframe where each sub-list is a row and each element a column
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param x a list of lists, with all sub-lists having the same length
#' @return a dataframe where each sub-list is a row and each element of that sub-list is a column
#'
#' @seealso \code{list_transform} is a helper function used inside of \code{swim_parse}, \code{swim_parse_ISL}, \code{event_parse} and \code{event_parse_ISL}
#'


list_transform <- function(x) {
  df <- as.data.frame(t(as.data.frame(x)),
                      row.names = FALSE,
                      stringsAsFactors = FALSE)
  return(df)
}
