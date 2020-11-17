#' Fills NA values with previous non-NA value
#'
#' This is a base approximation of \code{tidyr::fill()}
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param x a list having some number of non-NA values
#' @return a list where NA values have been replaced with the closest previous non-NA value
#'
#' @seealso \code{fill_down} is a helper function inside \code{lines_sort}
#'

fill_down <- function(x) {
  if (length(x) > 1) {
    keep <- c(TRUE, !is.na(x[-1]))
    x[keep][cumsum(keep)]
  } else {
    x
  }
}
