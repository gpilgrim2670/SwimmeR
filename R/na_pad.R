#' Pads shorter lists in a list-of-lists with \code{NA}s such that all lists are
#' the same length
#'
#' Adds \code{NA} values to the end of each list in a list of lists such that
#' they all become the length of the longest list.  The longest list will not
#' have any \code{NA}s added to it.
#'
#' @param x a list of lists, with sub-lists having different lengths
#' @param y a list of the nubmer of \code{NA} values to append to each sub-list
#' @return a list of lists with each sub-list the same length


na_pad <- function(x, y){
  padded_list <- c(x, rep(NA, y))
  return(padded_list)
}
