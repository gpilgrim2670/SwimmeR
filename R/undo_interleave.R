#' Undoes interleaving of lists
#'
#' If two lists have been interleaved this function will return the lists
#' separated and then concatenated
#'
#' @param x a list to be un-interleaved
#' @return a list comprising the interleaved components of \code{x} joined into
#'   one list
#' @examples
#' l <- c("A", "D", "B", "E", "C", "F")
#' undo_interleave(l)

undo_interleave <- function(x){

  l <- seq(1, length(x[is.na(x) == FALSE]), 1)
  evens <- l[l %% 2 != 1]
  odds <- l[l %% 2 == 1]

  resort_index <- order(c(seq_along(odds), seq_along(evens)))

  if(any(is.na(x)) == TRUE){
  pad <- seq(max(c(odds, evens) + 1), length(x), 1)

  resort_index <- c(resort_index, pad)
  }

  x <- x[resort_index]

  return(x)

}
