#' Breaks out lists of lists by sub-list length
#'
#' XXXXXX
#'
#' @importFrom purrr map
#'
#' @param x a list of lists, with at least some sub-lists having length
#'   \code{len}
#' @param len an numeric value for the length of sub-lists that
#'   \code{list_breaker} should break out.  Must be a whole number.
#' @return returns a list of lists, with all sub-lists having length \code{len}

list_breaker <- function(x, len){

  if(is.list(x) == FALSE){
    stop("x must be a list")
  }

  if(all.equal(len, as.integer(len)) == FALSE){
    stop("len must be a whole number")
  }

  list_portion <- x[purrr::map(x, length) == len]

  return(list_portion)

}
