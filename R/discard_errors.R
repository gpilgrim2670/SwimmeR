#' Discards elements of list that have an error value from \code{purrr::safely}.
#'
#' Used in scrapping, when \code{swim_parse} is applied over a list of results using \code{purrr::map} the result is a list of two element lists.
#' The first element is the results, the second element is an error register.  This funtion removes all elements where the error register is not NULL,
#' and then returns the results (first element) of the remaining lists.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param x a list of lists from \code{purrr::map} and \code{purrr:safely}
#' @return a list of lists where elements with an error have been discarded and all error elements have been removed
#'
#' @importFrom purrr discard


discard_errors <- function(x) {
  element_extract <- function(a_list, n) {
    sapply(a_list, `[`, n)
  }
  x <- purrr::discard(x, ~ !is.null(.x$error))
  x <- element_extract(x, 1)
  return(x)
}
