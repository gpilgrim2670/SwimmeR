#' Discards elements of list that have an error value from \code{purrr::safely}.
#'
#' Used in scrapping, when \code{swim_parse} is applied over a list of results
#' using \code{purrr::map} the result is a list of two element lists. The first
#' element is the results, the second element is an error register.  This
#' function removes all elements where the error register is not NULL, and then
#' returns the results (first element) of the remaining lists.
#'
#' @param x a list of lists from \code{purrr::map} and \code{purrr:safely}
#' @return a list of lists where sub lists containing a non-NULL error have been
#'   discarded and error elements have been removed from all remaining sub lists
#'
#' @importFrom purrr discard
#'
#' @examples result_1 <- data.frame(result = c(1, 2, 3))
#' error <- NULL
#'
#' list_1 <- list(result_1, error)
#' names(list_1) <- c("result", "error")
#'
#' result_2 <- data.frame(result = c(4, 5, 6))
#' error <- "result is corrupt"
#'
#' list_2 <- list(result_2, error)
#' names(list_2) <- c("result", "error")
#'
#' list_of_lists <- list(list_1, list_2)
#' discard_errors(list_of_lists)
#'
#' @export


discard_errors <- function(x) {

  # function to extract the nth element of a list
  element_extract <- function(a_list, n) {
    sapply(a_list, `[`, n)
  }

  # discard all elements named error that aren't NULL
  x <- purrr::discard(x, ~ !is.null(.x$error))

  # extract the first element of each list
  x <- element_extract(x, n = 1)
  return(x)
}
