#' Discards elements of list that have an error value from \code{purrr::safely}
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param x a list of lists from \code{purrr::map} and \code{purrr:safely}
#' @return a list of lists where elements with an error have been discarded and all error elements have been removed
#'
#' @importFrom purrr discard
#'
#' @references https://pilgrim.netlify.app/post/scrapping-websites-and-building-a-large-dataset-with-swimmer/

discard_errors <- function(x) {
  element_extract <- function(lst, n) {
    sapply(lst, `[`, n)
  }
  x <- purrr::discard(x, ~ !is.null(.x$error))
  x <- element_extract(x, 1)
  return(x)
}
