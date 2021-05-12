#' "Not in" function
#'
#' The opposite of `%in%`.  Returns `TRUE` if `x` is not `%in%` `y`.  Returns `FALSE` otherwise.
#'
#' @param x a value
#' @param y a list of values
#' @return a `TRUE` or `FALSE`
#'
#' @examples
#' "a" %!in% c("a", "b", "c")
#' "a" %notin% c("b", "c")
#'
#' @export


'%notin%' <- function(x, y) {
  !('%in%'(x, y))
}

#' @rdname grapes-notin-grapes
#' @export
`%!in%` <- `%notin%`
