#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


na_if_character <- function(df, val) {
  dplyr::mutate(df, dplyr::across(where(is.character), ~ na_if(.x, val)))
}
na_if_numeric <- function(df, val) {
  dplyr::mutate(df, dplyr::across(where(is.numeric), ~ na_if(.x, val)))
}
