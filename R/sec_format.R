#' Formatting mm:ss.tt times as seconds
#'
#' Takes a character string (or list) representing time in swimming format (eg 1:35.37) and converts it to a numeric value (95.37) or a list of values representing seconds.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @import stringr
#' @import purrr
#'
#' @param x A character vector of time(s) in swimming format (eg 1:35.93) to be converted to seconds (95.93)
#'
#' @seealso \code{\link{mmss_format}} \code{sec_format} is the reverse of \code{mmss_format}
#'
#' @export


sec_format <- function(x, ...) {
  x <- map_dbl(x, sec_format_helper)
  return(x)
}
