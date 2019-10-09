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
#' @return returns the value of the string \code{x} which represents a time in swimming format (mm:ss.hh) and converts it to seconds
#'
#' @examples sec_format("1:35.93")
#' sec_format("16:45.19")
#' sec_format("25.43")
#' sec_format(c("1:35.93", "16:45.19", "25.43"))
#'
#' @seealso \code{\link{mmss_format}} \code{sec_format} is the reverse of \code{mmss_format}
#'
#' @export


sec_format <- function(x) {
  if(sum(map_lgl(x, is.character)) != 1) stop("Enter swim formatted time as character string eg '1:35.97'")
  x <- map_dbl(x, sec_format_helper)
  return(x)
}
