#' Formatting seconds as mm:ss.tt
#'
#' Takes a numeric item or list of numeric items representing seconds (eg 95.37) and converts to a character string or list of strings in swimming format ("1:35.37").
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param x A number of seconds to be converted to swimming format
#'
#' @seealso \code{\link{sec_format}} \code{mmss_format} is the reverse of \code{sec_format}
#'
#' @export

mmss_format <- function(x, ...) {
  sec <- x%%60
  min <- x%/%60
  sec <- base::sprintf("%05.2f", sec)
  ifelse(min == 0, paste(sec),
         paste(min, sec, sep = ":"))
}
