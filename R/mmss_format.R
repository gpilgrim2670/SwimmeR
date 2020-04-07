#' Formatting seconds as mm:ss.hh
#'
#' Takes a numeric item or list of numeric items representing seconds (eg 95.37) and converts to a character string or list of strings in swimming format ("1:35.37").
#'
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param x A number of seconds to be converted to swimming format
#' @return the number of seconds \code{x} converted to conventional swimming format mm:ss.hh
#'
#' @examples mmss_format(95.37)
#' mmss_format(200.95)
#' mmss_format(59.47)
#' mmss_format(c(95.37, 200.95, 59.47, NA))
#'
#' @seealso \code{\link{sec_format}} \code{mmss_format} is the reverse of \code{sec_format}
#'
#' @export

mmss_format <- function(x) {
  # if (na.rm) x <- na.omit(x)
  sec <- x%%60
  min <- x%/%60
  sec <- base::sprintf("%05.2f", sec)
  ifelse(min == 0, paste(sec),
         paste(min, sec, sep = ":"))
}
