#' Formatting yy-mm ages as years
#'
#' Takes a character string (or list) representing an age as years-months (e.g. 13-06 for 13 years, 6 months) and converts it to a character value (13.5) or a list of values representing ages in years.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom purrr map_chr
#'
#' @param x A character vector of time(s) in swimming format (e.g. 1:35.93) to be converted to seconds (95.93)
#' @return returns the value of the string \code{x} which represents a time in swimming format (mm:ss.hh) and converts it to seconds
#'
#' @examples age_format("13-06")
#' age_format(c("13-06", "25-03", NA))
#'
#' @seealso \code{\link{age_format_helper}} \code{age_format} is uses \code{age_format_helper}
#'
#' @export

age_format <- function(x) {
  x <- purrr::map_chr(x, age_format_helper)
  return(x)
}
