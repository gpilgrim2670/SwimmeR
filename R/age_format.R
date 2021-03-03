#' Formatting yyy-mm ages as years
#'
#' Takes a character string (or list) representing an age as years-months (e.g. 13-06 for 13 years, 6 months) and converts it to a character value (13.5) or a list of values representing ages in years.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom purrr map_chr
#'
#' @param x A character vector of ages in yyy-mm format (e.g. 93-03) to be converted to years (93.25)
#' @return returns the value of the string \code{x} which represents an age in yyy-mm format (93-03) and converts it to years (93.25)
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
