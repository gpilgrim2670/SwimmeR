#' Formatting yyy-mm ages as years
#'
#' Takes a character string (or list) representing an age as years-months (e.g.
#' 13-06 for 13 years, 6 months) and converts it to a character value (13.5) or
#' a list of values representing ages in years.
#'
#' @importFrom purrr map_chr
#'
#' @param x A character vector of ages in yyy-mm format (e.g. 93-03) to be
#'   converted to years (93.25)
#' @return returns the value of the string \code{x} which represents an age in
#'   yyy-mm format (93-03) and converts it to years (93.25)
#'
#' @examples age_format("13-06")
#' age_format(c("13-06", "25-03", NA))
#'
#' @seealso \code{\link{age_format_helper}} \code{age_format} uses
#'   \code{age_format_helper}
#'
#' @export

age_format <- function(x) {
  x <- purrr::map_chr(x, age_format_helper)
  return(x)
}

#' Helper function for formatting yyy-mm ages as years, enables vectorization of
#' \code{age_format}
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_remove
#'
#' @param x A character vector of age(s) in yyyy-mm format (e.g. 13-06) to be
#'   converted to years (13.5)


age_format_helper <- function(x) {

  x <- as.character(x)
  if(is.na(x) == TRUE) return(NA)
  if (stringr::str_detect(x, "-") == TRUE) {
    years <- as.numeric(stringr::str_split_fixed(x, "-", n = 2)[,1])
    months <- as.numeric(stringr::str_split_fixed(x, "-", n = 2)[,2])
    if (months >= 12){
      # some Australian results do age_in_years-YOB as 32-88 (I am 32, born 1988)
      # but this conflicts with yyy-mm format
      x <- as.character(years)
    } else {
      # x <- as.character(years + round(months/12, 2))
      x <- as.character(years)
    }
  } else {
    as.character(x)
  }
  return(as.character(x))

}
