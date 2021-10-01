#' Formatting mm:ss.tt times as seconds
#'
#' Takes a character string (or list) representing time in swimming format (e.g.
#' 1:35.37) and converts it to a numeric value (95.37) or a list of values
#' representing seconds.
#'
#' @importFrom purrr map_dbl
#'
#' @param x A character vector of time(s) in swimming format (e.g. 1:35.93) to
#'   be converted to seconds (95.93)
#' @return returns the value of the string \code{x} which represents a time in
#'   swimming format (mm:ss.hh) and converts it to seconds
#'
#' @examples sec_format("1:35.93")
#' sec_format("16:45.19")
#' sec_format("25.43")
#' sec_format(c("1:35.93", "16:45.19", "25.43"))
#' sec_format(c("1:35.93", "16:45.19", NA, "25.43", ":55.23"))
#'
#' @seealso \code{sec_format} is the reverse of \code{\link{mmss_format}}
#'
#' @export


sec_format <- function(x) {
  x <- purrr::map_dbl(x, sec_format_helper)
  return(x)
}


#' Helper function for formatting mm:ss.hh times as seconds, used to enable
#' vectorized operation of \code{sec_format}
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_remove
#'
#' @param x A character vector of time(s) in swimming format (e.g. 1:35.93) to
#'   be converted to seconds (95.93)


sec_format_helper <- function(x) {

  x <- as.character(x)
  x <- stringr::str_remove(x, "^\\:")
  x <- stringr::str_remove(x, "^`")
  if(is.na(x) == TRUE) return(NA)

  # if (stringr::str_detect(x, "`") == TRUE){
  #   x <- stringr::str_remove(x, "^`")
  # }

  if (stringr::str_detect(x, ":") == TRUE) {
    min <- as.numeric(stringr::str_split_fixed(x, ":", n = 2)[,1])
    sec <- as.numeric(stringr::str_split_fixed(x, ":", n = 2)[,2])
    if (sec >= 60) warning("Seconds should be less than 60 in a swim formatted time")
    x <- (min*60) + sec
  } else {
    as.numeric(x)
  }
  return(as.numeric(x))

}
