#' Helper function for formatting mm:ss.hh times as seconds
#'
#'
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#'
#'
#' @param x A character vector of time(s) in swimming format (eg 1:35.93) to be converted to seconds (95.93)


sec_format_helper <- function(x) {

  x <- as.character(x)
  if(is.na(x) == TRUE) return(NA)
  if (stringr::str_detect(x, ":") == TRUE) {
    min <- as.numeric(stringr::str_split_fixed(x, ":", n = 2)[,1])
    sec <- as.numeric(stringr::str_split_fixed(x, ":", n = 2)[,2])
    if (sec > 60) stop("Seconds cannot be greater than 60 in a swim formatted time")
    x <- (min*60) + sec
  } else {
    as.numeric(x)
  }
  return(as.numeric(x))

}
