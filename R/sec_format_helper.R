#' Helper fucntion for formatting mm:ss.tt times as seconds
#'
#'
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @import stringr
#'
#' @param x A character vector of time(s) in swimming format (eg 1:35.93) to be converted to seconds (95.93)
#'


sec_format_helper <- function(x) {
  x <- as.character(x)
  if (str_detect(x, ":")){
    min <- as.numeric(str_split_fixed(x, ":", n = 2)[,1])
    sec <- as.numeric(str_split_fixed(x, ":", n = 2)[,2])
    x <- (min*60) + sec
  } else {
    as.numeric(x)
  }
  return(as.numeric(x))
}
