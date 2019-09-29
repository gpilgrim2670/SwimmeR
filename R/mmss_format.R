#' Formatting seconds as mm:ss.tt
#' 
#'  @author Greg Pilgrim
#' 
#'  @param x A number of seconds to be converted to swimming format
#'  
#'  @export

mmss_format <- function(x, ...) {
  sec <- x%%60
  min <- x%/%60
  sec <- base::sprintf("%05.2f", sec)
  ifelse(min == 0, paste(sec), 
         paste(min, sec, sep = ":"))
}