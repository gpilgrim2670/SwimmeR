#' Helper function for formatting yyy-mm ages as years
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_remove
#'
#' @param x A character vector of age(s) in yyy-mm format (e.g. 13-06) to be converted to years (13.5)


age_format_helper <- function(x) {

  x <- as.character(x)
  if(is.na(x) == TRUE) return(NA)
  if (stringr::str_detect(x, "-") == TRUE) {
    years <- as.numeric(stringr::str_split_fixed(x, "-", n = 2)[,1])
    months <- as.numeric(stringr::str_split_fixed(x, "-", n = 2)[,2])
    if (months >= 12) stop("Months must be less than 12")
    x <- as.character(years + round(months/12, 2))
  } else {
    as.character(x)
  }
  return(as.character(x))

}
