#' Reads swimming and diving results into a list of strings in preparation for parsing with \code{Swim_Parse}
#'
#' Outputs list of strings to be processed by \code{Swim_Parse}
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @import stringr
#' @import rvest
#' @import pdftools
#'
#' @param x a .pdf or .html file (could be a url) where containing swimming results.  Must be formatted in a "normal" fashion - see vignette
#' @param node a CSS node where html results are stored.  Required for html results.
#' @return returns a list of strings containing the information from \code{x}.  Should then be parsed with \code{Swim_Parse}
#'
#' @examples Read_Results("2008 NYSPHAA Federation Championship - 2_29_2008 to 3_1_2008.html", node = "pre")
#' Read_Results("Texas-Florida-Indiana.pdf")
#'
#' @seealso \code{Read_Results} is mean to be followed by \code{\link{Swim_Parse}}
#'
#' @export

Read_Results <- function(x, node = NULL) {
  '%!in%' <- function(x,y)!('%in%'(x,y))
  ### PDF ###
  if(str_detect(x, "\\.pdf$") == TRUE) {
  results <- pdf_text(x)

  } else if (str_detect(x, "\\.htm") == TRUE) {
  ### HTML ###
    if(is.character(node) == FALSE) {
  stop(" Please supply a value for node")
    } else {
  webpage <- read_html(x)
  html <- html_nodes(webpage, node)
  results <- html_text(html)

    }

  } else {
    stop("Please supply a valid .html or .pdf document")
  }

  as_lines <- str_extract_all(results, "\n.*")
  as_lines_list_2 <- unlist(as_lines, recursive = FALSE)

  return(as_lines_list_2)
}
