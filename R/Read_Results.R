#' Reads swimming and diving results into a list of strings in preparation for parsing with \code{Swim_Parse}
#'
#' Outputs list of strings to be processed by \code{Swim_Parse}
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom  stringr str_detect
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom pdftools pdf_text
#'
#'
#'
#' @param file a .pdf or .html file (could be a url) where containing swimming results.  Must be formatted in a "normal" fashion - see vignette
#' @param node a CSS node where html results are stored.  Required for html results.
#' @return returns a list of strings containing the information from \code{x}.  Should then be parsed with \code{Swim_Parse}
#'
#' @examples \dontrun{Read_Results("http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm", node = "pre")}
#'
#' @seealso \code{Read_Results} is meant to be followed by \code{\link{Swim_Parse}}
#'
#' @export

Read_Results <- function(file, node = NULL) {
  '%!in%' <- function(x,y)!('%in%'(x,y))
  ### PDF ###
  if(stringr::str_detect(file, "\\.pdf$") == TRUE) {
  results <- pdftools::pdf_text(file)

  } else if (stringr::str_detect(file, "\\.htm") == TRUE) {
  ### HTML ###
    if(is.character(node) == FALSE) {
  stop(" Please supply a value for node")
    } else {
  webpage <- read_html(file)
  html <- rvest::html_nodes(webpage, node)
  results <- rvest::html_text(html)

    }

  } else {
    stop("Please supply a valid .html or .pdf document")
  }

  as_lines <- str_extract_all(results, "\n.*")
  as_lines_list_2 <- unlist(as_lines, recursive = FALSE)

  return(as_lines_list_2)
}
