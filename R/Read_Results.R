#' Reads swimming and diving results into a list of strings in preparation for
#' parsing with \code{swim_parse}
#'
#' Outputs list of strings to be processed by \code{swim_parse}
#'
#' @importFrom stringr str_detect
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom pdftools pdf_text
#'
#' @param file a .pdf or .html file (could be a url) where containing swimming
#'   results.  Must be formatted in a "normal" fashion - see vignette
#' @param node a CSS node where html results are stored.  Required for html
#'   results.  Default is "pre", which nearly always works.
#' @return returns a list of strings containing the information from
#'   \code{file}.  Should then be parsed with \code{swim_parse}
#'
#' @examples \dontrun{read_results("http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm", node = "pre")}
#'
#' @seealso \code{read_results} is meant to be followed by
#'   \code{\link{swim_parse}}
#'
#' @export

Read_Results <- function(file, node = "pre") {
  #### testing ####
  # file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")

  #### begin actual function ####
  if (stringr::str_detect(file, "\\.pdf$|\\.aspx$") == TRUE) {
    ### PDF ###
    results <- pdftools::pdf_text(file)
    as_lines <- stringr::str_extract_all(results, "\n.*")
    as_lines_list_2 <- unlist(as_lines, recursive = FALSE)
    as_lines_list_2 <- read_results_flag(as_lines_list_2)

    return(as_lines_list_2)

  } else if (stringr::str_detect(file, "\\.htm") == TRUE) {
    ### HTML ###
    if (is.character(node) == FALSE) {
      stop(" Please supply a value for node")
    } else {
      webpage <- xml2::read_html(file)
      html <- rvest::html_nodes(webpage, node)
      results <- rvest::html_text(html)
      as_lines <- stringr::str_extract_all(results, "\n.*")
      as_lines_list_2 <- unlist(as_lines, recursive = FALSE)
      as_lines_list_2 <- read_results_flag(as_lines_list_2)

      return(as_lines_list_2)

    }
  } else if (stringr::str_detect(file, "\\.hy3") == TRUE) {
    ### hy3 ###
    ### add automatic unzipping?
    results <-
      readr::read_delim(file, delim = "\\s\\2", col_names = FALSE)
    as_lines_list_2 <- unlist(results, recursive = FALSE)
    as_lines_list_2 <- read_results_flag(as_lines_list_2)
    row_numbs <- seq(1, length(as_lines_list_2), 1)
    as_lines_list_2 <- paste(as_lines_list_2, row_numbs, sep = "  ")
    return(as_lines_list_2)
  } else {
    stop("Please supply a valid .html, .hy3 or .pdf document")
  }
}


#' @rdname Read_Results
#' @export
read_results <- Read_Results


#' used to indicate that results have been read in with \code{read_results}
#' prior to being parsed by \code{swim_parse}
#'
#' Used to insure that \code{read_results} has been run on a data source prior
#' to running \code{swim_parse}
#'
#' @param x a list of results, line by line
#' @return returns list x, with "read_results_flag" added as the first element
#'   of the list


read_results_flag <- function(x){
  x <- c("read_results_flag", x)

}
