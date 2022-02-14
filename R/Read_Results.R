#' Reads swimming and diving results into a list of strings in preparation for
#' parsing with \code{swim_parse}
#'
#' Outputs list of strings to be processed by \code{swim_parse}
#'
#' @importFrom stringr str_detect
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom pdftools pdf_text
#' @importFrom readr read_delim
#'
#' @param file a pdf, url or Hytek .hy3 file containing swimming
#'   results.  Must be formatted in a "normal" fashion - see vignette
#' @param node a CSS node where html results are stored.  Required for html
#'   results.  Default is "pre", which nearly always works.
#' @return returns a list of strings containing the information from
#'   \code{file}.  Should then be parsed with \code{swim_parse}
#'
#' @examples \dontrun{
#' link <-
#'   "http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm", node = "pre"
#' read_results(link)}
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

    results <- read_pdf(file)

    if (all("error" %in% class(results),
            stringr::str_detect(file, "\\.aspx$")) == TRUE) {
      results <- file %>%
        read_htm(node_helper = node)
    }

  } else if (stringr::str_detect(file, "\\.htm|\\.aspx$") == TRUE) {
    ### HTML ###
    results <- file %>%
      read_htm(node_helper = node)
  } else if (stringr::str_detect(file, "\\.hy3") == TRUE) {
    ### hy3 ###
    ### add automatic unzipping?
    results <- file %>%
      read_hy3()

  }

  if ("error" %in% class(results) == TRUE) {
    stop("Please supply a valid .html, .hy3 or .pdf document")
  }


  return(results)
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

#' Read in pdf files of swimming results
#'
#' Based on pdftools, this function can be temperamental
#'
#' @param x a .pdf or .aspx location containing swimming results.  Must be
#'   formatted in a "normal" fashion - see vignette
#' @return returns a list of results, with "read_results_flag" added as the
#'   first element of the list
#'

read_pdf <- function(x){
  suppressMessages(
    results <-
      tryCatch(
        pdftools::pdf_text(x),
        error = function(e)
          e
      )
  )

  if ("error" %in% class(results) == FALSE) {
    results <- stringr::str_extract_all(results, "\n.*")
    results <- unlist(results, recursive = FALSE)
    results <- read_results_flag(results)
  }

  return(results)
}

#' Read in html files of swimming results
#'
#' @param x an .html, .htm or .aspx location containing swimming results.  Must be
#'   formatted in a "normal" fashion - see vignette
#' @param node_helper receives \code{node} from \code{read_results}
#' @return returns a list of results, with "read_results_flag" added as the
#'   first element of the list
#'

read_htm <- function(x, node_helper) {
  if (is.character(node_helper) == FALSE) {
    stop(" Please supply a value for node")
  }

  webpage <- xml2::read_html(x)
  html <- rvest::html_nodes(webpage, node_helper)
  results <- rvest::html_text(html)
  results <- stringr::str_extract_all(results, "\n.*")
  results <- unlist(results, recursive = FALSE)
  results <- read_results_flag(results)

  return(results)

}


#' Read in hy3 files of swimming results
#'
#' @param x an unzipped hy3 file containing swimming results.  Must be
#'   formatted in a "normal" fashion - see vignette
#' @return returns a list of results, with "read_results_flag" added as the
#'   first element of the list
#'

read_hy3 <- function(x) {
  results <-
    readr::read_delim(file, delim = "\\s\\2", col_names = FALSE)
  results <- unlist(results, recursive = FALSE)
  results <- read_results_flag(results)
  row_numbs <- seq(1, length(results), 1)
  results <-
    paste(results, row_numbs, sep = "  ")

  return(results)

}
