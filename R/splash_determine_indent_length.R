#' Determines indent length for data within \code{swim_parse_splash}
#'
#' In Splash results there are two line types that are of interest and don't
#' begin with either a place or a special string (DNS, DSQ etc.).  These are
#' ties and relays swimmers.  Relay swimmers are indented further than ties.
#' This function determines the number of spaces, called indent length, prior to
#' a tie row, plus a pad of four spaces.
#'
#' @importFrom stringr str_extract
#' @importFrom stringr str_length
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#'
#' @param x output from \code{read_results} followed by \code{add_row_numbers}
#' @return returns a number indicating the number of spaces preceding an
#'   athlete's name in a tie row
#'
#' @seealso \code{determine_indent_length_splash} runs inside of
#'   \code{swim_parse_splash}
#'

determine_indent_length_splash <- function(x, time_score_string){

Indent_Length <- x %>%
  .[purrr::map_lgl(.,
                   stringr::str_detect,
                   paste0(time_score_string, "|DSQ|SCR|DNS"))] %>%
  .[purrr::map_lgl(.,
                   stringr::str_detect,
                   "\n\\s+\\d\\.")] %>%
  head(1) %>%
  stringr::str_extract("(?<=\n)\\s+(?=\\d\\.)") %>%
  stringr::str_length() + 4

Indent_Length <- ifelse(is.na(Indent_Length), 12, Indent_Length)
Indent_Length <- ifelse(Indent_Length < 12, 12, Indent_Length)

return(Indent_Length)

}
