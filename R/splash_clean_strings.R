#' Cleans input strings
#'
#' Cleans input from \code{read_results} is passed to \code{splash_swim_parse}
#' to remove unnneded characters and otherwise set it up for sorting.  Input is
#' in the form of character strings
#'
#' @importFrom stringr str_remove
#' @importFrom stringr str_length
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#'
#' @param x a list of character strings
#' @param indent_length a numeric value denoting the number of spaces some
#'   results are indented by.  \code{indent_length} is determined by
#'   \code{splash_determine_indent_length}. Must be a whole number.
#' @param time_score_string a regex string for matching results (times and
#'   scores) but not special strings like DQ
#' @param record_string a regex string for matching denoted records, rather than
#'   results
#' @param header_string a regex string from matching splash headers/footers
#'   included in result documents
#' @param sponsorship_string a regex string for matching sponsorship text within
#'   result documents
#' @param reaction_string a regex string for matching reaction times
#' @param rule_string a regex string for matching rule text e.g. 'Rule 4.24'
#'   that sometimes accompanies DQs
#' @return returns a list of character strings that have been cleaned in
#'   preparation for parsing/sorting
#'
#'   #' @seealso \code{splash_clean_strings} runs inside of
#'   \code{swim_parse_splash}

splash_clean_strings <-
  function(x,
           indent_length = Indent_Length,
           time_score_string = Time_Score_String,
           record_string = Record_String,
           header_string = Header_String,
           sponsorship_string = Sponsorship_String,
           reaction_string = Reaction_String,
           rule_string = Rule_String) {

    if (all.equal(indent_length, as.integer(indent_length)) == FALSE) {
      stop("indent_length must be a whole number")
    }

    data_cleaned <- x %>%
      stringr::str_remove("^\n") %>%
      .[stringr::str_detect(.,
                            paste0("^\\s{", indent_length, ",}"),
                            negate = TRUE)] %>% # removes relay swimmer rows
      stringr::str_remove("^\\s{0,}") %>%
      .[stringr::str_length(.) > 50] %>% # slight speed boost from cutting down length of file
      .[stringr::str_detect(.,
                            paste0(time_score_string, "|DSQ|SCR|DNS"))] %>% # must have \\.\\d\\d because all swimming and diving times do
      .[stringr::str_detect(.,
                            paste0(record_string, "|Splash Meet Manager"),
                            negate = TRUE)] %>%
      .[stringr::str_detect(.,
                            header_string,
                            negate = TRUE)] %>%
      .[stringr::str_detect(.,
                            sponsorship_string,
                            negate = TRUE)] %>%
      .[stringr::str_detect(., "\\dm\\:", negate = TRUE)] %>% # removes split lines
      .[stringr::str_detect(., "\\d\\.\\d{2}\\s+[[:alpha:]\\'\\.]{2,}", negate = TRUE)] %>% # removes relay swimmer rows
      .[stringr::str_detect(., reaction_string, negate = TRUE)] %>% # also removes relay swimmer rows
      .[stringr::str_detect(., rule_string, negate = TRUE)] %>% # also removes rows with rule numbers for DQ reasons
      stringr::str_replace_all("(?<=\\d\\.) (?=[:alpha:])", "  ") %>% # split places (1.) and names
      stringr::str_replace_all("(?<=^DNS)(?=[:alpha:])", "  ") %>% # split DNS and names
      stringr::str_replace_all("(?<=^DSQ)(?=[:alpha:])", "  ") %>% # split DNS and names
      stringr::str_replace_all("(?<=\\d) (?=\\d)", "  ") %>% # split times and scores
      stringr::str_replace_all("(?<=[:alpha:]\\.) (?=\\d\\d)", "  ") %>% # split names ending in "." and ages
      stringr::str_replace_all("(?<=[:alpha:]) (?=\\d)", "  ") %>% # split names and ages
      stringr::str_replace_all(" \\? ", "  ") %>% # remove ? as label
      stringr::str_replace_all(" \\* ", "  ") %>%
      stringr::str_replace_all("(?<=\\d)\\s+[:upper:]R?\\*?\\s", "  ") %>% # remove Q, R etc. as label
      stringr::str_replace_all("(?<=\\d)[:upper:]R?\\*?\\s", "  ") %>% # remove Q, R etc. as label
      stringr::str_replace_all("(?<=\\d)[:upper:]{1,2}[:lower:]{0,2}\\.?\\*?\\s", "  ") %>% # remove Q, R etc. as label
      stringr::str_replace_all("(?<=\\d)\\*[:alpha:]{0,4}\\.?\\s", "  ") %>% # remove * as label
      stringr::str_replace_all(" q ", "  ") %>% # remove Q, R etc. as label
      stringr::str_replace_all("(?<=\\d)\\.(?=[:alpha:])", "\\.   ") %>%
      stringr::str_replace_all("  ([:upper:]{2,3})\\s\\s+([:alpha:]{2,}\\s?[:alpha:]{0,})",
                               " \\1-\\2   ") %>% # merge team and country names
      trimws()

    data_cleaned <- data_cleaned %>%
      stringr::str_replace("^DNS", "888\\.  DNS") %>%  # splash for dealing with ties, DQS etc.
      stringr::str_replace("^DFS", "888\\.  DFS") %>%  # splash for dealing with ties
      stringr::str_replace("^DSQ", "888\\.  DSQ") %>%  # splash for dealing with ties
      stringr::str_replace("^([^[0-9]])", "999\\.  \\1") %>%   # splash for dealing with ties
      stringr::str_replace(" \\/ ", "/") %>%   # splash for dealing with Heat/Lane columns
      stringr::str_replace_all("([:alpha:]\\.?\\:?\\s?)(\\d{1,2}[\\:|\\.])", "\\1  \\2") %>%  # splits teams and times
      stringr::str_remove("^888\\.  ") %>%  # want to keep DSQ, DNS in first column, but need to move ties over one column
      stringr::str_replace("(?<=[:alpha:])\\s{1,}\\d{1,4}\\s{0,}(?=\\s{2}\\d{1,2}(\\:|\\.))",
                           "   ") %>%  # remove numbers floating off of team names
      stringr::str_replace("(?<=[:alpha:]\\s{1,4})\\d{1,4}\\s{0,}(?=\\s{2}\\d{1,5}$)",
                           "   ") %>%  # remove numbers floating off of team names
      stringr::str_replace("(?<=[:alpha:])\\s{1,2}\\d{4}\\s{1,2}(?=[:alpha:])", " ") %>%
      stringr::str_replace_all("(\\s{2}\\d{2}\\s)(?=[:alpha:])", "\\1  ") %>%  # splits ages and teams
      stringr::str_replace_all("(?<=[:alpha:])(?=\\d{1,3})", "  ") %>%  # splits ages and teams
      stringr::str_replace_all("(\\.\\d{2}\\s)\\s(?=\\d{3})", "\\1  ") %>%  # splits times and scores
      stringr::str_replace_all("(\\d{2,3})\\s(?=[\\+|\\-]\\d\\.\\d{2})", "  ") %>%  # splits reaction times and scores
      stringr::str_replace("DNS ", "DNS  ") %>%
      stringr::str_replace("DFS ", "DFS  ") %>%
      stringr::str_replace("DSQ ", "DSQ  ") %>%
      stringr::str_replace_all("1950 e.V:", "  ")  # bug fix for 2018 Euros

    return(data_cleaned)
  }
