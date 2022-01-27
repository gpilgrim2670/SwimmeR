#' Cleans input strings
#'
#' Cleans input from \code{read_results} is passed to \code{hytek_swim_parse}
#' to remove unnneded characters and otherwise set it up for sorting.  Input is
#' in the form of character strings
#'
#' @importFrom stringr str_remove
#' @importFrom stringr str_length
#' @importFrom stringr str_count
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#'
#' @param x a list of character strings
#' @param time_score_string a regex string for matching results (times and
#'   scores) but not special strings like DQ
#' @return returns a list of character strings that have been cleaned in
#'   preparation for parsing/sorting
#'
#'   #' @seealso \code{hytek_clean_strings} runs inside of
#'   \code{hytek_parse_splash}

hytek_clean_strings <- function(x,
                                time_score_string = Time_Score_String){

  #### check input types ####

  if(is.character(time_score_string) == FALSE){
    stop("time_score_string should be regex, as a character string")
  }

  #### actual function ####

  data_cleaned <- x %>%
    stringr::str_remove("^\n\\s{0,}") %>%
    .[stringr::str_length(.) > 50] %>% # slight speed boost from cutting down length of file
    .[stringr::str_detect(., paste0(time_score_string, "|DQ|SCR|DFS"))] %>% # must have \\.\\d\\d because all swimming and diving times do
    .[stringr::str_detect(., "[:alpha:]{2,}.*[:alpha:]")] %>% # need some letters, need them to not just be a single instance of DQ etc.
    .[stringr::str_detect(., "r\\:\\+?\\-?\\s?\\d", negate = TRUE)] %>% # remove reaction times
    .[stringr::str_detect(., "^50m", negate = TRUE)] %>%  # remove British results that start with 50m for splits lines
    .[stringr::str_count(., "\\d\\)") < 2] %>%  # remove inline splits from older style hy-tek results circa 2005
    stringr::str_replace_all("\\s?[&%]\\s?", " ") %>% # added 8/21 for removing "&" and "%" as record designator
    stringr::str_remove_all("(?<=\\d\\.\\d{2}\\s?)[:punct:]") %>% # remove symbols attached to times as record designator
    stringr::str_remove_all("(?<=\\.\\d{2}\\s?)[A-WYZ|\\$|q](?=\\s)") %>% # remove letters attached to times as record designator
    stringr::str_replace_all(" [qQ](?=\\d{1,5} )", "   ") %>% # remove q|Q attached to points
    # removed J etc. from next to swim, but does not remove X or x (for exhibition tracking)
    stringr::str_replace_all("[A-WYZa-wyz]+(\\d{1,2}\\:\\d{2}\\.\\d{2})", "\\1") %>%
    stringr::str_replace_all("(\\d{1,2}\\:\\d{2}\\.\\d{2})[A-WYZa-wyz]+", "\\1") %>%
    stringr::str_replace_all("[A-WYZa-wyz]+(\\d{2,3}\\.\\d{2})", "\\1") %>%
    stringr::str_replace_all("(\\d{2,3}\\.\\d{2})[A-WYZa-wyz]+", "\\1") %>%
    stringr::str_replace_all(" [:punct:]+(\\d{1,2}\\:\\d{2}\\.\\d{2})", " \\1") %>%
    stringr::str_replace_all("(\\d{1,2}\\:\\d{2}\\.\\d{2})[:punct:]+", " \\1") %>%
    stringr::str_replace_all(" [:punct:]+(\\d{2,3}\\.\\d{2})", " \\1") %>%
    stringr::str_replace_all("(\\d{2,3}\\.\\d{2})[:punct:]+", "\\1 ") %>%
    stringr::str_remove_all("\\s{2}J\\s{2}") %>%
    # remove 'A', 'B' etc. relay designators - should this go in typo instead?
    stringr::str_replace_all(" \\'[A-Z]\\' ", "  ") %>%
    stringr::str_replace_all("  [A-WYZ]  ", "  ") %>%
    stringr::str_replace_all("\\'\\'", "  ") %>%
    # remove meet ID from old result
    stringr::str_replace_all(" \\d{4} ", "   ") %>%
    stringr::str_replace_all(" \\d{1,3} (?=\\s*\\d{1,2}\\.?\\d?\\d?\\s+\\d{1,5}$)", "   ") %>% # remove column of powerpoint values in NYS Boys 2007.  It goes time, powerpoint, actual points, row numb
    stringr::str_replace_all("(?<=\\.\\d)(\\d)\\s+X(?=\\s+\\d{1,5}$)", "\\1X") %>% # brings Xs for exhibition that are spaced out in closer
    # remove q from next to time 10/21/2020
    stringr::str_remove_all(" q ") %>% # removes " q " sometimes used to designate a qualifying time
    stringr::str_replace_all("-{2,5}", "10000") %>% #8/26
    stringr::str_replace_all("(\\.\\d{2})\\d+", "\\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
    stringr::str_replace_all("\\d{1,2} (\\d{1,})$", "  \\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
    stringr::str_replace_all("\\*(?=[:alpha:])", "_") %>% # for * prior to name for foreign athletes
    stringr::str_replace_all("\\*", "  ") %>%
    stringr::str_replace_all("(?<=\\d)\\.\\s{1,}(?=[:alpha:])", "  ") %>% # for British results where places are 1. Name
    stringr::str_replace_all("(?<=\\s)\\+\\s(?=[:digit:])", "  +") %>% # for reaction times
    stringr::str_replace_all("(?<=\\s)\\-\\s(?=[:digit:])", "  -") %>% # for reaction times
    trimws()


#### insert double spaces where needed ####
data_cleaned <- data_cleaned %>%
  stringr::str_replace_all("(?<=\\d)\\s+[:upper:]?\\s+(?=\\d)", "  ") %>% # letters like P or M to denote pool or meet record
  stringr::str_replace_all("(?<=^\\s?\\d{1,5}) (?=[:alpha:])", "  ") %>% # mostly to split place and name
  stringr::str_replace_all("(?<=SB?M?\\d{1,2}) (?=[:alpha:])", "  ") %>% # split para classifications 1
  stringr::str_replace_all("(?<=[:alpha:]) (?=SB?M?\\d{1,2})", "  ") %>% # split para classifications 2
  stringr::str_replace_all("(?<=\\d) (?=_)", "  ") %>% # mostly to split place and name, if name is preceded by "_" as a stand-in for "*"
  stringr::str_replace_all("(?<=\\d) (?=\\d)", "  ") %>% # mostly to split place team names that start with a number, like in NYS results (5-Fairport etc.)
  stringr::str_replace_all("(?<=[:alpha:]),(?=[:alpha:])", ", ") %>% # split names that don't have a space between last,first
  stringr::str_replace_all("(?<=[:alpha:])\\. (?=[:digit:])", "\\.  ") %>% # split abbreviated team names like Southern Cal. and times
  stringr::str_replace_all("(?<=\\d) (?=_)", "  ") %>% # spacing between place and athletes with */_ leading name
  stringr::str_replace_all("(?<=\\)) (?=[:alpha:])", "  ") %>% # spacing between place) and names
  stringr::str_replace_all("\\((?=[:digit:])", "  \\(") %>% # spacing between (YoB) and name, for British results
  stringr::str_replace_all(" (\\d{1,3}) ", "       \\1      ") %>% # split age and team
  stringr::str_replace_all(" FR ", "       FR      ") %>% # split age and team
  stringr::str_replace_all(" SO ", "       SO      ") %>% # split age and team
  stringr::str_replace_all(" JR ", "       JR      ") %>% # split age and team
  stringr::str_replace_all(" SR ", "       SR      ") %>% # split age and team
  stringr::str_replace_all("(SM?B?1\\d{1})(\\d{1,2})", "\\1   \\2") %>%  # split para classification and age
  stringr::str_replace_all("(\\d{6,7})([:alpha:])", "\\1   \\2") %>%  # split Brit ID and Name
  stringr::str_replace_all(" NT ", "       NT      ") %>% # split prelim and final
  stringr::str_replace_all(" DQ ", "       DQ      ") %>% # split prelim and final
  stringr::str_replace_all(" DFS ", "       DFS      ") %>% # split prelim and final
  stringr::str_replace_all(" SCR ", "       SCR      ") %>% # split prelim and final
  stringr::str_replace_all("(?<=[:alpha:])\\s{2,3}(?=[:alpha:])", " ") %>% # testing 12/21/2020 would help with typos
  stringr::str_replace_all("(?<=[:alpha:]) (?=\\d)", "  ") %>% # split name and age
  stringr::str_replace_all("(?<=[:alpha:])(\\d{1,3}\\-\\d{2})", "  \\1  ") %>% # split name and yyy-mm age
  stringr::str_replace_all("(?<=\\,) (?=\\d)", "  ") %>% # split name and age if name is so long that it ends with a ","
  stringr::str_replace_all("(?<=\\d) (?=\\d{1,}$)", "  ") %>%  # split off row_numb
  stringr::str_replace_all(
    "(?<=\\.\\d\\d\\s{1,10}\\d?\\d?\\:?\\d?\\d\\.\\d\\d)\\s{1,10}[:alpha:]{1,5}\\d?\\s{1,10}(?=\\d{1,})",
    "  "
  ) %>%  # removes "AAC" or "AAA" or "NYS" or "SEC1" etc. from after finals time
  # stringr::str_replace_all("NYS|AAA|AAC|SEC\\d{1,2}", "  ") %>%
  # stringr::str_replace_all("SEC\\d{1,2}", "  ") %>%  # for old NYS results
  stringr::str_replace_all(
    "\\d{3}\\.\\d{2}\\s+(\\d{3}\\.\\d{2})\\s+\\d{3}\\.\\d{2}\\s+(\\d{3}\\.\\d{2})",
    "\\1  \\2"
  ) %>%  # for old NCAA results with diving scores and DDs
  stringr::str_replace_all("(?<=\\s)S(?=B?M?\\d{1,2})", "  S") %>%  # for para classifications
  stringr::str_remove_all("(?<=\\d\\d\\.\\d\\d )[:upper:]{1,}") # removes AAA, AAC from after time

return(data_cleaned)

}
