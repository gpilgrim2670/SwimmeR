#' Formats Hytek style swimming and diving Top Times reports read with
#' \code{read_results} into a data frame
#'
#' Takes the output of \code{read_results} and cleans it, yielding a data frame
#' of swimming (and diving) top times
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lag
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_sort
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @param file_hytek_toptimes output from \code{read_results}
#' @param avoid_hytek_toptimes a list of strings.  Rows in
#'   \code{file_hytek_toptimes} containing these strings will not be included.
#'   For example "Pool:", often used to label pool records, could be passed to
#'   \code{avoid_hytek_toptimes}.  The default is \code{avoid_default}, which
#'   contains many strings similar to "Pool:", such as "STATE:" and "Qual:".
#'   Users can supply their own lists to \code{avoid_hytek_toptimes}.
#'   \code{avoid_hytek_toptimes} is handled before \code{typo_hytek_toptimes}
#'   and \code{replacement_hytek_toptimes}.
#' @param typo_hytek_toptimes a list of strings that are typos in the original
#'   results. \code{swim_parse} is particularly sensitive to accidental double
#'   spaces, so "Central  High School", with two spaces between "Central" and
#'   "High" is a problem, which can be fixed.  Pass "Central  High School" to
#'   \code{typo_hytek_toptimes}. Unexpected commas as also an issue, for example
#'   "Texas, University of" should be fixed using \code{typo_hytek_toptimes} and
#'   \code{replacement_hytek_toptimes}
#' @param replacement_hytek_toptimes a list of fixes for the strings in
#'   \code{typo_hytek}.  Here one could pass "Central High School" (one space
#'   between "Central" and "High") and "Texas" to
#'   \code{replacement_hytek_toptimes} fix the issues described in
#'   \code{typo_hytek_toptimes}
#' @return returns a data frame with columns \code{Rank}, \code{Result},
#'   \code{Name}, \code{Age}, \code{Date} \code{Meet} & \code{Event}.  Top Times
#'   reports do not designate Team.
#'
#' @seealso \code{toptimes_parse_hytek} must be run on the output of
#'   \code{\link{read_results}}

toptimes_parse_hytek <- function(file_hytek_toptimes,
                                 avoid_hytek_toptimes = avoid,
                                 typo_hytek_toptimes = typo,
                                 replacement_hytek_toptimes = replacement) {

  #### set up strings ####
  time_score_string <- "\\d{0,2}\\:?\\d{1,3}\\.\\d{2}"

  #### add row numbers ####
  file_hytek_toptimes <- file_hytek_toptimes %>%
    add_row_numbers()

  #### get event names ####
  events <- file_hytek_toptimes %>%
    event_parse()

  #### clean data ####
  data_cleaned <- file_hytek_toptimes %>%
    stringr::str_remove("^\n\\s{0,}") %>%
    .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid_hytek_toptimes)))] %>%
    stringr::str_replace_all(stats::setNames(replacement_hytek_toptimes, typo_hytek_toptimes)) %>% # replace typos with replacements
    .[stringr::str_detect(., time_score_string)] %>%
    stringr::str_replace_all("(?<=\\d)\\s+[:upper:]?\\s+(?=[:alpha:])", "  ") %>%
    stringr::str_replace_all("(?<=\\d)\\s+[:upper:]?\\s+(?=[:alpha:])", "  ") %>%
    stringr::str_replace_all("(?<=\\d)\\s+[:upper:]?\\s+(?=[:alpha:])", "  ") %>%
    stringr::str_replace_all("(?<=\\s)x(?=\\d)", "  ") %>%
    stringr::str_replace_all("(?<=\\d)\\s(?=[:alpha:])", "  ")

  data_cleaned <-
    unlist(purrr::map(data_cleaned, stringr::str_split, "\\s{2,}"),
           recursive = FALSE)

  # unique(map(data_cleaned, length))

  #### break out data lists by length ####
  data_length_6 <- list_breaker(data_cleaned, len = 6)
  data_length_7 <- list_breaker(data_cleaned, len = 7)

  #### data length 7 ####

  if (length(data_length_7) > 0) {
  df_7 <- data_length_7 %>%
    list_transform() %>%
    dplyr::select(
      Rank = V1,
      Result = V2,
      Name = V3,
      Age = V4,
      Date = V5,
      Meet = V6,
      Row_Numb = V7
    )
  } else {
    df_7 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  #### data length 6 ####

  if (length(data_length_6) > 0) {
  df_6 <- data_length_6 %>%
    list_transform() %>%
    dplyr::select(
      Rank = V1,
      Result = V2,
      Name = V3,
      Date = V4,
      Meet = V5,
      Row_Numb = V6
    )
  } else {
    df_6 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  #### recombine data parts ####
  data <- dplyr::bind_rows(df_7, df_6) %>%
    dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
    dplyr::arrange(Row_Numb)

  if(nrow(data) > 0){

  #### add events back in ####
  Min_Row_Numb <- min(events$Event_Row_Min)

  if (min(data$Row_Numb) < min(events$Event_Row_Min)) {
    unknown_event <- data.frame(
      Event = "Unknown",
      Event_Row_Min = min(data$Row_Numb),
      Event_Row_Max = min(events$Event_Row_Min) - 1
    )
    events <- dplyr::bind_rows(unknown_event, events)
  }

  data  <-
    transform(data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)]) %>%
    na_if_character("Unknown")

  #### Rank column to numeric ####
  if ("Rank" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(Rank = as.numeric(Rank))
  }

  #### final clean ####
  data <- data %>%
    dplyr::select(-Row_Numb)

  } else {
    warning("No Top Times returned.  Please check data source.")
  }

  return(data)
}


# RIT_IC <- "https://s3.amazonaws.com/sidearm.sites/bombers.ithaca.edu/documents/2021/11/20/11_20_21_RIT_IC_Results.pdf" %>%
#   read_results() %>%
#   swim_parse()
#
# IC <- RIT_IC %>%
#   filter(Team == "Ithaca College-NI") %>%
#   filter(str_detect(Event, "Div") == FALSE) %>%
#   filter(str_detect(Event, "Relay") == FALSE) %>%
#   mutate(Event = str_remove(Event, " Yard"),
#          Event = str_remove(Event, "style"),
#          Event = str_remove(Event, "stroke"),
#          Event = str_replace(Event, "Butterfly", "Fly"))


