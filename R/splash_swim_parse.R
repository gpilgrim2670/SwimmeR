#' Formats Splash style swimming and diving data read with \code{read_results}
#' into a data frame
#'
#' Takes the output of \code{read_results} and cleans it, yielding a data frame
#' of swimming (and diving) results
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
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_length
#' @importFrom stringr str_sort
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @param file_splash output from \code{read_results}
#' @param avoid_splash a list of strings.  Rows in \code{file_splash} containing
#'   these strings will not be included. For example "Pool:", often used to
#'   label pool records, could be passed to \code{avoid_splash}.  The default is
#'   \code{avoid_default}, which contains many strings similar to "Pool:", such
#'   as "STATE:" and "Qual:".  Users can supply their own lists to
#'   \code{avoid_splash}. \code{avoid_splash} is handled before \code{typo_splash}
#'   and \code{replacement_splash}.
#' @param typo_splash a list of strings that are typos in the original results.
#'   \code{swim_parse} is particularly sensitive to accidental double spaces, so
#'   "Central  High School", with two spaces between "Central" and "High" is a
#'   problem, which can be fixed.  Pass "Central  High School" to
#'   \code{typo_splash}. Unexpected commas as also an issue, for example "Texas,
#'   University of" should be fixed using \code{typo_splash} and
#'   \code{replacement_splash}
#' @param replacement_splash a list of fixes for the strings in
#'   \code{typo_splash}.  Here one could pass "Central High School" (one space
#'   between "Central" and "High") and "Texas" to \code{replacement_splash} fix
#'   the issues described in \code{typo_splash}
#' @param format_results should the results be formatted for analysis (special
#'   strings like \code{"DQ"} replaced with \code{NA}, \code{Finals_Time} as
#'   definitive column)?  Default is \code{TRUE}
#' @param splits either \code{TRUE} or the default, \code{FALSE} - should
#'   \code{swim_parse} attempt to include splits.
#' @param split_length_splash either \code{25} or the default, \code{50}, the
#'   length of pool at which splits are recorded.  Not all results are
#'   internally consistent on this issue - some have races with splits by 50 and
#'   other races with splits by 25.
#' @param relay_swimmers_splash should names of relay swimmers be captured?
#'   Default is \code{FALSE}
#' @return returns a data frame with columns \code{Name}, \code{Place},
#'   \code{Age}, \code{Team}, \code{Prelims_Time}, \code{Finals_Time},
#'   \code{Points}, \code{Event} & \code{DQ}.  Note all swims will have a
#'   \code{Finals_Time}, even if that time was actually swam in the prelims
#'   (i.e. a swimmer did not qualify for finals).  This is so that final results
#'   for an event can be generated from just one column.
#'
#' @seealso \code{swim_parse_splash} must be run on the output of
#'   \code{\link{read_results}}

swim_parse_splash <-
  function(file_splash,
           avoid_splash = avoid,
           typo_splash = typo,
           replacement_splash = replacement,
           format_results = TRUE,
           splits = FALSE,
           split_length_splash = split_length,
           relay_swimmers_splash = relay_swimmers) {
    #### Testing ####
    # file_splash <-
    #   read_results(
    #     "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/India%20Swimming%20Federation/Glenmark_Senior_Nationals_2019.pdf"
    #   )
    # avoid_splash <- c("abcxyz")
    # typo_splash <- c("typo")
    # replacement_splash <- c("typo")
    # split_length_splash <- 50
    # relay_swimmers_splash <- TRUE
    # splits <- TRUE

    #### Begin Actual Function ####
    as_lines_list_2 <- file_splash %>%
      add_row_numbers() %>%
      .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid_splash)))] %>%
      stringr::str_replace_all(stats::setNames(replacement_splash, typo_splash))

    #### Pulls out event labels from text ####
    events <- event_parse(as_lines_list_2)

    #### set up strings ####
    Name_String <-
      "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
    Time_Score_String <- "\\d{0,2}\\:?\\d{1,3}\\.\\d{2}"
    Time_Score_Specials_String <-
      paste0("^NT$|^NP$|^DQ$|^DSQ$|^D?NS$|^SCR$|^x?X?",
             Time_Score_String,
             "x?X?$")
    Time_Score_Specials_String_Extract <-
      paste0(Time_Score_String, "|^NT$|^NP$|^DQ$|^NS$|^SCR$")
    Para_String <- "^SB?M?\\d{1,2}$"
    Reaction_String <-
      "^\\+\\s?\\d\\.\\d{3}$|^\\-\\s?\\d\\.\\d{3}$|^0\\.00$|^0\\.\\d\\d$"
    Record_String <- "^NMR"
    Header_string <- "\\sDisqualified\\s|\\sReaction\\sTime\\s"
    Heat_String <-
      "Heat\\s\\d{1,}\\sof\\s\\d{1,}|Semifinal\\s+\\d{1,}|Final|(Heats?)(?![:alpha:])"

    data_cleaned <- as_lines_list_2 %>%
      stringr::str_remove("^\n\\s{0,}") %>%
      .[purrr::map(., stringr::str_length) > 50] %>% # slight speed boost from cutting down length of file
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       paste0(Time_Score_String, "|DS?Q|SCR|D?NS"))] %>% # must have \\.\\d\\d because all swimming and diving times do
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       paste0(Record_String, "|Splash Meet Manager"),
                       negate = TRUE)] %>%
      .[purrr::map_lgl(., stringr::str_detect, "\\dm\\:", negate = TRUE)] %>%
      .[purrr::map_lgl(., stringr::str_detect, "^\\d+|^DSQ")] %>%
      stringr::str_replace_all("(?<=\\d\\.) (?=[:alpha:])", "  ") %>% # split places (1.) and names
      stringr::str_replace_all("(?<=\\d) (?=\\d)", "  ") %>% # split times and scores
      trimws()

    #### splits data into variables by splitting at multiple (>= 2) spaces ####
    data_cleaned <-
      unlist(purrr::map(data_cleaned, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    # unique(map(data_cleaned, length))

    #### breaks data into subsets based on how many variables it has ####
    # data_length_4 <- data_cleaned[purrr::map(data_cleaned, length) == 4]
    data_length_5 <- data_cleaned[purrr::map(data_cleaned, length) == 5]
    data_length_6 <- data_cleaned[purrr::map(data_cleaned, length) == 6]
    data_length_7 <- data_cleaned[purrr::map(data_cleaned, length) == 7]
    data_length_8 <- data_cleaned[purrr::map(data_cleaned, length) == 8]
    data_length_9 <- data_cleaned[purrr::map(data_cleaned, length) == 9]
    data_length_10 <- data_cleaned[purrr::map(data_cleaned, length) == 10]
    data_length_11 <- data_cleaned[purrr::map(data_cleaned, length) == 11]
    # data_length_12 <- data_cleaned[purrr::map(data_cleaned, length) == 12]
    # data_length_13 <- data_cleaned[purrr::map(data_cleaned, length) == 13]


    #### eleven variables ####
    if (length(data_length_11) > 0) {
      suppressWarnings(df_11 <- data_length_11 %>%
                         list_transform() %>%
                         dplyr::select(Place = V1,
                                Name = V2,
                                Age = V3,
                                Team = V4,
                                Finals_Time = V5,
                                Points = V6,
                                Row_Numb = V11))
    } else {
      df_11 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    #### ten variables ####
    if (length(data_length_10) > 0) {
      suppressWarnings(df_10 <- data_length_10 %>%
                         list_transform() %>%
                         dplyr::select(Place = V1,
                                Name = V2,
                                Team = V3,
                                Finals_Time = V4,
                                Points = V5,
                                Row_Numb = V10))
    } else {
      df_10 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### nine variables ####
    if (length(data_length_9) > 0) {
      suppressWarnings(df_9 <- data_length_9 %>%
                         list_transform() %>%
                         dplyr::select(Place = V1,
                                       Name = V2,
                                       Age = V3,
                                       Team = V4,
                                       Finals_Time = V5,
                                       Points = V6,
                                       Row_Numb = V9))
    } else {
      df_9 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### eight variables ####
    if (length(data_length_8) > 0) {
      suppressWarnings(df_8 <- data_length_8 %>%
                         list_transform() %>%
                         dplyr::select(Place = V1,
                                       Name = V2,
                                       Team = V3,
                                       Finals_Time = V4,
                                       Points = V5,
                                       Row_Numb = V8))
    } else {
      df_8 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### seven variables ####
    if (length(data_length_7) > 0) {
      suppressWarnings(df_7 <- data_length_7 %>%
                         list_transform() %>%
                         dplyr::mutate(Age = dplyr::case_when(stringr::str_detect(V3, "\\d\\d") == TRUE ~ V3,
                                                              TRUE ~ "Unknown")) %>%
                         dplyr::mutate(Team = dplyr::case_when(stringr::str_detect(V3, Age) == TRUE ~ V4,
                                                               stringr::str_detect(V3, "[:alpha:]") == TRUE &
                                                                 stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                                                               TRUE ~ "Unknown")) %>%
                         dplyr::mutate(
                           Finals_Time = dplyr::case_when(
                             str_detect(V4, Time_Score_Specials_String) == TRUE ~ V4,
                             stringr::str_detect(V4, Time_Score_Specials_String) == FALSE &
                               stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V5,
                             TRUE ~ "Unknown"
                           )
                         ) %>%
                         dplyr::mutate(
                           Points = dplyr::case_when(
                             stringr::str_detect(V5, "\\d+") == TRUE &
                               stringr::str_detect(V5, "\\.") == FALSE ~ V5,
                             stringr::str_detect(V6, "\\d+") == TRUE &
                               stringr::str_detect(V6, "\\.") == FALSE ~ V6,
                             TRUE ~ "Unknown"
                           )
                         ) %>%
                         select(
                           Place = V1,
                           Name = V2,
                           Age,
                           Team,
                           Finals_Time,
                           Points,
                           Row_Numb = V7
                         )
                       )
    } else {
      df_7 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### six variables ####
    if (length(data_length_6) > 0) {
      suppressWarnings(df_6 <- data_length_6 %>%
                         list_transform() %>%
                         dplyr::mutate(Age = dplyr::case_when(stringr::str_detect(V3, "\\d\\d") == TRUE ~ V3,
                                                              TRUE ~ "Unknown")) %>%
                         dplyr::mutate(Team = dplyr::case_when(stringr::str_detect(V3, Age) == TRUE ~ V4,
                                                               stringr::str_detect(V3, "[:alpha:]") == TRUE &
                                                                 stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                                                               TRUE ~ "Unknown")) %>%
                         dplyr::mutate(
                           Finals_Time = dplyr::case_when(
                             stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V4,
                             stringr::str_detect(V4, Time_Score_Specials_String) == FALSE &
                               stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V5,
                             TRUE ~ "Unknown"
                           )
                         ) %>%
                         dplyr::mutate(
                           Points = dplyr::case_when(
                             stringr::str_detect(V5, "\\d+") == TRUE &
                               stringr::str_detect(V5, "\\.") == FALSE ~ V5,
                             stringr::str_detect(V6, "\\d+") == TRUE &
                               stringr::str_detect(V6, "\\.") == FALSE ~ V6,
                             TRUE ~ "Unknown"
                           )
                         ) %>%
                         dplyr::select(
                           Place = V1,
                           Name = V2,
                           Age,
                           Team,
                           Finals_Time,
                           Points,
                           Row_Numb = V6
                         )
                         )
    } else {
      df_6 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### five variables ####
    if (length(data_length_5) > 0) {
      suppressWarnings(df_5 <- data_length_5 %>%
                         list_transform() %>%
                         dplyr::select(Place = V1,
                                       Team = V3,
                                       Finals_Time = V4,
                                       Row_Numb = V5))
    } else {
      df_5 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### Rejoin data frames from each number of variables ####
    Min_Row_Numb <- min(events$Event_Row_Min)
    suppressWarnings(
      data <- dplyr::bind_rows(df_11, df_10) %>%
        dplyr::bind_rows(df_9) %>%
        dplyr::bind_rows(df_8) %>%
        dplyr::bind_rows(df_7) %>%
        dplyr::bind_rows(df_6) %>%
        dplyr::bind_rows(df_5) %>%
        dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
        dplyr::arrange(Row_Numb)
    )


    #### Clean Up Data ####
    data <- data %>%
      dplyr::mutate(DQ = dplyr::case_when(stringr::str_detect(Place, "DSQ") == TRUE ~ 1,
                                          TRUE ~ 0)) %>%
      mutate(Place = str_remove(Place, "\\.")) %>%
      mutate(Place = dplyr::case_when(stringr::str_detect(Place, "DSQ") == TRUE ~ "Unknown",
                                      TRUE ~ Place))

    data <- data %>%
      dplyr::na_if("Unknown")

    #### add in events based on row number ranges ####
    if(min(data$Row_Numb) < min(events$Event_Row_Min)){
      unknown_event <- data.frame(Event = "Event Unknown",
                                  Event_Row_Min = min(data$Row_Numb),
                                  Event_Row_Max = min(events$Event_Row_Min) - 1)
      events <- dplyr::bind_rows(unknown_event, events)
    }

    data  <-
      transform(data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)]) %>%
      dplyr::na_if("Unknown")

    #### adding relay swimmers in ####
    # if (relay_swimmers_omega == TRUE) {
    #   relay_swimmers_df <- collect_relay_swimmers_omega(as_lines_list_2)
    #
    #   relay_swimmers_df <-
    #     transform(relay_swimmers_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
    #     dplyr::select(-Row_Numb)
    #
    #   data <- data %>%
    #     dplyr::left_join(relay_swimmers_df, c("Row_Numb" = "Row_Numb_Adjusted"))
    # }

    #### adding splits back in ####
    # if (splits == TRUE) {
    #   # split_length_hytek <- 50
    #   splits_df <- splits_parse(as_lines_list_2, split_len = split_length_hytek)
    #
    #   #### matches row numbers in splits_df to available row numbers in data
    #   # helps a lot with relays, since their row numbers vary based on whether or not relay swimmers are included
    #   # and if those swimmers are listed on one line or two
    #   splits_df  <-
    #     transform(splits_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
    #     dplyr::select(-Row_Numb)
    #
    #   data <- data %>%
    #     dplyr::left_join(splits_df, by = c("Row_Numb" = "Row_Numb_Adjusted")) %>%
    #     dplyr::select(!dplyr::starts_with("Split"), stringr::str_sort(names(.), numeric = TRUE)) # keep splits columns in order
    #
    # }

    ### remove empty columns (all values are NA) ###
    data <- Filter(function(x)
      !all(is.na(x)), data)

    #### if there is a Place column it should be first ####
    if("Place" %in% names(data)){
      data <- data %>%
        dplyr::select(Place, dplyr::everything())
    }

    data$Row_Numb <- NULL

    return(data)

  }

