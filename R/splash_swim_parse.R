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
#'   strings like \code{"DQ"} replaced with \code{NA}, \code{Finals} as
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
#'   \code{Age}, \code{Team}, \code{Prelims}, \code{Finals},
#'   \code{Points}, \code{Event} & \code{DQ}.  Note all swims will have a
#'   \code{Finals}, even if that time was actually swam in the prelims
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
    #     "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Glenmark_Senior_Nationals_2019.pdf"
    #   )
    # file_splash <-
    #   "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Khelo_India_Youth_Games_2020.pdf" %>%
    #   read_results()
    # file_splash <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/European_Jr_Champs_2012.pdf" %>%
    #   read_results()
    # file_splash <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Arena_European_Junior_Swimming_Champs_2013.pdf" %>%
    #   read_results()
    # file_splash <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Open_Belgian_Champs_2017.pdf" %>%
    #   read_results()
    # file_splash <- "http://www.toptime.be/oresults/ResultList_111_us.pdf" %>%
    #   read_results()
    # file_splash <- "http://www.toptime.be/oresults/ResultList_75_us.pdf" %>%
    #   read_results()
    # file_splash <- "https://eoz.in.th/eozlive/ResultList_101.pdf" %>%
    #   read_results()
    # file_splash <- "http://eliteteamenergystandard.org/images/EURO_MEET_2018.pdf" %>%
    #   read_results()
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
    Time_Score_String <- "1?\\:?\\d{0,2}\\:?\\d{1,3}\\.\\d{2}"
    Time_Score_Specials_String <-
      paste0("^NT$|^NP$|^DQ$|^DSQ$|^D?NS$|^SCR$|^x?X?",
             Time_Score_String,
             "x?X?$")
    Time_Score_Specials_String_Extract <-
      paste0(Time_Score_String, "|^NT$|^NP$|^DQ$|^NS$|^SCR$")
    Para_String <- "^SB?M?\\d{1,2}$"
    Reaction_String <-
      "^\\+\\s?\\d\\.\\d{3}$|^\\-\\s?\\d\\.\\d{3}$|^0\\.00$|^0\\.\\d\\d$"
    Record_String <- "^NMR|^E J C|^W J C|^Open [W|M] |[:alpha:] Record "
    Rule_String <- "SW \\d\\d?\\.\\d\\d?"
    Header_String <- "\\sDisqualified\\s|\\sReaction\\sTime\\s|Prelims|Finals|Semifinal"
    Heat_String <-
      "Heat\\s\\d{1,}\\sof\\s\\d{1,}|Semifinal\\s+\\d{1,}|Final|(Heats?)(?![:alpha:])"
    Heat_Lane_String <- "\\d{1,}\\/\\d{1,}"
    Sponsorship_String <- "sponsored by"

    # Indent Length is used to differentiate ties (whose lines don't start with a place/DQ) and relay swimmers
    # whose lines also don't start with a place/DQ.  Relay swimmers are indented further, but overall indents
    # vary from results to results
    Indent_Length <- as_lines_list_2 %>%
      splash_determine_indent_length(time_score_string = Time_Score_String)

    data_cleaned <- splash_clean_strings(as_lines_list_2,
                                         indent_length = Indent_Length,
                                         time_score_string = Time_Score_String,
                                         record_string = Record_String,
                                         header_string = Header_String,
                                         sponsorship_string = Sponsorship_String,
                                         reaction_string = Reaction_String,
                                         rule_string = Rule_String)

    #### if data_cleaned is empty ####
    if(!length(data_cleaned) > 0){
      message("No results found in file")

    } else {

    #### splits data into variables by splitting at multiple (>= 2) spaces ####
    data_cleaned <-
      unlist(purrr::map(data_cleaned, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    # unique(map(data_cleaned, length))

    #### breaks data into subsets based on how many variables it has ####

    data_length_4 <- list_breaker(data_cleaned, len = 4)
    data_length_5 <- list_breaker(data_cleaned, len = 5)
    data_length_6 <- list_breaker(data_cleaned, len = 6)
    data_length_7 <- list_breaker(data_cleaned, len = 7)
    data_length_8 <- list_breaker(data_cleaned, len = 8)
    data_length_9 <- list_breaker(data_cleaned, len = 9)
    data_length_10 <- list_breaker(data_cleaned, len = 10)
    data_length_11 <- list_breaker(data_cleaned, len = 11)
    data_length_12 <- list_breaker(data_cleaned, len = 12)

    #### twelve variables ####

    df_12 <- splash_length_12_sort(data_length_12)

    #### eleven variables ####
    df_11 <- splash_length_11_sort(data_length_11,
                                   time_score_specials_string = Time_Score_Specials_String)
    #### ten variables ####
    df_10 <- splash_length_10_sort(
      data_length_10,
      time_score_string = Time_Score_String,
      time_score_specials_string = Time_Score_Specials_String
    )

    #### nine variables ####

    df_9 <- splash_length_9_sort(
      data_length_9,
      heat_lane_string = Heat_Lane_String,
      time_score_string = Time_Score_String,
      time_score_specials_string = Time_Score_Specials_String
    )

    #### eight variables ####

    df_8 <- splash_length_8_sort(
      data_length_8,
      time_score_string = Time_Score_String,
      time_score_specials_string = Time_Score_Specials_String
    )

    #### seven variables ####

    df_7 <- splash_length_7_sort(
      data_length_7,
      time_score_string = Time_Score_String,
      time_score_specials_string = Time_Score_Specials_String
    )

    #### six variables ####

    df_6 <- splash_length_6_sort(
      data_length_6,
      time_score_specials_string = Time_Score_Specials_String
    )

    #### five variables ####

    df_5 <- splash_length_5_sort(
      data_length_5,
      name_string = Name_String,
      time_score_specials_string = Time_Score_Specials_String
    )

    #### four variables ####

    df_4 <- splash_length_4_sort(
      data_length_4,
      name_string = Name_String,
      time_score_specials_string = Time_Score_Specials_String
    )

    #### Rejoin data frames from each number of variables ####
    Min_Row_Numb <- min(events$Event_Row_Min)
    suppressWarnings(
      data <- dplyr::bind_rows(df_12, df_11) %>%
        dplyr::bind_rows(df_10) %>%
        dplyr::bind_rows(df_9) %>%
        dplyr::bind_rows(df_8) %>%
        dplyr::bind_rows(df_7) %>%
        dplyr::bind_rows(df_6) %>%
        dplyr::bind_rows(df_5) %>%
        dplyr::bind_rows(df_4) %>%
        dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
        dplyr::arrange(Row_Numb)
    )

    if("Prelims" %in% names(data) == FALSE){
      data$Prelims <- "Unknown"
    }


    #### Clean Up Data ####
    data <- data %>%
      dplyr::mutate(DQ = dplyr::case_when(stringr::str_detect(Place, "DSQ|DNS|DFS") == TRUE ~ 1,
                                          TRUE ~ 0)) %>%
      dplyr::mutate(Place = str_remove(Place, "\\.")) %>%
      dplyr::mutate(Age = dplyr::case_when(Age == "Unknown" &
                                             stringr::str_detect(Team, "/\\d\\d$") == TRUE ~ stringr::str_extract(Team, "\\d\\d$"),
                                           TRUE ~ Age)) %>%
      dplyr::mutate(Team = stringr::str_remove(Team, "/\\d{1,}/\\d{1,}$")) %>%
      dplyr::mutate(Prelims = dplyr::case_when(stringr::str_detect(Place, "DNS") == TRUE &
                                                      stringr::str_detect(Finals, Time_Score_String) == TRUE &
                                                      is.na(Prelims) == TRUE ~ Finals,
                                                    TRUE ~ Prelims)) %>%
      dplyr::mutate(Finals = dplyr::case_when(stringr::str_detect(Place, "DNS") == TRUE &
                                                      stringr::str_detect(Finals, Prelims) == TRUE ~ "Unknown",
                                                    TRUE ~ Finals)) %>%
      dplyr::mutate(Place = dplyr::case_when(stringr::str_detect(Place, "DSQ|DNS|DFS") == TRUE ~ "Unknown",
                                      TRUE ~ Place)) %>%
      dplyr::mutate(Place = dplyr::case_when(stringr::str_detect(Place, "999") == TRUE ~ dplyr::lag(Place),
                                      TRUE ~ Place)) %>%
      # run twice in case of three way tie
      dplyr::mutate(Place = dplyr::case_when(stringr::str_detect(Place, "999") == TRUE ~ dplyr::lag(Place),
                                             TRUE ~ Place))

    data <- data %>%
      na_if_character("Unknown") %>%
      dplyr::mutate(Finals = stringr::str_remove(Finals, "[:alpha:]{1,}")) %>%
      dplyr::mutate(Finals = stringr::str_remove(Finals, "\\?|\\*"))

    #### add in events based on row number ranges ####
    if(min(data$Row_Numb) < min(events$Event_Row_Min)){
      unknown_event <- data.frame(Event = "Event Unknown",
                                  Event_Row_Min = min(data$Row_Numb),
                                  Event_Row_Max = min(events$Event_Row_Min) - 1)
      events <- dplyr::bind_rows(unknown_event, events)
    }

    data  <-
      transform(data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)])

    data <- data %>%
      mutate(Name = case_when(Team == Name &
                                stringr::str_detect(Event, "elay|\\sx\\s") == TRUE ~ "Unknown",
                              TRUE ~ Name)) %>%
      mutate(Team = case_when(Team == Name &
                                stringr::str_detect(Event, "elay|\\sx\\s") == FALSE ~ "Unknown",
                              TRUE ~ Team)) %>%
      na_if_character("Unknown")

    #### adding relay swimmers in ####
    if (relay_swimmers_splash == TRUE) {
      relay_swimmers_df <- collect_relay_swimmers_splash(as_lines_list_2, relay_indent = Indent_Length)

      relay_swimmers_df <-
        transform(relay_swimmers_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
        dplyr::select(-Row_Numb)

      data <- data %>%
        dplyr::left_join(relay_swimmers_df, c("Row_Numb" = "Row_Numb_Adjusted"))
    }

    #### adding splits back in ####
    if (splits == TRUE) {

      data <- data %>%
        dplyr::mutate(dplyr::across(where(is.numeric), as.character))

      #### rename existing splits columns ####
      if(any(stringr::str_detect(names(data), "Split")) == TRUE){
        old_names <- names(data)[grep("^Split", names(data))]
        new_names <-
          paste("Split", seq(1, length(old_names)) * split_length_splash, sep = "_")

        data <- data %>%
          dplyr::rename_at(dplyr::vars(dplyr::all_of(old_names)), ~ new_names)
      }

      data_ind <- data %>%
        dplyr::filter(stringr::str_detect(Event, "(R|r)elay|\\sx\\s|\\dx\\d") == FALSE)

      data_relay <- data %>%
        dplyr::filter(stringr::str_detect(Event, "(R|r)elay|\\sx\\s|\\dx\\d") == TRUE)

      if(nrow(data_ind) > 0) {

        # split_length <- 50
        splits_df <-
          splits_parse_splash(raw_results = as_lines_list_2)


        #### matches row numbers in splits_df to available row numbers in data
        # helps a lot with relays, since their row numbers vary based on whether or not relay swimmers are included
        # and if those swimmers are listed on one line or two
        splits_df  <-
          transform(splits_df, Row_Numb_Adjusted = data_ind$Row_Numb[findInterval(Row_Numb, as.numeric(data_ind$Row_Numb))]) %>%
          dplyr::mutate(Row_Numb_Adjusted = as.character(Row_Numb_Adjusted)) %>%
          dplyr::select(-Row_Numb) %>%
          dplyr::relocate(Row_Numb = Row_Numb_Adjusted)

        suppressMessages(
        data_ind <- data_ind %>%
          dplyr::left_join(splits_df, by = "Row_Numb", suffix = c(".x", ".y")) %>%
          coalesce_many() %>%
          dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), format, nsmall = 2)) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), as.character)) %>%
          dplyr::mutate(dplyr::across(where(is.character), trimws)) %>%
          na_if_character("NA")
        )
      }

      if(nrow(data_relay) > 0){
        splits_df <-
          splits_parse_splash_relays(as_lines_list_2, split_len = split_length_splash) %>%
          dplyr::filter(Row_Numb %in% data_relay$Row_Numb)

        #### matches row numbers in splits_df to available row numbers in data
        # helps a lot with relays, since their row numbers vary based on whether or not relay swimmers are included
        # and if those swimmers are listed on one line or two
        splits_df  <-
          transform(splits_df, Row_Numb_Adjusted = data_relay$Row_Numb[findInterval(Row_Numb, as.numeric(data_relay$Row_Numb))]) %>%
          dplyr::mutate(Row_Numb_Adjusted = as.character(Row_Numb_Adjusted)) %>%
          dplyr::select(-Row_Numb) %>%
          dplyr::relocate(Row_Numb = Row_Numb_Adjusted)

        suppressMessages(
        data_relay <- data_relay %>%
          dplyr::left_join(splits_df, by = "Row_Numb") %>%
          coalesce_many() %>%
          # na_if_character("10000") %>%
          na_if_numeric(10000) %>%
          dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), format, nsmall = 2)) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), as.character)) %>%
          dplyr::mutate(dplyr::across(where(is.character), trimws)) %>%
          na_if_character("NA")
        )
      }


      data <- data_ind %>%
        dplyr::bind_rows(data_relay) %>%
        dplyr::arrange(as.numeric(Row_Numb)) %>%
        dplyr::select(!dplyr::starts_with("Split"),
                      stringr::str_sort(names(.), numeric = TRUE)) # keep splits columns in order

    } else {
      data <- data %>%
        dplyr::select(-dplyr::starts_with("Split"))
    }

    ### remove empty columns (all values are NA) ###
    data <- Filter(function(x)
      !all(is.na(x)), data)

    #### if there is a Place column it should be first ####
    if("Place" %in% names(data)){
      data <- data %>%
        dplyr::select(Place, dplyr::everything())
    }


    data$Row_Numb <- NULL
    row.names(data) <- NULL

    return(data)
    }

  }

