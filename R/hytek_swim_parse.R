#' Formats Hytek style swimming and diving data read with \code{read_results}
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
#' @importFrom stringer str_remove_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_sort
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @param file_hytek output from \code{read_results}
#' @param avoid_hytek a list of strings.  Rows in \code{file_hytek} containing
#'   these strings will not be included. For example "Pool:", often used to
#'   label pool records, could be passed to \code{avoid_hytek}.  The default is
#'   \code{avoid_default}, which contains many strings similar to "Pool:", such
#'   as "STATE:" and "Qual:".  Users can supply their own lists to
#'   \code{avoid_hytek}. \code{avoid_hytek} is handled before \code{typo_hytek}
#'   and \code{replacement_hytek}.
#' @param typo_hytek a list of strings that are typos in the original results.
#'   \code{swim_parse} is particularly sensitive to accidental double spaces, so
#'   "Central  High School", with two spaces between "Central" and "High" is a
#'   problem, which can be fixed.  Pass "Central  High School" to
#'   \code{typo_hytek}. Unexpected commas as also an issue, for example "Texas,
#'   University of" should be fixed using \code{typo_hytek} and
#'   \code{replacement_hytek}
#' @param replacement_hytek a list of fixes for the strings in
#'   \code{typo_hytek}.  Here one could pass "Central High School" (one space
#'   between "Central" and "High") and "Texas" to \code{replacement_hytek} fix
#'   the issues described in \code{typo_hytek}
#' @param format_results should the results be formatted for analysis (special
#'   strings like \code{"DQ"} replaced with \code{NA}, \code{Finals} as
#'   definitive column)?  Default is \code{TRUE}
#' @param splits either \code{TRUE} or the default, \code{FALSE} - should
#'   \code{swim_parse} attempt to include splits.
#' @param split_length_hytek either \code{25} or the default, \code{50}, the
#'   length of pool at which splits are recorded.  Not all results are
#'   internally consistent on this issue - some have races with splits by 50 and
#'   other races with splits by 25.
#' @param relay_swimmers_hytek should names of relay swimmers be captured?
#'   Default is \code{FALSE}
#' @return returns a data frame with columns \code{Name}, \code{Place},
#'   \code{Age}, \code{Team}, \code{Prelims}, \code{Finals},
#'   \code{Points}, \code{Event} & \code{DQ}.  Note all swims will have a
#'   \code{Finals}, even if that time was actually swam in the prelims
#'   (i.e. a swimmer did not qualify for finals).  This is so that final results
#'   for an event can be generated from just one column.
#'
#' @seealso \code{swim_parse_hytek} must be run on the output of
#'   \code{\link{read_results}}

swim_parse_hytek <-
  function(file_hytek,
           avoid_hytek = avoid,
           typo_hytek = typo,
           replacement_hytek = replacement,
           format_results = TRUE,
           splits = FALSE,
           split_length_hytek = split_length,
           relay_swimmers_hytek = relay_swimmers) {


    #### testing ####
    # file_hytek <- read_results("https://data.ohiostatebuckeyes.com/livestats/m-swim/210302F001.htm")
    # file_hytek <- read_results(system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR"))

    # file_hytek <- read_results("https://www.somersetasa.org/sasa/media/archive1/swimchamps2020/d7/1500m_mixed_090220.pdf")
    # file_hytek <- read_results("https://www.somersetasa.org/sasa/media/archive1/swimchamps2020/d6/s11_0802.pdf")
    # file_hytek <- read_results("https://www.somersetasa.org/sasa/media/archive1/swimchamps2020/d4/s7_0102.pdf")
    # file_hytek <- read_results("https://swimswam.com/wp-content/uploads/2018/08/2004-Division-I-NCAA-Championships-Women-results1.pdf")
    # file_hytek <- read_results("http://www.swmeets.com/Realtime/Speedo%20Champions/210803F004.htm")
    # file_hytek <-
    #   system.file("extdata", "2018_jimi_flowers_PARA.pdf", package = "SwimmeR") %>%
    #   read_results()
    # file_hytek <- "http://www.swmeets.com/Realtime/Speedo%20Champions/210803F004.htm" %>%
    #   read_results()
    # avoid_hytek <- (    avoid_default <-
    # c(
    #   # "[:upper:]\\:",
    #   "[A-S]\\:",
    #   # to allow EVENT:
    #   "[U-Z]\\:",
    #   # to allow EVENT:
    #   "[A-MO-Z]T\\:",
    #   # to allow EVENT:
    #   "[a-q]\\:",
    #   # want to make sure to include r: for reaction times in splits lines
    #   "[s-z]\\:",
    #   # want to make sure to include r: for reaction times in splits lines
    #   "[:alpha:]r\\:",
    #   "\\.\\:",
    #   "\\d\\:\\s",
    #   "\\'\\:",
    #   "QUALIFYING "
    # ))
    # typo_avoid_hytek <- c("typo")
    # replacement_hytek <- c("typo")


    as_lines_list_2 <- file_hytek %>%
      .[stringr::str_detect(., "Early take-off", negate = TRUE)] %>% # removes DQ rational used in some relay DQs that messes up line spacing between relay and swimmers/splits - must happen before adding in row numbers
      add_row_numbers() %>%
      .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid_hytek)))] %>%
      stringr::str_replace_all(stats::setNames(replacement_hytek, typo_hytek)) %>% # replace typos with replacements
      stringr::str_replace_all("DISQUAL", " DQ ") %>%
      stringr::str_replace_all("EVENT\\:", "Event")

    #### parsing html and pdf files ####
    # if (stringr::str_detect(file[1], "^A107") == FALSE) {

    #### Pulls out event labels from text ####
    events <- event_parse(as_lines_list_2)

    #### Pulls out reaction times from text ####
    reaction_times <- reaction_times_parse(as_lines_list_2)

    #### set up strings ####
    Name_String <-
      "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
    Time_Score_String <- "\\d{0,2}\\:?\\d{1,3}\\.\\d{2}"
    Time_Score_Specials_String <- paste0("^NT$|^NP$|^DQ$|^NS$|^SCR$|^x?X?", Time_Score_String, "x?X?$")
    Time_Score_Specials_String_Extract <- paste0(Time_Score_String, "|^NT$|^NP$|^DQ$|^NS$|^SCR$")
    Age_String <- "^SR$|^JR$|^SO$|^FR$|^[:digit:]{1,3}$|^\\d{1,3}\\-\\d{2}$"
    Para_String <- "^SB?M?\\d{1,2}$"
    Reaction_String <- "^\\+\\s?\\d\\.\\d{3}$|^\\-\\s?\\d\\.\\d{3}$|^0.00$"
    Brit_ID_String <- "\\d{6,7}"
    # Colon_String <- "\\:\\d\\d"

    #### clean input data ####

    data_cleaned <- hytek_clean_strings(as_lines_list_2,
                                        time_score_string = Time_Score_String)

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
    data_length_3 <- list_breaker(data_cleaned, len = 3)
    data_length_4 <- list_breaker(data_cleaned, len = 4)
    data_length_5 <- list_breaker(data_cleaned, len = 5)
    data_length_6 <- list_breaker(data_cleaned, len = 6)
    data_length_7 <- list_breaker(data_cleaned, len = 7)
    data_length_8 <- list_breaker(data_cleaned, len = 8)
    data_length_9 <- list_breaker(data_cleaned, len = 9)

    # treatment of DQs new 8/19
    suppressWarnings(DQ <-
                       data_cleaned[stringr::str_detect(data_cleaned, Time_Score_String, negate = TRUE) == TRUE])

    DQ_length_3 <- list_breaker(DQ, len = 3)
    DQ_length_4 <- list_breaker(DQ, len = 4)

    #### nine variables
    df_9 <- hytek_length_9_sort(data_length_9,
                                brit_id_string = Brit_ID_String,
                                para_string = Para_String,
                                age_string = Age_String,
                                time_score_specials_string = Time_Score_Specials_String)

    #### eight variables
    df_8 <- hytek_length_8_sort(data_length_8,
                                brit_id_string = Brit_ID_String,
                                para_string = Para_String,
                                age_string = Age_String,
                                time_score_specials_string = Time_Score_Specials_String)


    #### seven variables ####
    df_7 <- hytek_length_7_sort(data_length_7,
                                brit_id_string = Brit_ID_String,
                                para_string = Para_String,
                                age_string = Age_String,
                                time_score_specials_string = Time_Score_Specials_String)

    #### six variables ####
    df_6 <- hytek_length_6_sort(data_length_6,
                                name_string = Name_String,
                                para_string = Para_String,
                                age_string = Age_String,
                                time_score_specials_string = Time_Score_Specials_String)

    #### five variables ####
    df_5 <- hytek_length_5_sort(data_length_5,
                                name_string = Name_String,
                                para_string = Para_String,
                                age_string = Age_String,
                                time_score_specials_string = Time_Score_Specials_String)

    #### four variables ####
    df_4 <- hytek_length_4_sort(data_length_4,
                                time_score_specials_string = Time_Score_Specials_String)

    #### three variables ####
    df_3 <- hytek_length_3_sort(data_length_3)

    #### DQ data ####
    #### DQ four variables ####
    df_DQ_4 <- hytek_length_4_DQ_sort(DQ_length_4)

    #### DQ three variables ####
    df_DQ_3 <- hytek_length_3_DQ_sort(DQ_length_3)

    #### Rejoin data frames from each number of variables ####
    Min_Row_Numb <- min(events$Event_Row_Min)
    suppressWarnings(
      data <- dplyr::bind_rows(df_9, df_8) %>%
        dplyr::bind_rows(df_7) %>%
        dplyr::bind_rows(df_6) %>%
        dplyr::bind_rows(df_5) %>%
        dplyr::bind_rows(df_4) %>%
        dplyr::bind_rows(df_3) %>%
        dplyr::left_join(df_DQ_4) %>%
        dplyr::left_join(df_DQ_3) %>%
        dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
        dplyr::arrange(Row_Numb)
    )


    suppressWarnings(
      data <- data %>%
        dplyr::mutate(
          Exhibition = dplyr::case_when(stringr::str_detect(Finals, "x|X") == TRUE ~ 1,
                                        TRUE ~ 0),
          ###
          Finals = stringr::str_extract(Finals, Time_Score_Specials_String_Extract),
          Prelims = stringr::str_extract(Prelims, Time_Score_Specials_String_Extract)
        ) %>%
        ### moved up from below for DQ work 8/20
        dplyr::mutate(DQ = dplyr::case_when(Place == 10000 &
                                              Exhibition == 0 ~ 1, # added exhibition condition 8/27
                                            stringr::str_detect(Finals, "DQ") == TRUE ~ 1,
                                            is.na(DQ) ~ 0,
                                            TRUE ~ DQ)) %>%
        na_if(10000) %>%
        dplyr::mutate(dplyr::across(
          c(Name, Team), ~ stringr::str_replace_all(., "10000", "--")
        )) %>% # remove any "10000"s added in erroniuously
        ####
        dplyr::mutate(
          Place = str_remove(Place, "\\)"),
          Place = str_remove(Place, "_"),
          Place = as.numeric(Place),
          Place = dplyr::case_when(
            is.na(dplyr::lag(Place)) == TRUE ~ Place,
            dplyr::lag(Place) == Place ~ Place + 0.1,
            dplyr::lag(Place) != Place ~ Place
          ),
          Place = as.character(Place),
          Row_Numb = as.numeric(Row_Numb)
        )
    )

    if("Points" %in% names(data) == FALSE){
      data$Points <- NA}

    #### cleaning ####
    if(format_results == TRUE){
      data <- format_results(data)
    }

    #### add in events based on row number ranges ####
    if(min(data$Row_Numb) < min(events$Event_Row_Min)){
      unknown_event <- data.frame(Event = "Unknown",
                                  Event_Row_Min = min(data$Row_Numb),
                                  Event_Row_Max = min(events$Event_Row_Min) - 1)
      events <- dplyr::bind_rows(unknown_event, events)
    }

    data  <-
      transform(data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)]) %>%
      dplyr::na_if("Unknown")

    #### add in reaction times based on row number ranges ####
    if(min(data$Row_Numb) < min(reaction_times$Reaction_Time_Row_Numb)){
      unknown_reaction_time <- data.frame(Reaction_Time = "NA",
                                          Reaction_Time_Row_Numb = min(data$Row_Numb))
      reaction_times <- dplyr::bind_rows(unknown_reaction_time, reaction_times)
    }

    data  <-
      dplyr::left_join(data, reaction_times, by = c("Row_Numb" = "Reaction_Time_Row_Numb")) %>%
      dplyr::mutate(Reaction_Time = dplyr::case_when(is.na(Finals) == TRUE ~ "NA",
                                                     TRUE ~ Reaction_Time)) %>%
      dplyr::na_if("NA")

    #### cleaning up final results ####

    suppressWarnings(
      data <- data %>%
        dplyr::mutate(
          Name = stringr::str_replace_all(Name, "_", "\\*"),
          Place = round(as.numeric(Place)),
          Event = as.character(Event)
        ) %>%
        dplyr::mutate(Age = dplyr::case_when(stringr::str_detect(Age, "\\d{1,3}\\-\\d{2}") == TRUE ~ age_format(Age),
                                             TRUE ~ Age)) %>%
        dplyr::mutate(
          Place = dplyr::case_when(is.na(Place) == TRUE &
                                     DQ == 0 ~ dplyr::lag(Place) + 1,
                                   TRUE ~ Place)
        ) %>%
        dplyr::mutate(Points = stringr::str_remove_all(Points, " "),
                      Points = as.numeric(Points))
    )

    #### adding relay swimmers in ####
    if (relay_swimmers_hytek == TRUE) {
      relay_swimmers_df <- collect_relay_swimmers(as_lines_list_2)

      relay_swimmers_df <-
        transform(relay_swimmers_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
        dplyr::select(-Row_Numb)

      data <- data %>%
        dplyr::left_join(relay_swimmers_df, c("Row_Numb" = "Row_Numb_Adjusted"))
    }

    #### adding splits back in ####
    if (splits == TRUE) {
      splits_df <- splits_parse(as_lines_list_2, split_len = split_length_hytek)

      #### matches row numbers in splits_df to available row numbers in data
      # helps a lot with relays, since their row numbers vary based on whether or not relay swimmers are included
      # and if those swimmers are listed on one line or two
      splits_df  <-
        transform(splits_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
        dplyr::select(-Row_Numb)

      data <- data %>%
        dplyr::left_join(splits_df, by = c("Row_Numb" = "Row_Numb_Adjusted")) %>%
        dplyr::select(!dplyr::starts_with("Split"), stringr::str_sort(names(.), numeric = TRUE)) # keep splits columns in order

    }

    ### remove empty columns (all values are NA) ###
    data <- Filter(function(x)
      !all(is.na(x)), data)

    ##### remove duplicated results ####
    # data <- data %>%
    #   dplyr::arrange(Name, Team, is.na(Wind_Speed), is.na(Prelims_Result)) %>% # new 1/1/21 to deal with results presented by heat and as final on same page
    #   dplyr::distinct(Name, Team, Event, Prelims_Result, Finals_Result, .keep_all = TRUE) # new 1/1/21 to deal with results presented by heat and as final on same page
    #
    #### if there is a Place column it should be first ####
    if("Place" %in% names(data)){
      data <- data %>%
        dplyr::select(Place, dplyr::everything())
    }

    data$Row_Numb <- NULL

    return(data)

    }
  }



#' @export

