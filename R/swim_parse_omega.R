#' Formats swimming and diving data read with \code{read_results} into a data
#' frame
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
#' @param file_omega output from \code{read_results}
#' @param avoid_omega a list of strings.  Rows in \code{file_omega} containing these strings
#'   will not be included. For example "Pool:", often used to label pool
#'   records, could be passed to \code{avoid_omega}.  The default is
#'   \code{avoid_default}, which contains many strings similar to "Pool:", such
#'   as "STATE:" and "Qual:".  Users can supply their own lists to \code{avoid_omega}.
#'   \code{avoid_omega} is handled before \code{typo_omega} and \code{replacement_omega}.
#' @param typo_omega a list of strings that are typos in the original results.
#'   \code{swim_parse} is particularly sensitive to accidental double spaces, so
#'   "Central  High School", with two spaces between "Central" and "High" is a
#'   problem, which can be fixed.  Pass "Central  High School" to \code{typo_omega}.
#'   Unexpected commas as also an issue, for example "Texas, University of"
#'   should be fixed using \code{typo_omega} and \code{replacement_omega}
#' @param replacement_omega a list of fixes for the strings in \code{typo_omega}.  Here one
#'   could pass "Central High School" (one space between "Central" and "High")
#'   and "Texas" to \code{replacement_omega} fix the issues described in \code{typo_omega}
#' @param format_results should the results be formatted for analysis (special
#'   strings like \code{"DQ"} replaced with \code{NA}, \code{Finals_Time} as
#'   definitive column)?  Default is \code{TRUE}
#' @param splits either \code{TRUE} or the default, \code{FALSE} - should
#'   \code{swim_parse} attempt to include splits.
#' @param split_length_omega either \code{25} or the default, \code{50}, the length of
#'   pool at which splits are recorded.  Not all results are internally
#'   consistent on this issue - some have races with splits by 50 and other
#'   races with splits by 25.
#' @return returns a data frame with columns \code{Name}, \code{Place},
#'   \code{Age}, \code{Team}, \code{Prelims_Time}, \code{Finals_Time},
#'   \code{Points}, \code{Event} & \code{DQ}.  Note all swims will have a
#'   \code{Finals_Time}, even if that time was actually swam in the prelims
#'   (i.e. a swimmer did not qualify for finals).  This is so that final results
#'   for an event can be generated from just one column.
#'
#' @seealso \code{swim_parse_omega} must be run on the output of
#'   \code{\link{read_results}}

swim_parse_omega <-
  function(file_omega,
           avoid_omega = avoid,
           typo_omega = typo,
           replacement_omega = replacement,
           format_results = TRUE,
           splits = FALSE,
           split_length_omega = split_length) {


    #### testing ####

    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030105EF01FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030103EC02FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030101EE01FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030103EC06FFFFFFFFFFFF01.pdf")
    # avoid_omega <- c("abcxyz")
    # typo_omega <- c("typo")
    # replacement_omega <- c("typo")

    #### assign row numbers ####
    as_lines_list_2 <- file_omega %>%
      .[purrr::map_lgl(., stringr::str_detect, "Early take-off", negate = TRUE)] %>% # removes DQ rational used in some relay DQs that messes up line spacing between relay and swimmers/splits - must happen before adding in row numbers
      add_row_numbers() %>%
      .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid_omega)))] %>%
      stringr::str_replace_all(stats::setNames(replacement_omega, typo_omega)) %>% # replace typos with replacements
      # stringr::str_replace_all("DISQUAL", " DQ ") %>%
      stringr::str_replace_all("EVENT\\:", "Event")

    #### parsing omega files ####

      #### Pulls out event labels from text ####
      events <- event_parse(as_lines_list_2)

      #### set up strings ####
      Name_String <-
        "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
      Time_Score_String <- "\\d{0,2}\\:?\\d{1,3}\\.\\d{2}"
      Time_Score_Specials_String <- paste0("^NT$|^NP$|^DQ$|^NS$|^SCR$|^x?X?", Time_Score_String, "x?X?$")
      Time_Score_Specials_String_Extract <- paste0(Time_Score_String, "|^NT$|^NP$|^DQ$|^NS$|^SCR$")
      # Age_String <- "^SR$|^JR$|^SO$|^FR$|^[:digit:]{1,3}$|^\\d{1,3}\\-\\d{2}$"
      # Para_String <- "^SB?M?\\d{1,2}$"
      Reaction_String <- "^\\+\\s?\\d\\.\\d{3}$|^\\-\\s?\\d\\.\\d{3}$|^0\\.00$|^0\\.\\d\\d$"
      # Brit_ID_String <- "\\d{6,7}"
      # Year_String <- "19\\d\\d|20\\d\\d"
      Record_String <- "^WR$|^AR$|^US$|^CR$"
      # Colon_String <- "\\:\\d\\d"

      #### clean input data ####

      data_cleaned <- as_lines_list_2 %>%
        stringr::str_remove("^\n\\s{0,}") %>%
        # .[purrr::map(., length) > 0] %>%
        .[purrr::map(., stringr::str_length) > 50] %>% # slight speed boost from cutting down length of file
        .[purrr::map_lgl(., stringr::str_detect, paste0(Time_Score_String,"|DQ|SCR"))] %>% # must have \\.\\d\\d because all swimming and diving times do
        .[purrr::map_lgl(., stringr::str_detect, "[:alpha:]{2,}.*[:alpha:]")] %>% # need some letters, need them to not just be a single instance of DQ etc.
        .[purrr::map_lgl(., stringr::str_detect, "r\\:\\+?\\-?\\s?\\d", negate = TRUE)] %>% # remove reaction times
        .[purrr::map_lgl(., stringr::str_detect, "^50m", negate = TRUE)] %>%  # remove British results that start with 50m for splits lines
        # .[purrr::map_lgl(., stringr::str_detect, "[:alpha:]\\:", negate = TRUE)] %>% # remove records
        .[purrr::map_dbl(., stringr::str_count, "\\d\\)") < 2] %>%  # remove inline splits from older style hy-tek results circa 2005
        # .[purrr::map_lgl(., stringr::str_detect, " \\:\\d", negate = TRUE)] %>% # remove other inline splits from older style hytek results circa 2005
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
        stringr::str_remove_all("\\=(?=\\d)") %>%
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
        # stringr::str_replace_all("\\*(?=[:digit:])", "_") %>% # for * prior to place for foreign athletes and sometimes ties
        stringr::str_replace_all("\\*(?=[:alpha:])", "_") %>% # for * prior to name for foreign athletes
        stringr::str_replace_all("\\*", "  ") %>%
        stringr::str_replace_all("(?<=\\d)\\.\\s{1,}(?=[:alpha:])", "  ") %>% # for British results where places are 1. Name
        stringr::str_replace_all("(?<=\\s)\\+\\s(?=[:digit:])", "  +") %>% # for reaction times
        stringr::str_replace_all("(?<=\\s)\\-\\s(?=[:digit:])", "  -") %>% # for reaction times
        trimws()


      #### insert double spaces where needed ####
      data_cleaned <- data_cleaned %>%
        stringr::str_replace_all("(?<=\\d)\\s+[:upper:]?\\s+(?=\\d)", "  ") %>% # letters like P or M to denote pool or meet record
        stringr::str_replace_all("(?<=\\d) (?=[:alpha:])", "  ") %>% # mostly to split place and name
        stringr::str_replace_all("(?<=\\d) (?=_)", "  ") %>% # mostly to split place and name, if name is preceded by "_" as a stand-in for "*"
        stringr::str_replace_all("(?<=\\d) (?=\\d)", "  ") %>% # mostly to split place team names that start with a number, like in NYS results (5-Fairport etc.)
        stringr::str_replace_all("(?<=[:alpha:]),(?=[:alpha:])", ", ") %>% # split names that don't have a space between last,first
        stringr::str_replace_all("(?<=[:alpha:])\\. (?=[:digit:])", "\\.  ") %>% # split abbreviated team names like Southern Cal. and times
        stringr::str_replace_all("(?<=\\d) (?=_)", "  ") %>% # spacing between place and athletes with */_ leading name

        stringr::str_replace_all("(?<=\\)) (?=[:alpha:])", "  ") %>% # spacing between place) and names
        stringr::str_replace_all("\\((?=[:digit:])", "  \\(") %>% # spacing between (YoB) and name, for British results
        stringr::str_replace_all(" FR ", "       FR      ") %>% # split age and team
        stringr::str_replace_all(" SO ", "       SO      ") %>% # split age and team
        stringr::str_replace_all(" JR ", "       JR      ") %>% # split age and team
        stringr::str_replace_all(" SR ", "       SR      ") %>% # split age and team
        stringr::str_replace_all("(SM?B?1\\d{1})(\\d{1,2})", "\\1   \\2") %>%  # split para classification and age
        stringr::str_replace_all("(\\d{6,7})([:alpha:])", "\\1   \\2") %>%  # split Brit ID and Name
        stringr::str_replace_all(" NT ", "       NT      ") %>% # split prelim and final
        stringr::str_replace_all("(?<=[:alpha:])\\s{2,3}(?=[:alpha:])", " ") %>% # testing 12/21/2020 would help with typos
        stringr::str_replace_all("(?<=[:alpha:]) (?=\\d)", "  ") %>% # split name and age
        stringr::str_replace_all("(?<=[:alpha:])(\\d{1,3}\\-\\d{2})", "  \\1  ") %>% # split name and yyy-mm age
        stringr::str_replace_all("(?<=\\,) (?=\\d)", "  ") %>% # split name and age if name is so long that it ends with a ","
        stringr::str_replace_all("(?<=\\d) (?=\\d{1,}$)", "  ") %>%  # split off row_numb
        stringr::str_replace_all("(?<=\\.\\d\\d\\s{1,10}\\d?\\d?\\:?\\d?\\d\\.\\d\\d)\\s{1,10}[:alpha:]{1,5}\\d?\\s{1,10}(?=\\d{1,})", "  ") %>%  # removes "AAC" or "AAA" or "NYS" or "SEC1" etc. from after finals time
        # stringr::str_replace_all("NYS|AAA|AAC|SEC\\d{1,2}", "  ") %>%
        # stringr::str_replace_all("SEC\\d{1,2}", "  ") %>%  # for old NYS results
        stringr::str_replace_all("\\d{3}\\.\\d{2}\\s+(\\d{3}\\.\\d{2})\\s+\\d{3}\\.\\d{2}\\s+(\\d{3}\\.\\d{2})", "\\1  \\2") %>%  # for old NCAA results with diving scores and DDs
        stringr::str_replace_all("(?<=\\s)S(?=B?M?\\d{1,2})", "  S") # for para classifications

      #### splits data into variables by splitting at multiple (>= 2) spaces ####
      data_cleaned <-
        unlist(purrr::map(data_cleaned, stringr::str_split, "\\s{2,}"),
               recursive = FALSE)

      # unique(map(data_cleaned, length))

      #### breaks data into subsets based on how many variables it has ####
      data_length_8 <- data_cleaned[purrr::map(data_cleaned, length) == 8]
      data_length_9 <- data_cleaned[purrr::map(data_cleaned, length) == 9]
      data_length_10 <- data_cleaned[purrr::map(data_cleaned, length) == 10]
      data_length_11 <- data_cleaned[purrr::map(data_cleaned, length) == 11]
      data_length_12 <- data_cleaned[purrr::map(data_cleaned, length) == 12]
      data_length_13 <- data_cleaned[purrr::map(data_cleaned, length) == 13]

      # treatment of DQs new 8/19
      suppressWarnings(DQ <-
                         data_cleaned[stringr::str_detect(data_cleaned, Time_Score_String, negate = TRUE) == TRUE])

      DQ_length_3 <- DQ[purrr::map(DQ, length) == 3]
      DQ_length_4 <- DQ[purrr::map(DQ, length) == 4]
      # DQ_length_5 <- DQ[purrr::map(DQ, length) == 5]

      #### thirteen variables ####
      if (length(data_length_13) > 0) {
        suppressWarnings(
          df_13 <- data_length_13 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat = V2,
              Lane = V3,
              Name = V4,
              Team = V5,
              Reaction_Time = V7,
              Finals_Time = V11,
              Row_Numb = V13
            )
        )
      } else {
        df_13 <- data.frame(
          Row_Numb = character(),
          stringsAsFactors = FALSE
        )
      }

      #### twelve variables ####
      if (length(data_length_12) > 0) {
        suppressWarnings(
          df_12 <- data_length_12 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat = V2,
              Lane = V3,
              Name = V4,
              Team = V5,
              Reaction_Time = V6,
              Finals_Time = V10,
              Row_Numb = V12
            )
        )
      } else {
        df_12 <- data.frame(
          Row_Numb = character(),
          stringsAsFactors = FALSE
        )
      }

      #### eleven variables ####
      if (length(data_length_11) > 0) {
        suppressWarnings(
          df_11 <- data_length_11 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::select(
              Place = V1,
              Heat = V2,
              Lane = V3,
              Name = V4,
              Team = V5,
              Reaction_Time = V6,
              Finals_Time = V10,
              Row_Numb = V11
            )
        )
      } else {
        df_11 <- data.frame(
          Row_Numb = character(),
          stringsAsFactors = FALSE
        )
      }

      #### ten variables ####
      if (length(data_length_10) > 0) {
        suppressWarnings(
          df_10 <- data_length_10 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::mutate(
              Reaction_Time = dplyr::case_when(
                stringr::str_detect(V6, Reaction_String) == TRUE ~ V6,
                stringr::str_detect(V7, Reaction_String) == TRUE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::select(
              Place = V1,
              Heat = V2,
              Lane = V3,
              Name = V4,
              Team = V5,
              Reaction_Time,
              Finals_Time = V8,
              Row_Numb = V10
            )
        )
      } else {
        df_10 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      #### nine variables ####
      if (length(data_length_9) > 0) {
        suppressWarnings(
          df_9 <- data_length_9 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::mutate(Heat = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Lane = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V3,
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == FALSE ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Name = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V3,
                                                  TRUE ~ V4)) %>%
            dplyr::mutate(Team = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V4,
                                                  TRUE ~ V5)) %>%
            dplyr::mutate(Reaction_Time = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V5,
                                                           TRUE ~ V6)) %>%

            dplyr::mutate(
              Finals_Time = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE ~ V7,
                stringr::str_detect(V7, "[1-9]\\:\\d\\d") == TRUE  ~ V7,
                stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat,
              Lane,
              Name,
              Team,
              Reaction_Time,
              Finals_Time,
              Row_Numb = V9
            )
        )
      } else {
        df_9 <- data.frame(
          Row_Numb = character(),
          stringsAsFactors = FALSE
        )
      }

      #### eight variables
      if (length(data_length_8) > 0) {
        suppressWarnings(
          df_8 <- data_length_8 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::mutate(Heat = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Lane = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V3,
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == FALSE ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Name = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V3,
                                                  TRUE ~ V4)) %>%
            dplyr::mutate(Team = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V4,
                                                  TRUE ~ V5)) %>%
            dplyr::mutate(Reaction_Time = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V5,
                                                           TRUE ~ V6)) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat,
              Lane,
              Name,
              Team,
              Reaction_Time,
              Finals_Time = V7,
              Row_Numb = V8
            )
        )
      } else {
        df_8 <- data.frame(Row_Numb = character(),
                           stringsAsFactors = FALSE)
      }


      #### DQ data ####
      #### DQ 4 ####
      if (length(DQ_length_4) > 0) {
        suppressWarnings(
          df_DQ_4 <- DQ_length_4 %>%
            list_transform() %>%
            dplyr::mutate(Place = "10000") %>%
            dplyr::select(
              Place = V1,
              Team = V2,
              Finals_Time = V3,
              Row_Numb = V4
            ) %>%
            dplyr::mutate(DQ = 1)
        )

      } else {
        df_DQ_4 <- data.frame(
          Row_Numb = character(),
          DQ = numeric(),
          stringsAsFactors = FALSE
        )
      }

      #### DQ 3 ####
      if (length(DQ_length_3) > 0) {
        suppressWarnings(
          df_DQ_3 <- DQ_length_3 %>%
            list_transform() %>%
            dplyr::mutate(Place = "10000") %>%
            dplyr::select(
              Place,
              Team = V1,
              Finals_Time = V2,
              Row_Numb = V3
            ) %>%
            dplyr::mutate(DQ = 1)
        )
      } else {
        df_DQ_3 <- data.frame(
          Row_Numb = character(),
          stringsAsFactors = FALSE
        )
      }

      #### Rejoin dataframes from each number of variables ####
      Min_Row_Numb <- min(events$Event_Row_Min)
      suppressWarnings(
        data <- dplyr::bind_rows(df_13, df_12) %>%
          dplyr::bind_rows(df_11) %>%
          dplyr::bind_rows(df_10) %>%
          dplyr::bind_rows(df_9) %>%
          dplyr::bind_rows(df_8) %>%
          dplyr::left_join(df_DQ_4) %>%
          dplyr::left_join(df_DQ_3) %>%
          dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
          dplyr::arrange(Row_Numb)
      )


      # if("Prelims_Time" %in% names(data) == FALSE){
      #   data$Prelims_Time <- NA
      # }
      # if("Age" %in% names(data) == FALSE){
      #   data$Age <- "NA"
      # }

      suppressWarnings(
        data <- data %>%
          dplyr::mutate(
            Exhibition = dplyr::case_when(stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
                                          TRUE ~ 0),
          ) %>%
          dplyr::mutate(DQ = dplyr::case_when(Place == 10000 &
                                                Exhibition == 0 ~ 1, # added exhibition condition 8/27
                                              stringr::str_detect(Finals_Time, "DQ") == TRUE ~ 1,
                                              is.na(DQ) ~ 0,
                                              TRUE ~ DQ)) %>%
          na_if(10000) %>%
          dplyr::mutate(dplyr::across(
            c(Name, Team), ~ stringr::str_replace_all(., "10000", "--")
          )) %>% # remove any "10000"s added in erroneously
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

      #### cleaning up final results ####

      suppressWarnings(
        data <- data %>%
          dplyr::mutate(
            Name = stringr::str_replace_all(Name, "_", "\\*"),
            Place = round(as.numeric(Place)),
            Event = as.character(Event)
          ) %>%
          dplyr::mutate(
            Place = dplyr::case_when(is.na(Place) == TRUE &
                                       DQ == 0 ~ dplyr::lag(Place) + 1,
                                     TRUE ~ Place)
          )
      )

      #### cleaning ####
      if(format_results == TRUE){
        data <- format_results(data)
      }

      #### adding relay swimmers in ####
      # if (relay_swimmers == TRUE) {
      #   relay_swimmers_df <- collect_relay_swimmers(as_lines_list_2)
      #
      #   relay_swimmers_df <-
      #   transform(relay_swimmers_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
      #     dplyr::select(-Row_Numb)
      #
      #   data <- data %>%
      #     dplyr::left_join(relay_swimmers_df, c("Row_Numb" = "Row_Numb_Adjusted"))
      # }

      #### adding splits back in ####
      if (splits == TRUE) {
        # split_length <- 50
        splits_df <-
          splits_parse(as_lines_list_2, split_len = split_length_omega)

        #### matches row numbers in splits_df to available row numbers in data
        # helps a lot with relays, since their row numbers vary based on whether or not relay swimmers are included
        # and if those swimmers are listed on one line or two
        splits_df  <-
          transform(splits_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
          dplyr::select(-Row_Numb)

        # new names for omega splits, because first 50 isn't captured
        splits_df <- splits_df %>%
          dplyr::rename_with(splits_rename_omega, starts_with("Split"), split_len = split_length_omega) %>%
          dplyr::mutate(across(starts_with("Split"), sec_format))


        data <- data %>%
          dplyr::left_join(splits_df, by = c("Row_Numb" = "Row_Numb_Adjusted")) %>%
          dplyr::mutate(
            Split_Total = rowSums(dplyr::across(dplyr::starts_with("Split"))),
            Finals_Time_Sec = sec_format(Finals_Time),
            Split_50 = Finals_Time_Sec - Split_Total
          ) %>%
          dplyr::select(-Split_Total,-Finals_Time_Sec) %>%
          dplyr::mutate(across(starts_with("Split"), format, nsmall = 2)) %>%
          dplyr::mutate(across(where(is.numeric), as.character)) %>%
          dplyr::select(!dplyr::starts_with("Split"),
                        stringr::str_sort(names(.), numeric = TRUE)) # keep splits columns in order

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

      # message("Beginning with SwimmeR v0.6.0 the Grade and School output columns are renamed Age and Team respectively.  Please adjust your work flows as needed.")

      return(data)

  }


#' @export

