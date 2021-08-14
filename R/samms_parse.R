#' Formats swimming and diving data read with \code{read_results} into a
#' dataframe
#'
#' Takes the output of \code{read_results} of S.A.M.M.S. results and cleans it,
#' yielding a dataframe of swimming (and diving) results
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_length
#' @importFrom stringr str_extract
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @param file_samms output from \code{read_results} of S.A.M.M.S. style results
#' @param avoid_samms a list of strings.  Rows in \code{file} containing these
#'   strings will not be included. For example "Pool:", often used to label pool
#'   records, could be passed to \code{avoid}.  The default is
#'   \code{avoid_default}, which contains many strings similar to "Pool:", such
#'   as "STATE:" and "Qual:".  Users can supply their own lists to \code{avoid}.
#' @param typo_samms a list of strings that are typos in the original results.
#'   \code{swim_parse} is particularly sensitive to accidental double spaces, so
#'   "Central  High School", with two spaces between "Central" and "High" is a
#'   problem, which can be fixed.  Pass "Central  High School" to \code{typo}.
#'   Unexpected commas as also an issue, for example "Texas, University of"
#'   should be fixed using \code{typo} and \code{replacement}
#' @param replacement_samms a list of fixes for the strings in \code{typo}.
#'   Here one could pass "Central High School" (one space between "Central" and
#'   "High") and "Texas" to \code{replacement} fix the issues described in
#'   \code{typo}
#' @param format_samms should the data be formatted for analysis (special
#'   strings like \code{"DQ"} replaced with \code{NA}, \code{Finals_Time} as
#'   definitive column)?  Default is \code{TRUE}
#' @return returns a data frame with columns \code{Name}, \code{Place},
#'   \code{Age}, \code{Team}, \code{Prelims_Time}, \code{Finals_Time},
#'   \code{Event} & \code{DQ}.  Note all swims will have a \code{Finals_Time},
#'   even if that time was actually swam in the prelims (i.e. a swimmer did not
#'   qualify for finals).  This is so that final results for an event can be
#'   generated from just one column.
#'
#' @seealso \code{swim_parse} must be run on the output of
#'   \code{\link{read_results}}

samms_parse <-
  function(file_samms,
           avoid_samms = avoid,
           typo_samms = typo,
           replacement_samms = replacement,
           format_samms = format_results) {

    # typo_default <- c("typo")
    #
    # replacement_default <- c("typo")
    #
    # avoid_default <-
    #   c(
    #     # "[:upper:]\\:",
    #     "[A-S]\\:", # to allow EVENT:
    #     "[U-Z]\\:", # to allow EVENT:
    #     "[A-MO-Z]T\\:", # to allow EVENT:
    #     "[a-q]\\:", # want to make sure to include r: for reaction times in splits lines
    #     "[s-z]\\:", # want to make sure to include r: for reaction times in splits lines
    #     "[:alpha:]r\\:",
    #     "\\.\\:",
    #     "\\d\\:\\s",
    #     "\\'\\:"
    #     # "Record",
    #     # "RECORD",
    #     # "^\\s*NYSPHSAA",
    #     # "^\\s*NYSPHAA",
    #     # "^\\s*Finals",
    #     # "^\\s*Prelims",
    #     # "^\\s*Hosted",
    #     # "^\\s*Meet",
    #     # "^\\s*MEET",
    #     # "^\\s*Points",
    #     # "^\\s*League",
    #     # "^\\s*LEAGUE",
    #     # "^\\s*School\\s*Prelims\\s*Finals",
    #     # "^\\s*r\\:",
    #     # "NCAA",
    #   )
    #
    # file_samms <- read_results(url_5)
    # avoid_samms <- avoid_default
    # typo_samms <- typo_default
    # replacement_samms <- replacement_default
    # format_samms <- TRUE

    as_lines_list_2 <- file_samms %>%
      add_row_numbers() %>%
      .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid_samms)))] %>%
      stringr::str_replace_all(stats::setNames(replacement_samms, typo_samms)) %>% # replace typos with replacements
      stringr::str_replace_all("DQUED", "   DQ   ") %>%
      stringr::str_replace_all(" NO SHOW ", "   NS   ")

    #### Set up strings ####
    # Name_String <-
    #   "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
    Time_Score_String <- "\\d{0,2}\\:?\\d{1,3}\\.\\d{2}"
    Time_Score_Specials_String <- paste0(Time_Score_String, "|^NT$|^NP$|^DQ$|^NS$|^SCR$")
    Finals_Place_String <- "^[FCB]?\\d{0,4}$"
    Age_String <- "^SR$|^JR$|^SO$|^FR$|^[:digit:]{1,3}$"
    # Relay_Age_String <- "^\\d\\d?\\-UP$|^\\d\\d?&UN$|^\\d\\d?\\-\\d\\d?$"

    #### Collect event names ####
    events <- event_parse(as_lines_list_2)

    data_cleaned <- as_lines_list_2 %>%
      stringr::str_remove("^\n\\s{0,}") %>%
      .[purrr::map(., stringr::str_length) > 50] %>% # slight speed boost from cutting down length of file
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       paste0(Time_Score_String, "|DQ|SCR|NS"))] %>% # must have \\.\\d\\d because all swimming and diving results do
      .[purrr::map_lgl(., stringr::str_detect, "[:alpha:]{2,}")] %>%
      trimws()


    #### insert double spaces where needed ####
    data_cleaned <- data_cleaned %>%
      stringr::str_replace_all("(?<=\\d) (?=[:alpha:])", "  ") %>% # mostly to split place and name
      stringr::str_replace_all("(?<=\\d) (?=_)", "  ") %>% # mostly to split place and name, if name is precended by "_" as a stand-in for "*"
      stringr::str_replace_all("(?<=\\d) (?=\\d)", "  ") %>% # mostly to split place team names that start with a number, like in NYS results (5-Fairport etc.)
      stringr::str_replace_all("(?<=[:alpha:]),(?=[:alpha:])", ", ") %>% # split names that don't have a space between last,first
      stringr::str_replace_all("(?<=[:alpha:])\\. (?=[:digit:])", "\\.  ") %>% # split abreviated team names like Southern Cal. and times
      stringr::str_replace_all("(?<=[:alpha:])\\s{2,3}(?=[:alpha:])", " ") %>% # testing 12/21/2020 would help with typos
      stringr::str_replace_all("(?<=[:alpha:]) (?=\\d)", "  ") %>% # split name and age
      stringr::str_replace_all("(?<=\\,) (?=\\d)", "  ") %>% # split name and age if name is so long that it ends with a ","
      stringr::str_replace_all("(?<=\\d) (?=\\d{1,}$)", "  ") %>%  # split off row_numb
      stringr::str_replace_all(
        "(?<=\\.\\d\\d\\s{1,10}\\d?\\d?\\:?\\d?\\d\\.\\d\\d)\\s{1,10}[:alpha:]{1,5}\\d?\\s{1,10}(?=\\d{1,})",
        "  "
      ) %>%  # removes "AAC" or "AAA" or "NYS" or "SEC1" etc. from after finals time
      stringr::str_replace_all("(?<=[:upper:]),\\s{2,}(?=[:upper:] )", ", ") %>% # relay designators like KVAC,  A for A relay are brought together
      stringr::str_replace_all("(?<=, [:upper:])\\s+(?=\\d\\d?\\-UP)", " ") %>% # bring in relay age designators
      stringr::str_replace_all("(?<=, [:upper:])\\s+(?=\\d\\d?&UN)", " ") %>% # bring in relay age designators
      stringr::str_replace_all("(?<=, [:upper:])\\s+(?=\\d\\d?\\-\\d\\d?)", " ") %>%  # bring in relay age designators
      stringr::str_replace_all(" \\([:upper:]{1,}\\) ", "   ") %>%
      stringr::str_replace_all("\\*", "  ") %>%
      stringr::str_remove_all("(?<=\\s[:upper:]) +P ") %>% # spread out heat markers
      stringr::str_remove_all("(?<=\\s[:upper:]\\d{1,3}) +P ") %>% # spread out heat markers
      stringr::str_remove_all("(?<=\\s[:upper:]) +P ") %>% # spread out heat markers
      stringr::str_remove_all("(?<=\\s\\d{1,3}) +P ") %>% # spread out heat markers
      stringr::str_replace_all("(?<=\\d) B ", "  B  ") %>% # spread out heat markers
      stringr::str_replace_all("(?<=\\d) F ", "  F  ") %>% # spread out heat markers
      stringr::str_replace_all("(?<=\\d) C ", "  C  ") %>%  # spread out heat markers
      stringr::str_replace_all("(?<=SCR) B ", "  B  ") %>% # spread out heat markers
      stringr::str_replace_all("(?<=SCR) F ", "  F  ") %>% # spread out heat markers
      stringr::str_replace_all("(?<=SCR) C ", "  C  ") %>%  # spread out heat markers
      stringr::str_replace_all("(?<=DQ) B ", "  B  ") %>% # spread out heat markers
      stringr::str_replace_all("(?<=DQ) F ", "  F  ") %>% # spread out heat markers
      stringr::str_replace_all("(?<=DQ) C ", "  C  ") %>%  # spread out heat markers
      stringr::str_replace_all("(?<=NS) B ", "  B  ") %>% # spread out heat markers
      stringr::str_replace_all("(?<=NS) F ", "  F  ") %>% # spread out heat markers
      stringr::str_replace_all("(?<=NS) C ", "  C  ") %>%  # spread out heat markers
      stringr::str_replace_all("  \\([:upper:]{2}\\)  ", "    ") %>%  # spread out heat markers
      stringr::str_replace_all("(?<=\\d) (?=\\d)", "  ") %>%
      stringr::str_replace_all(" F\\.S\\.", "  ") %>%
      stringr::str_replace_all(" VAR\\.", "  ")

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
    data_length_12 <- data_cleaned[purrr::map(data_cleaned, length) == 12]
    data_length_13 <- data_cleaned[purrr::map(data_cleaned, length) == 13]
    data_length_14 <- data_cleaned[purrr::map(data_cleaned, length) == 14]
    data_length_15 <- data_cleaned[purrr::map(data_cleaned, length) == 15]
    # data_length_16 <- data_cleaned[purrr::map(data_cleaned, length) == 16]

    # treatment of DQs new 8/19
    suppressWarnings(DQ <-
                       data_cleaned[stringr::str_detect(data_cleaned, Time_Score_String, negate = TRUE) == TRUE])

    DQ_length_5 <- DQ[purrr::map(DQ, length) == 5]
    DQ_length_7 <- DQ[purrr::map(DQ, length) == 7]

    # #### seventeen variables
    # if (length(data_length_17) > 0) {
    #   suppressWarnings(
    #     df_17 <- data_length_17 %>%
    #       list_transform() %>%
    #       dplyr::select(
    #         Name = V1,
    #         Age = V2,
    #         Team = V3,
    #         Prelims_Time = V4,
    #         Prelims_Place = V5,
    #         Finals_Time = V6,
    #         Finals_Place = V7,
    #         Row_Numb = V17
    #       )
    #   )
    # } else {
    #   df_17 <- data.frame(Row_Numb = character(),
    #                       stringsAsFactors = FALSE)
    # }

    #### sixteen variables
    # if (length(data_length_16) > 0) {
    #   suppressWarnings(
    #     df_16 <- data_length_16 %>%
    #       list_transform() %>%
    #       dplyr::mutate(Finals_Place = stringr::str_remove_all(V7, " P"),
    #                     Finals_Place = dplyr::case_when(stringr::str_detect(Finals_Place, "\\d") == FALSE ~ paste0(Finals_Place, "10"),
    #                                                     TRUE ~ Finals_Place)) %>%
    #       dplyr::select(
    #         Name = V1,
    #         Age = V2,
    #         Team = V3,
    #         Prelims_Time = V4,
    #         Prelims_Place = V5,
    #         Finals_Time = V6,
    #         Finals_Place,
    #         Row_Numb = V16
    #       )
    #   )
    # } else {
    #   df_16 <- data.frame(Row_Numb = character(),
    #                       stringsAsFactors = FALSE)
    # }


    #### fifteen variables
    if (length(data_length_15) > 0) {
      suppressWarnings(
        df_15 <- data_length_15 %>%
          list_transform() %>%
          dplyr::mutate(Finals_Place = dplyr::case_when(stringr::str_detect(V7, "\\d") == FALSE ~ paste0(V7, "10"),
                                                 TRUE ~ V7)) %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE &
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V4,
              stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V8, Finals_Place_String) == TRUE &
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V6, Finals_Place_String) == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Prelims_Place = dplyr::case_when(
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE &
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V5,
              stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V8, Finals_Place_String) == TRUE &
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V6, Finals_Place_String) == TRUE ~ V6,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals_Time = dplyr::case_when(
              stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V8, Finals_Place_String) == TRUE ~ V7,
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE ~ V6,
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V4,
              stringr::str_detect(V6, "DQ") ~ "DQ",
              stringr::str_detect(V5, "DQ") ~ "DQ",
              stringr::str_detect(V4, "DQ") ~ "DQ",
              stringr::str_detect(V6, "NS") ~ "NS",
              stringr::str_detect(V5, "NS") ~ "NS",
              stringr::str_detect(V4, "NS") ~ "NS",
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals_Place = dplyr::case_when(
              stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V8, Finals_Place_String) == TRUE ~ V8,
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE ~ V7,
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Prelims_Time,
            Prelims_Place,
            Finals_Time,
            Finals_Place,
            Row_Numb = V15
          ) %>%
          dplyr::na_if("NA")
      )
    } else {
      df_15 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    #### fourteen variables
    if (length(data_length_14) > 0) {
      suppressWarnings(
        df_14 <- data_length_14 %>%
          list_transform() %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE &
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V4,
              stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V8, Finals_Place_String) == TRUE &
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V6, Finals_Place_String) == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Prelims_Place = dplyr::case_when(
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE &
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V5,
              stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V8, Finals_Place_String) == TRUE &
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V6, Finals_Place_String) == TRUE ~ V6,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals_Time = dplyr::case_when(
              stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V8, Finals_Place_String) == TRUE ~ V7,
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE ~ V6,
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V4,
              stringr::str_detect(V6, "DQ") ~ "DQ",
              stringr::str_detect(V5, "DQ") ~ "DQ",
              stringr::str_detect(V4, "DQ") ~ "DQ",
              stringr::str_detect(V6, "NS") ~ "NS",
              stringr::str_detect(V5, "NS") ~ "NS",
              stringr::str_detect(V4, "NS") ~ "NS",
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals_Place = dplyr::case_when(
              stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V8, Finals_Place_String) == TRUE ~ V8,
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE ~ V7,
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Prelims_Time,
            Prelims_Place,
            Finals_Time,
            Finals_Place,
            Row_Numb = V14
          ) %>%
          dplyr::na_if("NA")
      )
    } else {
      df_14 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    #### thirteen variables
    if (length(data_length_13) > 0) {
      suppressWarnings(
        df_13 <- data_length_13 %>%
          list_transform() %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE &
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V4,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Prelims_Place = dplyr::case_when(
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE &
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals_Time = dplyr::case_when(
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE ~ V6,
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V4,
              stringr::str_detect(V6, "DQ") ~ "DQ",
              stringr::str_detect(V5, "DQ") ~ "DQ",
              stringr::str_detect(V4, "DQ") ~ "DQ",
              stringr::str_detect(V6, "NS") ~ "NS",
              stringr::str_detect(V5, "NS") ~ "NS",
              stringr::str_detect(V4, "NS") ~ "NS",
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals_Place = dplyr::case_when(
              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE ~ V7,
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Prelims_Time,
            Prelims_Place,
            Finals_Time,
            Finals_Place,
            Row_Numb = V13
          ) %>%
          dplyr::na_if("NA")
      )
    } else {
      df_13 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    #### twelve variables
    if (length(data_length_12) > 0) {
      suppressWarnings(
        df_12 <- data_length_12 %>%
          list_transform() %>%
          dplyr::mutate(Finals_Place = dplyr::case_when(stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V5,
                                                        TRUE ~ "NA")) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Finals_Place,
            Finals_Time = V4,
            Row_Numb = V12
          ) %>%
          dplyr::na_if("NA")
      )
    } else {
      df_12 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### eleven variables
    if (length(data_length_11) > 0) {
      suppressWarnings(
        df_11 <- data_length_11 %>%
          list_transform() %>%
          dplyr::mutate(Prelims_Time = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                   stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V7, Time_Score_Specials_String) == FALSE ~ V4,
                                                 TRUE ~ "NA")) %>%
          dplyr::mutate(Prelims_Place = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                    stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                    stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                    stringr::str_detect(V7, Time_Score_Specials_String) == FALSE ~ V5,
                                                  TRUE ~ "NA")) %>%
          dplyr::mutate(Finals_Time = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                  stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                  # stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                  stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                                                  stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V4,
                                                  stringr::str_detect(V6, "DQ") ~ "DQ",
                                                  stringr::str_detect(V5, "DQ") ~ "DQ",
                                                  stringr::str_detect(V4, "DQ") ~ "DQ",
                                                  stringr::str_detect(V6, "NS") ~ "NS",
                                                  stringr::str_detect(V5, "NS") ~ "NS",
                                                  stringr::str_detect(V4, "NS") ~ "NS",
                                                TRUE ~ V6)) %>%
          dplyr::mutate(Finals_Place = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                   # stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V7, Finals_Place_String) == TRUE &
                                                   stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V7,
                                                 stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                   # stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V7, Finals_Place_String) == FALSE &
                                                   stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V5,
                                                 TRUE ~ "NA")) %>%
          dplyr::mutate(
            Finals_Place = dplyr::case_when(
              stringr::str_detect(V7, "\\d") == FALSE &
                stringr::str_detect(V7, Time_Score_Specials_String) == FALSE &
                stringr::str_detect(V7, "^F$|^B$|^C$") == TRUE ~ paste0(V7, "10"),
              TRUE ~ Finals_Place
            )
          ) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Prelims_Time,
            Prelims_Place,
            Finals_Time,
            Finals_Place,
            Row_Numb = V11
          ) %>%
          dplyr::na_if("NA")
      )
    } else {
      df_11 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }


    #### ten variables
    if (length(data_length_10) > 0) {
      suppressWarnings(
        df_10 <- data_length_10 %>%
          list_transform() %>%
          dplyr::mutate(Prelims_Time = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                   stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V7, Time_Score_Specials_String) == FALSE ~ V4,
                                                 TRUE ~ "NA")) %>%
          dplyr::mutate(Prelims_Place = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                    stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                    stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                    stringr::str_detect(V7, Time_Score_Specials_String) == FALSE ~ V5,
                                                  TRUE ~ "NA")) %>%
          dplyr::mutate(Finals_Time = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                  stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                  stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                                                  stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V4,
                                                  stringr::str_detect(V6, "DQ") ~ "DQ",
                                                  stringr::str_detect(V5, "DQ") ~ "DQ",
                                                  stringr::str_detect(V4, "DQ") ~ "DQ",
                                                  stringr::str_detect(V6, "NS") ~ "NS",
                                                  stringr::str_detect(V5, "NS") ~ "NS",
                                                  stringr::str_detect(V4, "NS") ~ "NS",
                                                TRUE ~ V6)) %>%
          dplyr::mutate(Finals_Place = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                   stringr::str_detect(V7, Finals_Place_String) == TRUE &
                                                   stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V7,
                                                 stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                   stringr::str_detect(V7, Finals_Place_String) == FALSE &
                                                   stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V5,
                                                 TRUE ~ "NA")) %>%
          dplyr::mutate(
            Finals_Place = dplyr::case_when(
              stringr::str_detect(V7, "\\d") == FALSE &
                stringr::str_detect(V7, Time_Score_Specials_String) == FALSE &
                stringr::str_detect(V7, "^F$|^B$|^C$") == TRUE ~ paste0(V7, "10"),
              TRUE ~ Finals_Place
            )
          ) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Prelims_Time,
            Prelims_Place,
            Finals_Time,
            Finals_Place,
            Row_Numb = V10
          ) %>%
          dplyr::na_if("NA")
      )
    } else {
      df_10 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    #### nine variables
    if (length(data_length_9) > 0) {
      suppressWarnings(
        df_9 <- data_length_9 %>%
          list_transform() %>%
          dplyr::mutate(Prelims_Time = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                   stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V7, Finals_Place_String) == TRUE ~ V4,
                                                 TRUE ~ "NA")) %>%
          dplyr::mutate(Prelims_Place = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                    stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                    stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                    stringr::str_detect(V7, Finals_Place_String) == TRUE ~ V5,
                                                  TRUE ~ "NA")) %>%
          dplyr::mutate(Finals_Time = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                  stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                  # stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                  stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                                                  stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V4,
                                                  stringr::str_detect(V6, "DQ") ~ "DQ",
                                                  stringr::str_detect(V5, "DQ") ~ "DQ",
                                                  stringr::str_detect(V4, "DQ") ~ "DQ",
                                                  stringr::str_detect(V6, "NS") ~ "NS",
                                                  stringr::str_detect(V5, "NS") ~ "NS",
                                                  stringr::str_detect(V4, "NS") ~ "NS",
                                                TRUE ~ V6)) %>%
          dplyr::mutate(Finals_Place = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                   # stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V7, Finals_Place_String) == TRUE &
                                                   stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V7,
                                                 stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V5, Finals_Place_String) == TRUE &
                                                   # stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                                                   stringr::str_detect(V7, Finals_Place_String) == FALSE &
                                                   stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V5,
                                                 TRUE ~ "NA")) %>%
          dplyr::mutate(
            Finals_Place = dplyr::case_when(
              stringr::str_detect(V7, "\\d") == FALSE &
                stringr::str_detect(V7, Time_Score_Specials_String) == FALSE &
                stringr::str_detect(V7, "^F$|^B$|^C$") == TRUE ~ paste0(V7, "10"),
              TRUE ~ Finals_Place
            )
          ) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Prelims_Time,
            Prelims_Place,
            Finals_Time,
            Finals_Place,
            Row_Numb = V9
          ) %>%
          dplyr::na_if("NA")
      )

    } else {
      df_9 <- data.frame(Row_Numb = character(),
                          stringsAsFactors = FALSE)
    }

    #### eight variables
    if (length(data_length_8) > 0) {
      suppressWarnings(
        df_8 <- data_length_8 %>%
          list_transform() %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Time_Score_Specials_String) == FALSE &
                stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Time_Score_Specials_String) == FALSE ~ V4,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Prelims_Place = dplyr::case_when(
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE &
                stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Time_Score_Specials_String) == FALSE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals_Time = dplyr::case_when(
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE &
                stringr::str_detect(V6, paste0(Time_Score_Specials_String, "|^[:upper:]$")) == TRUE &
                stringr::str_detect(V7, Time_Score_Specials_String) == TRUE ~ V4,
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE &
                stringr::str_detect(V6, paste0(Time_Score_Specials_String, "|^[:upper:]$")) == TRUE &
                stringr::str_detect(V7, Time_Score_Specials_String) == TRUE ~ V4,
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE &
                stringr::str_detect(V6, paste0(Time_Score_Specials_String, "|^[:upper:]$")) == TRUE &
                stringr::str_detect(V7, Time_Score_Specials_String) == FALSE ~ V6,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals_Place = dplyr::case_when(
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE &
                stringr::str_detect(V6, paste0(Time_Score_Specials_String, "|^[:upper:]$")) == TRUE &
                stringr::str_detect(V7, Time_Score_Specials_String) == TRUE ~ V5,
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE &
                stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V7, Finals_Place_String) == TRUE ~ V7,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals_Place = dplyr::case_when(
              stringr::str_detect(V7, "\\d") == FALSE &
                stringr::str_detect(V7, Time_Score_Specials_String) == FALSE &
                stringr::str_detect(V7, "^F$|^B$|^C$") == TRUE ~ paste0(V7, "10"),
              TRUE ~ Finals_Place
            )
          ) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Prelims_Time,
            Prelims_Place,
            Finals_Time,
            Finals_Place,
            Row_Numb = V8
          ) %>%
          dplyr::na_if("NA")
      )
    } else {
      df_8 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### seven variables
    if (length(data_length_7) > 0) {
      suppressWarnings(
        df_7 <- data_length_7 %>%
          list_transform() %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              stringr::str_detect(V6, "DQ|SCR") == TRUE ~ V4,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Prelims_Place = dplyr::case_when(
              stringr::str_detect(V6, "DQ|SCR") == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals_Time = dplyr::case_when(
              stringr::str_detect(V4, "DQ") == TRUE ~ "DQ",
              stringr::str_detect(V6, "DQ") == TRUE ~ "DQ",
              stringr::str_detect(V4, "SCR") == TRUE ~ "SCR",
              stringr::str_detect(V6, "SCR") == TRUE ~ "SCR",
              stringr::str_detect(V4, "NS") == TRUE ~ "NS",
              stringr::str_detect(V6, "NS") == TRUE ~ "NS",
              stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V4,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(Finals_Place = dplyr::case_when(
            stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
              stringr::str_detect(V5, Finals_Place_String) == TRUE ~ V5,
            TRUE ~ "NA"
          )) %>%
          dplyr::mutate(DQ = dplyr::case_when(
            stringr::str_detect(V4, "DQ") ~ 1,
            stringr::str_detect(V6, "DQ") ~ 1,
            TRUE ~ 0
          )) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Prelims_Time,
            Prelims_Place,
            Finals_Time,
            Finals_Place,
            DQ,
            Row_Numb = V7
          ) %>%
          dplyr::na_if("NA")
      )
    } else {
      df_7 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### six variables ####
    if (length(data_length_6) > 0) {
      suppressWarnings(
        df_6 <- data_length_6 %>%
          list_transform() %>%
          dplyr::mutate(Finals_Place = dplyr::case_when(stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                                                  stringr::str_detect(V5, "[FC]?^\\d{1,4}$") == TRUE ~ V5,
                                                TRUE ~ "NA")) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Finals_Time = V4,
            Finals_Place,
            Row_Numb = V6
          ) %>%
          dplyr::na_if("NA")
      )
    } else {
      df_6 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    #### five variables ####
    if (length(data_length_5) > 0) {
      suppressWarnings(
        df_5 <- data_length_5 %>%
          list_transform() %>%
          dplyr::mutate(DQ = dplyr::case_when(
            stringr::str_detect(V4, "DQ") ~ 1,
            TRUE ~ 0
          )) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Finals_Time = V4,
            DQ,
            Row_Numb = V5
          )
      )
    } else {
      df_5 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    # if (length(data_length_4) > 0) {
    #   suppressWarnings(
    #     df_4 <- data_length_4 %>%
    #       list_transform() %>%
    #       dplyr::mutate(DQ = dplyr::case_when(
    #         stringr::str_detect(V4, "DQ") ~ 1,
    #         TRUE ~ 0
    #       )) %>%
    #       dplyr::select(
    #         Name = V1,
    #         Age = V2,
    #         Team = V3,
    #         Finals_Time = V4,
    #         DQ,
    #         Row_Numb = V4
    #       )
    #   )
    # } else {
    #   df_4 <- data.frame(Row_Numb = character(),
    #                      stringsAsFactors = FALSE)
    # }

    #### DQ 5 ####
    if (length(DQ_length_5) > 0) {
      suppressWarnings(
        df_DQ_5 <- DQ_length_5 %>%
          list_transform() %>%
          dplyr::mutate(DQ = 1) %>%
          dplyr::select(
            Name = V1,
            Age = V2,
            Team = V3,
            Prelims_Time = V4,
            DQ,
            Row_Numb = V5
          )
      )
    } else {
      df_DQ_5 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
    }

    #### Rejoin dataframes from each number of variables ####
    Min_Row_Numb <- min(events$Event_Row_Min)
    suppressWarnings(
      data <- dplyr::bind_rows(df_15, df_14) %>%
        dplyr::bind_rows(df_13) %>%
        dplyr::bind_rows(df_12) %>%
        dplyr::bind_rows(df_11) %>%
        dplyr::bind_rows(df_10) %>%
        dplyr::bind_rows(df_9) %>%
        dplyr::bind_rows(df_8) %>%
        dplyr::bind_rows(df_7) %>%
        dplyr::bind_rows(df_6) %>%
        dplyr::bind_rows(df_5) %>%
        dplyr::left_join(df_DQ_5) %>%
        dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
        dplyr::arrange(Row_Numb) %>%
        dplyr::mutate(
          DQ = dplyr::case_when(
            stringr::str_detect(Finals_Time, "NS|DQ") == TRUE ~ 1,
            stringr::str_detect(Prelims_Time, "NS|DQ") == TRUE ~ 1,
            is.na(DQ) == TRUE ~ 0,
            TRUE ~ DQ
          )
        )
    )

    #### add in events based on row number ranges ####
    if (min(data$Row_Numb) < min(events$Event_Row_Min)) {
      unknown_event <- data.frame(
        Event = "Unknown",
        Event_Row_Min = min(data$Row_Numb),
        Event_Row_Max = min(events$Event_Row_Min) - 1
      )
      events <- dplyr::bind_rows(unknown_event, events)
    }

    data  <- data %>%
      transform(Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)]) %>%
      dplyr::filter( # remove occasional odd results reported outside of events
        Event != "Unknown" &
          stringr::str_detect(Team, "[:alpha:]") == TRUE &
          stringr::str_detect(Finals_Time, Time_Score_Specials_String) == TRUE
      ) %>%
      dplyr::na_if("Unknown") %>%
      dplyr::mutate(
        Event = stringr::str_remove_all(Event, " EVENT \\d{1,3}$"),
        Event = stringr::str_replace_all(Event, "F\\. ?R\\.", "FREE RELAY"),
        Event = stringr::str_replace_all(Event, "M\\. ?R\\.", "MEDLEY RELAY"),
        Event = stringr::str_replace_all(Event, "I\\. ?M\\.", "INDIVIDUAL MEDLEY")
      )

    #### Remove Empty Columns ####
    data <- Filter(function(x)
      ! all(is.na(x)), data)

    #### Correct places for consolation and finals ####
    if("Finals_Place" %in% names(data) == TRUE){
      suppressWarnings(
        C_Places <- data %>%
          dplyr::group_by(Event) %>%
          dplyr::mutate(Place_1 = dplyr::row_number()) %>%
          dplyr::filter(stringr::str_detect(Finals_Place, "C") == TRUE) %>%
          dplyr::summarize(Min_C_Place = as.integer(min(Place_1)),
                           Event = unique(Event))
      )

    suppressWarnings(
      data <- data %>%
        dplyr::left_join(C_Places, by = "Event") %>%
        dplyr::mutate(
          Min_C_Place = dplyr::case_when(
            is.na(Min_C_Place) == TRUE ~ as.integer(0),
            stringr::str_detect(Finals_Place, "F") ~ as.integer(0),
            TRUE ~ as.integer(Min_C_Place - 1)
          )
        ) %>%
        dplyr::mutate(
          Place = as.numeric(stringr::str_remove_all(Finals_Place, "[:alpha:]")),
          Place = dplyr::case_when(
            stringr::str_detect(Finals_Place, "C") ~ Place + Min_C_Place,
            stringr::str_detect(Finals_Place, "B") ~ Place + (Min_C_Place * 2),
            stringr::str_detect(Finals_Place, "F") ~ Place,
            TRUE ~ as.numeric(Finals_Place)
          )
        ) %>%
        dplyr::select(Place, dplyr::everything(),-Min_C_Place, -Prelims_Place, -Finals_Place, -Row_Numb)
    )

    remove(C_Places)

    } else { # for timed finals results without places
      suppressWarnings(
        data <- data %>%
          dplyr::group_by(Event) %>%
          dplyr::mutate(Place = as.numeric(factor(rank(
            sec_format(Finals_Time)
          )))) %>%
          dplyr::select(Place, dplyr::everything(), -Row_Numb)
      )
    }

    #### Cleaning ####
    if (format_samms == TRUE) {
      if("Prelims_Time" %!in% names(data)){
        data <- data %>%
          dplyr::mutate(Prelims_Time = "NA") %>%
          dplyr::mutate(Prelims_Time = dplyr::na_if(Prelims_Time, "NA"))
      }
      data <- format_results(data)
    }

    ### Ages ###
    # relay abbreviations often end up in age column
    data <- data %>%
      dplyr::mutate(Name = dplyr::case_when(stringr::str_detect(Age, Age_String) == FALSE ~ paste(Name, Age, sep = " "),
                                            TRUE ~ Name)) %>%
      dplyr::mutate(Age = dplyr::case_when(stringr::str_detect(Age, Age_String) == FALSE ~ "NA",
                                            TRUE ~ Age)) %>%
      dplyr::na_if("NA")

    ## Diving ##
    data <- data %>%
      dplyr::mutate(
        Finals_Time = dplyr::case_when(
          stringr::str_detect(Event, "DIVING|Diving") == TRUE ~ stringr::str_remove(Finals_Time, "\\:"),
          TRUE ~ Finals_Time
        )
      ) %>%
      dplyr::mutate(
        Prelims_Time = dplyr::case_when(
          stringr::str_detect(Event, "DIVING|Diving") == TRUE ~ stringr::str_remove(Prelims_Time, "\\:"),
          TRUE ~ Prelims_Time
        )
      )

    ## DQ ##
    # data_1 <- data %>%
    #   dplyr::mutate(DQ = dplyr::case_when(stringr::str_detect(Finals_Time, "DQ") == TRUE ~ 1,
    #                                stringr::str_detect(Prelims_Time, "DQ") == TRUE ~ 1,
    #                                # is.na(DQ) == TRUE ~ 0,
    #                                TRUE ~ DQ)) %>%
    #   dplyr::mutate(Finals_Time = dplyr::case_when(DQ > 0 ~ "NA",
    #                                                TRUE ~ Finals_Time)) %>%
    #   dplyr::na_if("NA")


    #### Remove Empty Columns ####
    data <- Filter(function(x)
      ! all(is.na(x)), data)

    #### this does work, to change listed Finals_Time for athletes who complete in Prelims and Finals to Prelims_Time for preliminary round.
    # However it leaves athletes who didn't make finals with their times as Finals_Times (also as desired)
    # I have kept this out to match how Hy-tek results are handled
    # Means athletes have a place for their performance in finals and another place, as another row, for their place in prelims

    # data <- data %>%
    #   dplyr::group_by(Name, Event) %>%
    #   dplyr::add_tally() %>%
    #   dplyr::ungroup() %>%
    #   dplyr::mutate(Switch = dplyr::case_when(is.na(Prelims_Time) == TRUE &
    #                                             n == 2 ~ "Y",
    #                                           TRUE ~ "N")) %>%
    #   dplyr::mutate(Prelims_Time = dplyr::case_when(Switch == "Y" ~ Finals_Time,
    #                                                 TRUE ~ Prelims_Time),
    #                 Prelims_Place = dplyr::case_when(Switch == "Y" ~ Finals_Place,
    #                                                  TRUE ~ Prelims_Place),
    #                 Finals_Time = dplyr::case_when(Switch == "Y" ~ "NA",
    #                                                TRUE ~ Finals_Time),
    #                 Finals_Place = dplyr::case_when(Switch == "Y" ~ "NA",
    #                                                 TRUE ~ Finals_Place)) %>%
    #  dplyr::na_if("NA") %>%
    #  dplyr::select(-Switch)


    return(data)
  }
