#' Sort data in lists of length 9 within \code{hytek_swim_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 9
#' @param brit_id_string a regex string for matching British swimming IDs
#' @param para_string a regex string for matching Paralympics classification
#'   strings
#' @param age_string a regex string for matching athlete ages
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{hytek_swim_parse}


hytek_length_9_sort <-
  function(x,
           brit_id_string = Brit_ID_String,
           para_string = Para_String,
           age_string = Age_String,
           time_score_specials_string = Time_Score_Specials_String) {

    if (length(x) > 0) {
      suppressWarnings(
        df_9 <- x %>%
          list_transform() %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            ID = dplyr::case_when(
              stringr::str_detect(V2, brit_id_string) == TRUE ~ V2,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Name = dplyr::case_when(
              stringr::str_detect(V2, brit_id_string) == TRUE ~ V3,
              TRUE ~ V2
            )
          ) %>%
          dplyr::mutate(
            Para = dplyr::case_when(stringr::str_detect(V3, para_string) == TRUE ~ V3,
                                    TRUE ~ "NA"),
            Age = dplyr::case_when(
              stringr::str_detect(V3, age_string) == TRUE ~ V3,
              stringr::str_detect(V4, age_string) == TRUE ~ V4,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Team = dplyr::case_when(
              stringr::str_detect(V3, age_string) == TRUE ~ V4,
              stringr::str_detect(V3, para_string) == TRUE &
                stringr::str_detect(V4, age_string) == TRUE ~ V5,
              stringr::str_detect(V2, brit_id_string) == TRUE &
                stringr::str_detect(V4, age_string) == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Prelims = dplyr::case_when(
              stringr::str_detect(V4, time_score_specials_string) == TRUE &
                stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V4,
              stringr::str_detect(V5, time_score_specials_string) == TRUE &
                stringr::str_detect(V6, time_score_specials_string) == TRUE ~ V5,
              stringr::str_detect(V6, time_score_specials_string) == TRUE &
                stringr::str_detect(V7, time_score_specials_string) == TRUE ~ V6,
              stringr::str_detect(V7, time_score_specials_string) == TRUE &
                stringr::str_detect(V8, time_score_specials_string) == TRUE ~ V7,
              TRUE ~ "NA"
            ),
            Finals = dplyr::case_when(
              stringr::str_detect(V4, time_score_specials_string) == TRUE &
                stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V5,
              stringr::str_detect(V5, time_score_specials_string) == TRUE &
                stringr::str_detect(V6, time_score_specials_string) == TRUE ~ V6,
              stringr::str_detect(V6, time_score_specials_string) == TRUE &
                stringr::str_detect(V7, time_score_specials_string) == TRUE ~ V7,
              stringr::str_detect(V7, time_score_specials_string) == TRUE &
                stringr::str_detect(V8, time_score_specials_string) == TRUE ~ V8,
              TRUE ~ "NA"
            ),
            Points = dplyr::case_when(
              stringr::str_detect(V6, time_score_specials_string) == TRUE &
                stringr::str_detect(V7, time_score_specials_string) == TRUE &
                stringr::str_detect(V8, "^\\d{1,4}\\.?\\d?\\d?$") == TRUE ~ V8,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::na_if("NA") %>%
          dplyr::select(
            Place = V1,
            ID,
            Name,
            Para,
            Age,
            Team,
            Prelims,
            Finals,
            Points,
            Row_Numb = V9
          )
      )
    } else {
      df_9 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    return(df_9)
  }

#' Sort data in lists of length 8 within \code{hytek_swim_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 8
#' @param brit_id_string a regex string for matching British swimming IDs
#' @param para_string a regex string for matching Paralympics classification
#'   strings
#' @param age_string a regex string for matching athlete ages
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{hytek_swim_parse}


hytek_length_8_sort <-
  function(x,
           brit_id_string = Brit_ID_String,
           para_string = Para_String,
           age_string = Age_String,
           time_score_specials_string = Time_Score_Specials_String) {

    if (length(x) > 0) {
      suppressWarnings(
        df_8 <- x %>%
          list_transform() %>%
          filter(stringr::str_detect(V1, "\\.") == FALSE) %>% # occasionally old results with DQs in the splits will end up here - this removes them
          dplyr::na_if("") %>%
          dplyr::mutate(ID = dplyr::case_when(
            stringr::str_detect(V2, brit_id_string) == TRUE ~ V2,
            TRUE ~ "NA"
          )) %>%
          dplyr::mutate(Name = dplyr::case_when(
            stringr::str_detect(V2, brit_id_string) == TRUE ~ V3,
            TRUE ~ V2
          )) %>%
          dplyr::mutate(
            Para = dplyr::case_when(stringr::str_detect(V3, para_string) == TRUE ~ V3,
                                    TRUE ~ "NA"),
            Age = dplyr::case_when(
              stringr::str_detect(V3, age_string) == TRUE ~ V3,
              stringr::str_detect(V4, age_string) == TRUE ~ V4,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Team = dplyr::case_when(
              stringr::str_detect(V3, age_string) == TRUE ~ V4,
              stringr::str_detect(V3, para_string) == TRUE &
                stringr::str_detect(V4, age_string) == TRUE ~ V5,
              stringr::str_detect(V2, brit_id_string) == TRUE &
                stringr::str_detect(V4, age_string) == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Prelims = dplyr::case_when(
              stringr::str_detect(V4, time_score_specials_string) == TRUE &
                stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V4,
              stringr::str_detect(V5, time_score_specials_string) == TRUE &
                stringr::str_detect(V6, time_score_specials_string) == TRUE ~ V5,
              stringr::str_detect(V6, time_score_specials_string) == TRUE &
                stringr::str_detect(V7, time_score_specials_string) == TRUE ~ V6,
              TRUE ~ "NA"
            ),
            Finals = dplyr::case_when(
              stringr::str_detect(V4, time_score_specials_string) == TRUE &
                stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V5,
              stringr::str_detect(V5, time_score_specials_string) == TRUE &
                stringr::str_detect(V6, time_score_specials_string) == TRUE ~ V6,
              stringr::str_detect(V6, time_score_specials_string) == TRUE &
                stringr::str_detect(V7, time_score_specials_string) == TRUE ~ V7,
              TRUE ~ "NA"
            ),
            Points = dplyr::case_when(
              stringr::str_detect(V5, time_score_specials_string) == TRUE &
                stringr::str_detect(V6, time_score_specials_string) == TRUE &
                stringr::str_detect(V7, "^\\d{1,4}\\.?\\d?\\d?$") == TRUE ~ V7,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::na_if("NA") %>%
          dplyr::select(
            Place = V1,
            ID,
            Name,
            Age,
            Para,
            Team,
            Prelims,
            Finals,
            Points,
            Row_Numb = V8
          )
      )
    } else {
      df_8 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    return(df_8)
  }


#' Sort data in lists of length 7 within \code{hytek_swim_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 7
#' @param brit_id_string a regex string for matching British swimming IDs
#' @param para_string a regex string for matching Paralympics classification
#'   strings
#' @param age_string a regex string for matching athlete ages
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{hytek_swim_parse}


hytek_length_7_sort <-
  function(x,
           brit_id_string = Brit_ID_String,
           para_string = Para_String,
           age_string = Age_String,
           time_score_specials_string = Time_Score_Specials_String) {

    if (length(x) > 0) {
      suppressWarnings(
        df_7 <- x %>%
          list_transform() %>%
          dplyr::mutate(
            Place = dplyr::case_when(
              stringr::str_detect(V1, "^\\d{1,3}\\)?$") == TRUE ~ V1,
              stringr::str_detect(V1, brit_id_string) == TRUE ~ "NA",
              TRUE ~ "10000"
            )
          ) %>%
          dplyr::mutate(
            ID = dplyr::case_when(
              stringr::str_detect(V1, brit_id_string) == TRUE ~ V1,
              stringr::str_detect(V2, brit_id_string) == TRUE ~ V2,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Name = dplyr::case_when(
              stringr::str_detect(V1, brit_id_string) == TRUE &
                stringr::str_detect(V2, age_string) == TRUE ~ "NA",
              stringr::str_detect(V2, brit_id_string) == TRUE ~ V3,
              TRUE ~ V2
            )
          ) %>%
          dplyr::mutate(
            Para = dplyr::case_when(stringr::str_detect(V3, para_string) == TRUE ~ V3,
                                    TRUE ~ "NA"),
            Age = dplyr::case_when(
              stringr::str_detect(V2, age_string) == TRUE ~ V2,
              stringr::str_detect(V3, age_string) == TRUE ~ V3,
              stringr::str_detect(V4, age_string) == TRUE ~ V4,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Team = dplyr::case_when(
              stringr::str_detect(V2, age_string) == TRUE ~ V3,
              stringr::str_detect(V3, age_string) == TRUE ~ V4,
              stringr::str_detect(V3, age_string) == FALSE &
                stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
              stringr::str_detect(V3, age_string) == TRUE ~ V4,
              stringr::str_detect(V3, para_string) == TRUE &
                stringr::str_detect(V4, age_string) == TRUE ~ V5,
              stringr::str_detect(V2, brit_id_string) == TRUE &
                stringr::str_detect(V4, age_string) == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Prelims = dplyr::case_when(
              stringr::str_detect(V4, time_score_specials_string) == TRUE &
                stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V4,
              stringr::str_detect(V5, time_score_specials_string) == TRUE &
                stringr::str_detect(V6, time_score_specials_string) == TRUE ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Finals = dplyr::case_when(
              stringr::str_detect(V4, time_score_specials_string) == TRUE &
                stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V5,
              stringr::str_detect(V5, time_score_specials_string) == TRUE &
                stringr::str_detect(V6, time_score_specials_string) == FALSE ~ V5,
              stringr::str_detect(V5, time_score_specials_string) == TRUE &
                stringr::str_detect(V6, time_score_specials_string) == TRUE ~ V6,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::mutate(
            Points = dplyr::case_when(
              stringr::str_detect(V5, Finals) == TRUE &
                stringr::str_detect(V6, "^5\\d\\.|^4\\d\\.|^3\\d\\.|^2\\d\\.") == FALSE &
                stringr::str_detect(V6, "^\\d{1,2}\\.?\\d?\\d?") ~ V6,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::na_if("NA") %>%
          dplyr::select(
            Place,
            ID,
            Para,
            Name,
            Age,
            Team,
            Prelims,
            Finals,
            Points,
            Row_Numb = V7
          )
      )

    } else {
      df_7 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    return(df_7)
  }


#' Sort data in lists of length 6 within \code{hytek_swim_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 6
#'   strings
#' @param name_string a regex string for matching athlete names
#' @param age_string a regex string for matching athlete ages
#' @param para_string a regex string for matching Paralympics classification
#'   strings
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{hytek_swim_parse}


hytek_length_6_sort <-
  function(x,
           name_string = Name_String,
           age_string = Age_String,
           para_string = Para_String,
           time_score_specials_string = Time_Score_Specials_String) {

    if (length(x) > 0) {
      suppressWarnings(
        df_6 <- x %>%
          list_transform() %>%
          dplyr::mutate(
            Place = dplyr::case_when(stringr::str_detect(V1, "^\\d{1,3}\\)?$") == TRUE ~ V1,
                                     TRUE ~ "10000"),
            Name = dplyr::case_when(
              stringr::str_detect(V2, name_string) == TRUE &
                stringr::str_detect(V3, time_score_specials_string) == FALSE ~ V2,
              TRUE ~ "NA"
            ),
            Age = dplyr::case_when(stringr::str_detect(V3, age_string) == TRUE ~ V3,
                                   TRUE ~ "NA"),
            Para = dplyr::case_when(stringr::str_detect(V3, para_string) == TRUE ~ V3,
                                    TRUE ~ "NA"),
            Team = dplyr::case_when(
              stringr::str_detect(V3, time_score_specials_string) == TRUE ~ V2,
              stringr::str_detect(V3, time_score_specials_string) == FALSE &
                stringr::str_detect(V3, para_string) == FALSE &
                stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
              stringr::str_detect(V3, age_string) == TRUE ~ V4,
              TRUE ~ "NA"
            ),
            Prelims = dplyr::case_when(
              stringr::str_detect(V3, time_score_specials_string) == TRUE &
                stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
              stringr::str_detect(V4, time_score_specials_string) == TRUE &
                stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V4,
              TRUE ~ "NA"
            ),
            Finals = dplyr::case_when(
              stringr::str_detect(V3, time_score_specials_string) == TRUE &
                stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V4,
              stringr::str_detect(V4, time_score_specials_string) == TRUE &
                stringr::str_detect(V5, time_score_specials_string) == FALSE ~ V4,
              stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V5,
              TRUE ~ "NA"
            ),
            Points = dplyr::case_when(
              stringr::str_detect(V4, Finals) == TRUE &
                stringr::str_detect(V5, "^5\\d\\.|^4\\d\\.|^3\\d\\.|^2\\d\\.") == FALSE &
                stringr::str_detect(V5, "^\\d{1,2}\\.?\\d?\\d?") ~ V5,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::na_if("NA") %>%
          dplyr::select(Place,
                        Name,
                        Age,
                        Para,
                        Team,
                        Prelims,
                        Finals,
                        Points,
                        Row_Numb = V6)
      )
    } else {
      df_6 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    return(df_6)
  }

#' Sort data in lists of length 5 within \code{hytek_swim_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 5
#'   strings
#' @param name_string a regex string for matching athlete names
#' @param age_string a regex string for matching athlete ages
#' @param para_string a regex string for matching Paralympics classification
#'   strings
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{hytek_swim_parse}


hytek_length_5_sort <-
  function(x,
           name_string = Name_String,
           age_string = Age_String,
           para_string = Para_String,
           time_score_specials_string = Time_Score_Specials_String) {

    if (length(x) > 0) {
      suppressWarnings(
        df_5 <- x %>%
          list_transform() %>%
          dplyr::mutate(
            Place = dplyr::case_when(stringr::str_detect(V1, "^\\d{1,3}\\)?$") == TRUE ~ V1,
                                     TRUE ~ "NA"),
            Name = dplyr::case_when(
              stringr::str_detect(V1, name_string) == TRUE &
                stringr::str_detect(V2, time_score_specials_string) == FALSE ~ V1,
              stringr::str_detect(V2, name_string) == TRUE &
                stringr::str_detect(V3, time_score_specials_string) == FALSE ~ V2,
              TRUE ~ "NA"
            ),
            Age = dplyr::case_when(stringr::str_detect(V2, age_string) == TRUE ~ V2,
                                   stringr::str_detect(V3, age_string) == TRUE ~ V3,
                                   TRUE ~ "NA"),
            Para = dplyr::case_when(stringr::str_detect(V2, para_string) == TRUE ~ V2,
                                    stringr::str_detect(V3, para_string) == TRUE ~ V3,
                                    TRUE ~ "NA"),
            Team = dplyr::case_when(
              stringr::str_detect(V3, time_score_specials_string) == TRUE &
                stringr::str_detect(V2, para_string) == FALSE  ~ V2,
              stringr::str_detect(V3, time_score_specials_string) == FALSE &
                stringr::str_detect(V3, para_string) == FALSE ~ V3,
              TRUE ~ "NA"
            ),
            Prelims = dplyr::case_when(
              stringr::str_detect(V3, time_score_specials_string) == TRUE &
                stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
              TRUE ~ "NA"
            ),
            Finals = dplyr::case_when(
              stringr::str_detect(V3, time_score_specials_string) == TRUE &
                stringr::str_detect(V4, time_score_specials_string) == FALSE ~ V3,
              stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V4,
              TRUE ~ "NA"
            )
          ) %>%

          dplyr::na_if("NA") %>%
          dplyr::select(
            Place,
            Name,
            Age,
            Para,
            Team,
            Prelims,
            Finals,
            Row_Numb = V5
          )
      )
    } else {
      df_5 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }


    return(df_5)
  }


#' Sort data in lists of length 4 within \code{hytek_swim_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 4
#'   strings
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{hytek_swim_parse}


hytek_length_4_sort <-
  function(x,
           time_score_specials_string = Time_Score_Specials_String) {

    if (length(x) > 0) {
      suppressWarnings(
        df_4 <- x %>%
          list_transform() %>%
          filter(stringr::str_detect(V1, "\\.") == FALSE) %>% # occasionally old results with DQs in the splits will end up here - this removes them
          dplyr::mutate(
            Place = dplyr::case_when(stringr::str_detect(V1, "^\\d{1,3}\\)?$") == TRUE ~ V1,
                                     TRUE ~ "10000"),
            Team = dplyr::case_when(
              stringr::str_detect(V1, "\\d") == FALSE ~ V1,
              stringr::str_detect(V1, "\\d") == TRUE &
                stringr::str_detect(V2, "[:alpha:]{2,}") == TRUE ~ V2,
              TRUE ~ "NA"
            ),
            Prelims = dplyr::case_when(
              stringr::str_detect(V2, time_score_specials_string) == TRUE &
                stringr::str_detect(V3, time_score_specials_string) == TRUE ~ V2,
              TRUE ~ "NA"
            ),
            Finals = dplyr::case_when(
              stringr::str_detect(V3, time_score_specials_string) == TRUE ~ V3,
              TRUE ~ "NA"
            )
          ) %>%
          dplyr::na_if("") %>%
          dplyr::select(Place,
                        Team,
                        Prelims,
                        Finals,
                        Row_Numb = V4)
      )
    } else {
      df_4 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }


    return(df_4)
  }

#' Sort data in lists of length 3 within \code{hytek_swim_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param x a list of lists with all sub-lists having length 3
#'   strings
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{hytek_swim_parse}


hytek_length_3_sort <-
  function(x) {

    if (length(x) > 0) {
      suppressWarnings(
        df_3 <- x %>%
          list_transform() %>%
          dplyr::mutate(Place = "10000") %>%
          dplyr::select(
            Place,
            Team = V1,
            Finals = V2,
            Row_Numb = V3
          )
      )
    } else {
      df_3 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }


    return(df_3)
  }

#' Sort data in DQ lists of length 4 within \code{hytek_swim_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param x a list of lists containing DQ results with all sub-lists having
#'   length 4 strings
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{hytek_swim_parse}


hytek_length_4_DQ_sort <-
  function(x) {

    if (length(x) > 0) {
      suppressWarnings(
        df_DQ_4 <- x %>%
          list_transform() %>%
          dplyr::mutate(Place = "10000") %>%
          dplyr::select(
            Place = V1,
            Team = V2,
            Finals = V3,
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


    return(df_DQ_4)
  }

#' Sort data in DQ lists of length 3 within \code{hytek_swim_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param x a list of lists containing DQ results with all sub-lists having
#'   length 3 strings
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{hytek_swim_parse}


hytek_length_3_DQ_sort <-
  function(x) {

    if (length(x) > 0) {
      suppressWarnings(
        df_DQ_3 <- x %>%
          list_transform() %>%
          dplyr::mutate(Place = "10000") %>%
          dplyr::select(
            Place,
            Team = V1,
            Finals = V2,
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


    return(df_DQ_3)
  }
