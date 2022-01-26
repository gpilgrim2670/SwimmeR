#' Sorts data in lists of length 12 within \code{splash_swim_parse}
#'
#' XXXXXX
#'
#' @importFrom dplyr select
#'
#' @param x a list of lists with all sub-lists having length 12
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{splash_swim_parse}


splash_length_12_sort <- function(x) {

  if (length(x) > 0) {
    suppressWarnings(
      df_12 <- x %>%
        list_transform() %>%
        dplyr::select(
          Place = V1,
          Name = V2,
          Age = V3,
          Team = V4,
          Finals = V5,
          Points = V6,
          Reaction_Time = V7,
          Split_1 = V8,
          Split_2 = V9,
          Split_3 = V10,
          Split_4 = V11,
          Row_Numb = V12
        )
    )
  } else {

    df_12 <- data.frame(Row_Numb = character(),
                        stringsAsFactors = FALSE)
  }
}


#' Sorts data in lists of length 11 within \code{splash_swim_parse}
#'
#' XXXXXX
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 11
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{splash_swim_parse}


splash_length_11_sort <- function(x,
                                  time_score_specials_string = Time_Score_Specials_String){

  if (length(x) > 0) {
    suppressWarnings(
      df_11 <- x %>%
        list_transform() %>%
        dplyr::mutate(
          Age = dplyr::case_when(
            stringr::str_detect(V3, "\\d\\d") == TRUE &
              stringr::str_detect(V3, "[:alpha:]|\\.") == FALSE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Team = dplyr::case_when(
            stringr::str_detect(V3, Age) == TRUE ~ V4,
            stringr::str_detect(V3, "[:alpha:]") == TRUE &
              stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Finals = dplyr::case_when(
            str_detect(V4, time_score_specials_string) == TRUE ~ V4,
            stringr::str_detect(V4, time_score_specials_string) == FALSE &
              stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V5,
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
        dplyr::mutate(
          Reaction_Time = dplyr::case_when(
            stringr::str_detect(V6, "[\\+|\\-]\\d\\.\\d{2}") == TRUE ~ V6,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::select(
          Place = V1,
          Name = V2,
          Age,
          Team,
          Finals,
          Points,
          Reaction_Time,
          Split_1 = V7,
          Split_2 = V8,
          Split_3 = V9,
          Split_4 = V10,
          Row_Numb = V11
        )
    )
  } else {
    df_11 <- data.frame(Row_Numb = character(),
                        stringsAsFactors = FALSE)

    return(df_11)
  }
}

#' Sorts data in lists of length 10 within \code{splash_swim_parse}
#'
#' XXXXXX
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 10
#' @param time_score_string a regex string for matching results (times and
#'   scores) but not special strings like DQ
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{splash_swim_parse}


splash_length_10_sort <- function(x,
                                  time_score_string = Time_Score_String,
                                  time_score_specials_string = Time_Score_Specials_String){
  if (length(x) > 0) {

    suppressWarnings(
      df_10 <- x %>%
        list_transform() %>%
        dplyr::mutate(
          Age = dplyr::case_when(
            stringr::str_detect(V3, "\\d\\d\\d?") == TRUE &
              stringr::str_detect(V3, "[:alpha:]|\\.") == FALSE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Team = dplyr::case_when(
            stringr::str_detect(V3, Age) == TRUE ~ V4,
            stringr::str_detect(V3, "[:alpha:]") == TRUE &
              stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Finals = dplyr::case_when(
            str_detect(V3, time_score_specials_string) == TRUE &
              str_detect(V4, time_score_specials_string) == FALSE ~ V3,
            str_detect(V4, time_score_specials_string) == TRUE ~ V4,
            stringr::str_detect(V4, time_score_specials_string) == FALSE &
              stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V5,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Points = dplyr::case_when(
            stringr::str_detect(V4, "\\d+") == TRUE &
              stringr::str_detect(V4, "\\.") == FALSE ~ V4,
            stringr::str_detect(V5, "\\d+") == TRUE &
              stringr::str_detect(V5, "\\.") == FALSE ~ V5,
            stringr::str_detect(V6, "\\d+") == TRUE &
              stringr::str_detect(V6, "\\.") == FALSE ~ V6,
            stringr::str_detect(V9, "\\d+") == TRUE &
              stringr::str_detect(V9, "\\.") == FALSE ~ V9,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Reaction_Time = dplyr::case_when(
            stringr::str_detect(V5, "[\\+|\\-]\\d\\.\\d{2}") == TRUE ~ V5,
            stringr::str_detect(V6, "[\\+|\\-]\\d\\.\\d{2}") == TRUE ~ V6,
            stringr::str_detect(V7, "[\\+|\\-]\\d\\.\\d{2}") == TRUE ~ V7,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Split_1 = dplyr::case_when(
            V5 == Points & V4 == Finals &
              stringr::str_detect(V6, time_score_string) == TRUE ~ V6,
            V4 == Points &
              V5 == Reaction_Time &
              stringr::str_detect(V6, time_score_string) == TRUE ~ V6,
            V7 == Reaction_Time &
              stringr::str_detect(V8, time_score_string) == TRUE ~ V8,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Split_2 = dplyr::case_when(
            V5 == Points & V4 == Finals &
              V6 == Split_1 &
              stringr::str_detect(V7, time_score_string) == TRUE ~ V7,
            V4 == Points &
              V5 == Reaction_Time &
              V6 == Split_1 &
              stringr::str_detect(V7, time_score_string) == TRUE ~ V7,
            V7 == Reaction_Time &
              V8 == Split_1 &
              stringr::str_detect(V9, time_score_string) == TRUE ~ V9,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Split_3 = dplyr::case_when(
            V5 == Points & V4 == Finals &
              V7 == Split_2 &
              stringr::str_detect(V8, time_score_string) == TRUE ~ V8,
            V4 == Points &
              V5 == Reaction_Time &
              V7 == Split_2 &
              stringr::str_detect(V8, time_score_string) == TRUE ~ V8,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Split_4 = dplyr::case_when(
            V5 == Points & V4 == Finals &
              V8 == Split_3 &
              stringr::str_detect(V9, time_score_string) == TRUE ~ V9,
            V4 == Points &
              V5 == Reaction_Time &
              V8 == Split_3 &
              stringr::str_detect(V9, time_score_string) == TRUE ~ V9,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::select(
          Place = V1,
          Name = V2,
          Age,
          Team,
          Finals,
          Points,
          Reaction_Time,
          Split_1,
          Split_2,
          Split_3,
          Split_4,
          Row_Numb = V10
        )
    )
  } else {
    df_10 <- data.frame(Row_Numb = character(),
                        stringsAsFactors = FALSE)
  }

  return(df_10)

  }

#' Sorts data in lists of length 9 within \code{spash_swim_parse}
#'
#' XXXXXX
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 9
#' @param heat_lane_string a regex string for matching heat-lane pairs
#' @param time_score_string a regex string for matching results (times and
#'   scores) but not special strings like DQ
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{splash_swim_parse}


splash_length_9_sort <- function(x,
                                 heat_lane_string = Heat_Lane_String,
                                 time_score_string = Time_Score_String,
                                 time_score_specials_string = Time_Score_Specials_String) {

  if (length(x) > 0) {
    suppressWarnings(
      df_9 <- x %>%
        list_transform() %>%
        dplyr::mutate(
          Age = dplyr::case_when(
            stringr::str_detect(V3, "^1?\\d\\d$") == TRUE &
              stringr::str_detect(V3, "[:alpha:]|\\.") == FALSE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Heat_Lane = dplyr::case_when(
            stringr::str_detect(V4, heat_lane_string) == TRUE ~ V4,
            stringr::str_detect(V5, heat_lane_string) == TRUE ~ V5,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Team = dplyr::case_when(
            stringr::str_detect(V3, Age) == TRUE &
              stringr::str_detect(V4, heat_lane_string) == FALSE &
              stringr::str_detect(V4, "[\\+|\\-]\\d\\.\\d{2}") == FALSE &
              stringr::str_length(V4) > 1 ~ V4,
            stringr::str_detect(V3, "^\\d\\s?") == TRUE &
              stringr::str_detect(V4, heat_lane_string) == FALSE &
              stringr::str_detect(V4, "[\\+|\\-]\\d\\.\\d{2}") == FALSE &
              stringr::str_length(V4) > 1 ~ V4,
            stringr::str_detect(V3, "[:alpha:]") == TRUE &
              stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
            stringr::str_detect(V4, heat_lane_string) == TRUE &
              stringr::str_detect(V5, time_score_specials_string) == FALSE ~ V5,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Prelims = dplyr::case_when(
            stringr::str_detect(V5, time_score_specials_string) == FALSE &
              stringr::str_detect(V6, time_score_specials_string) == TRUE &
              stringr::str_detect(V7, time_score_specials_string) == TRUE &
              stringr::str_detect(V5, time_score_specials_string) == FALSE &
              stringr::str_detect(V8, time_score_specials_string) == FALSE ~ V6,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Finals = dplyr::case_when(
            stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V4,
            stringr::str_detect(V4, time_score_specials_string) == FALSE &
              stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V5,
            stringr::str_detect(V5, time_score_specials_string) == FALSE &
              stringr::str_detect(V6, time_score_specials_string) == TRUE ~ V6,
            stringr::str_detect(V6, time_score_specials_string) == TRUE &
              stringr::str_detect(V7, time_score_specials_string) == TRUE &
              stringr::str_detect(V5, time_score_specials_string) == FALSE &
              stringr::str_detect(V8, time_score_specials_string) == FALSE ~ V7,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Points = dplyr::case_when(
            stringr::str_detect(V5, "\\d+") == TRUE &
              stringr::str_detect(V5, "\\.") == FALSE ~ V5,
            stringr::str_detect(V6, "\\d+") == TRUE &
              stringr::str_detect(V6, "\\.") == FALSE ~ V6,
            stringr::str_detect(V7, "\\d+") == TRUE &
              stringr::str_detect(V7, "\\.") == FALSE ~ V7,
            stringr::str_detect(V8, "\\d+") == TRUE &
              stringr::str_detect(V8, "\\.") == FALSE ~ V8,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Reaction_Time = dplyr::case_when(
            stringr::str_detect(V6, "[\\+|\\-]\\d\\.\\d{2}") == TRUE ~ V6,
            stringr::str_detect(V7, "[\\+|\\-]\\d\\.\\d{2}") == TRUE ~ V7,
            stringr::str_detect(V8, "[\\+|\\-]\\d\\.\\d{2}") == TRUE ~ V8,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Split_1 = dplyr::case_when(
            V6 %in% c(Points, Reaction_Time) & V5 == Finals &
              stringr::str_detect(V7, time_score_string) == TRUE ~ V7,
            V5 == Points &
              V6 == Reaction_Time & V4 == Finals &
              stringr::str_detect(V7, time_score_string) == TRUE ~ V7,

            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Split_2 = dplyr::case_when(
            V6 %in% c(Points, Reaction_Time) & V5 == Finals &
              V7 == Split_1 &
              stringr::str_detect(V8, time_score_string) == TRUE ~ V8,
            V5 == Points &
              V6 == Reaction_Time & V4 == Finals &
              V7 == Split_1 &
              stringr::str_detect(V8, time_score_string) == TRUE ~ V8,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::select(
          Place = V1,
          Name = V2,
          Age,
          Heat_Lane,
          Team,
          Prelims,
          Finals,
          Points,
          Reaction_Time,
          Split_1,
          Split_2,
          Row_Numb = V9
        )
    )
  } else {

    df_9 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  return(df_9)
}

#' Sorts data in lists of length 8 within \code{spash_swim_parse}
#'
#' XXXXXX
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 8
#' @param time_score_string a regex string for matching results (times and
#'   scores) but not special strings like DQ
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{splash_swim_parse}


splash_length_8_sort <- function(x,
                                 time_score_string = Time_Score_String,
                                 time_score_specials_string = Time_Score_Specials_String){
  if (length(x) > 0) {

    suppressWarnings(
      df_8 <- x %>%
        list_transform() %>%
        dplyr::mutate(
          Age = dplyr::case_when(
            stringr::str_detect(V3, "\\d\\d") == TRUE &
              stringr::str_detect(V3, "[:alpha:]|\\.") == FALSE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Team = dplyr::case_when(
            stringr::str_detect(V3, Age) == TRUE ~ V4,
            stringr::str_detect(V3, "^\\d\\s?") == TRUE ~ V4,
            stringr::str_detect(V3, "[:alpha:]") == TRUE &
              stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
            stringr::str_detect(V3, "[:alpha:]") == TRUE &
              stringr::str_detect(V4, "^[:upper:]+$") == TRUE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Prelims = dplyr::case_when(
            stringr::str_detect(V5, time_score_specials_string) == TRUE &
              stringr::str_detect(V6, time_score_specials_string) == TRUE &
              stringr::str_detect(V4, time_score_specials_string) == FALSE &
              stringr::str_detect(V7, time_score_specials_string) == FALSE ~ V5,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Finals = dplyr::case_when(
            str_detect(V4, time_score_specials_string) == TRUE ~ V4,
            stringr::str_detect(V5, time_score_specials_string) == TRUE &
              stringr::str_detect(V4, time_score_specials_string) == FALSE &
              stringr::str_detect(V6, time_score_specials_string) == FALSE ~ V5,
            stringr::str_detect(V5, time_score_specials_string) == TRUE &
              stringr::str_detect(V6, time_score_specials_string) == TRUE &
              stringr::str_detect(V4, time_score_specials_string) == FALSE &
              stringr::str_detect(V7, time_score_specials_string) == FALSE ~ V6,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Points = dplyr::case_when(
            stringr::str_detect(V5, "\\d+") == TRUE &
              stringr::str_detect(V5, "\\.") == FALSE ~ V5,
            stringr::str_detect(V6, "\\d+") == TRUE &
              stringr::str_detect(V6, "\\.") == FALSE ~ V6,
            stringr::str_detect(V7, "\\d+") == TRUE &
              stringr::str_detect(V7, "\\.") == FALSE ~ V7,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Reaction_Time = dplyr::case_when(
            stringr::str_detect(V6, "[\\+|\\-]\\d\\.\\d{2}") == TRUE ~ V6,
            stringr::str_detect(V7, "[\\+|\\-]\\d\\.\\d{2}") == TRUE ~ V7,
            stringr::str_detect(V8, "[\\+|\\-]\\d\\.\\d{2}") == TRUE ~ V8,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Split_1 = dplyr::case_when(
            V5 == Points & V4 == Finals &
              stringr::str_detect(V6, time_score_string) == TRUE ~ V6,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Split_2 = dplyr::case_when(
            V5 == Points & V4 == Finals &
              V6 == Split_1 &
              stringr::str_detect(V7, time_score_string) == TRUE ~ V7,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::select(
          Place = V1,
          Name = V2,
          Age,
          Team,
          Prelims,
          Finals,
          Points,
          Reaction_Time,
          Split_1,
          Split_2,
          Row_Numb = V8
        )
    )
  } else {

    df_8 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  return(df_8)

}

#' Sorts data in lists of length 7 within \code{spash_swim_parse}
#'
#' XXXXXX
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 7
#' @param time_score_string a regex string for matching results (times and
#'   scores) but not special strings like DQ
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{splash_swim_parse}


splash_length_7_sort <-
  function(x,
           time_score_string = Time_Score_String,
           time_score_specials_string = Time_Score_Specials_String) {

  if (length(x) > 0) {

    suppressWarnings(
      df_7 <- x %>%
        list_transform() %>%
        dplyr::mutate(
          Age = dplyr::case_when(
            stringr::str_detect(V3, "\\d\\d") == TRUE &
              stringr::str_detect(V3, "[:alpha:]|\\.") == FALSE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Team = dplyr::case_when(
            stringr::str_detect(V3, Age) == TRUE ~ V4,
            stringr::str_detect(V3, "[:alpha:]") == TRUE &
              stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
            stringr::str_detect(V4, "[:alpha:]") == TRUE &
              stringr::str_detect(V4, "[:alpha:]") == TRUE ~ V4,
            stringr::str_detect(V3, "[:alpha:]") == TRUE &
              stringr::str_detect(V4, "^[:upper:]+$") == TRUE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Finals = dplyr::case_when(
            str_detect(V4, time_score_specials_string) == TRUE ~ V4,
            stringr::str_detect(V4, time_score_specials_string) == FALSE &
              stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V5,
            stringr::str_detect(V6, time_score_specials_string) == TRUE &
              stringr::str_detect(V7, time_score_specials_string) == FALSE &
              stringr::str_detect(V5, time_score_specials_string) == FALSE ~ V6,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Points = dplyr::case_when(
            stringr::str_detect(V5, "\\d+") == TRUE &
              stringr::str_detect(V5, "\\.") == FALSE ~ V5,
            stringr::str_detect(V6, "\\d+") == TRUE &
              stringr::str_detect(V6, "\\.") == FALSE ~ V6,
            stringr::str_detect(V7, "\\d+") == TRUE &
              stringr::str_detect(V7, "\\.") == FALSE ~ V7,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Split_1 = dplyr::case_when(
            V5 == Points & V4 == Finals &
              stringr::str_detect(V6, time_score_string) == TRUE ~ V6,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::select(
          Place = V1,
          Name = V2,
          Age,
          Team,
          Finals,
          Points,
          Split_1,
          Row_Numb = V7
        )
    )
  } else {
    df_7 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  return(df_7)
}

#' Sorts data in lists of length 6 within \code{spash_swim_parse}
#'
#' XXXXXX
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 6
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{splash_swim_parse}


splash_length_6_sort <- function(x,
                                 time_score_specials_string = Time_Score_Specials_String){

  if (length(x) > 0) {
    suppressWarnings(
      df_6 <- x %>%
        list_transform() %>%
        dplyr::mutate(Name = dplyr::case_when(V2 == V3 ~ "Unknown",
                                              TRUE ~ V2)) %>%
        dplyr::mutate(
          Age = dplyr::case_when(
            stringr::str_detect(V3, "\\d\\d") == TRUE &
              stringr::str_detect(V3, "[:alpha:]|\\.") == FALSE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Team = dplyr::case_when(
            stringr::str_detect(V3, Age) == TRUE ~ V4,
            stringr::str_detect(V3, "^\\d\\s?") == TRUE ~ V4,
            stringr::str_detect(V3, "[:alpha:]") == TRUE &
              stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Prelims = dplyr::case_when(
            stringr::str_detect(V4, time_score_specials_string) == TRUE &
              stringr::str_detect(V5, time_score_specials_string) == TRUE &
              stringr::str_detect(V3, time_score_specials_string) == FALSE ~ V4,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Finals = dplyr::case_when(
            stringr::str_detect(V4, time_score_specials_string) == TRUE &
              V4 != Prelims ~ V4,
            stringr::str_detect(V5, time_score_specials_string) == TRUE &
              V4 == Prelims ~ V5,
            stringr::str_detect(V4, time_score_specials_string) == FALSE &
              stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V5,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Points = dplyr::case_when(
            stringr::str_detect(V5, "\\d+") == TRUE &
              stringr::str_detect(V5, "\\.") == FALSE ~ V5,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::select(
          Place = V1,
          Name,
          Age,
          Team,
          Prelims,
          Finals,
          Points,
          Row_Numb = V6
        )
    )
  } else {
    df_6 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  return(df_6)
}

#' Sorts data in lists of length 5 within \code{spash_swim_parse}
#'
#' XXXXXX
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 5
#' @param name_string a regex string for matching athlete names
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{splash_swim_parse}

splash_length_5_sort <- function(x,
                                 name_string = Name_String,
                                 time_score_specials_string = Time_Score_Specials_String){

  if (length(x) > 0) {
    suppressWarnings(
      df_5 <- x %>%
        list_transform() %>%
        dplyr::mutate(
          Name = dplyr::case_when(
            stringr::str_detect(V2, name_string) == TRUE ~ V2,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Age = dplyr::case_when(
            stringr::str_detect(V3, "\\d\\d") == TRUE &
              stringr::str_detect(V3, "[:alpha:]|\\.") == FALSE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Team = dplyr::case_when(
            stringr::str_detect(V3, Age) == TRUE ~ V4,
            stringr::str_detect(V3, "[:alpha:]") == TRUE &
              stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Finals = dplyr::case_when(
            stringr::str_detect(V3, time_score_specials_string) == TRUE ~ V3,
            stringr::str_detect(V4, time_score_specials_string) == TRUE ~ V4,
            stringr::str_detect(V4, time_score_specials_string) == FALSE &
              stringr::str_detect(V5, time_score_specials_string) == TRUE ~ V5,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::select(
          Place = V1,
          Name,
          Age,
          Team,
          Finals,
          Row_Numb = V5
        ) %>%
        dplyr::filter(
          stringr::str_detect(Team, time_score_specials_string) == FALSE
        )
    )
  } else {
    df_5 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  return(df_5)
}

#' Sorts data in lists of length 4 within \code{spash_swim_parse}
#'
#' XXXXXX
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_detect
#'
#' @param x a list of lists with all sub-lists having length 4
#' @param name_string a regex string for matching athlete names
#' @param time_score_specials_string a regex string for matching results - i.e.
#'   times, diving scores and 'specials' like DQ
#' @return returns a formatted data frame to be combined with others to make the
#'   output of \code{splash_swim_parse}

splash_length_4_sort <- function(x,
                                 name_string = Name_String,
                                 time_score_specials_string = Time_Score_Specials_String){

  if (length(x) > 0) {
    suppressWarnings(
      df_4 <- x %>%
        list_transform() %>%
        dplyr::mutate(
          Name = dplyr::case_when(
            stringr::str_detect(V2, name_string) == TRUE ~ V2,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Team = dplyr::case_when(
            stringr::str_detect(V2, "[:alpha:]") == TRUE &
              stringr::str_detect(V3, time_score_specials_string) == TRUE ~ V2,
            stringr::str_detect(V3, "[:alpha:]") == TRUE &
              stringr::str_detect(V3, time_score_specials_string) == FALSE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::mutate(
          Finals = dplyr::case_when(
            stringr::str_detect(V3, time_score_specials_string) == TRUE ~ V3,
            TRUE ~ "Unknown"
          )
        ) %>%

        dplyr::select(Place = V1,
                      Name,
                      Team,
                      Finals,
                      Row_Numb = V4) %>%
        dplyr::filter(
          stringr::str_detect(Team, time_score_specials_string) == FALSE
        )
    )

  } else {
    df_4 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  return(df_4)
}
