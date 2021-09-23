#' Collects splits within \code{swim_parse_splash} for Splash results
#'
#' Takes the output of \code{read_results} and, inside of
#' \code{swim_parse_splash}, extracts split times and associated row numbers
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_at
#' @importFrom dplyr mutate_at
#' @importFrom dplyr rename
#' @importFrom dplyr vars
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param raw_results output of \code{read_results} with row numbers appended by
#'   \code{add_row_numbers}
#' @param split_len length of pool at which splits are measured - usually 25 or
#'   50
#' @return returns a data frame with split times and row numbers
#'
#' @seealso \code{splits_parse} runs inside \code{\link{swim_parse_splash}} on
#'   the output of \code{\link{read_results}} with row numbers from
#'   \code{\link{add_row_numbers}}

splits_parse_splash <- function(raw_results) {

  #### Testing ####
  # raw_results <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Glenmark_Senior_Nationals_2019.pdf") %>%
  #   add_row_numbers()
  # raw_results <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Open_Belgian_Champs_2017.pdf") %>%
  #   add_row_numbers()
  # raw_results <- as_lines_list_2

  #### Actual Function ####
  ### collect row numbers from rows containing splits ###
  ### define strings ###

  split_string <- "\\d{1,}m\\:\\s+\\d{0,2}\\:?\\d?\\d\\.\\d\\d|\\d{1,}m\\:\\s+Unknown"

  #### Clean up raw_results ####
  raw_results <- raw_results %>%
    stringr::str_replace_all("(?<=\\dm\\:)\\s+(?=\\d{1,}m\\:)", "  Unknown  ") %>%
    stringr::str_replace_all("(?<=\\dm\\:)\\s+(?=\\d{1,}$)", "  Unknown  ")

  #### Pull out Row numbers for lines with splits ####

  row_numbs <- raw_results %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     split_string)] %>%
    stringr::str_extract_all("\\d{1,}$")


  #### if there are still no valid splits return blank data frame ####
  if (length(row_numbs) > 0) {
    minimum_row <- min(as.numeric(row_numbs))
    maximum_row <- as.numeric(length(raw_results))

    #### help out a little, in case there are splits that only have one space between them ####
    raw_results <- stringr::str_replace_all(raw_results, "(\\d) (\\d)", "\\1  \\2")

    #### pull out rows containing splits, which will remove row numbers ####

    data_1_splits <- raw_results %>%
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       split_string)] %>%
      stringr::str_extract_all(split_string) %>%
      purrr::map(paste, collapse = "  ") %>%
      stringr::str_replace_all("\\:\\s+", " ") %>%
      trimws()


    #### add row numbers back in since they were removed ####
    data_1_splits <- paste(row_numbs, data_1_splits, sep = "   ")

    # data_splits <- data_1_splits %>%
    #   splits_parse_splash_helper_1()

    #### break out by length ####
    data_1_splits <-
      unlist(purrr::map(data_1_splits, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    # unique(map(data_1_splits, length))

    data_splits_length_2 <- data_1_splits[purrr::map(data_1_splits, length) == 2]
    data_splits_length_3 <- data_1_splits[purrr::map(data_1_splits, length) == 3]
    data_splits_length_4 <- data_1_splits[purrr::map(data_1_splits, length) == 4]
    data_splits_length_5 <- data_1_splits[purrr::map(data_1_splits, length) == 5]
    data_splits_length_6 <- data_1_splits[purrr::map(data_1_splits, length) == 6]
    data_splits_length_7 <- data_1_splits[purrr::map(data_1_splits, length) == 7]
    data_splits_length_8 <- data_1_splits[purrr::map(data_1_splits, length) == 8]
    data_splits_length_9 <- data_1_splits[purrr::map(data_1_splits, length) == 9]
    data_splits_length_10 <- data_1_splits[purrr::map(data_1_splits, length) == 10]

    #### transform all lists to data frames ####
    if (length(data_splits_length_10) > 0) {
      df_10_splits <- data_splits_length_10 %>%
        list_transform()
    } else {
      df_10_splits <- data.frame(Row_Numb = character(),
                                 stringsAsFactors = FALSE)
    }

    if (length(data_splits_length_9) > 0) {
      df_9_splits <- data_splits_length_9 %>%
        list_transform()
    } else {
      df_9_splits <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_splits_length_8) > 0) {
      df_8_splits <- data_splits_length_8 %>%
        list_transform()
    } else {
      df_8_splits <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_splits_length_7) > 0) {
      df_7_splits <- data_splits_length_7 %>%
        list_transform()

      df_7_splits_2 <- df_7_splits %>%
        select(V1, "Dummy" = V2) %>%
        splash_collect_splits()

      df_7_splits_3 <- df_7_splits %>%
        select(V1, "Dummy" = V3) %>%
        splash_collect_splits()

      df_7_splits_4 <- df_7_splits %>%
        select(V1, "Dummy" = V4) %>%
        splash_collect_splits()

      df_7_splits_5 <- df_7_splits %>%
        select(V1, "Dummy" = V5) %>%
        splash_collect_splits()

      df_7_splits_6 <- df_7_splits %>%
        select(V1, "Dummy" = V6) %>%
        splash_collect_splits()

      df_7_splits_7 <- df_7_splits %>%
        select(V1, "Dummy" = V7) %>%
        splash_collect_splits()

      df_7_splits <- df_7_splits_2 %>%
        dplyr::bind_rows(df_7_splits_3) %>%
        dplyr::bind_rows(df_7_splits_4) %>%
        dplyr::bind_rows(df_7_splits_5) %>%
        dplyr::bind_rows(df_7_splits_6) %>%
        dplyr::bind_rows(df_7_splits_7)

      rm(df_7_splits_2, df_7_splits_3, df_7_splits_4, df_7_splits_5, df_7_splits_6, df_7_splits_7)

      df_7_splits <- df_7_splits %>%
        reshape(direction = "wide",
                timevar = "Split_Distance",
                idvar = "Row_Numb",
                v.names = "Split",
                sep = "")

    } else {
      df_7_splits <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_splits_length_6) > 0) {
      df_6_splits <- data_splits_length_6 %>%
        list_transform()

      df_6_splits_2 <- df_6_splits %>%
        select(V1, "Dummy" = V2) %>%
        splash_collect_splits()

      df_6_splits_3 <- df_6_splits %>%
        select(V1, "Dummy" = V3) %>%
        splash_collect_splits()

      df_6_splits_4 <- df_6_splits %>%
        select(V1, "Dummy" = V4) %>%
        splash_collect_splits()

      df_6_splits_5 <- df_6_splits %>%
        select(V1, "Dummy" = V5) %>%
        splash_collect_splits()

      df_6_splits_6 <- df_6_splits %>%
        select(V1, "Dummy" = V6) %>%
        splash_collect_splits()

      df_6_splits <- df_6_splits_2 %>%
        dplyr::bind_rows(df_6_splits_3) %>%
        dplyr::bind_rows(df_6_splits_4) %>%
        dplyr::bind_rows(df_6_splits_5) %>%
        dplyr::bind_rows(df_6_splits_6)

      rm(df_6_splits_2, df_6_splits_3, df_6_splits_4, df_6_splits_5, df_6_splits_6)

      df_6_splits <- df_6_splits %>%
        reshape(direction = "wide",
                timevar = "Split_Distance",
                idvar = "Row_Numb",
                v.names = "Split",
                sep = "")

    } else {
      df_6_splits <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_splits_length_5) > 0) {
      df_5_splits <- data_splits_length_5 %>%
        list_transform()

      df_5_splits_2 <- df_5_splits %>%
        select(V1, "Dummy" = V2) %>%
        splash_collect_splits()

      df_5_splits_3 <- df_5_splits %>%
        select(V1, "Dummy" = V3) %>%
        splash_collect_splits()

      df_5_splits_4 <- df_5_splits %>%
        select(V1, "Dummy" = V4) %>%
        splash_collect_splits()

      df_5_splits_5 <- df_5_splits %>%
        select(V1, "Dummy" = V5) %>%
        splash_collect_splits()

      df_5_splits <- df_5_splits_2 %>%
        dplyr::bind_rows(df_5_splits_3) %>%
        dplyr::bind_rows(df_5_splits_4) %>%
        dplyr::bind_rows(df_5_splits_5)

      rm(df_5_splits_2, df_5_splits_3, df_5_splits_4, df_5_splits_5)

      df_5_splits <- df_5_splits %>%
        reshape(direction = "wide",
                timevar = "Split_Distance",
                idvar = "Row_Numb",
                v.names = "Split",
                sep = "")

    } else {
      df_5_splits <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_splits_length_4) > 0) {
      df_4_splits <- data_splits_length_4 %>%
        list_transform()

      df_4_splits_2 <- df_4_splits %>%
        select(V1, "Dummy" = V2) %>%
        splash_collect_splits()

      df_4_splits_3 <- df_4_splits %>%
        select(V1, "Dummy" = V3) %>%
        splash_collect_splits()

      df_4_splits_4 <- df_4_splits %>%
        select(V1, "Dummy" = V4) %>%
        splash_collect_splits()

      df_4_splits <- df_4_splits_2 %>%
        dplyr::bind_rows(df_4_splits_3) %>%
        dplyr::bind_rows(df_4_splits_4)

      rm(df_4_splits_2, df_4_splits_3, df_4_splits_4)

      df_4_splits <- df_4_splits %>%
        reshape(direction = "wide",
                timevar = "Split_Distance",
                idvar = "Row_Numb",
                v.names = "Split",
                sep = "")

    } else {
      df_4_splits <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_splits_length_3) > 0) {
      df_3_splits <- data_splits_length_3 %>%
        list_transform()

      df_3_splits_2 <- df_3_splits %>%
        select(V1, "Dummy" = V2) %>%
        splash_collect_splits()

      df_3_splits_3 <- df_3_splits %>%
        select(V1, "Dummy" = V3) %>%
        splash_collect_splits()

      df_3_splits <- df_3_splits_2 %>%
        dplyr::bind_rows(df_3_splits_3)

      rm(df_3_splits_2, df_3_splits_3)

      df_3_splits <- df_3_splits %>%
        reshape(direction = "wide",
                timevar = "Split_Distance",
                idvar = "Row_Numb",
                v.names = "Split",
                sep = "")


    } else {
      df_3_splits <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_splits_length_2) > 0) {
      df_2_splits <- data_splits_length_2 %>%
        list_transform()
    } else {
      df_2_splits <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    #### bind up results ####
    # results are bound before going to lines_sort so that in cases where there are multiple rows with splits for the same race,
    # like in longer events with many splits, those splits can be collected and treated together
    data_splits <- df_10_splits %>%
      dplyr::full_join(df_9_splits) %>%
      dplyr::full_join(df_8_splits) %>%
      dplyr::full_join(df_7_splits) %>%
      dplyr::full_join(df_6_splits) %>%
      dplyr::full_join(df_5_splits) %>%
      dplyr::full_join(df_4_splits) %>%
      dplyr::full_join(df_3_splits) %>%
      dplyr::full_join(df_2_splits) %>%
      rename(V1 = Row_Numb)

    data_splits <- data_splits %>%
      dplyr::na_if("Unknown") %>%
      lines_sort(min_row = minimum_row, to_wide = FALSE) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb) - 1)   # make row number of split match row number of performance


  } else { # if there are no rows with valid splits return blank dataframe
    data_splits <- data.frame(Row_Numb = as.numeric())
  }

  row.names(data_splits) <- NULL

  return(data_splits)

}

#' Produces data frames of splits within \code{swim_parse_splash} for Splash
#' results
#'
#' Converts strings of splits and row numbers into data frames with a row number
#' column (V1) and a splits column (Split_XX)
#'
#' @importFrom dplyr left_join
#' @importFrom stringr str_extract_all
#' @importFrom purrr map
#' @importFrom purrr as_vector
#' @importFrom purrr reduce
#'
#' @param data a list of lists containing splits and row numbers
#' @return returns a data frame with split times and row numbers
#'
#' @seealso \code{splits_parse_splash_helper_1} runs inside
#'   \code{\link{splits_parse_splash}}

splits_parse_splash_helper_1 <- function(data){

  split_distances <- data %>%
    purrr::as_vector() %>%
    stringr::str_extract_all("\\d{2,}m") %>%
    unique() %>%
    purrr::as_vector()

  i <- seq(1, length(split_distances), 1)

  suppressMessages(
    splits_df_complete <- i %>%
      purrr::map(splits_parse_splash_helper_2, data = data, split_distances = split_distances) %>%
      purrr::reduce(dplyr::left_join) %>%
      unique()
  )

  return(splits_df_complete)

}

#' Produces data frames of splits within \code{swim_parse_splash} for Splash
#' results
#'
#' Converts strings of splits and row numbers into data frames with a row number
#' column (V1) and a splits column (Split_XX)
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_at
#' @importFrom dplyr mutate_at
#' @importFrom dplyr rename
#' @importFrom dplyr vars
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove
#' @importFrom stringr str_extract_all
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param data a list of lists containing splits and row numbers
#' @param split_distances a list of distances for splits, e.g. "50m", "100m"
#' @param i a number between 1 and the length of \code(split_distances)
#' @return returns a data frame with split times and row numbers
#'
#' @seealso \code{splits_parse_splash_helper_2} runs inside
#'   \code{\link{splits_parse_splash}}

splits_parse_splash_helper_2 <- function(data, split_distances, i){

  splits_df <- data %>%
    stringr::str_extract_all(
      paste0(
        "^\\d{1,}|",
        "(?<=\\s)",
        split_distances[i],
        "\\s\\d?\\d?\\:?\\d{2}\\.\\d{2}"
      )
    ) %>%

    do.call(rbind, .) %>%
    as.data.frame() %>%
    dplyr::mutate(V2 = stringr::str_remove(V2, "\\d{1,}m\\s"))

  names(splits_df) <- names(splits_df) %>%
    stringr::str_replace("V2", paste0("Split_", split_distances[i])) %>%
    stringr::str_remove("m$")

  return(splits_df)
}
