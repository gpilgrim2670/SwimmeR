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
#' @param text output of \code{read_results} with row numbers appended by
#'   \code{add_row_numbers}
#' @param split_len length of pool at which splits are measured - usually 25 or
#'   50
#' @return returns a dataframe with split times and row numbers
#'
#' @seealso \code{splits_parse} runs inside \code{\link{swim_parse_splash}} on
#'   the output of \code{\link{read_results}} with row numbers from
#'   \code{\link{add_row_numbers}}

splits_parse_splash <- function(text, split_len = split_length_splash) {

  #### Testing ####
  # text <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Glenmark_Senior_Nationals_2019.pdf") %>%
  #   add_row_numbers()

  #### Actual Function ####
  ### collect row numbers from rows containing splits ###
  ### define strings ###

  text <- text %>%
    stringr::str_replace_all("(?<=\\dm\\:)\\s+(?=\\d{1,}m\\:)", "  Unknown  ") %>%
    stringr::str_replace_all("(?<=\\dm\\:)\\s+(?=\\d{1,}$)", "  Unknown  ")

  split_string <- "\\d{1,}m\\:\\s+\\d{0,2}\\:?\\d?\\d\\.\\d\\d|\\d{1,}m\\:\\s+Unknown"


  row_numbs <- text %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     split_string)] %>%
    stringr::str_extract_all("\\d{1,}$")

  #### if there are still no valid splits return blank dataframe ####
  if (length(row_numbs) > 0) {
    minimum_row <- min(as.numeric(row_numbs))
    maximum_row <- as.numeric(length(text))

    #### help out a little, in case there are splits that only have one space between them ####
    text <- stringr::str_replace_all(text, "(\\d) (\\d)", "\\1  \\2")

    #### pull out rows containing splits, which will remove row numbers ####

    data_1_splits <- text %>%
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       split_string)] %>%
      stringr::str_extract_all(split_string) %>%
      purrr::map(paste, collapse = "  ") %>%
      stringr::str_replace_all("\\:\\s+", " ") %>%
      trimws()


    #### add row numbers back in since they were removed ####
    data_1_splits <- paste(row_numbs, data_1_splits, sep = "   ")

    #### break out by length ####
    data_1_splits <-
      unlist(purrr::map(data_1_splits, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    data_splits_length_2 <- data_1_splits[purrr::map(data_1_splits, length) == 2]
    data_splits_length_3 <- data_1_splits[purrr::map(data_1_splits, length) == 3]
    data_splits_length_4 <- data_1_splits[purrr::map(data_1_splits, length) == 4]
    data_splits_length_5 <- data_1_splits[purrr::map(data_1_splits, length) == 5]
    data_splits_length_6 <- data_1_splits[purrr::map(data_1_splits, length) == 6]
    data_splits_length_7 <- data_1_splits[purrr::map(data_1_splits, length) == 7]
    data_splits_length_8 <- data_1_splits[purrr::map(data_1_splits, length) == 8]
    data_splits_length_9 <- data_1_splits[purrr::map(data_1_splits, length) == 9]
    data_splits_length_10 <- data_1_splits[purrr::map(data_1_splits, length) == 10]

    #### transform all lists to dataframes ####
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
    } else {
      df_7_splits <- data.frame(Row_Numb = character(),
                                stringsAsFactors = FALSE)
    }

    if (length(data_splits_length_6) > 0) {
      df_6_splits <- data_splits_length_6 %>%
        list_transform()
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

    data_splits %>%
      lines_sort(min_row = minimum_row, to_wide = FALSE) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb) - 1)  # make row number of split match row number of performance
      # dplyr::group_by(Row_Numb) %>%
      # summarise(across(starts_with("Split"), ~na.omit()))

    # names_data_splits <- data_splits %>%
    #   select(-Row_Numb) %>%
    #   names()
    #
    # Row_Numb <- data_splits$Row_Numb
    #
    #
    # df_splits <- data_splits %>%
    #   dplyr::select(-Row_Numb) %>%
    #   t() %>%
    #   data.frame() %>%
    #   map(undo_interleave)
    #
    # df_splits_2 <- df_splits %>%
    #   data.frame() %>%
    #   t() %>%
    #   data.frame()
    #
    # names(df_splits) <- names_data_splits
    #
    # df_splits_2$Row_Numb <- Row_Numb


    #### rename columns V1, V2 etc. by 50 ####
    # old_names <- names(data_splits)[grep("^V", names(data_splits))]
    # new_names <-
    #   paste("Split", seq(1, length(names(data_splits)) - 1) * split_len, sep = "_")
    #
    # data_splits <- data_splits %>%
    #   dplyr::rename_at(dplyr::vars(old_names), ~ new_names)

  } else { # if there are no rows with valid splits return blank dataframe
    data_splits <- data.frame(Row_Numb = as.numeric())
  }

  row.names(data_splits) <- NULL

  return(data_splits)

}
