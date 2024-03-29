#' Collects splits for relays within \code{swim_parse_splash}
#'
#' Takes the output of \code{read_results} and, inside of \code{swim_parse_splash},
#' extracts split times and associated row numbers
#'
#' @importFrom dplyr full_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_at
#' @importFrom dplyr mutate_at
#' @importFrom dplyr rename
#' @importFrom dplyr vars
#' @importFrom dplyr na_if
#' @importFrom dplyr all_of
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
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
#' @seealso \code{splits_parse} runs inside \code{\link{swim_parse_splash}} on the
#'   output of \code{\link{read_results}} with row numbers from
#'   \code{\link{add_row_numbers}}

splits_parse_splash_relays <-
  function(text, split_len = split_length_splash) {
    #### Testing ####

    # text <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Open_Belgian_Champs_2017.pdf") %>%
    #   add_row_numbers()
    # split_len <- 50

    # text <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Glenmark_Senior_Nationals_2019.pdf") %>%
    #   add_row_numbers()
    # split_len <- 50

    #### Actual Function ####
    ### collect row numbers from rows containing splits ###
    ### define strings ###
    split_string <- "(\\d?\\:?\\d{2}\\.\\d{2})|(  NA  )"
    relay_swimmer_string <- "^\n\\s*[:alpha:]"
    record_string <- "\n\\s+[:upper:]R\\s|\n\\s+US\\s|[:upper:][:alpha:]+ Record|\n\\s?[:upper:]{1,}\\s|\n\\s+NMR\\s+\\d{1,}$"
    splash_string <- "Splash Meet Manager"

    text <- text %>%
      .[stringr::str_detect(., record_string, negate = TRUE)] %>%
      stringr::str_replace_all(" \\:", "  ") %>%
      stringr::str_remove_all("\\(\\=?\\d?\\d?\\)\\s+\\d?\\:?\\d{2}\\.\\d{2}") %>%
      stringr::str_replace("([:alpha:])\\s{6,}(\\d{1,}$)", "\\1  NA  \\2") %>%
      stringr::str_replace("(?<=\\d\\.\\d\\d)\\s{16,}(?=\\d?\\:?\\d{2}\\.\\d{2} )", "   NA   ")

    ### collect splits

    row_numbs <- text %>%
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       split_string)] %>%
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       relay_swimmer_string)] %>%
      .[!purrr::map_lgl(.,
                        stringr::str_detect,
                        record_string)] %>%
      .[!purrr::map_lgl(.,
                        stringr::str_detect,
                        splash_string)] %>%
      stringr::str_extract_all("\\d{1,}$")
    flag <- FALSE


    #### if there are still no valid splits return blank data frame ####
    if (length(row_numbs) > 0) {
      minimum_row <- min(as.numeric(row_numbs))
      maximum_row <- as.numeric(length(text))

      suppressWarnings(
        data_1_splits <- text %>%
          .[purrr::map_lgl(.,
                           stringr::str_detect,
                           split_string)] %>%
          .[purrr::map_lgl(.,
                           stringr::str_detect,
                           relay_swimmer_string)] %>%
          .[!purrr::map_lgl(.,
                            stringr::str_detect,
                            record_string)] %>%
          .[!purrr::map_lgl(.,
                            stringr::str_detect,
                            splash_string)] %>%
          stringr::str_extract_all(split_string) %>%
          purrr::map(paste, collapse = "  ") %>%
          trimws()
      )


      #### add row numbers back in since they were removed ####
      data_1_splits <- paste(row_numbs, data_1_splits, sep = "   ")

      #### break out by length ####
      data_1_splits <-
        unlist(purrr::map(data_1_splits, stringr::str_split, "\\s{2,}"),
               recursive = FALSE)

      data_splits_length_2 <-
        data_1_splits[purrr::map(data_1_splits, length) == 2]
      data_splits_length_3 <-
        data_1_splits[purrr::map(data_1_splits, length) == 3]
      data_splits_length_4 <-
        data_1_splits[purrr::map(data_1_splits, length) == 4]
      data_splits_length_5 <-
        data_1_splits[purrr::map(data_1_splits, length) == 5]
      data_splits_length_6 <-
        data_1_splits[purrr::map(data_1_splits, length) == 6]
      data_splits_length_7 <-
        data_1_splits[purrr::map(data_1_splits, length) == 7]
      data_splits_length_8 <-
        data_1_splits[purrr::map(data_1_splits, length) == 8]
      data_splits_length_9 <-
        data_1_splits[purrr::map(data_1_splits, length) == 9]
      data_splits_length_10 <-
        data_1_splits[purrr::map(data_1_splits, length) == 10]

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
      } else {
        df_5_splits <- data.frame(Row_Numb = character(),
                                  stringsAsFactors = FALSE)
      }

      if (length(data_splits_length_4) > 0) {
        df_4_splits <- data_splits_length_4 %>%
          list_transform()
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
      data_splits <-
        dplyr::bind_rows(
          df_10_splits,
          df_9_splits,
          df_8_splits,
          df_7_splits,
          df_6_splits,
          df_5_splits,
          df_4_splits,
          df_3_splits,
          df_2_splits
        ) %>%
        lines_sort(min_row = minimum_row) %>%
        dplyr::mutate(Row_Numb = as.numeric(Row_Numb) - 1) # make row number of split match row number of performance

      if ("V1" %in% names(data_splits) &
          any(stringr::str_detect(data_splits$V1, "\\.") == FALSE)) {
        data_splits <- data_splits %>%
          dplyr::rename("Row_Numb" = V1)
      }

      #### rename columns V1, V2 etc. by 50 ####
      old_names <- names(data_splits)[grep("^V", names(data_splits))]
      new_names <-
        paste("Split", seq(1, length(names(data_splits)) - 1) * split_len, sep = "_")

      data_splits <- data_splits %>%
        dplyr::rename_at(dplyr::vars(dplyr::all_of(old_names)), ~ new_names)

      data_splits <- data_splits %>%
        na_if_character("NA")

      row.names(data_splits) <- NULL

    } else {
      # if there are no rows with valid splits return blank data frame
      data_splits <- data.frame(Row_Numb = as.numeric())
    }
    return(data_splits)

  }

