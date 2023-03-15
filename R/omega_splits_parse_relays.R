#' Collects splits for relays within \code{swim_parse_omega}
#'
#' Takes the output of \code{read_results} and, inside of
#' \code{swim_parse_omega}, extracts split times and associated row numbers
#'
#' @importFrom dplyr full_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_at
#' @importFrom dplyr mutate_at
#' @importFrom dplyr rename
#' @importFrom dplyr vars
#' @importFrom dplyr na_if
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_count
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param text output of \code{read_results} with row numbers appended by
#'   \code{add_row_numbers}
#' @param split_len length of pool at which splits are measured - usually 25 or
#'   50
#' @return returns a data frame with split times and row numbers
#'
#' @seealso \code{splits_parse} runs inside \code{\link{swim_parse_omega}} on
#'   the output of \code{\link{read_results}} with row numbers from
#'   \code{\link{add_row_numbers}}

splits_parse_omega_relays <-
  function(text, split_len = split_length_omega) {
    #### Testing ####
    # file <-
    #   "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMW4X200MFR_HEAT.pdf"
    # file <- "https://olympics.com/tokyo-2020/paralympic-games/resPG2020-/pdf/PG2020-/SWM/PG2020-_SWM_C73B1_SWMX4X100MFR13033-----FNL-000100--.pdf"
    # file <- "https://olympics.com/tokyo-2020/paralympic-games/resPG2020-/pdf/PG2020-/SWM/PG2020-_SWM_C73B1_SWMM4X100MFR10102-----FNL-000100--.pdf"
    # file <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Paralympics2020/raw_files/PG2020_SWMX4X50MFR_10101_FNL.pdf"
    # file <- system.file("extdata", "RESULTS_BOOK.pdf", package = "SwimmeR")
    # file <-
    #   system.file("extdata", "Results_Book.pdf", package = "SwimmeR")
    # text <-   read_results(file) %>%
    #   add_row_numbers()
    # split_len <- 50

    #### Actual Function ####
    ### collect row numbers from rows containing splits ###
    ### define strings ###
    split_string <- "(\\d?\\:?\\d{2}\\.\\d{2})|(  NA  )"
    relay_swimmer_string <- "^\n\\s*[:alpha:]"
    record_string <- "\n\\s+[:upper:]R\\s|\n\\s+[:upper:]J\\s|\n\\s+US\\s|[:upper:][:alpha:]+ Record"

    text <- text %>%
      stringr::str_replace_all(" \\:", "  ") %>%
      stringr::str_remove_all("\\(\\=?\\d?\\d?\\)\\s+\\d?\\:?\\d{2}\\.\\d{2}") %>%
      stringr::str_replace("(?<=\\d\\.\\d\\d)\\s{16,}(?=\\d?\\:?\\d{2}\\.\\d{2} )", "   NA   ")

    ### collect splits

    row_numbs <- text %>%
      .[any(stringr::str_count(., "\\.") > 1, stringr::str_detect(., "S\\d+"))] %>%
      .[stringr::str_detect(.,
                       split_string)] %>%
      .[stringr::str_detect(.,
                       relay_swimmer_string)] %>%
      .[stringr::str_detect(.,
                        record_string, negate = TRUE)] %>%
      stringr::str_extract_all("\\d{1,}$")
    flag <- FALSE


    #### if there are still no valid splits return blank data frame ####
    if (length(row_numbs) > 0) {
      minimum_row <- min(as.numeric(row_numbs))
      maximum_row <- as.numeric(length(text))

      # row_numbs <- unlist(row_numbs) %>%
      #   paste0(" ", ., "$") %>%
      #   paste(collapse = "|")

      suppressWarnings(
        data_1_splits <- text %>%
          .[any(stringr::str_count(., "\\.") > 1, stringr::str_detect(., "S\\d+"))] %>%
          .[purrr::map_lgl(.,
                           stringr::str_detect,
                           split_string)] %>%
          .[purrr::map_lgl(.,
                           stringr::str_detect,
                           relay_swimmer_string)] %>%
          .[!purrr::map_lgl(.,
                            stringr::str_detect,
                            record_string)] %>%
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
          list_transform() %>%
          dplyr::mutate(
            V6 = dplyr::case_when(
              all.equal(sec_format(V6), sec_format(V5) + sec_format(dplyr::lag(V5, default = "0"))) == TRUE ~ "NA",
              all.equal(sec_format(V6), sec_format(V5) + sec_format(dplyr::lag(V6, default = "0"))) == TRUE ~ "NA",
              sec_format(V6) - sec_format(V5) > 2 *
                (sec_format(V5) - sec_format(V4)) ~ "NA",
              V6 == V5 ~ "NA",
              TRUE ~ V6
            )
          ) %>% na_if_character("NA") %>%
          purrr::discard( ~ all(is.na(.)))
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
          list_transform() %>%
          dplyr::mutate(
            V4 = dplyr::case_when(
              all.equal(sec_format(V4), sec_format(V3) + sec_format(dplyr::lag(V3, default = "0"))) == TRUE ~ "NA",
              all.equal(sec_format(V4), sec_format(V3) + sec_format(dplyr::lag(V4, default = "0"))) == TRUE ~ "NA",
              sec_format(V4) - sec_format(V3) > 1.2 * (sec_format(V3) - sec_format(V2)) ~ "NA",
              V4 == V3 ~ "NA",
              TRUE ~ V4
            )
          ) %>% na_if_character("NA") %>%
          purrr::discard( ~ all(is.na(.)))
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
        # filter(!is.na(Split_50))

    } else {
      # if there are no rows with valid splits return blank data frame
      data_splits <- data.frame(Row_Numb = as.numeric())
    }
    return(data_splits)

  }

