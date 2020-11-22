#' Collects splits within \code{swim_parse_ISL}
#'
#' Takes the output of \code{read_results} and, inside of \code{swim_parse_ISL}, extracts split times and associated row numbers
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_at
#' @importFrom dplyr vars
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param text output of \code{read_results} with tow numbers appended by \code{add_row_numbers}
#' @return returns a dataframe with split times and row numbers
#'
#' @seealso \code{splits_parse_ISL} runs inside \code{\link{swim_parse_ISL}} on the output of \code{\link{read_results}} with row numbers from \code{\link{add_row_numbers}}

splits_parse_ISL <- function(text) {

  ### define strings ###
  split_string <- "\\s\\d\\d\\.\\d\\d\\s"

  ### collect row numbers from rows containing splits ###
  row_numbs_ind <- text %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     split_string)] %>%
    .[purrr::map_lgl(., # remove rows with letters, which should take care of removing normal (non-split) times
                     stringr::str_detect,
                     "[:alpha:]", negate = TRUE)] %>%
    stringr::str_extract_all("\\d{1,}$")

  #### if there are still no valid splits return blank dataframe ####
  if (length(row_numbs_ind) > 0) {
    minimum_row <- min(as.numeric(row_numbs_ind))

    #### pull out rows containing splits, which will also remove row numbers ####
      suppressWarnings(
        data_1_ind <- text %>%
          .[purrr::map_lgl(.,
                           stringr::str_detect,
                           split_string)] %>%
          .[purrr::map_lgl(., # remove rows with letters, which should take care of removing normal (non-split) times
                           stringr::str_detect,
                           "[:alpha:]", negate = TRUE)] %>%
          stringr::str_remove_all("\n") %>%
          stringr::str_remove_all("r\\:\\+\\s?\\d\\.\\d\\d") %>%
          stringr::str_extract_all(paste0(
            "^\\s+\\d\\d\\.\\d\\d|", split_string
          )) %>%
          stringr::str_remove_all('\\"') %>%
          stringr::str_replace_all("\\(", " ") %>%
          stringr::str_replace_all("\\)", " ") %>%
          stringr::str_remove_all("c") %>%
          stringr::str_remove_all(',') %>%
          trimws()
      )

    #### add in blank spot for missing 50 split ####
    data_1_ind <- paste("00.00", data_1_ind, sep = "   ") # add in blank spot for missing 50 split

    #### add row numbers back in since they were removed ####
    data_1_ind <- paste(row_numbs_ind, data_1_ind, sep = "   ")

    #### break out by length ####
    data_1_ind <-
      unlist(purrr::map(data_1_ind, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    data_length_3 <- data_1_ind[purrr::map(data_1_ind, length) == 3] # 100s
    data_length_5 <- data_1_ind[purrr::map(data_1_ind, length) == 5] # 200s
    data_length_9 <- data_1_ind[purrr::map(data_1_ind, length) == 9] # 400s


    #### transform all lists to dataframes ####
    if (length(data_length_9) > 0) { # splits from 400M races
      df_9 <- data_length_9 %>%
        list_transform() %>%
        mutate(V1 = as.numeric(V1) -1) %>%
        mutate(V1 = as.character(V1))
    } else {
      df_9 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_length_5) > 0) { # splits from 200M races
      df_5 <- data_length_5 %>%
        list_transform()
    } else {
      df_5 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

    if (length(data_length_3) > 0) { # splits from 100M races
      df_3 <- data_length_3 %>%
        list_transform()
    } else {
      df_3 <- data.frame(Row_Numb = character(),
                         stringsAsFactors = FALSE)
    }

  #### relays ####
  row_numbs_relay <- text %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     split_string)] %>%
    .[purrr::map_lgl(., # remove rows with letters, which should take care of removing normal (non-split) times
                     stringr::str_detect,
                     "[:alpha:]")] %>%
    .[purrr::map_lgl(., # remove rows with letters, which should take care of removing normal (non-split) times
                     stringr::str_detect,
                     "^\n\\s*\\d", negate = TRUE)] %>%
    stringr::str_extract_all("\\d{1,}$")

  #### pull out rows containing splits, which will also remove row numbers ####
  suppressWarnings(
    data_1_relay <- text %>%
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       split_string)] %>%
      .[purrr::map_lgl(., # include rows with letters, which are the names of the relay swimmers
                       stringr::str_detect,
                       "[:alpha:]")] %>%
      .[purrr::map_lgl(., # remove rows that start with a place because relay swimmer rows don't
                       stringr::str_detect,
                       "^\n\\s+\\d", negate = TRUE)] %>%
      stringr::str_remove_all("\n") %>%
      stringr::str_remove_all("(?<=\\)) [\\d|\\.\\:]+") %>%
      stringr::str_extract_all("\\d?\\:?\\d\\d\\.\\d\\d") %>%
      stringr::str_remove_all('\\"') %>%
      stringr::str_replace_all("\\(", " ") %>%
      stringr::str_replace_all("\\)", " ") %>%
      stringr::str_remove_all("c") %>%
      stringr::str_remove_all(',') %>%
      stringr::str_replace_all(" ", "  ") %>%
      .[purrr::map_lgl(., # remove rows with letters, which should take care of removing normal (non-split) times
                       stringr::str_detect,
                       "[:alpha:]", negate = TRUE)] %>%
      trimws()
  )

    data_1_relay <- paste(row_numbs_relay, data_1_relay, sep = "   ")

    data_1_relay <-
      unlist(purrr::map(data_1_relay, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    data_length_3_relay <- data_1_relay[purrr::map(data_1_relay, length) == 3] # 100 splits for relays

    if (length(data_length_3_relay) > 0) {
      # splits from 100M relay legs
      df_3_relay <- data_length_3_relay %>%
        list_transform()
    } else {
      df_3_relay <- data.frame(Row_Numb = character(),
                               stringsAsFactors = FALSE)
    }

    #### bind up results ####
    # results are bound before going to lines_sort so that in cases where there are multiple rows with splits for the same race,
    # like in longer events with many splits, those splits can be collected and treated together
    data_splits <-
      dplyr::bind_rows(df_9, df_5, df_3, df_3_relay) %>%
      lines_sort(min_row = minimum_row) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb) - 1)  # make row number of split match row number of performance
      # dplyr::filter(Row_Numb < maximum_row)

    #### rename columns V1, V2 etc. by 50 ####
    old_names <- names(data_splits)[grep("^V", names(data_splits))]
    new_names <-
      paste("Split", seq(1, length(names(data_splits)) - 1) * 50, sep = "_")

    data_splits <- data_splits %>%
      dplyr::rename_at(dplyr::vars(old_names), ~ new_names)

  } else { # if there are no rows with valid splits return blank dataframe
    data_splits <- data.frame(Row_Numb = as.numeric())
  }

  return(data_splits)
}

