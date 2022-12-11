#' Collects relay swimmers as a data frame within \code{swim_parse_omega}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom dplyr filter
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#'
#' @param x output from \code{read_results} followed by \code{add_row_numbers}
#' @return returns a data frame of relay swimmers and the associated performance
#'   row number
#'
#' @seealso \code{collect_relay_swimmers_data} runs inside of
#'   \code{swim_parse_omega}
#'

collect_relay_swimmers_omega <- function(x){

  # x <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMX4X100MMD_FNL.pdf" %>%
  #   read_results() %>%
  #   add_row_numbers()

  # x <- as_lines_list_2

  relay_swimmer_string <- "^\n\\s*[:alpha:]"
  record_string <- "\n\\s+[:upper:]R\\s|\n\\s+US\\s|[:upper:][:alpha:]+ Record|\n\\s+W[:upper:]\\s|FINA\\s|BRONZE\\s{2,}|\\s+GOLD\\s{2,}|SILVER\\s{2,}|\n\\s?BRONZE|\n\\s?SILVER|\n\\s?GOLD"
  header_string <- "Record\\s+Split|Record\\s+Name|Reaction\\sTime|EVENT NO\\.|Rank\\s+Name|Rank\\s+NAT|NAT Code|Total\\s+\\d+$|(Women)|(Men)|\\s{3,}F\\s+\\d+$|\\s{3,}M\\s+\\d+$"

  # medallists_row <- stringr::str_extract(x, "Medallists\\s+\\d+$")
  # if (length(medallists_row) > 0) {
  #   medallists_row <- medallists_row[!is.na(medallists_row)]
  #   medallists_row <-
  #     as.numeric(stringr::str_extract(medallists_row[1], "\\d+"))
  # }

  row_numbs_relay_swimmer <- x %>%
    .[stringr::str_detect(.,
                     relay_swimmer_string)] %>%
    .[stringr::str_detect(.,
                     "[:alpha:][A-Za-z\\s]*")] %>%
    .[stringr::str_detect(.,
                      "[:upper:]\\s[:upper:]")] %>%
    .[stringr::str_detect(.,
                     record_string, negate = TRUE)] %>%
    .[stringr::str_detect(.,
                      header_string, negate = TRUE)] %>%
    .[stringr::str_detect(., "protest", negate = TRUE)] %>%
    .[stringr::str_count(., "\\(") < 2] %>%
    .[stringr::str_count(., "\\.") >= 1] %>%
    stringr::str_extract_all("\\d{1,}$")

  if (length(row_numbs_relay_swimmer) > 0) {
    minimum_row <- min(as.numeric(row_numbs_relay_swimmer))

    relay_rows <- unlist(row_numbs_relay_swimmer) %>%
      paste0(" ", ., "$") %>%
      paste(collapse = "|")

    suppressWarnings(
      data_1_relay_swimmer <- x %>%
        .[stringr::str_detect(., relay_rows)] %>%
        .[stringr::str_detect(., "protest", negate = TRUE)] %>%
        stringr::str_remove_all("\n") %>%
        stringr::str_remove_all("(?<=\\d) [:upper:] ") %>%
        stringr::str_remove_all("(?<=[:alpha:])\\. ") %>%
        stringr::str_extract_all("[:alpha:][A-Za-z\\s([:alpha:]\\-[:alpha:])([:alpha:]\\'[:alpha:])(SB?M?\\d{1,2})]*") %>%
        stringr::str_remove_all("\\s+\\-?\\d+$") %>%
        stringr::str_remove_all("\\s{2,}\\-") %>%
        .[lengths(.) == 1] %>%
        .[stringr::str_detect(.,
                         "[:alpha:]\\s[:alpha:]")] %>%
        # .[stringr::str_count(., "\\.") >= 1] %>%
        trimws()
    )

    data_1_relay_swimmer <- paste(row_numbs_relay_swimmer, data_1_relay_swimmer, sep = "   ")

    data_1_relay_swimmer <-
      unlist(purrr::map(data_1_relay_swimmer, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    relay_swimmers_data <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 2] %>%
      list_transform()

    #### to capture athlete genders for mixed relays
    if(length(relay_swimmers_data) < 1){
      relay_swimmers_data <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 3] %>%
        list_transform()

    }

    #### to capture athlete genders and para codes for mixed para relays
    if(length(relay_swimmers_data) < 1){
      relay_swimmers_data <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 4] %>%
        list_transform()

    }


    relay_swimmers_data <- relay_swimmers_data %>%
      lines_sort(min_row = min(as.numeric(relay_swimmers_data$V1) - 2)) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb))  # make row number of relay match row number of performance

    if(length(relay_swimmers_data) == 5){
      relay_swimmers_data <- relay_swimmers_data %>%
        dplyr::select(
          "Relay_Swimmer_1" = V2,
          "Relay_Swimmer_2" = V3,
          "Relay_Swimmer_3" = V4,
          "Relay_Swimmer_4" = V5,
          Row_Numb
        ) %>%
        na_if_character("NA")

    } else if(length(relay_swimmers_data) == 9) {
      if(stringr::str_detect(relay_swimmers_data$V3[1], "^SB?M?\\d{1,2}$") == TRUE){
      relay_swimmers_data <- relay_swimmers_data %>%
        dplyr::select(
          "Relay_Swimmer_1" = V2,
          "Relay_Swimmer_1_Para" = V3,
          "Relay_Swimmer_2" = V4,
          "Relay_Swimmer_2_Para" = V5,
          "Relay_Swimmer_3" = V6,
          "Relay_Swimmer_3_Para" = V7,
          "Relay_Swimmer_4" = V8,
          "Relay_Swimmer_4_Para" = V9,
          Row_Numb
        ) %>%
        na_if_character("NA")
      } else {
        relay_swimmers_data <- relay_swimmers_data %>%
          dplyr::select(
            "Relay_Swimmer_1" = V2,
            "Relay_Swimmer_1_Gender" = V3,
            "Relay_Swimmer_2" = V4,
            "Relay_Swimmer_2_Gender" = V5,
            "Relay_Swimmer_3" = V6,
            "Relay_Swimmer_3_Gender" = V7,
            "Relay_Swimmer_4" = V8,
            "Relay_Swimmer_4_Gender" = V9,
            Row_Numb
          ) %>%
          na_if_character("NA")
      }
    } else if(length(relay_swimmers_data) == 13) {
      relay_swimmers_data <- relay_swimmers_data %>%
        dplyr::select(
          "Relay_Swimmer_1" = V2,
          "Relay_Swimmer_1_Gender" = V3,
          'Relay_Swimmer_1_Para' = V4,
          "Relay_Swimmer_2" = V5,
          "Relay_Swimmer_2_Gender" = V6,
          'Relay_Swimmer_2_Para' = V7,
          "Relay_Swimmer_3" = V8,
          "Relay_Swimmer_3_Gender" = V9,
          'Relay_Swimmer_3_Para' = V10,
          "Relay_Swimmer_4" = V11,
          "Relay_Swimmer_4_Gender" = V12,
          'Relay_Swimmer_4_Para' = V13,
          Row_Numb
        ) %>%
        na_if_character("NA")
    }

  } else {
    relay_swimmers_data <- data.frame(Row_Numb = as.numeric())
  }

  #### clean up ####

  if("V2" %in% names(relay_swimmers_data)){
  relay_swimmers_data <- relay_swimmers_data %>%
    dplyr::filter(stringr::str_detect(V2, " DSQ ") == FALSE) %>%
    dplyr::filter(stringr::str_detect(V2, " RECORD$") == FALSE) %>%
    dplyr::filter(stringr::str_detect(V2, " US Open Records ") == FALSE)
  }

  # if(length(medallists_row) > 0){
  #   relay_swimmers_data <- relay_swimmers_data %>%
  #     filter(Row_Numb >= medallists_row)
  # }

  if(nrow(relay_swimmers_data) < 1){
    relay_swimmers_data <- data.frame(Row_Numb = as.numeric())
  }

  return(relay_swimmers_data)
}
