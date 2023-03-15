#' Collects relay swimmers as a data frame within \code{swim_parse}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param x output from \code{read_results} followed by \code{add_row_numbers}
#' @return returns a data frame of relay swimmers and the associated performance row number
#'
#' @seealso \code{collect_relay_swimmers_data} runs inside of \code{swim_parse}
#'

collect_relay_swimmers <- function(x){
  # x <- read_results("http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm")
  #
  # x <- read_results(system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR"))
  # x <- add_row_numbers(x)
  # x <- as_lines_list_2
  # x <- x_1

  relay_swimmer_string <- "\n\\s*[1-4]\\)"

  row_numbs_relay_swimmer <- x %>%
    .[stringr::str_detect(.,
                     relay_swimmer_string)] %>%
    stringr::str_extract_all("\\d{1,}$")

  if (length(row_numbs_relay_swimmer) > 0) {
    minimum_row <- min(as.numeric(row_numbs_relay_swimmer))

    suppressWarnings(
      data_1_relay_swimmer <- x %>%
        .[stringr::str_detect(.,
                         relay_swimmer_string)] %>%
        stringr::str_remove_all("\n") %>%
        stringr::str_replace_all("\\s(?=\\d)", "  ") %>% # make to sure have enough spaces between athlete names
        stringr::str_replace_all("(?<=\\s[1-4]\\))   ", " NA  ") %>% # if a relay swimmer is missing should replace spot with "NA"
        # stringr::str_replace_all(stats::setNames(replacement_2, typo_2)) %>%
        stringr::str_remove_all("\\)") %>%
        stringr::str_remove_all("[A-Z]\\d{1,3}") %>% # for M25 designations in masters - Male 25
        stringr::str_remove_all(" M?FR | M?SO | M?JR | M?SR | F?FR | F?SO | F?JR | F?SR | W?FR | W?SO | W?JR | W?SR ") %>% # for gender/grade designations
        stringr::str_remove_all("r\\:\\+?\\-?\\d?\\.\\d\\d?") %>% # for reaction pad outputs
        stringr::str_remove_all("r\\:NRT") %>% # for reaction time fail to register
        stringr::str_remove_all("\\d+|\\:|\\.|DQ|\\=\\=|\\*\\*") %>% # all digits or colons or periods (times, DQ, record designators)
        # stringr::str_remove_all("\\:\\.") %>% # all digits
        stringr::str_remove_all("r\\:\\+?\\-?\\.") %>%
        stringr::str_remove_all("\\+\\+|\\*\\*") %>%
        trimws() %>%
        stringr::str_remove_all(" SR$| SR | JR$| JR | SO$| SO | FR$| FR ") %>% # grade designators
        trimws()
    )

    data_1_relay_swimmer <- paste(row_numbs_relay_swimmer, data_1_relay_swimmer, sep = "   ")

    data_1_relay_swimmer <-
      unlist(purrr::map(data_1_relay_swimmer, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    data_length_5_relay_swimmer <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 5] # all four swimmers on one line
    data_length_4_relay_swimmer <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 4] # all four swimmers on one line but one is missing
    data_length_3_relay_swimmer <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 3] # for two-line relays, two swimmers per line
    data_length_2_relay_swimmer <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 2] %>%  # for two-line relays, two swimmers per line, but one is missing
      .[purrr::map_lgl(., ~
                       any(stringr::str_detect(.,
                       "\\s|\\,")))] # to differentiate names from teams (like in circa 2005 NCAA results) - must have space or comma for separating names

    if (length(data_length_5_relay_swimmer) > 0) {
      # splits from 100M relay legs
      df_5_relay_swimmer <- data_length_5_relay_swimmer %>%
        list_transform()

    } else {
      df_5_relay_swimmer <- data.frame(Row_Numb = character(),
                                       stringsAsFactors = FALSE)
    }

    if (length(data_length_4_relay_swimmer) > 0) {
      df_4_relay_swimmer <- data_length_4_relay_swimmer %>%
        list_transform()

    } else {
      df_4_relay_swimmer <- data.frame(Row_Numb = character(),
                                       stringsAsFactors = FALSE)
    }

    if (length(data_length_3_relay_swimmer) > 0) {
      df_3_relay_swimmer <- data_length_3_relay_swimmer %>%
        list_transform()

    } else {
      df_3_relay_swimmer <- data.frame(Row_Numb = character(),
                                       stringsAsFactors = FALSE)
    }

    if (length(data_length_2_relay_swimmer) > 0) {
      df_2_relay_swimmer <- data_length_2_relay_swimmer %>%
        list_transform() %>%
        # dplyr::mutate(V1 = as.numeric(V1)) %>%
        dplyr::filter((as.numeric(V1) + 1) %!in% as.numeric(unlist(row_numbs_relay_swimmer)) & (as.numeric(V1) + 2) %!in% as.numeric(unlist(row_numbs_relay_swimmer))) # sometimes team names get caught up in relay data - this removes them by making sure no relay covers more than two rows
        # dplyr::mutate(V1 = as.character(V1))

    } else {
      df_2_relay_swimmer <- data.frame(Row_Numb = character(),
                                       stringsAsFactors = FALSE)
    }

    #### bind up results ####
    # results are bound before going to lines_sort so that in cases where there are multiple rows with splits for the same race,
    # like in results where relays swimmers are reported on two lines, the results can be collected together
    relay_swimmers_data <-
      dplyr::bind_rows(df_5_relay_swimmer, df_4_relay_swimmer, df_3_relay_swimmer, df_2_relay_swimmer)

    relay_swimmers_data <- relay_swimmers_data %>%
      lines_sort(min_row = min(as.numeric(relay_swimmers_data$V1) - 2)) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%   # make row number of relay match row number of performance
      dplyr::select(
        "Relay_Swimmer_1" = V2,
        "Relay_Swimmer_2" = V3,
        "Relay_Swimmer_3" = V4,
        "Relay_Swimmer_4" = V5,
        Row_Numb
      ) %>%
      na_if_character("NA")

  } else {
    relay_swimmers_data <- data.frame(Row_Numb = as.numeric())
  }

  return(relay_swimmers_data)
}
