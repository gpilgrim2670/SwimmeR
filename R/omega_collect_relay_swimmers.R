#' Collects relay swimmers as a data frame within \code{swim_parse_omega}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr na_if
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param x output from \code{read_results} followed by \code{add_row_numbers}
#' @return returns a data frame of relay swimmers and the associated performance row number
#'
#' @seealso \code{collect_relay_swimmers_data} runs inside of \code{swim_parse_omega}
#'

collect_relay_swimmers_omega <- function(x){

  # x <- "https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73B1_SWMW4X100MFR----------HEAT000100--.pdf" %>%
  #   read_results() %>%
  #   add_row_numbers()

  # x <- as_lines_list_2

  relay_swimmer_string <- "^\n\\s*[:alpha:]"
  record_string <- "\n\\s+WR\\s|\n\\s+OR\\s"

  row_numbs_relay_swimmer <- x %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     relay_swimmer_string)] %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     "[:alpha:][A-Za-z\\s]*")] %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     "\\d{2}\\.\\d{2}")] %>%
    .[!purrr::map_lgl(.,
                     stringr::str_detect,
                     record_string)] %>%
    stringr::str_extract_all("\\d{1,}$")

  if (length(row_numbs_relay_swimmer) > 0) {
    minimum_row <- min(as.numeric(row_numbs_relay_swimmer))

    suppressWarnings(
      data_1_relay_swimmer <- x %>%
        .[purrr::map_lgl(.,
                         stringr::str_detect,
                         relay_swimmer_string)] %>%
        .[purrr::map_lgl(.,
                         stringr::str_detect,
                         "\\d{2}\\.\\d{2}")] %>%
        stringr::str_remove_all("\n") %>%
        stringr::str_extract_all("[:alpha:][A-Za-z\\s]*") %>%
        .[lengths(.) == 1] %>%
        .[purrr::map_lgl(.,
                         stringr::str_detect,
                         "[:alpha:]\\s[:alpha:]")] %>%
        trimws()
    )

    data_1_relay_swimmer <- paste(row_numbs_relay_swimmer, data_1_relay_swimmer, sep = "   ")

    data_1_relay_swimmer <-
      unlist(purrr::map(data_1_relay_swimmer, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    relay_swimmers_data <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 2] %>%
      list_transform()

    if(length(relay_swimmers_data) < 1){
      relay_swimmers_data <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 3] %>%
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
        dplyr::na_if("NA")

    } else if(length(relay_swimmers_data) == 9) {
      relay_swimmers_data <- relay_swimmers_data %>%
        dplyr::select(
          "Relay_Swimmer_1" = V2,
          "Relay_Swimmer_1_Gender" = V3,
          "Relay_Swimmer_2" = V4,
          "Relay_Swimmer_2_Gender" = V5,
          "Relay_Swimmer_3" = V6,
          "Relay_Swimmer_3_Gender" = V7,
          "Relay_Swimmer_4" = V8,
          "Relay_Swimmer_5_Gender" = V9,
          Row_Numb
        ) %>%
        dplyr::na_if("NA")
    }

  } else {
    relay_swimmers_data <- data.frame(Row_Numb = as.numeric())
  }

  return(relay_swimmers_data)
}
