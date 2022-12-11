#' Collects relay swimmers as a data frame within \code{swim_parse_splash}
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
#' @param relay_indent the number of spaces relay swimmer lines are indented
#'   compared to regular swimmer lines
#' @return returns a data frame of relay swimmers and the associated performance
#'   row number
#'
#' @seealso \code{collect_relay_swimmers_data} runs inside of
#'   \code{swim_parse_splash}
#'

collect_relay_swimmers_splash <-
  function(x, relay_indent = Indent_Length) {

    #### testing ####
    # x <-
    #   "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Open_Belgian_Champs_2017.pdf" %>%
    #   read_results() %>%
    #   add_row_numbers()
    #
    # x <- as_lines_list_2
    #
    # x <-
    #   "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Splash/Glenmark_Senior_Nationals_2019.pdf" %>%
    #   read_results() %>%
    #   add_row_numbers()
    # x <-
    #   "http://www.toptime.be/oresults/ResultList_75_us.pdf" %>%
    #   read_results() %>%
    #   add_row_numbers()
    # relay_indent <- x %>%
    #   determine_indent_length_splash(time_score_string = "1?\\:?\\d{0,2}\\:?\\d{1,3}\\.\\d{2}")

    #### Actual Function ####

    relay_swimmer_string <-
      paste0("^\n", "\\s{", relay_indent , ",22}", "[:alpha:]", "|[\\+|\\-]\\d\\.\\d\\d.*[\\+|\\-]\\d\\.\\d\\d|\\d{2}\\.\\d{2}.*\\d{2}\\.\\d{2}.*[:alpha:].*\\d{2}\\.\\d{2}.*\\d{2}\\.\\d{2}")
    record_string <-
      "\n\\s+[:upper:]R\\s|\n\\s+US\\s|[:upper:][:alpha:]+ Record|\n\\s+W[:upper:]\\|Open [W|M] | Record "
    header_string <-
      "Record\\s+Split|Record\\s+Name|Reaction\\sTime|EVENT NO\\."
    # fina_points_string <- "\\s\\d{3}\\s"
    # date_string <- "\\d\\d?rd|\\d\\d?th"

    row_numbs_relay_swimmer <- x %>%
      .[stringr::str_detect(., relay_swimmer_string)] %>%
      .[stringr::str_detect(., "[:alpha:][A-Za-z\\s]*")] %>%
      .[stringr::str_detect(., "[:alpha:]\\,?\\.?\\s[:alpha:]")] %>%
      .[stringr::str_detect(., record_string, negate = TRUE)] %>%
      .[stringr::str_detect(., header_string, negate = TRUE)] %>%
      # .[stringr::str_detect(., fina_points_string, negate = TRUE)] %>%
      # .[stringr::str_detect(., date_string, negate = TRUE)] %>%
      .[stringr::str_count(., "\\(") < 2] %>%
      stringr::str_extract_all("\\d{1,}$")

    if (length(row_numbs_relay_swimmer) > 0) {
      relay_rows <- unlist(row_numbs_relay_swimmer) %>%
        paste0(" ", ., "$") %>%
        paste(collapse = "|")

      minimum_row <- min(as.numeric(row_numbs_relay_swimmer))

      suppressWarnings(
        data_1_relay_swimmer <- x %>%
          .[stringr::str_detect(., relay_rows)] %>%
          stringr::str_remove_all("\n") %>%
          stringr::str_remove_all("\\s\\d{1,}\\:?\\d{0,}\\.?\\d{0,}") %>% # remove splits
          stringr::str_remove_all("[\\+|\\-]\\d\\.\\d\\d") %>% # remove reaction times
          trimws()
      )

      data_1_relay_swimmer <-
        paste(row_numbs_relay_swimmer, data_1_relay_swimmer, sep = "   ")

      data_1_relay_swimmer <-
        unlist(purrr::map(data_1_relay_swimmer, stringr::str_split, "\\s{2,}"),
               recursive = FALSE)

      relay_swimmers_data_2 <-
        data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 2]
      if (length(relay_swimmers_data_2) > 0) {
        relay_swimmers_data_2 <- relay_swimmers_data_2 %>%
          list_transform() %>%
          lines_sort(min_row = minimum_row)
      } else {
        relay_swimmers_data_2 <- data.frame(Row_Numb = as.character())
      }

      relay_swimmers_data_3 <-
        data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 3]
      if (length(relay_swimmers_data_3) > 0) {
        relay_swimmers_data_3 <- relay_swimmers_data_3 %>%
          list_transform() %>%
          lines_sort(min_row = minimum_row)
      } else {
        relay_swimmers_data_3 <- data.frame(Row_Numb = as.character())
      }

      relay_swimmers_data <-
        bind_rows(relay_swimmers_data_2, relay_swimmers_data_3) %>%
        dplyr::mutate(Row_Numb = as.numeric(Row_Numb))

      if (length(relay_swimmers_data) == 5) {
        relay_swimmers_data <- relay_swimmers_data %>%
          dplyr::select(
            "Relay_Swimmer_1" = V2,
            "Relay_Swimmer_2" = V3,
            "Relay_Swimmer_3" = V4,
            "Relay_Swimmer_4" = V5,
            Row_Numb
          ) %>%
          na_if_character("NA")
      }

      } else {
        relay_swimmers_data <- data.frame(Row_Numb = as.numeric())
      }

    return(relay_swimmers_data)
  }
