#' Collects relay swimmers as a data frame within \code{swim_parse}
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @param x output from \code{read_results} followed by \code{add_row_numbers}
#' @param typo_2 list of typos from \code{swim_parse}
#' @param replacement_2 list of replacements for typos from \code{swim_parse}
#' @return returns a data frame of relay swimmers and the associated performance row number
#'
#' @seealso \code{collect_relay_swimmers} runs inside of \code{swim_parse}
#'

collect_relay_swimmers <- function(x, typo_2 = typo, replacement_2 = replacement){
  # x <- read_results("http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm")
  #
  # x <- read_results(system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR"))
  # x <- add_row_numbers(x)

  relay_swimmer_string <- "\n\\s*[1-4]\\)"

  row_numbs_relay_swimmer <- x %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     relay_swimmer_string)] %>%
    stringr::str_extract_all("\\d{1,}$")

  if (length(row_numbs_relay_swimmer) > 0) {
    minimum_row <- min(as.numeric(row_numbs_relay_swimmer))

    suppressWarnings(
      data_1_relay_swimmer <- x %>%
        .[purrr::map_lgl(.,
                         stringr::str_detect,
                         relay_swimmer_string)] %>%
        stringr::str_remove_all("\n") %>%
        stringr::str_replace_all(stats::setNames(replacement_2, typo_2)) %>%
        stringr::str_remove_all("\\)") %>%
        stringr::str_remove_all("[A-Z]\\d{1,3}") %>% # for M25 designations in masters - Male 25
        stringr::str_remove_all("r\\:\\+?\\-?\\d?\\.\\d\\d") %>% # for reaction pad outputs
        stringr::str_remove_all("r\\:NRT") %>% # for reaction time fail to register
        stringr::str_remove_all("\\d+") %>% # all digits
        stringr::str_remove_all("r\\:\\+?\\-?\\.") %>%
        stringr::str_remove_all(" SR| JR| SO| FR") %>% # grade designators
        trimws()
    )

    data_1_relay_swimmer <- paste(row_numbs_relay_swimmer, data_1_relay_swimmer, sep = "   ")

    data_1_relay_swimmer <-
      unlist(purrr::map(data_1_relay_swimmer, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    data_length_5_relay_swimmer <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 5] # all four swimmers on one line
    data_length_4_relay_swimmer <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 4] # all four swimmers on one line but one is missing
    data_length_3_relay_swimmer <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 3] # for two-line relays, two swimmers per line
    data_length_2_relay_swimmer <- data_1_relay_swimmer[purrr::map(data_1_relay_swimmer, length) == 2] # for two-line relays, two swimmers per line, but one is missing

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
        list_transform()

    } else {
      df_2_relay_swimmer <- data.frame(Row_Numb = character(),
                                       stringsAsFactors = FALSE)
    }

    #### bind up results ####
    # results are bound before going to lines_sort so that in cases where there are multiple rows with splits for the same race,
    # like in results where relays swimmers are reported on two lines, the results can be collected together
    relay_swimmers <-
      dplyr::bind_rows(df_5_relay_swimmer, df_4_relay_swimmer, df_3_relay_swimmer, df_2_relay_swimmer) %>%
      lines_sort(min_row = minimum_row) %>%
      dplyr::mutate(Row_Numb = as.numeric(Row_Numb) - 1) %>%   # make row number of relay match row number of performance
      dplyr::rename(
        "Relay_Swimmer_1" = V2,
        "Relay_Swimmer_2" = V3,
        "Relay_Swimmer_3" = V4,
        "Relay_Swimmer_4" = V5
      )

  } else {
    relay_swimmers <- data.frame(Row_Numb = as.numeric())
  }

  return(relay_swimmers)
}
