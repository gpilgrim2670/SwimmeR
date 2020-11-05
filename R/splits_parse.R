#' Collects splits within \code{swim_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{swim_parse}, extracts split times and associated row numbers
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr full_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_at
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param text output of \code{read_results} with tow numbers appended by \code{add_row_numbers}
#' @return returns a dataframe with split times and row numbers
#'
#' @seealso \code{splits_parse} runs inside \code{\link{swim_parse}} on the output of \code{\link{read_results}} with row numbers from \code{\link{add_row_numbers}}

splits_parse <- function(text) {

  # text <- read_results("inst/extdata/jets08082019_067546.pdf")
  # text <- read_results("inst/extdata/s2-results.pdf")
  # text <- add_row_numbers(text)

  ### collect row numbers from rows containing splits ###

  ### define strings ###
  # split_string <- "\\(\\d\\d\\.\\d\\d\\)|\\s\\d\\d\\.\\d\\d\\s"
  split_string <- "\\(\\d\\d\\.\\d\\d\\)"

  row_numbs <- text %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     split_string)] %>%
    # .[purrr::map_lgl(.,
    #                  stringr::str_detect,
    #                  "[:alpha:]", negate = TRUE)] %>%
    stringr::str_extract_all("\\d{1,}$")
  flag <- FALSE

  if(length(row_numbs) == 0){
    split_string <- "\\(\\d\\d\\.\\d\\d\\)|\\s\\d\\d\\.\\d\\d\\s"
    row_numbs <- text %>%
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       split_string)] %>%
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       "[:alpha:]", negate = TRUE)] %>%
      stringr::str_extract_all("\\d{1,}$")
    flag <- TRUE
  }

  minimum_row <- min(as.numeric(row_numbs))
  maximum_row <- as.numeric(length(text))

  ### pull out rows containing splits, which will remove row numbers ###
  if(flag == TRUE){
  suppressWarnings(
    data_1 <- text %>%
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       split_string)] %>%
      .[purrr::map_lgl(.,
                       stringr::str_detect,
                       "[:alpha:]", negate = TRUE)] %>%
      stringr::str_replace_all("\n", "") %>%
      stringr::str_replace_all("r\\:\\+\\s?\\d\\.\\d\\d", "") %>%
      stringr::str_extract_all(paste0("^\\s+\\d\\d\\.\\d\\d|", split_string)) %>%
      stringr::str_remove_all('\\"') %>%
      stringr::str_replace_all("\\(", " ") %>%
      stringr::str_replace_all("\\)", " ") %>%
      stringr::str_remove_all("c") %>%
      stringr::str_remove_all(',') %>%
      trimws())
    } else{
    suppressWarnings(
      data_1 <- text %>%
        .[purrr::map_lgl(.,
                         stringr::str_detect,
                         split_string)] %>%
        stringr::str_replace_all("\n", "") %>%
        stringr::str_replace_all("r\\:\\+\\s?\\d\\.\\d\\d", "") %>%
        stringr::str_extract_all(paste0("^\\s+\\d\\d\\.\\d\\d|", split_string)) %>%
        stringr::str_remove_all('\\"') %>%
        stringr::str_replace_all("\\(", " ") %>%
        stringr::str_replace_all("\\)", " ") %>%
        stringr::str_remove_all("c") %>%
        stringr::str_remove_all(',') %>%
        trimws())
  }


  #### add row numbers back in since they were removed ####
  data_1 <- paste(row_numbs, data_1, sep = "   ")

  #### break out by length ####
  data_1 <-
    unlist(purrr::map(data_1, stringr::str_split, "\\s{2,}"),
           recursive = FALSE)

  data_length_2 <- data_1[purrr::map(data_1, length) == 2]
  data_length_3 <- data_1[purrr::map(data_1, length) == 3]
  data_length_4 <- data_1[purrr::map(data_1, length) == 4]
  data_length_5 <- data_1[purrr::map(data_1, length) == 5]
  data_length_6 <- data_1[purrr::map(data_1, length) == 6]
  data_length_7 <- data_1[purrr::map(data_1, length) == 7]
  data_length_8 <- data_1[purrr::map(data_1, length) == 8]
  data_length_9 <- data_1[purrr::map(data_1, length) == 9]
  data_length_10 <- data_1[purrr::map(data_1, length) == 10]

  #### transform all lists to dataframes ####
  if (length(data_length_10) > 0) {
    df_10 <- data_length_10 %>%
      list_transform()
  } else {
    df_10 <- data.frame(Row_Numb = character(),
                        stringsAsFactors = FALSE)
  }

  if (length(data_length_9) > 0) {
    df_9 <- data_length_9 %>%
      list_transform()
  } else {
    df_9 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_8) > 0) {
    df_8 <- data_length_8 %>%
      list_transform()
  } else {
    df_8 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_7) > 0) {
    df_7 <- data_length_7 %>%
      list_transform()
  } else {
    df_7 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_6) > 0) {
    df_6 <- data_length_6 %>%
      list_transform()
  } else {
    df_6 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_5) > 0) {
    df_5 <- data_length_5 %>%
      list_transform()
  } else {
    df_5 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_4) > 0) {
    df_4 <- data_length_4 %>%
      list_transform()
  } else {
    df_4 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_3) > 0) {
    df_3 <- data_length_3 %>%
      list_transform()
  } else {
    df_3 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_2) > 0) {
    df_2 <- data_length_2 %>%
      list_transform()
  } else {
    df_2 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  #### bind up results ####
  # results are bound before going to splits_sort so that in cases where there are multiple rows with splits for the same race,
  # like in longer events with many splits, those splits can be collected and treated together
  data <-
    dplyr::bind_rows(df_10, df_9, df_8, df_7, df_6, df_5, df_4, df_3, df_2) %>%
    splits_sort(min_row = minimum_row) %>%
    dplyr::mutate(Row_Numb = as.numeric(Row_Numb) - 1) # make row number of split match row number of performance

  #### rename columns V1, V2 etc. by 50 ####
  old_names <- names(data)[grep("^V", names(data))]
  new_names <-
    paste("Split", seq(1, length(names(data)) - 1) * 50, sep = "_")

  data <- data %>%
    dplyr::rename_at(dplyr::vars(old_names), ~ new_names) %>%
    dplyr::mutate_at(dplyr::vars(new_names), as.numeric)

  return(data)

}

