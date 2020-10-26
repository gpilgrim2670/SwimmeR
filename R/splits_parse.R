#' Collects splits within \code{swim_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{swim_parse}, extracts split times and associated row numbers
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr full_join
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
#'

# file <- read_results("~/SwimmeR/inst/extdata/s2-results.pdf")
# file <- read_results("inst/extdata/s2-results.pdf")
# avoid <- c("MR:")
# typo <-
#   c(
#     "Swim\\s{2,}Club",
#     "Performance\\s{2,}Swim",
#     "Swimming\\s{2,}Club",
#     "Stamford\\s{2,}American\\s{2,}Internationa",
#     "Uwcsea\\s{2,}Phoenix-ZZ",
#     "AquaTech\\s{2,}Swimming",
#     "Chinese\\s{2,}Swimming",
#     "Aquatic\\s{2,}Performance",
#     "SwimDolphia\\s{2}Aquatic School"
#   )
# replacement <-
#   c(
#     "Swim Club",
#     "Performance Swim",
#     "Swimming Club",
#     "Stamford American International",
#     "Uwcsea Phoenix-ZZ",
#     "AquaTech Swimming",
#     "Chinese Swimming",
#     "Aquatic Performance",
#     "SwimDolphia Aquatic School"
#   )
#
#
# text <- add_row_numbers(text = file)

splits_parse <- function(text) {
  ### collect row numbers from rows containing splits ###
  row_numbs <- text %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     "\\(\\d\\d\\.\\d\\d\\)")] %>%
    stringr::str_extract_all("\\d{1,}$")

  minimum_row <- min(as.numeric(row_numbs))
  maximum_row = as.numeric(length(text))

  ### pull out rows containing splits, which will remove row numbers ###
  data_1 <- text %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     "\\(\\d\\d\\.\\d\\d\\)")] %>%
    stringr::str_extract_all("[\\(\\s)]\\d\\d\\.\\d\\d[\\)\\s]") %>%
    stringr::str_remove_all('\\"') %>%
    stringr::str_replace_all("\\(", " ") %>%
    stringr::str_replace_all("\\)", " ") %>%
    stringr::str_remove_all("c") %>%
    stringr::str_remove_all(',') %>%
    trimws()


  ### add row numbers back in since they were removed ###
  data_1 <- paste(row_numbs, data_1, sep = "   ")

  ### break out by length
  data_1 <-
    unlist(purrr::map(data_1, stringr::str_split, "\\s{2,}"),
           recursive = FALSE)

  data_length_3 <- data_1[purrr::map(data_1, length) == 3]
  data_length_4 <- data_1[purrr::map(data_1, length) == 4]
  data_length_5 <- data_1[purrr::map(data_1, length) == 5]
  data_length_6 <- data_1[purrr::map(data_1, length) == 6]
  data_length_7 <- data_1[purrr::map(data_1, length) == 7]
  data_length_8 <- data_1[purrr::map(data_1, length) == 8]

  if (length(data_length_8) > 0) {
    df_8 <- data_length_8 %>%
      list_transform() %>%
      split_sort()
  } else {
    df_8 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_7) > 0) {
    df_7 <- data_length_7 %>%
      list_transform() %>%
      split_sort()
  } else {
    df_7 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_6) > 0) {
    df_6 <- data_length_6 %>%
      list_transform() %>%
      split_sort()
  } else {
    df_6 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_5) > 0) {
    df_5 <- data_length_5 %>%
      list_transform() %>%
      split_sort()
  } else {
    df_5 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_4) > 0) {
    df_4 <- data_length_4 %>%
      list_transform() %>%
      split_sort()
  } else {
    df_4 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if (length(data_length_3) > 0) {
    df_3 <- data_length_3 %>%
      list_transform() %>%
      split_sort()
  } else {
    df_3 <- data.frame(Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  data <- dplyr::full_join(df_8, df_7) %>%
    dplyr::full_join(df_6) %>%
    dplyr::full_join(df_5) %>%
    dplyr::full_join(df_4) %>%
    dplyr::full_join(df_3)

  return(data)

}

