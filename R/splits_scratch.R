#' #' Collects splits within \code{swim_parse}
#' #'
#' #' Takes the output of \code{read_results} and, inside of \code{swim_parse}, extracts split times and associated row numbers
#' #'
#' #' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#' #'
#' #' @export
#' #'
#' #' @importFrom dplyr mutate
#' #' @importFrom dplyr lead
#' #' @importFrom dplyr lag
#' #' @importFrom dplyr case_when
#' #' @importFrom dplyr na_if
#' #' @importFrom dplyr select
#' #' @importFrom dplyr bind_rows
#' #' @importFrom dplyr group_by
#' #' @importfrom dplyr row_number
#' #' @importFrom stringr str_replace_all
#' #' @importFrom stringr str_extract_all
#' #' @importFrom stringr str_split
#' #' @importFrom stringr str_detect
#' #' @importFrom purrr map_lgl
#' #' @importFrom purrr map
#' #' @improtFrom tidyr fill
#' #' @importfrom tidyr pivot_wider
#' #'
#' #' @param text output of \code{read_results} with tow numbers appended by \code{add_row_numbers}
#' #' @return returns a dataframe with split times and row numbers
#' #'
#' #' @seealso \code{splits_parse} runs inside \code{\link{swim_parse}} on the output of \code{\link{read_results}} with row numbers from \code{\link{add_row_numbers}}
#' #'
#'
#' # file <- read_results("~/SwimmeR/inst/extdata/s2-results.pdf")
#' file <- read_results("inst/extdata/s2-results.pdf")
#' avoid <- c("MR:")
#' typo <-
#'   c(
#'     "Swim\\s{2,}Club",
#'     "Performance\\s{2,}Swim",
#'     "Swimming\\s{2,}Club",
#'     "Stamford\\s{2,}American\\s{2,}Internationa",
#'     "Uwcsea\\s{2,}Phoenix-ZZ",
#'     "AquaTech\\s{2,}Swimming",
#'     "Chinese\\s{2,}Swimming",
#'     "Aquatic\\s{2,}Performance",
#'     "SwimDolphia\\s{2}Aquatic School"
#'   )
#' replacement <-
#'   c(
#'     "Swim Club",
#'     "Performance Swim",
#'     "Swimming Club",
#'     "Stamford American International",
#'     "Uwcsea Phoenix-ZZ",
#'     "AquaTech Swimming",
#'     "Chinese Swimming",
#'     "Aquatic Performance",
#'     "SwimDolphia Aquatic School"
#'   )
#'
#'
#' text <- add_row_numbers(text = file)
#'
#'
#' splits_parse <- function(text) {
#'   row_numbs <- text %>%
#'     .[purrr::map_lgl(.,
#'                      stringr::str_detect,
#'                      "\\(\\d\\d\\.\\d\\d\\)")] %>%
#'     stringr::str_extract_all("\\d{1,}$")
#'
#'   minimum_row <- min(as.numeric(row_numbs))
#'   maximum_row = as.numeric(length(text))
#'
#'   data_1 <- text %>%
#'     .[purrr::map_lgl(# new 10/16
#'       .,
#'       stringr::str_detect,
#'       "\\(\\d\\d\\.\\d\\d\\)")] %>%
#'     stringr::str_extract_all("[\\(\\s)]\\d\\d\\.\\d\\d[\\)\\s]") %>%
#'     str_remove_all('\\"') %>%
#'     str_replace_all("\\(", " ") %>%
#'     str_replace_all("\\)", " ") %>%
#'     str_remove_all("c") %>%
#'     str_remove_all(',') %>%
#'     trimws()
#'
#'   data_2 <- paste(row_numbs, data_1, sep = "   ")
#'
#'   data_3 <-
#'     unlist(purrr::map(data_2, stringr::str_split, "\\s{2,}"),
#'            recursive = FALSE)
#'
#'   # data_3 <- paste(data_2, row_numbs, sep = "   ")
#'
#'   data_length_3 <- data_3[purrr::map(data_3, length) == 3]
#'   data_length_4 <- data_3[purrr::map(data_3, length) == 4]
#'   data_length_5 <- data_3[purrr::map(data_3, length) == 5]
#'   data_length_6 <- data_3[purrr::map(data_3, length) == 6]
#'
#'   df_6 <- data_length_6 %>%
#'   list_transform()
#'
#'   df_5 <- # works!
#'     data_length_5 %>%
#'     list_transform() %>%
#'     split_sort()
#'
#'   split_sort <- function(x) {
#'     x <- x %>%
#'       mutate(Row_Numb = as.numeric(V1)) %>%
#'       mutate(
#'         Row_Numb_2 = case_when(
#'           Row_Numb - lag(Row_Numb, default = minimum_row) == 0 ~ "Different",
#'           Row_Numb - lag(Row_Numb, default = minimum_row) <= 1 ~ "Same",
#'           Row_Numb - lag(Row_Numb, default = minimum_row) > 1 ~ "Different",
#'           TRUE ~ "Different"
#'         )
#'       ) %>%
#'       mutate(
#'         Row_Fill = case_when(Row_Numb_2 == "Different" ~ Row_Numb - 1,
#'                              Row_Numb_2 == "Same" ~ 0),
#'         Row_Fill = as.character(Row_Fill)
#'       ) %>%
#'       na_if(0) %>%
#'       mutate(Row_Fill = fill_down(Row_Fill)) %>%
#'
#'       select(-V1, -Row_Numb, -Row_Numb_2) %>%
#'       mutate(row_index = 1:dplyr::n()) %>%
#'       reshape(direction = "wide",
#'               idvar = "Row_Fill",
#'               timevar = "row_index") %>%
#'       fill_left()
#'
#'     names(x)[1] <- "Row_Numb"
#'
#'     return(x)
#'  }
