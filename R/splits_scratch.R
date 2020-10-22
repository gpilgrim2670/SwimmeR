# file <- read_results("~/SwimmeR/inst/extdata/s2-results.pdf")
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
#
#
# splits_parse <- function(text) {
#   row_numbs <- text %>%
#     .[purrr::map_lgl(.,
#                      stringr::str_detect,
#                      "\\(\\d\\d\\.\\d\\d\\)")] %>%
#     stringr::str_extract_all("\\d{1,}$")
#
#   minimum_row <- min(as.numeric(row_numbs))
#   maximum_row = as.numeric(length(text))
#
#   data_1 <- text %>%
#     .[purrr::map_lgl(# new 10/16
#       .,
#       stringr::str_detect,
#       "\\(\\d\\d\\.\\d\\d\\)")] %>%
#     stringr::str_extract_all("[\\(\\s)]\\d\\d\\.\\d\\d[\\)\\s]") %>%
#     str_remove_all('\\"') %>%
#     str_replace_all("\\(", " ") %>%
#     str_replace_all("\\)", " ") %>%
#     str_remove_all("c") %>%
#     str_remove_all(',') %>%
#     trimws()
#
#   data_2 <- paste(data_1, row_numbs, sep = "   ")
#
#   data_3 <-
#     unlist(purrr::map(data_2, stringr::str_split, "\\s{2,}"),
#            recursive = FALSE)
#
#   # data_3 <- paste(data_2, row_numbs, sep = "   ")
#
#   data_length_5 <- data_3[purrr::map(data_3, length) == 5]
#   data_length_6 <- data_3[purrr::map(data_3, length) == 6]
#
#   df_6 <- #url76, url14, url78
#     as.data.frame(t(as.data.frame(data_length_6)),
#                   row.names = FALSE,
#                   stringsAsFactors = FALSE)
#
#   df_5 <- # works!
#     as.data.frame(t(as.data.frame(data_length_5)),
#                   row.names = FALSE,
#                   stringsAsFactors = FALSE) %>%
#   mutate(Row_Numb = as.numeric(V5)) %>%
#   mutate(
#     Row_Numb_2 = case_when(
#       Row_Numb - lag(Row_Numb, default = minimum_row) == 0 ~ "Different",
#       Row_Numb - lag(Row_Numb, default = minimum_row) <= 1 ~ "Same",
#       Row_Numb - lag(Row_Numb, default = minimum_row) > 1 ~ "Different",
#       TRUE ~ "Different"
#     )
#   ) %>%
#   mutate(
#     Row_Fill = case_when(Row_Numb_2 == "Different" ~ Row_Numb - 1,
#                          Row_Numb_2 == "Same" ~ 0),
#     Row_Fill = as.character(Row_Fill)
#   ) %>%
#   na_if(0) %>%
#   tidyr::fill(Row_Fill, .direction = "down") %>%
#     select(-V5, -Row_Numb, -Row_Numb_2) %>%
#     group_by(Row_Fill) %>%
#     mutate(rn = dplyr::row_number(Row_Fill)) %>%
#     tidyr::pivot_wider(names_from = rn, values_from = V1:V4)
#
#
#   df_5_clean <- as.data.frame(t(apply(df_5, 1, function(x) {return(c(x[!is.na(x)], x[is.na(x)]))})))
#   colnames(df_5_clean) = colnames(df_5)
#
#
#   df_final <- df %>%
#     mutate(
#       Event_Row_Min = as.numeric(V2),
#       Event_Row_Max = dplyr::lead(Event_Row_Min, 1L, default = length(text)) - 1,
#     )
#   return(df_final)
# }
