# base <- "http://www.results.teamunify.com/clov/2019/CIFSTATEMEET/190510F0" # base url
# event_numbers <- 1:24 # sequence of numbers, total of 24 evetns across boys and girls
# event_numbers <- str_pad(event_numbers, width = 2, side = "left", pad = "0") # add leading zeros to single digit numbers
# CA_Links <- paste0(base, event_numbers, ".htm") # paste together base urls and sequence of numbers (with leading zeroes as needed)
#
# CA_Results <- map(CA_Links, read_results, node = "pre") %>% # map SwimmeR::read_results over the list of links
#   map(swim_parse, splits = TRUE) %>%
#   bind_rows() %>% # bind together results from each link
#   mutate(State = "CA") # add column for state since we'll be combining results with GA
#
# df <- CA_Results %>%
#   filter(DQ != 1,
#          stringr::str_detect(Event, "Diving") == FALSE, # diving does not have splits
#          stringr::str_detect(Event, "Relay") == FALSE, # relays do not have splits
#          stringr::str_detect(Event, "\\s50\\s") == FALSE) %>% # 50s do not have splits
#   dplyr::mutate(F_sec = sec_format(Finals_Time)) %>% # finals time in seconds
#   dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), ~ sec_format(.x))) %>% # all splits in seconds to account for splits over 59.99
#   dplyr::mutate(total = dplyr::select(., 10:19) %>% rowSums(na.rm = TRUE)) %>% # total up splits
#   dplyr::mutate(not_matching = dplyr::case_when(round(F_sec - total, 2) == 0 ~ FALSE, # does total match finals time?
#                                                 round(F_sec - total, 2) != 0 ~ TRUE))
#
# match_sum <- sum(df$not_matching, na.rm = TRUE)
#
# df %>%
#   filter(not_matching == TRUE) %>%
#   View()
#
#
# file <- read_results("inst/extdata/11102019roc.pdf")
# df <- swim_parse(file, splits = TRUE)
#
# file <- read_results('inst/extdata/7th-snsc-scm-2017-full-results.pdf')
# file <- read_results("https://www.swimming.org.sg/getattachment/SSA/SWIMMING/Events/7th-SNSC-SCM-2017/7th-snsc-scm-2017-full-results.pdf.aspx")
# df <-
#   swim_parse(
#     file,
#     avoid = c("NR\\:", "MR\\:"),
#     typo = c("Muhammad Azlan, Inarah Farahah 15 SwimDolphia Aquatic School"),
#     replacement = c("Muhammad Azlan, Inarah Farahah  15  SwimDolphia Aquatic School"),
#     splits = TRUE,
#     split_length = 25
#   )
#
# file <- read_results("https://www.swimming.org.sg/SSA/SWIMMING/Events/China-Life-48th-SNAG-Swimming-Championships/full-results.aspx")
# df <- swim_parse(file, splits = TRUE,
#                  avoid = c("SEA GAMES", "Meet Qualifying"))
#
#
# # str_detect("sds 12 xyt", "(?<=[:alpha:]\\s{1,20}\\d\\d) (?=[:alpha:])")
# # str_replace("12 xyt", "(?<=\\d) (?=[:alpha:])", "AAA")
#
# file <- read_results("http://www.section6swim.com/Results/GirlsHS/2014/CCAA-1/Single.htm")
# df <- swim_parse(file, splits = TRUE, typo = "&", replacement = " ")
#
# file <- read_results("http://www.section1swim.com/Results/BoysHS/2000/Sec1/Single.htm")
# df <- swim_parse(file, splits = TRUE, typo = c("  [A-Z]  ", "(?<=[:alpha:]) (?=[:digit:])") , replacement = c("", "  "))
#
# file <- read_results("http://www.section5swim.com/Results/GirlsHS/2000/Sec5/B/Single.htm")
# df <- swim_parse(file, splits = TRUE)
#
# file <- read_results("http://www.section1swim.com/Results/BoysHS/2020/Conf2/Single.htm")
# df <- swim_parse(file)
#
#
# name_test <- c("Mikulka, J.T.", "Jorgensen-Sauve, Kelsey M", "KAUS JR., HARRY L")
#
# # Name_String <- "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
# Name_String <- "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
# stringr::str_extract(name_test, Name_String)
# stringr::str_extract(name_test, "[:alpha:]+ [:alpha:]*\\., [:alpha:]+ [:alpha:]+")
#
# dplyr::anti_join(df_standard, df_test)
# dplyr::anti_join(df_test, df_standard) %>%
#   View()
#
#
# df <- data.frame(a = c("xxx", "123", "10000"),
#                     b = c("10000", "12", "dfdff"))
# df
#
# df %>%
#   mutate(dplyr::across(everything(), ~ stringr::str_replace_all(., "10000", 'replacement')))
#
# text <- read_results("https://www.teamunify.com/eznslsc/UserFiles/File/Meet-Results/2018-2019/ttsc10262018results-rev2.pdf")
# df <- swim_parse(read_results(file_5), typo = "Clifton Park-Hal-AD", replacement = "Clifton Park-Hal-AD  ")

# file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
# df <- swim_parse(
#   read_results(file),
#   typo =  c("\n", "Indiana  University", ", University of"),
#
#   replacement = c("\n", "Indiana University", "")
# )
