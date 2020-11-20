# base <- "http://www.results.teamunify.com/clov/2019/CIFSTATEMEET/190510F0" # base url
# event_numbers <- 1:24 # sequence of numbers, total of 24 events across boys and girls
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
# name_test <-
#   c(
#     "Mikulka, J.T.",
#     "Jorgensen-Sauve, Kelsey M",
#     "KAUS JR., HARRY L",
#     "Anderson P C Davies, Tiago",
#     "Ritaj El Ghissassi"
#   )
#
# # Name_String <- "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
# Name_String <-
#   "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
# stringr::str_extract(name_test, Name_String)
# stringr::str_extract(name_test, "[:alpha:]+ [:alpha:]*\\., [:alpha:]+ [:alpha:]+")

#
# dplyr::anti_join(df_standard, df_test)
# dplyr::anti_join(df_test, df_standard) %>%
#   View()

#
# df <- data.frame(a = c("xxx", "123", "10000"),
#                     b = c("10000", "12", "dfdff"))
# df
#
# df %>%
#   mutate(dplyr::across(everything(), ~ stringr::str_replace_all(., "10000", 'replacement')))
#
# text <- read_results("http://www.teamunify.com/eznslsc/UserFiles/File/Meet-Results/2018-2019/baac05052019_089141.pdf")
# df <- swim_parse(text, typo = "(?<=[:alpha:]) (?=[:digit:])", replacement = "   ")

# file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
# df <- swim_parse(
#   read_results(file),
#   typo =  c("\n", "Indiana  University", ", University of"),
#
#   replacement = c("\n", "Indiana University", "")
# )


# Niagara_2018_2019 <-
#   map(
#     clean_results,
#     safely(Swim_Parse, otherwise = NA),
#     typo = c("Greater Rochester Area YMCA --NI", "Clifton Park-Hal-AD", "(?<=[:alpha:]) (?=[:digit:])", "111 ", "Alexis/Lexi J", "DQY", "La Face, Isabella or Bella F", "Cunningham, Rhys *", "(?<=[:digit:]) (?=[:alpha:])"),
#     replacement = c("Greater Rochester Area YMCA-NI", "Clifton Park-Hal-AD  ", "   ", "", "Alexis J", "DQ", "La Face, Isabella", "Cunningham, Rhys", "   ")
#   )

# base <- "http://sidearmstats.com/auburn/swim/200218F0"
# event_numbers <-
#   1:42 # sequence of numbers, total of 42 events across men and women
# event_numbers <-
#   str_pad(event_numbers,
#           width = 2,
#           side = "left",
#           pad = "0") # add leading zeros to single digit numbers
# SEC_Links <-
#   paste0(base, event_numbers, ".htm") # paste together base urls and sequence of numbers (with leading zeroes as needed)
#
# SEC_Results <-
#   map(SEC_Links, read_results, node = "pre") %>% # map SwimmeR::read_results over the list of links
#   map(
#     swim_parse,
#     typo = c(
#       "A&M",
#       "FLOR",
#       "Celaya-Hernande",
#       # names which were cut off, and missing the last, first structure
#       "Hernandez-Tome",
#       "Garcia Varela,",
#       "Von Biberstein,"
#     ),
#     replacement = c(
#       "AM",
#       "Florida",
#       "Celaya, Hernande",
#       # replacement names that artificially impose last, first structure.  Names can be fixed after parsing
#       "Hernandez, Tome",
#       "Garcia, Varela",
#       "Von, Biberstein"
#     )
#   ) %>%
#   bind_rows()

# ISL_1 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_16102020_Budapest_Match_1.pdf"
# ISL_2 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_18102020_Budapest_Match_2.pdf"
# ISL_3 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_24102020_Budapest_Match_3.pdf"
# ISL_4 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_26102020_Budapest_Match_4.pdf"
# ISL_5 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_30102020_Budapest_Match_5.pdf"
# ISL_6 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_01112020_Budapest_Match_6.pdf"
# ISL_7 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_05112020_Budapest_Match_7.pdf"
# ISL_8 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_05112020_Budapest_Match_8.pdf"
# ISL_9 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_09112020_Budapest_Match_9.pdf"
# ISL_10 <- "C:/Users/gpilgrim/Documents/Pilgrim_Data/ISL/Season_2_2020/ISL_09112020_Budapest_Match_10.pdf"

# ISL_1_df <- swim_parse_ISL(read_results(ISL_1))
# ISL_2_df <- swim_parse_ISL(read_results(ISL_2))
# ISL_3_df <- swim_parse_ISL(read_results(ISL_3))
# ISL_4_df <- swim_parse_ISL(read_results(ISL_4))
# ISL_5_df <- swim_parse_ISL(read_results(ISL_5))
# ISL_6_df <- swim_parse_ISL(read_results(ISL_6))
# ISL_7_df <- swim_parse_ISL(read_results(ISL_7))
# ISL_8_df <- swim_parse_ISL(read_results(ISL_8))
# ISL_9_df <- swim_parse_ISL(read_results(ISL_9))
# ISL_10_df <- swim_parse_ISL(read_results(ISL_10), splits = TRUE, relay_swimmers = FALSE)

# df <- swim_parse(read_results("http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm"), splits = TRUE, relay_swimmers = TRUE)
#
# df <- swim_parse(read_results(system.file("extdata", "11102019roc.pdf", package = "SwimmeR")), splits = TRUE, relay_swimmers = TRUE)

# df <- swim_parse_ISL(read_results("https://isl.global/wp-content/uploads/2019/10/dallas_lewisville_isl_results_day_1.pdf"), splits= TRUE, relay_swimmers = TRUE)
#
file <- read_results("https://iuhoosiers.com/services/download_file.ashx?file_location=https://s3.amazonaws.com/sidearm.sites/iuhoosiers.com/documents/2019/2/1/Results_IU_UL.pdf")
df <- swim_parse(file, splits = TRUE, relay_swimmers = TRUE)
