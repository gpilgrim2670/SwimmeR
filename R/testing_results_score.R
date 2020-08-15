# library(SwimmeR)

##### Timed Finals #####
# base_6A <-
#   "https://www.uiltexas.org/tournament-results/swimming-diving/2020/6a/200214F0"
#
# base_5A <-
#   "https://www.uiltexas.org/tournament-results/swimming-diving/2020/5a/200214F0"
#
# real_time_links <- function(base, event_numbers) {
#   event_numbers <- sprintf("%02d", as.numeric(event_numbers))
#   links <- map(base, paste0, event_numbers, ".htm")
#   links <- unlist(links, recursive = FALSE)
#   return(links)
# }
#
# TX_Links <-
#   real_time_links(base = c(base_6A, base_5A),
#                   event_numbers = 1:24)
#
# TX_Results <-
#   map(TX_Links, read_results, node = "pre") %>% # map SwimmeR::read_results over the list of links
#   map(swim_parse,
#       typo = c("\\s\\d{1,2}\\s{2,}"),
#       replacement = c(" ")) %>%
#   bind_rows() %>% # bind together results from each link
#   select(Name, School, Finals_Time, Event) %>% # only the columns we need
#   mutate(State = "TX") # add column for state since we'll be combining results with OH
#
#
# Ohio_DI_Link <-
#   "https://ohsaaweb.blob.core.windows.net/files/Sports/Swimming-Diving/2019-20/State%20Tournament/Division%20I/2020DivisionISwimmingFinalsResults.pdf"
#
# OH_DI <- swim_parse(
#   read_results(Ohio_DI_Link),
#   typo = c(# fix issue with some class designation strings
#     "SR\\s{2,}",
#     "JR\\s{2,}",
#     "SO\\s{2,}",
#     "FR\\s{2,}"),
#   replacement = c("SR ",
#                   "JR ",
#                   "SO ",
#                   "FR ")
# )
#
# OH_DI_Diving_Link <-
#   "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/OH_DI_Diving_2020.txt"
#
# OH_DI_Diving <-
#   read_delim(url(OH_DI_Diving_Link), delim = "\t") %>%
#   tidyr::fill(Event, .direction = "down") %>%
#   select(-c(Semis_Time, Order)) %>%
#   na_if("Cut")
#
# Ohio_DII_Link <-
#   "https://ohsaaweb.blob.core.windows.net/files/Sports/Swimming-Diving/2019-20/State%20Tournament/Division%20II/2020DivisionIISwimmingFinalsResults.pdf"
#
# OH_DII <- swim_parse(read_results(Ohio_DII_Link))
#
# OH_Results <- bind_rows(OH_DI, OH_DII, OH_DI_Diving) %>%
#   mutate(State = "OH",
#          Event = str_remove(Event, " DIVISION \\d")) # remove division
#
# Results <- bind_rows(TX_Results, OH_Results) %>%
#   mutate(Gender = case_when(
#     str_detect(Event, "Girls") == TRUE ~ "Girls",
#     str_detect(Event, "Boys") == TRUE ~ "Boys"
#   )) %>%
#   filter(str_detect(Event, "Swim-off") == FALSE) %>%
#   select(-Points)
#
# Results_Final <-
#   results_score(
#     results = Results,
#     events = unique(Results$Event),
#     meet_type = "timed_finals",
#     lanes = 8,
#     scoring_heats = 2,
#     point_values = c(20, 17, 16, 15, 14, 13, 12, 11, 9, 7, 6, 5, 4, 3, 2, 1)
#   )
#
# Scores <- Results_Final %>%
#   group_by(State, Gender) %>%
#   summarise(Score = sum(Points))
#
# Scores %>%
#   arrange(Gender, desc(Score)) %>%
#   ungroup()


#### Prelims_Finals ####
# BigTenRaw <- read_results("C:/Users/gpilgrim/Documents/SwimmeR/inst/extdata/BigTen_WSWIM_2018.pdf")
# BigTenRaw <- read_results("~/SwimmeR/inst/extdata/BigTen_WSWIM_2018.pdf")
#
# BigTen <- swim_parse(BigTenRaw,
#                            typo = c(
#                              "^\\s{1,}\\*",
#                             "^\\s{1,}(\\d{1,2})\\s{2,}", # not sure if needed
#                             # "\\s{1}\\d{1,2}\\s{2,}",
#                                     ",\\s{1,}University\\s{1,}of",
#                                     "University\\s{1,}of\\s{1,}",
#                                     # ",\\s{1,}Madi",
#                                     "\\s{1,}University",
#                             "SR\\s{2,}",
#                             "JR\\s{2,}",
#                             "SO\\s{2,}",
#                             "FR\\s{2,}"),
#                            replacement = c(
#                              " ",
#                              "  \\1 ",
#                              # "  ",
#                              "", "", "",
#                              "SR ",
#                              "JR ",
#                              "SO ",
#                              "FR "),
#                      avoid = c("B1G", "Pool"))
#
# # unique(BigTen$School)
#
# BigTen <- BigTen %>%
#   filter(str_detect(Event, "Time Trial") == FALSE,
#          str_detect(Event, "Swim-off") == FALSE) %>%
#   mutate(School = case_when(School == "Wisconsin, Madi" ~ "Wisconsin",
#                             TRUE ~ School))
#
# # unique(BigTen_2$Event)
#
# df <- BigTen %>%
#   results_score(events = unique(BigTen_2$Event),
#                 meet_type = "prelims_finals",
#                 lanes = 8,
#                 scoring_heats = 3,
#                 point_values = c(32, 28, 27, 26, 25, 24, 23, 22, 20, 17, 16, 15, 14 ,13, 12, 11, 9, 7, 6, 5, 4, 3, 2, 1))
#
# df <- df[order(match(df$Event, BigTen_2$Event)), ]
#
# df %>%
#   group_by(School) %>%
#   # filter(Event %in% unique(BigTen_2$Event)[1:7]) %>%
#   summarise(Score = sum(Points, na.rm = TRUE)) %>%
#   arrange(desc(Score)) %>%
#   ungroup() %>%
#   summarize(total = sum(Score)) # totals to 8596
#
# 1465 + 1152.5 + 1094.5 + 1049 + 755 + 693 + 528 + 418 + 386 + 358 + 338 + 187 + 172
# # 8596 # actual score total
#
# BigTenRaw[1170]
# BigTenRaw[884]
#
# unique(df$Event)

#
#
# name <- c(rep(c("Jane", "Sarah", "Sally"), 2), "Prelim1", "Prelim2")
# score <- c(seq(6,1, -1), 0.5, 0.25)
# event <- "diving"
#
# df_test <- data.frame(name = name, score = score, event = event, stringsAsFactors = FALSE)
#
# df_test %>%
#   group_by(name, event) %>%
#   slice(1)
# df_test %>%
#   group_by(Name, Event)
#
# results_3 <- results %>%
#   dplyr::group_by(Event) %>%
#   dplyr::slice((1 + (lanes * 2)):max_place)


# BigTen_2 %>%
#   dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "relay") == FALSE) %>%
#   # dplyr::group_by(Event, Name, School) %>%
#   distinct(Event, Name, School, .keep_all = TRUE) %>%
#   View()

