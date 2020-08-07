# library(data.table)
# library(readr)
# library(dplyr)
# library(purrr)
# library(stringr)
#
# df <- read_delim("inst/extdata/Meet Results-Nassau County Championships-09Feb2011-002.hy3", delim = "\\s\\2", col_names = FALSE)
#
# # as_lines <- str_extract_all(df, "\n.*")
# as_lines_list_2 <- unlist(df, recursive = FALSE)
# row_numbs <- seq(1, length(as_lines_list_2), 1)
# as_lines_list_2 <- paste(as_lines_list_2, row_numbs, sep = "  ")
#
#
# # data beginning with E1M contains  %>%
# entry_1 <- as_lines_list_2 %>%
#   stringr::str_extract_all("^E1M.*|^E1F") %>%
#   .[purrr::map(., length)>0] %>%
#   str_replace_all("([:alpha:]{1,})\\s{1,}([:alpha:]{1,})", "\\1\\2") %>%
#   trimws()
#
# entry_2 <-
#   unlist(purrr::map(entry_1, stringr::str_split, "\\s{1,}"), recursive = FALSE)
#
# # unique(purrr::map(data_2, length))
#
# entry_3 <- data.frame(entry_2) %>%
#   t()
#
# rownames(entry_3) <- NULL
#
# entry_4 <- data.frame(entry_3)
# entry_4 <- entry_4[c("X2","X3","X8", "X14")]
# colnames(entry_4) <- c("ID", "Event", "Seed_Time", "Row_Numb")
# entry_5 <- entry_4 %>%
#   group_by(ID, Event, Seed_Time) %>%
#   summarise(Row_Numb = min(as.numeric(Row_Numb), na.rm = TRUE)) %>%
#   arrange(Row_Numb)
#
# entry <- entry_5 %>%
#   mutate(ID_Numb = str_extract(ID, "^\\d{1,}"),
#          Row_Numb = as.numeric(Row_Numb)) %>%
#   mutate(Gender = case_when(str_detect(ID, "MB$") ~ "M",
#                             str_detect(ID, "FG$") ~ "F")) %>%
#   mutate(Course = str_extract(Seed_Time, "[:alpha:]$"),
#          Course = case_when(Course == "Y" ~ "Yard",
#                             Course == "M" ~ "Meter"),
#          Event = case_when(Event == "25A" ~ paste("25", Course,  "Freestyle"),
#                            Event == "50A" ~ paste("50", Course,  "Freestyle"),
#                            Event == "100A" ~ paste("100", Course,  "Freestyle"),
#                            Event == "200A" ~ paste("200", Course,  "Freestyle"),
#                            Event == "400A" ~ paste("400", Course,  "Freestyle"),
#                            Event == "500A" ~ paste("500", Course,  "Freestyle"),
#                            Event == "800A" ~ paste("800", Course,  "Freestyle"),
#                            Event == "1000A" ~ paste("1000", Course,  "Freestyle"),
#                            Event == "1500A" ~ paste("1500", Course,  "Freestyle"),
#                            Event == "1650A" ~ paste("1650", Course,  "Freestyle"),
#                            Event == "25B" ~ paste("25", Course,  "Backstroke"),
#                            Event == "50B" ~ paste("50", Course,  "Backstroke"),
#                            Event == "100B" ~ paste("100", Course,  "Backstroke"),
#                            Event == "200B" ~ paste("200", Course,  "Backstroke"),
#                            Event == "25C" ~ paste("25", Course,  "Breaststroke"),
#                            Event == "50C" ~ paste("50", Course,  "Breaststroke"),
#                            Event == "100C" ~ paste("100", Course,  "Breaststroke"),
#                            Event == "200C" ~ paste("200", Course,  "Breaststroke"),
#                            Event == "25D" ~ paste("25", Course,  "Butterfly"),
#                            Event == "50D" ~ paste("50", Course,  "Butterfly"),
#                            Event == "100D" ~ paste("100", Course,  "Butterfly"),
#                            Event == "200D" ~ paste("200", Course,  "Butterfly"),
#                            Event == "100E" ~ paste("100", Course,  "Individual Medlay"),
#                            Event == "200E" ~ paste("200", Course,  "Individual Medlay"),
#                            Event == "400E" ~ paste("400", Course,  "Individual Medlay"),
#                            Event == "6F" ~ "1 mtr Diving (6 dives)",
#                            Event == "11F" ~ "1 mtr Diving (11 dives)"),
#          Seed_Time = str_remove(Seed_Time, "[:alpha:]$")) %>%
#   ungroup() %>%
#   mutate(Row_Min = as.numeric(Row_Numb),
#          Row_Max = dplyr::lead(Row_Min, 1L, default = length(as_lines_list_2) - 1)) %>%
#   select(-ID, -Course) %>%
#   arrange(Row_Min)
#
# # data beginning with E2F contains finals results
# finals_1 <- as_lines_list_2 %>%
#   stringr::str_extract_all("^E2F.*") %>%
#   .[purrr::map(., length)>0] %>%
#   str_replace_all("([:alpha:]{1,})\\s{1,}([:alpha:]{1,})", "\\1\\2") %>%
#   trimws()
#
# finals_2 <-
#   unlist(purrr::map(finals_1, stringr::str_split, "\\s{1,}"), recursive = FALSE)
#
# rows <- finals_2 %>%
#   map(tail, 1) %>%
#   unlist()
#
# finals_3 <- finals_2%>%
#   map(tail, -1) %>%
#   map(head, -15)
#
# finals <- data.frame(Finals_Time = unlist(finals_3), Row_Numb = as.numeric(rows)) %>%
#   mutate(Finals_Time = str_remove(Finals_Time, "[:alpha:]$"))
#
# # data beginning with E2P contains prelims results
# prelims_1 <- as_lines_list_2 %>%
#   stringr::str_extract_all("^E2P.*") %>%
#   .[purrr::map(., length)>0] %>%
#   str_replace_all("([:alpha:]{1,})\\s{1,}([:alpha:]{1,})", "\\1\\2") %>%
#   trimws()
#
# prelims_2 <-
#   unlist(purrr::map(prelims_1, stringr::str_split, "\\s{1,}"), recursive = FALSE)
#
# rows <- prelims_2 %>%
#   map(tail, 1) %>%
#   unlist()
#
# prelims_3 <- prelims_2%>%
#   map(tail, -1) %>%
#   map(head, -14)
#
# prelims <- data.frame(Prelims_Time = unlist(prelims_3), Row_Numb = as.numeric(rows)) %>%
#   mutate(Prelims_Time = str_remove(Prelims_Time, "[:alpha:]{1,}$"))
#
#
# entry <- data.table::setDT(entry)[finals, Finals_Time := Finals_Time, on = .(Row_Min < Row_Numb, Row_Max > Row_Numb)]
# entry <- data.table::setDT(entry)[prelims, Prelims_Time := Prelims_Time, on = .(Row_Min < Row_Numb, Row_Max > Row_Numb)]
#
# # data beginning with D1M contains swimmer info
# swimmer_1 <- as_lines_list_2 %>%
#   stringr::str_extract_all("^D1M.*") %>%
#   .[purrr::map(., length)>0] %>%
#   # str_replace_all("([:alpha:]{1,})\\s{2,}([:alpha:]{1,})", "\\1\\2") %>%
#   trimws()
#
# swimmer_2 <-
#   unlist(purrr::map(swimmer_1, stringr::str_split, "\\s{1,}"), recursive = FALSE)
#
# # unique(purrr::map(swimmer_2, length))
#
# swimmer_length_9 <- swimmer_2[purrr::map(swimmer_2, length) == 9]
# swimmer_length_10 <- swimmer_2[purrr::map(swimmer_2, length) == 10]
# swimmer_length_11 <- swimmer_2[purrr::map(swimmer_2, length) == 11]
#
# swimmer_9 <- data.frame(swimmer_length_9) %>%
#   t()
# swimmer_10 <- data.frame(swimmer_length_10) %>%
#   t()
# swimmer_11 <- data.frame(swimmer_length_11) %>%
#   t()
#
# rownames(swimmer_9) <- NULL
# rownames(swimmer_10) <- NULL
# rownames(swimmer_11) <- NULL
#
# swimmer_9 <- data.frame(swimmer_9)[c("X2","X3","X5", "X9")]
# colnames(swimmer_9) <- c("ID", "First", "Grade", "Row_Numb")
#
# swimmer_10 <- data.frame(swimmer_10)[c("X2","X3","X4","X5", "X6", "X10")]
# colnames(swimmer_10) <- c("ID", "First", "USA_ID", "Birthdate", "Grade", "Row_Numb")
#
# swimmer_11 <- data.frame(swimmer_11)[c("X2","X3","X4","X5", "X6", "X11")]
# colnames(swimmer_11) <- c("ID", "First", "USA_ID", "Birthdate", "Grade", "Row_Numb")
#
# swimmer <- bind_rows(swimmer_9, swimmer_10, swimmer_11) %>%
#   mutate(ID_Numb = str_extract(ID, "^\\d{1,}"),
#          Row_Numb = as.numeric(Row_Numb),
#          Last_Name = str_remove(ID, ID_Numb),
#          Name = paste0(Last_Name, ", ", First)) %>%
#   select(-Last_Name, -First, -ID)
#
#
# # data beginning with C1 contains team info
# team_1 <- as_lines_list_2 %>%
#   stringr::str_extract_all("^C1.*") %>%
#   .[purrr::map(., length)>0] %>%
#   # str_replace_all("([:alpha:]{1,})\\s{2,}([:alpha:]{1,})", "\\1\\2") %>%
#   trimws()
#
# team_2 <-
#   unlist(purrr::map(team_1, stringr::str_split, "\\s{1,}"), recursive = FALSE) %>%
#   map(unique)
#
# rows <- team_2 %>%
#   map(tail, 1) %>%
#   unlist()
#
# team_3 <- team_2 %>%
#   map(tail, -1) %>%
#   map(head, -3) %>%
#   map(paste, collapse = " ")
#
# team <- data.frame(School = unlist(team_3), Row_Numb = as.numeric(rows)) %>%
#   mutate(Row_Min = as.numeric(Row_Numb),
#          Row_Max = dplyr::lead(Row_Min, 1L, default = length(as_lines_list_2)) - 1,)
#
#
# # data beginning with F1 contains relay info
# relay_1 <- as_lines_list_2 %>%
#   stringr::str_extract_all("^F1.*") %>%
#   .[purrr::map(., length)>0] %>%
#   # str_replace_all("([:alpha:]{1,})\\s{2,}([:alpha:]{1,})", "\\1\\2") %>%
#   trimws()
#
# relay_2 <-
#   unlist(purrr::map(relay_1, stringr::str_split, "\\s{1,}"), recursive = FALSE) %>%
#   map(unique)
#
# relay_length_14 <- relay_2[purrr::map(relay_2, length) == 14]
# relay_length_15 <- relay_2[purrr::map(relay_2, length) == 15]
#
# relay_14 <- data.frame(relay_length_14) %>%
#   t()
# relay_15 <- data.frame(relay_length_15) %>%
#   t()
#
# rownames(relay_14) <- NULL
# rownames(relay_15) <- NULL
#
# relay_14 <- data.frame(relay_14)[c("X1", "X2", "X4","X9","X14")]
# colnames(relay_14) <- c("Team", "Relay_Rank", "Event", "Seed_Time", "Row_Numb")
#
# relay_15 <- data.frame(relay_15)[c("X1", "X2", "X4","X9","X15")]
# colnames(relay_15) <- c("Team", "Relay_Rank", "Event", "Seed_Time", "Row_Numb")
#
# relay <- bind_rows(relay_14, relay_15) %>%
#   group_by(Team, Relay_Rank, Event, Seed_Time) %>%
#   summarise(Row_Numb = min(as.numeric(Row_Numb), na.rm = TRUE)) %>%
#   arrange(Row_Numb) %>%
#   ungroup()
#
# relay <- relay %>%
#   mutate(Course = str_extract(Seed_Time, "[:alpha:]$"),
#          Course = case_when(Course == "Y" ~ "Yard",
#                             Course == "M" ~ "Meter"),
#          Event = case_when(Event == "200E" ~ paste("200", Course,  "Medlay Relay"),
#                            Event == "400E" ~ paste("400", Course,  "Medlay Relay"),
#                            Event == "200A" ~ paste("200", Course,  "Freesytle Relay"),
#                            Event == "400A" ~ paste("400", Course,  "Freesytle Relay"),
#                            Event == "800A" ~ paste("800", Course,  "Freesytle Relay")),
#          Seed_Time = str_remove(Seed_Time, "[:alpha:]$")) %>%
#   mutate(Row_Min = as.numeric(Row_Numb),
#          Row_Max = dplyr::lead(Row_Min, 1L, default = length(as_lines_list_2) - 1)) %>%
#   select(-Course, -Team, -Relay_Rank)
#
#
# # data beginning with F2F contains finals results
# relay_finals_1 <- as_lines_list_2 %>%
#   stringr::str_extract_all("^F2F.*") %>%
#   .[purrr::map(., length)>0] %>%
#   str_replace_all("([:alpha:]{1,})\\s{1,}([:alpha:]{1,})", "\\1\\2") %>%
#   trimws()
#
# relay_finals_2 <-
#   unlist(purrr::map(relay_finals_1, stringr::str_split, "\\s{1,}"), recursive = FALSE)
#
# rows <- relay_finals_2 %>%
#   map(tail, 1) %>%
#   unlist()
#
# relay_finals_3 <- relay_finals_2%>%
#   map(tail, -1) %>%
#   map(head, -15)
#
# relay_finals <- data.frame(Finals_Time = unlist(relay_finals_3), Row_Numb = as.numeric(rows)) %>%
#   mutate(Finals_Time = str_remove(Finals_Time, "[:alpha:]$"))
#
# # data beginning with F2P contains prelims relay results
# relay_prelims_1 <- as_lines_list_2 %>%
#   stringr::str_extract_all("^F2P.*") %>%
#   .[purrr::map(., length)>0] %>%
#   str_replace_all("([:alpha:]{1,})\\s{1,}([:alpha:]{1,})", "\\1\\2") %>%
#   trimws()
#
# relay_prelims_2 <-
#   unlist(purrr::map(relay_prelims_1, stringr::str_split, "\\s{1,}"), recursive = FALSE)
#
# rows <- relay_prelims_2 %>%
#   map(tail, 1) %>%
#   unlist()
#
# relay_prelims_3 <- relay_prelims_2%>%
#   map(tail, -1) %>%
#   map(head, -14)
#
# relay_prelims <- data.frame(Prelims_Time = unlist(relay_prelims_3), Row_Numb = as.numeric(rows)) %>%
#   mutate(Prelims_Time = str_remove(Prelims_Time, "[:alpha:]{1,}$"))
#
#
# relay <- data.table::setDT(relay)[relay_finals, Finals_Time := Finals_Time, on = .(Row_Min < Row_Numb, Row_Max > Row_Numb)]
# relay <- data.table::setDT(relay)[relay_prelims, Prelims_Time := Prelims_Time, on = .(Row_Min < Row_Numb, Row_Max > Row_Numb)]
#
#
# #### Binding up data
# data <- left_join(swimmer, entry, by = "ID_Numb") %>%
#   rowwise() %>%
#   mutate(Row_Numb = min(c(Row_Numb.x, Row_Numb.y), na.rm = TRUE)) %>%
#   select(-Row_Numb.x, -Row_Numb.y)
#
# data <- bind_rows(data, relay)
#
# data  <-
#   transform(data, School = team$School[findInterval(Row_Numb, team$Row_Min)])
#
# data_2 <- data %>%
#   mutate(Finals_Time = case_when(str_detect(Finals_Time, "[:alpha:]") ~ "Bad Entry",
#                                  TRUE ~ Finals_Time),
#          Prelims_Time = case_when(str_detect(Prelims_Time, "[:alpha:]") ~ "Bad Entry",
#                                  TRUE ~ Prelims_Time)) %>%
#   na_if("Bad Entry") %>%
#   mutate(Finals_Time = case_when((is.na(Prelims_Time) == FALSE & is.na(Finals_Time) == TRUE) ~ Prelims_Time,
#                                  TRUE ~ Finals_Time)) %>%
#   mutate(Birthdate = str_extract(USA_ID, "\\d{6,8}"),
#          USA_ID = case_when(str_length(USA_ID) < 8 ~ "Bad Entry",
#                             TRUE ~ USA_ID)) %>%
#   na_if("Bad Entry") %>%
#   select(-Row_Min, -Row_Max, -Row_Numb, -ID_Numb)
