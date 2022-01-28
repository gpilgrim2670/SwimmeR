# In progress.  The for loop below does work but obviously needs to be cleaned
# up and sent to map.  Need to break events into Event_A, Event_B etc. for
# actual use.  Will also need to accept scoring system.  Do keywords for scoring
# systems, for rescoring functions as well.  Maybe an events-back-to-back
# protocol as well.  Also something to exclude a particular athlete from a
# particular event




# Blue <- data.frame(
#   Event = rep(LETTERS[seq(1, 6)], 3),
#   Competitor = c(rep("Blue_Strong", 6), rep("Blue_Mid", 6), rep("Blue_Weak", 6)),
#   Blue_Strength = c(
#     sample(90:95, 6, replace = TRUE),
#     sample(46:54, 6, replace = TRUE),
#     sample(1:10, 6, replace = TRUE)
#   )
# )
#
#
#
# Red <- data.frame(Event = LETTERS[seq(1, 6)],
#                   Red_Strength = c(100, 60, 40, 85, 45, 25))
#
# # Event_Value = c(2, 2, 2, 2, 4, 2)
# Events = data.frame(Event = LETTERS[seq(1, 6)],
#                     Event_Value = c(3, 2, 1, 3, 2, 10))
#
#
# Target <- data.frame(
#   Event = LETTERS[seq(1, 6)],
#   Competitor = c(
#     "Blue_Weak",
#     "Blue_Strong",
#     "Blue_Weak",
#     "Blue_Strong",
#     "Blue_Mid",
#     "Blue_Mid"
#   ),
#   Event_Value = Events$Event_Value
# ) %>%
#   left_join(Blue) %>%
#   left_join(Red) %>%
#   mutate(Outcome = case_when(Blue_Strength - Red_Strength > 0 ~ "Blue",
#                              TRUE ~ "Red"))
#
# Target %>%
#   group_by(Outcome) %>%
#   summarise(Total_Score = sum(Event_Value))
#
#
# structure(list(Event = c("A", "B", "C", "D", "E", "F"), Competitor = c("Blue_Weak",
#                                                                        "Blue_Strong", "Blue_Weak", "Blue_Strong", "Blue_Mid", "Blue_Mid"
# ), Event_Value = c(3, 2, 1, 3, 2, 1), Blue_Strength = c(5L, 93L,
#                                                         10L, 95L, 48L, 54L), Red_Strength = c(100, 60, 40, 85, 52, 25
#                                                         ), Outcome = c("Red", "Blue", "Red", "Blue", "Red", "Blue")), class = "data.frame", row.names = c(NA,
#                                                                                                                                                           -6L))
#
# f <- function(aa, bb) {
#   eval(substitute(a <- b, list(a = as.name(aa), b = bb)))
# }
#
#
# Entries <- list()
#
# Competitors <- unique(Blue$Competitor)
#
# Times_Competed <- Map(f, Competitors, 0)
#
# for (i in 1:6){
#
#   Events <- Events %>%
#     left_join(Red) %>%
#     arrange(desc(Event_Value), desc(Red_Strength))
#
#   e <- Events$Event[i]
#
#   Red_Strength <- Red[Red$Event == e,][1,2]
#
#   Entry <- Blue %>%
#     filter(Competitor %!in% names(Times_Competed[which(Times_Competed > 1)])) %>%
#     rowwise() %>%
#     filter(Event == e, Blue_Strength > Red_Strength) %>%
#     ungroup()
#
#   if (nrow(Entry) > 1) {
#     Entry <-  Entry %>%
#       slice(which.min(Blue_Strength))
#   } else if (nrow(Entry) == 0) {
#     Entry <- Blue %>%
#       filter(Event == e) %>%
#       ungroup() %>%
#       slice(which.min(Blue_Strength))
#   }
#
#   if(Times_Competed[Entry$Competitor][[1]] > 1){
#
#     # Ineligible_Entry <- Entry$Competitor[1]
#
#     Entry <- Blue %>%
#       filter(Competitor != Entry$Competitor[1]) %>%
#       rowwise() %>%
#       filter(Event == e, Blue_Strength > Red_Strength) %>%
#       ungroup()
#   }
#
#   if (nrow(Entry) > 1) {
#     Entry <-  Entry %>%
#       slice(which.min(Blue_Strength))
#   } else if (nrow(Entry) == 0) {
#     Entry <- Blue %>%
#       filter(Event == e) %>%
#       filter(Competitor %!in% names(Times_Competed[which(Times_Competed > 1)])) %>%
#       ungroup() %>%
#       slice(which.min(Blue_Strength))
#   }
#
#
#   Times_Competed[Entry$Competitor][[1]] = Times_Competed[Entry$Competitor][[1]] + 1
#
#
#   Entries[[i]] <- Entry
#
#
# }
#
# dplyr::bind_rows(Entries) %>%
#   arrange(Event)
#
# Target
#
# Events
