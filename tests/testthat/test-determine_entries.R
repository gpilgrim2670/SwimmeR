

Target <- data.frame(
  Event = LETTERS[seq(1, 6)],
  Competitor = c(
    "Blue_Weak",
    "Blue_Strong",
    "Blue_Weak",
    "Blue_Strong",
    "Blue_Mid",
    "Blue_Mid"
  ),
  Event_Value = Events$Event_Value
) %>%
  left_join(Blue) %>%
  left_join(Red) %>%
  mutate(Outcome = case_when(Blue_Strength - Red_Strength > 0 ~ "Blue",
                             TRUE ~ "Red"))

Target %>%
  group_by(Outcome) %>%
  summarise(Total_Score = sum(Event_Value))



# testthat::test_file("tests/testthat/test-determine_entries.R")
