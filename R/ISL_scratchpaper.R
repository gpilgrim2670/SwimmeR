# lv1 <- "https://isl.global/wp-content/uploads/2019/12/isl_vegas_day_1_results.pdf" # pdf doesn't read
# lv2 <- "https://isl.global/wp-content/uploads/2019/12/las-vegas-day-2-isl-results.pdf" # works
# london1 <- "https://isl.global/wp-content/uploads/2019/11/isl_london_day_1_results.pdf" # works
# london2 <- "https://isl.global/wp-content/uploads/2019/11/isl_london_day_2_resuls.pdf" # works
# md1 <- "https://isl.global/wp-content/uploads/2019/11/isl_results_washington_college_park_day_1.pdf" # works
# md2 <- "https://isl.global/wp-content/uploads/2019/11/isl_college_park_results_day_2.pdf" # works
# hun1 <- "https://isl.global/wp-content/uploads/2019/10/isl_results_budapest_day_1-1.pdf" # works
# hun2 <- "https://isl.global/wp-content/uploads/2019/10/isl_results_budapest_sunday.pdf" # works
# dallas1 <- "https://isl.global/wp-content/uploads/2019/10/dallas_lewisville_isl_results_day_1.pdf" # works
# dallas2 <- "https://isl.global/wp-content/uploads/2019/10/isl_results_dallas_day_2.pdf" # works
# naples1 <- "https://isl.global/wp-content/uploads/2019/10/13_11_naples-results-day-1_final.-pdf-1.pdf" # works
# naples2 <- "https://isl.global/wp-content/uploads/2019/10/naples_results_day_2.pdf" # works
# hun1_2020 <- "https://swimswam.com/wp-content/uploads/2020/10/ISL-Budapest-2020-Meet-1-Day-1-CAC-NYB-ENS-LAC.pdf" #works
# match_1_2020 <- "https://cdn.swimswam.com/wp-content/uploads/2020/10/Results_Book_Match_1_V2.pdf"
# match_2_2020 <- "https://cdn.swimswam.com/wp-content/uploads/2020/10/ISL-2020-Match-2-Day-1.pdf"
#
# df <- read_results(match_1_2020) %>%
# swim_parse_ISL()
#
# df %>%
#   mutate(Gender = case_when(
#     str_detect(Event, "Women") ~ "F",
#     str_detect(Event, "Men") ~ "M",
#     str_detect(Event, "Mixed") ~ "Mixed"
#   )) %>%
#   # View()
#   group_by(Team, Gender) %>%
#   summarise(Score = sum(Points, na.rm = TRUE))
#
# sum(unique(df[df$Name == "KING Lilly",])$Points, na.rm = TRUE)

