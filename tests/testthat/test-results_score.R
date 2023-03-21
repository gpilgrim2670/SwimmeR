# # testthat::test_file("tests/testthat/test-results_score.R")
#
# test_that("prelims_finals works", {
#   file <- system.file("extdata", "not_included", "BigTen_WSWIM_2018.pdf", package = "SwimmeR")
#   BigTenRaw <- read_results(file)
#
#   BigTen <- swim_parse(
#     BigTenRaw,
#     typo = c(
#       # "^\\s{1,}\\*",
#       # "^\\s{1,}(\\d{1,2})\\s{2,}",
#       # not sure if needed
#       ",\\s{1,}University\\s{1,}of",
#       "University\\s{1,}of\\s{1,}",
#       "\\s{1,}University"
#       # "SR\\s{2,}",
#       # "JR\\s{2,}",
#       # "SO\\s{2,}",
#       # "FR\\s{2,}"
#     ),
#     replacement = c(
#       # " ",
#       #               "  \\1 ",
#                     "", "", ""
#                     # "SR ",
#                     # "JR ",
#                     # "SO ",
#                     # "FR "
#                     ),
#     avoid = c("B1G", "Pool")
#   )
#
#   BigTen <- BigTen %>%
#     dplyr::filter(
#       stringr::str_detect(Event, "Time Trial") == FALSE,
#       stringr::str_detect(Event, "Swim-off") == FALSE
#     ) %>%
#     dplyr::mutate(Team = dplyr::case_when(Team == "Wisconsin, Madi" ~ "Wisconsin",
#                                             TRUE ~ Team))
#
#   # begin results_score portion
#   df <- BigTen %>%
#     results_score(
#       events = unique(BigTen$Event),
#       meet_type = "prelims_finals",
#       lanes = 8,
#       scoring_heats = 3,
#       point_values = c(
#         32, 28, 27, 26, 25, 24, 23, 22, 20, 17, 16, 15, 14, 13, 12, 11, 9, 7, 6, 5, 4, 3, 2, 1
#       ),
#       max_relays_per_team = 1
#     )
#
#   Total <- df %>%
#     dplyr::group_by(Team) %>%
#     dplyr::summarise(Score = sum(Points, na.rm = TRUE)) %>%
#     dplyr::arrange(dplyr::desc(Score)) %>%
#     dplyr::ungroup() %>%
#     dplyr::summarize(total = sum(Score)) # should total to 8596
#
#
#   expect_equal(Total$total[1], 8596)
# })
#
# test_that("timed_finals works", {
#
#   df_test <- readRDS(system.file("extdata/not_included", "TX_OH_Results.rds", package = "SwimmeR"))
#
#
#   df_test <- df_test %>%
#     rename("Team" = School, "Finals" = Finals_Time, "Prelims" = Prelims_Time) %>%
#     mutate(DQ = 0,
#            Exhibition = 0)
#
#   Results_Final <-
#     results_score(
#       results = df_test,
#       events = unique(df_test$Event),
#       meet_type = "timed_finals",
#       lanes = 8,
#       point_values = c(20, 17, 16, 15, 14, 13, 12, 11, 9, 7, 6, 5, 4, 3, 2, 1),
#       max_relays_per_team = 1
#     )
#
#   Scores <- Results_Final %>%
#     group_by(State) %>%
#     summarise(Score = sum(as.numeric(Points), na.rm = TRUE))
#
#   # Total number of points is 4650
#
#   expect_equal(Scores$Score[1], 2155.5)
# })
