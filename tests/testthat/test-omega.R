test_that("400m prelims", {
  file <- system.file("extdata", "Omega_OT_400m_Prelims_2021.pdf", package = "SwimmeR")

  df <- swim_parse(
    read_results(file),
    splits = TRUE
  )

  df_test <- df %>%
    head(2)

  df_standard <- data.frame(Place = c("1", "2"),
                        Heat = rep("4", 2),
                        Lane = c("4", "5"),
                        Name = c("FOSTER Carson", "KALISZ Chase"),
                        Team = c("RAYSOH", "ABSC"),
                        Reaction_Time = c("0.66", "0.69"),
                        Finals_Time = c("4:10.50", "4:10.61"),
                        DQ = rep("0", 2),
                        Exhibition = rep("0", 2),
                        Event = rep("AM Men's 400m Individual Medley Heats", 2),
                        Split_50 = c("26.24", "26.48"),
                        Split_100 = c("29.56", "29.98"),
                        Split_150 = c("31.76", "32.74"),
                        Split_200 = c("30.87", "32.10"),
                        Split_250 = c("35.38", "34.39"),
                        Split_300 = c("35.69", "34.96"),
                        Split_350 = c("31.28", "30.94"),
                        Split_400 = c("29.72", "29.02")
                        )

  expect_equivalent(df_standard,
                    df_test)

})

test_that("100 br swim off no heat", {
  file <- system.file("extdata", "Omega_OT_100Br_Swimoff_2021.pdf", package = "SwimmeR")

  df_test <- swim_parse(
    read_results(file),
    splits = TRUE
  )

  df_standard <- data.frame(Place = c("1", "2"),
                            Lane = c("4", "5"),
                            Name = c("TYBUR Jonathan", "MASON Mitch"),
                            Team = c("GSC-FL", "LSU"),
                            Reaction_Time = c("0.66", "0.68"),
                            Finals_Time = c("1:00.91", "1:01.93"),
                            DQ = rep("0", 2),
                            Exhibition = rep("0", 2),
                            Event = rep("PM Men's 100m Breaststroke Heats Swim-off", 2),
                            Split_50 = c("28.17", "29.07"),
                            Split_100 = c("32.74", "32.86")
  )

  expect_equivalent(df_standard,
                    df_test)

})

# testthat::test_file("tests/testthat/test-omega.R")
