test_that("Men 400 IM prelims", {
  file <- system.file("extdata", "Omega_OT_400IM_Prelims_2021.pdf", package = "SwimmeR")

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

test_that("Women 400m finals", {
  file <- system.file("extdata", "Omega_OT_400m_Finals_2021.pdf", package = "SwimmeR")

  df <- swim_parse(
    read_results(file),
    splits = TRUE
  )

  df_test <- df %>%
    head(2)

  df_standard <- data.frame(Place = c("1", "2"),
                            Lane = c("4", "5"),
                            Name = c("LEDECKY Katie", "MADDEN Paige"),
                            Team = c("NCAP", "UVA"),
                            Reaction_Time = c("0.65", "0.63"),
                            Finals_Time = c("4:01.27", "4:04.86"),
                            DQ = rep("0", 2),
                            Exhibition = rep("0", 2),
                            Event = rep("PM Women's 400m Freestyle Final", 2),
                            Split_50 = c("27.70", "28.10"),
                            Split_100 = c("29.59", "30.16"),
                            Split_150 = c("29.95", "30.68"),
                            Split_200 = c("30.40", "31.06"),
                            Split_250 = c("30.47", "31.23"),
                            Split_300 = c("31.24", "31.05"),
                            Split_350 = c("30.88", "31.24"),
                            Split_400 = c("31.04", "31.34")
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


test_that("wave 1 200 fly", {
  file <- system.file("extdata", "Omega_Wave1_200fly_Finals_2021.pdf", package = "SwimmeR")

  df_test <- swim_parse(read_results(file),
                        splits = TRUE) %>%
    head(2)

  df_standard <- data.frame(Place = c("1", "2"),
                            Lane = c("4", "2"),
                            Name = c("SUN Eleanor", "THOMAS Luciana"),
                            Team = c("NCAP", "IA"),
                            Reaction_Time = c("0.72", "0.76"),
                            Finals_Time = c("2:13.76", "2:15.32"),
                            DQ = rep("0", 2),
                            Exhibition = rep("0", 2),
                            Event = rep("4 JUN 2021 - 7:23 PM Women's 200m Butterfly Final", 2),
                            Split_50 = c("29.70", "30.28"),
                            Split_100 = c("34.09", "33.80"),
                            Split_150 = c("35.21", "35.20"),
                            Split_200 = c("34.76", "36.04")
  )

  expect_equivalent(df_standard,
                    df_test)

})

test_that("wave 1 1500m", {
  file <- system.file("extdata", "Omega_Wave1_1500_Finals_2021.pdf", package = "SwimmeR")

  df <- swim_parse(read_results(file),
                        splits = TRUE) %>%
    filter(Name != "NARVID Jake") # Jake has issues with his reported splits

  list_test <- df %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), sec_format)) %>%
    mutate(Split_Total = rowSums(dplyr::across(dplyr::starts_with("Split")), na.rm = TRUE)) %>%
    pull(Split_Total)

  list_standard <- c(935.94, 936.24, 940.05, 944.30, 952.72, 954.12, 954.17, 962.61, 964.30, 965.41, 978.49)

  expect_equivalent(list_standard,
                    list_test)

})


# testthat::test_file("tests/testthat/test-omega.R")
