test_that("Men 400 IM prelims", {
  file <- system.file("extdata", "Omega_OT_400IM_Prelims_2021.pdf", package = "SwimmeR")

  df <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE)

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

test_that("Tokyo 2020 Men 400IM Heat 1", {

  skip_on_cran()

  file <- "https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73A1_SWMM400MIM------------FNL-000100--.pdf"

  df_test <- swim_parse(read_results(file),
                        splits = TRUE)

  df_standard <-
    structure(
      list(
        Place = c("1", "2", "3", "4", "4", "6", "7", "8"),
        Lane = c("3", "7", "4", "6", "8", "1", "5", "2"),
        Name = c(
          "KALISZ Chase",
          "LITHERLAND Jay",
          "SMITH Brendon",
          "VERRASZTO David",
          "LITCHFIELD Max",
          "MARCHAND Leon",
          "CLAREBURT Lewis",
          "RAZZETTI Alberto"
        ),
        Team = c("USA",
                 "USA", "AUS", "HUN", "GBR", "FRA", "NZL", "ITA"),
        Reaction_Time = c("0.73",
                          "0.71", "0.66", "0.79", "0.61", "0.67", "0.68", "0.68"),
        Finals_Time = c(
          "4:09.42",
          "4:10.28",
          "4:10.38",
          "4:10.59",
          "4:10.59",
          "4:11.16",
          "4:11.22",
          "4:11.32"
        ),
        DQ = c("0", "0", "0", "0", "0", "0", "0", "0"),
        Exhibition = c("0",
                       "0", "0", "0", "0", "0", "0", "0"),
        Event = c(
          "Men's 400m Individual Medley",
          "Men's 400m Individual Medley",
          "Men's 400m Individual Medley",
          "Men's 400m Individual Medley",
          "Men's 400m Individual Medley",
          "Men's 400m Individual Medley",
          "Men's 400m Individual Medley",
          "Men's 400m Individual Medley"
        ),
        Split_50 = c(
          "26.27",
          "26.38",
          "26.44",
          "27.09",
          "27.02",
          "25.73",
          "26.40",
          "26.09"
        ),
        Split_100 = c(
          "29.96",
          "30.27",
          "30.68",
          "30.51",
          "30.42",
          "30.41",
          "30.46",
          "30.24"
        ),
        Split_150 = c(
          "32.36",
          "32.76",
          "32.11",
          "32.06",
          "32.50",
          "33.68",
          "31.87",
          "34.06"
        ),
        Split_200 = c(
          "31.74",
          "31.60",
          "31.35",
          "31.76",
          "31.55",
          "32.99",
          "31.26",
          "32.96"
        ),
        Split_250 = c(
          "34.02",
          "36.04",
          "36.69",
          "35.27",
          "35.83",
          "34.51",
          "36.12",
          "34.78"
        ),
        Split_300 = c(
          "35.12",
          "36.51",
          "36.79",
          "35.45",
          "35.94",
          "34.94",
          "35.83",
          "35.66"
        ),
        Split_350 = c(
          "30.30",
          "28.76",
          "28.90",
          "30.02",
          "29.19",
          "30.24",
          "29.65",
          "29.52"
        ),
        Split_400 = c(
          "29.65",
          "27.96",
          "27.42",
          "28.43",
          "28.14",
          "28.66",
          "29.63",
          "28.01"
        )), row.names = c(NA, -8L), class = "data.frame")

  expect_equivalent(df_standard,
                    df_test)

})

test_that("Tokyo 2020 Women 4 x 100m Free Heat 1", {

  skip_on_cran()

  file <- "https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73B1_SWMW4X100MFR----------HEAT000100--.pdf"

  df_test <- swim_parse(read_results(file),
                        splits = TRUE,
                        relay_swimmers = TRUE)

  df_standard <-
    structure(
      list(
        Place = c("1", "2", "3", "4", "5", "6", "7"),
        Lane = c("5", "4", "2", "6", "3", "7", "1"),
        Team = c(
          "GBR - Great Britain",
          "USA - United States of America",
          "DEN - Denmark",
          "SWE - Sweden",
          "FRA - France",
          "BRA - Brazil",
          "HKG - Hong Kong, China"
        ),
        Finals_Time = c(
          "3:34.03",
          "3:34.80",
          "3:35.56",
          "3:35.93",
          "3:36.61",
          "3:39.19",
          "3:43.52"
        ),
        DQ = c("0", "0", "0", "0",
               "0", "0", "0"),
        Exhibition = c("0", "0", "0", "0", "0", "0",
                       "0"),
        Event = c(
          "Women's 4 x 100m Freestyle Relay",
          "Women's 4 x 100m Freestyle Relay",
          "Women's 4 x 100m Freestyle Relay",
          "Women's 4 x 100m Freestyle Relay",
          "Women's 4 x 100m Freestyle Relay",
          "Women's 4 x 100m Freestyle Relay",
          "Women's 4 x 100m Freestyle Relay"
        ),
        Relay_Swimmer_1 = c(
          "HOPE Lucy",
          "SMOLIGA Olivia",
          "BLUME Pernille",
          "SJOESTROEM Sarah",
          "GASTALDELLO Beryl",
          "OLIVEIRA Larissa",
          "TAM Hoi Lam"
        ),
        Relay_Swimmer_2 = c(
          "HOPKIN Anna",
          "de LOOF Catie",
          "BRO Signe",
          "COLEMAN Michelle",
          "BONNET Charlotte",
          "VIEIRA Ana Carolina",
          "CHENG Camille Lily Mei"
        ),
        Relay_Swimmer_3 = c(
          "WOOD Abbie",
          "SCHMITT Allison",
          "JENSEN Julie Kepp",
          "HANSSON Louise",
          "FABRE Margaux",
          "MEDEIROS Etiene",
          "AU Hoi Shun Stephanie"
        ),
        Relay_Swimmer_4 = c(
          "ANDERSON Freya",
          "HINDS Natalie",
          "OTTESEN Jeanette",
          "JUNEVIK Sara",
          "MARTIN Anouchka",
          "BALDUCCINI Stephanie",
          "HO Nam Wai Tinky"
        ),
        Split_50 = c("26.28", "25.96", "25.33",
                     "25.47", "25.97", "26.23", "26.50"),
        Split_100 = c("54.37",
                      "54.06", "53.15", "52.95", "54.28", "54.79", "55.58"),
        Split_150 = c("24.87",
                      "25.02", "25.50", "24.91", "25.35", "26.07", "26.05"),
        Split_200 = c("52.65",
                      "53.42", "53.19", "53.44", "53.05", "54.92", "54.61"),
        Split_250 = c("25.53",
                      "25.85", "25.56", "25.29", "26.09", "25.95", "26.96"),
        Split_300 = c("53.55",
                      "54.04", "54.72", "53.68", "54.83", "55.42", "56.96"),
        Split_350 = c("25.76",
                      "25.30", "25.46", "25.87", "25.75", "25.61", "27.43"),
        Split_400 = c("53.46",
                      "53.28", "54.50", "55.86", "54.45", "54.06", "56.37")
      ),
      row.names = c(NA,-7L),
      class = "data.frame"
    )

  expect_equivalent(df_standard,
                    df_test)

})


# testthat::test_file("tests/testthat/test-omega.R")
