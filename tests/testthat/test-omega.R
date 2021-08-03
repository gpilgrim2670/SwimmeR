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

test_that("Tokyo 2020 Women 100 Breast Heats, with DNS", {

  skip_on_cran()

  file <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/OG2020-_SWM_C73A2_SWMW100MBR------------HEAT--------.pdf"

  df_test <- swim_parse(read_results(file),
                        splits = TRUE,
                        relay_swimmers = TRUE)

  df_standard <-
    structure(
      list(
        Place = c(
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          NA,
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          NA,
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          NA,
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          NA,
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8"
        ),
        Heat = c(
          "1",
          "1",
          "1",
          "1",
          "1",
          "1",
          "1",
          "2",
          "2",
          "2",
          "2",
          "2",
          "2",
          "2",
          "2",
          "3",
          "3",
          "3",
          "3",
          "3",
          "3",
          "3",
          "3",
          "4",
          "4",
          "4",
          "4",
          "4",
          "4",
          "4",
          "4",
          "5",
          "5",
          "5",
          "5",
          "5",
          "5",
          "5",
          "5",
          "6",
          "6",
          "6",
          "6",
          "6",
          "6",
          "6",
          "6"
        ),
        Lane = c(
          "3",
          "5",
          "4",
          "2",
          "7",
          "6",
          NA,
          "3",
          "5",
          "7",
          "4",
          "6",
          "8",
          "2",
          NA,
          "1",
          "2",
          "6",
          "4",
          "5",
          "7",
          "3",
          "8",
          "4",
          "3",
          "2",
          "6",
          "1",
          "7",
          "8",
          NA,
          "5",
          "4",
          "6",
          "3",
          "7",
          "2",
          "1",
          NA,
          "4",
          "5",
          "3",
          "6",
          "8",
          "2",
          "7",
          "1"
        ),
        Name = c(
          "GRAND PIERRE Emilie Faith",
          "SEMYONOVA Darya",
          "PINA Jayla",
          "ADAMS Taeyanna",
          "BA MATRAF Nooran",
          "SAJINA Aishath",
          "TOURE Mariama",
          "PODMANIKOVA Andrea",
          "PHEE Jinq En",
          "SCANLAN Tilali",
          "RAJIC Ema",
          "SANTOS SILVA Emily M.",
          "FISHER-MARSTERS Kirsten Andrea",
          "KOK SHUN Alicia",
          "VERDINO Claudia",
          "TETEREVKOVA Kotryna",
          "ELENDT Anna Charlott Darcel",
          "ATKINSON Alia",
          "SCHOUTEN Tes",
          "LECLUYSE Fanny",
          "RODRIGUEZ VILLANUEVA Byanca MelissaMEX",
          "SEBASTIAN Julia",
          "PETKOVA Diana",
          "EFIMOVA Yuliya",
          "TANG Qianting",
          "WATANABE Kanako",
          "AOKI Reona",
          "HANSEN Jessica",
          "ZMUSHKA Alina",
          "WOG Kelsey Lauren",
          "PILATO Benedetta",
          "SCHOENMAKER Tatjana",
          "JACOBY Lydia",
          "Mc SHARRY Mona",
          "HODGES Chelsea",
          "MAMIE Lisa",
          "JEFIMOVA Eneli",
          "VALL MONTERO Jessica",
          "GORBENKO Anastasia",
          "KING Lilly",
          "HANSSON Sophie",
          "CARRARO Martina",
          "CHIKUNOVA Evgeniia",
          "HULKKO Ida",
          "VASEY Sarah",
          "SMITH Kierra",
          "FAST Emelie"
        ),
        Team = c(
          "HAI",
          "TKM",
          "CPV",
          "FSM",
          "YEM",
          "MDV",
          "GUI",
          "SVK",
          "MAS",
          "ASA",
          "CRO",
          "PAN",
          "COK",
          "MRI",
          "MON",
          "LTU",
          "GER",
          "JAM",
          "NED",
          "BEL",
          "0.65",
          "ARG",
          "BUL",
          "ROC",
          "CHN",
          "JPN",
          "JPN",
          "AUS",
          "BLR",
          "CAN",
          "ITA",
          "RSA",
          "USA",
          "IRL",
          "AUS",
          "SUI",
          "EST",
          "ESP",
          "ISR",
          "USA",
          "SWE",
          "ITA",
          "ROC",
          "FIN",
          "GBR",
          "CAN",
          "SWE"
        ),
        Reaction_Time = c(
          "0.67",
          "0.65",
          "0.69",
          "0.73",
          "0.73",
          "0.72",
          NA,
          "0.66",
          "0.70",
          "0.71",
          "0.71",
          "0.69",
          "0.68",
          "0.60",
          NA,
          "0.70",
          "0.72",
          "0.67",
          "0.66",
          "0.71",
          "31.77",
          "0.66",
          "0.74",
          "0.69",
          "0.73",
          "0.69",
          "0.62",
          "0.72",
          "0.70",
          "0.63",
          NA,
          "0.67",
          "0.67",
          "0.67",
          "0.70",
          "0.71",
          "0.67",
          "0.69",
          NA,
          "0.71",
          "0.72",
          "0.61",
          "0.71",
          "0.65",
          "0.67",
          "0.76",
          "0.65"
        ),
        Finals_Time = c(
          "1:14.82",
          "1:16.37",
          "1:16.96",
          "1:25.36",
          "1:27.79",
          "1:33.59",
          NA,
          "1:08.36",
          "1:08.40",
          "1:10.01",
          "1:10.02",
          "1:12.10",
          "1:13.98",
          "1:15.42",
          NA,
          "1:06.82",
          "1:06.96",
          "1:07.70",
          "1:07.89",
          "1:07.93",
          "1:08.76",
          "1:09.35",
          "1:10.61",
          "1:06.21",
          "1:06.47",
          "1:07.01",
          "1:07.29",
          "1:07.50",
          "1:07.58",
          "1:07.73",
          NA,
          "1:04.82",
          "1:05.52",
          "1:06.39",
          "1:06.70",
          "1:06.76",
          "1:06.79",
          "1:07.07",
          NA,
          "1:05.55",
          "1:05.66",
          "1:05.85",
          "1:06.16",
          "1:06.19",
          "1:06.61",
          "1:07.87",
          "1:07.98"
        ),
        DQ = c(
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "1",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "1",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "1",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "1",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0"
        ),
        Exhibition = c(
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0"
        ),
        Event = c(
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke",
          "Women's 100m Breaststroke"
        ),
        Split_50 = c(
          "34.33",
          "34.22",
          "34.18",
          "39.75",
          "41.53",
          "44.17",
          NA,
          "32.10",
          "32.50",
          "33.22",
          "32.10",
          "33.18",
          "34.16",
          "34.60",
          NA,
          "31.61",
          "31.57",
          "31.48",
          "32.09",
          "31.57",
          "31.77",
          "32.32",
          "32.81",
          "30.92",
          "30.81",
          "31.39",
          "31.01",
          "31.85",
          "31.76",
          "31.82",
          NA,
          "30.21",
          "30.99",
          "31.06",
          "30.86",
          "31.42",
          "31.76",
          "32.02",
          NA,
          "30.74",
          "31.01",
          "31.06",
          "31.63",
          "30.55",
          "31.30",
          "32.09",
          "31.23"
        ),
        Split_100 = c(
          "40.49",
          "42.15",
          "42.78",
          "45.61",
          "46.26",
          "49.42",
          NA,
          "36.26",
          "35.90",
          "36.79",
          "37.92",
          "38.92",
          "39.82",
          "40.82",
          NA,
          "35.21",
          "35.39",
          "36.22",
          "35.80",
          "36.36",
          "36.99",
          "37.03",
          "37.80",
          "35.29",
          "35.66",
          "35.62",
          "36.28",
          "35.65",
          "35.82",
          "35.91",
          NA,
          "34.61",
          "34.53",
          "35.33",
          "35.84",
          "35.34",
          "35.03",
          "35.05",
          NA,
          "34.81",
          "34.65",
          "34.79",
          "34.53",
          "35.64",
          "35.31",
          "35.78",
          "36.75"
        )
      ),
      row.names = c(NA,-47L),
      class = "data.frame"
    )

  expect_equivalent(df_standard,
                    df_test)

})

test_that("Tokyo 2020 Men 100 Fly Semis, with ORs broken", {

  skip_on_cran()

  file <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/OG2020-_SWM_C73A2_SWMM100MBF------------SFNL--------.pdf"

  df_test <- swim_parse(read_results(file),
                        splits = TRUE,
                        relay_swimmers = TRUE)

  df_standard <-
    structure(
      list(
        Place = c(
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "1",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8"
        ),
        Heat = c(
          "Semi_1",
          "Semi_1",
          "Semi_1",
          "Semi_1",
          "Semi_1",
          "Semi_1",
          "Semi_1",
          "Semi_1",
          "Semi_2",
          "Semi_2",
          "Semi_2",
          "Semi_2",
          "Semi_2",
          "Semi_2",
          "Semi_2",
          "Semi_2"
        ),
        Lane = c(
          "4",
          "3",
          "5",
          "6",
          "2",
          "7",
          "8",
          "1",
          "4",
          "3",
          "5",
          "6",
          "2",
          "7",
          "8",
          "1"
        ),
        Name = c(
          "MILAK Kristof",
          "MILADINOV Josif",
          "MINAKOV Andrei",
          "TEMPLE Matthew",
          "METELLA Mehdy",
          "MIZUNUMA Naoki",
          "SUN Jiajun",
          "RAMADAN Youssef",
          "DRESSEL Caeleb",
          "PONTI Noe",
          "MAJERSKI Jakub",
          "MARTINEZ Luis Carlos",
          "LIENDO EDWARDS Joshua",
          "KORSTANJE Nyls",
          "SZABO Szebasztian",
          "SHIELDS Tom"
        ),
        Team = c(
          "HUN",
          "BUL",
          "ROC",
          "AUS",
          "FRA",
          "JPN",
          "CHN",
          "EGY",
          "USA",
          "SUI",
          "POL",
          "GUA",
          "CAN",
          "NED",
          "HUN",
          "USA"
        ),
        Reaction_Time = c(
          "0.66",
          "0.65",
          "0.63",
          "0.63",
          "0.65",
          "0.60",
          "0.64",
          "0.61",
          "0.62",
          "0.69",
          "0.64",
          "0.62",
          "0.63",
          "0.61",
          "0.58",
          "0.71"
        ),
        Finals_Time = c(
          "50.31",
          "51.06",
          "51.11",
          "51.12",
          "51.32",
          "51.46",
          "51.82",
          "52.27",
          "49.71",
          "50.76",
          "51.24",
          "51.30",
          "51.50",
          "51.80",
          "51.89",
          "51.99"
        ),
        DQ = c(
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0"
        ),
        Exhibition = c(
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0",
          "0"
        ),
        Event = c(
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly",
          "Men's 100m Butterfly"
        ),
        Split_50 = c(
          "23.74",
          "24.02",
          "23.73",
          "23.88",
          "23.73",
          "24.04",
          "23.88",
          "23.78",
          "23.20",
          "23.72",
          "24.10",
          "24.23",
          "23.88",
          "23.54",
          "23.79",
          "24.07"
        ),
        Split_100 = c(
          "26.57",
          "27.04",
          "27.38",
          "27.24",
          "27.59",
          "27.42",
          "27.94",
          "28.49",
          "26.51",
          "27.04",
          "27.14",
          "27.07",
          "27.62",
          "28.26",
          "28.10",
          "27.92"
        )
      ),
      row.names = c(NA,-16L),
      class = "data.frame"
    )


  expect_equivalent(df_standard,
                    df_test)

})

test_that("Tokyo 2020 Women 400MR Finals, with record issues", {

  skip_on_cran()

  file <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/OG2020-_SWM_C73B1_SWMW4X100MMD----------FNL-000100--.pdf"

  df_test <- swim_parse(read_results(file),
                        splits = TRUE,
                        relay_swimmers = TRUE)

  df_standard <-
    structure(
      list(
        Place = c("1", "2", "3", "4", "5", "6", "7", "8"),
        Lane = c("3", "5", "4", "8", "2", "6", "1", "7"),
        Team = c(
          "AUS - Australia",
          "USA - United States of America",
          "CAN - Canada",
          "CHN - People's Republic of China",
          "SWE - Sweden",
          "ITA - Italy",
          "ROC - ROC",
          "JPN - Japan"
        ),
        Finals_Time = c(
          "3:51.60",
          "3:51.73",
          "3:52.60",
          "3:54.13",
          "3:54.27",
          "3:56.68",
          "3:56.93",
          "3:58.12"
        ),
        DQ = c("0", "0", "0", "0", "0", "0", "0", "0"),
        Exhibition = c("0",
                       "0", "0", "0", "0", "0", "0", "0"),
        Event = c(
          "Women's 4 x 100m Medley Relay",
          "Women's 4 x 100m Medley Relay",
          "Women's 4 x 100m Medley Relay",
          "Women's 4 x 100m Medley Relay",
          "Women's 4 x 100m Medley Relay",
          "Women's 4 x 100m Medley Relay",
          "Women's 4 x 100m Medley Relay",
          "Women's 4 x 100m Medley Relay"
        ),
        Relay_Swimmer_1 = c(
          "McKEOWN Kaylee",
          "SMITH Regan",
          "MASSE Kylie",
          "PENG Xuwei",
          "COLEMAN Michelle",
          "PANZIERA Margherita",
          "KAMENEVA Mariia",
          "KONISHI Anna"
        ),
        Relay_Swimmer_2 = c(
          "HODGES Chelsea",
          "JACOBY Lydia",
          "PICKREM Sydney",
          "TANG Qianting",
          "HANSSON Sophie",
          "CARRARO Martina",
          "CHIKUNOVA Evgeniia",
          "WATANABE Kanako"
        ),
        Relay_Swimmer_3 = c(
          "McKEON Emma",
          "HUSKE Torri",
          "MACNEIL Margaret",
          "ZHANG Yufei",
          "HANSSON Louise",
          "di LIDDO Elena",
          "CHIMROVA Svetlana",
          "IKEE Rikako"
        ),
        Relay_Swimmer_4 = c(
          "CAMPBELL Cate",
          "WEITZEIL Abbey",
          "OLEKSIAK Penny",
          "YANG Junxuan",
          "SJOESTROEM Sarah",
          "PELLEGRINI Federica",
          "SURKOVA Arina",
          "IGARASHI Chihiro"
        ),
        Split_50 = c(
          "28.57",
          "28.44",
          "27.92",
          "28.99",
          "28.64",
          "29.29",
          "29.26",
          "29.32"
        ),
        Split_100 = c(
          "58.01",
          "58.05",
          "57.90",
          "59.63",
          "59.75",
          "1:00.03",
          "59.95",
          "59.92"
        ),
        Split_150 = c(
          "30.58",
          "30.48",
          "31.57",
          "30.74",
          "30.71",
          "30.31",
          "31.20",
          "31.35"
        ),
        Split_200 = c(
          "1:05.57",
          "1:05.03",
          "1:07.17",
          "1:06.09",
          "1:05.67",
          "1:05.88",
          "1:05.99",
          "1:06.61"
        ),
        Split_250 = c(
          "25.86",
          "25.42",
          "25.58",
          "25.48",
          "25.86",
          "26.63",
          "26.21",
          "26.81"
        ),
        Split_300 = c(
          "55.91",
          "56.16",
          "55.27",
          "55.39",
          "56.12",
          "56.96",
          "56.70",
          "57.92"
        ),
        Split_350 = c(
          "24.78",
          "24.91",
          "25.06",
          "25.37",
          "25.27",
          "25.76",
          "25.37",
          "25.63"
        ),
        Split_400 = c(
          "52.11",
          "52.49",
          "52.26",
          "53.02",
          "52.73",
          "53.81",
          "54.29",
          "53.67"
        )
      ),
      row.names = c(NA,-8L),
      class = "data.frame"
    )

  expect_equivalent(df_standard,
                    df_test)

})



# testthat::test_file("tests/testthat/test-omega.R")
