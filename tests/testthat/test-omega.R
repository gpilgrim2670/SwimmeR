test_that("Men 400 IM prelims", {
  file <-
    system.file("extdata", "Omega_OT_400IM_Prelims_2021.pdf", package = "SwimmeR")

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
               relay_swimmers = TRUE) %>%
    head(2)

  df_standard <- data.frame(
    Place = c("1", "2"),
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

test_that("Women 200 fly prelims, need to not remove swimmers for decimal counting", {
  file <-
    "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Omega/Omega_OT_Wave1_W200Fl_Heats_2021.pdf"

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
               relay_swimmers = TRUE) %>%
    head(3)

  df_standard <-
    structure(
      list(
        Place = c("1", "2", "3"),
        Heat = c("4", "2", "4"),
        Lane = c("6", "5", "7"),
        Name = c("MASSEY Alex", "STICKLEN Emma",
                 "CULLEN Casey"),
        Team = c("NAC", "KATY", "PWAC"),
        Reaction_Time = c("0.73",
                          "0.72", "0.71"),
        Finals_Time = c("2:13.01", "2:13.20", "2:13.76"),
        DQ = c("0", "0", "0"),
        Exhibition = c("0", "0", "0"),
        Event = c(
          "AM Women's 200m Butterfly Heats",
          "AM Women's 200m Butterfly Heats",
          "AM Women's 200m Butterfly Heats"
        ),
        Split_50 = c("29.83", "29.46", "30.67"),
        Split_100 = c("33.41",
                      "33.20", "33.96"),
        Split_150 = c("33.99", "34.93", "34.27"),
        Split_200 = c("35.78", "35.61", "34.86")
      ),
      row.names = c(NA,
                    3L),
      class = "data.frame"
    )

  expect_equivalent(df_standard,
                    df_test)

})

test_that("US OT 2021 Wave I Men 100 Free Heats", {
  file <-
    "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Omega/Omega_OT_Wave1_M100Fr_Heats_2021.pdf"

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
               relay_swimmers = TRUE) %>%
    head(2)

  df_standard <-
    structure(
      list(
        Place = c("1", "2"),
        Heat = c("3", "4"),
        Lane = c("5",
                 "3"),
        Name = c("BENSON Andrew", "MILLER Luke"),
        Team = c("WA",
                 "EA"),
        Reaction_Time = c("0.62", "0.62"),
        Finals_Time = c("50.10",
                        "50.13"),
        DQ = c("0", "0"),
        Exhibition = c("0", "0"),
        Event = c("AM Men's 100m Freestyle Heats",
                  "AM Men's 100m Freestyle Heats"),
        Split_50 = c("23.58", "24.22"),
        Split_100 = c("26.52", "25.91")
      ),
      row.names = 1:2,
      class = "data.frame"
    )

  expect_equivalent(df_standard,
                    df_test)

})

test_that("Women 400m finals", {
  file <-
    system.file("extdata", "Omega_OT_400m_Finals_2021.pdf", package = "SwimmeR")

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
               relay_swimmers = TRUE) %>%
    head(2)

  df_standard <- data.frame(
    Place = c("1", "2"),
    Heat = rep("Final", 2),
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
  file <-
    system.file("extdata", "Omega_OT_100Br_Swimoff_2021.pdf", package = "SwimmeR")

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE)

  df_standard <- data.frame(
    Place = c("1", "2"),
    Heat = rep("Heats", 2),
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
  file <-
    system.file("extdata", "Omega_Wave1_200fly_Finals_2021.pdf", package = "SwimmeR")

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE) %>%
    head(2)

  df_standard <- data.frame(
    Place = c("1", "2"),
    Heat = rep("Final", 2),
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
  file <-
    system.file("extdata", "Omega_Wave1_1500_Finals_2021.pdf", package = "SwimmeR")

  df <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE) %>%
    filter(Name != "NARVID Jake") # Jake has issues with his reported splits

  list_test <- df %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), sec_format)) %>%
    mutate(Split_Total = rowSums(dplyr::across(dplyr::starts_with("Split")), na.rm = TRUE)) %>%
    pull(Split_Total)

  list_standard <-
    c(935.94,
      936.24,
      940.05,
      944.30,
      952.72,
      954.12,
      954.17,
      962.61,
      964.30,
      965.41,
      978.49)

  expect_equivalent(list_standard,
                    list_test)

})

test_that("Tokyo 2020 Men 400IM Heat 1", {
  skip_on_cran()

  file <-
    "https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73A1_SWMM400MIM------------FNL-000100--.pdf"

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
               relay_swimmers = TRUE)

  df_standard <-
    structure(
      list(
        Place = c("1", "2", "3", "4", "4", "6", "7", "8"),
        Heat = rep("Final", 8),
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
        )
      ),
      row.names = c(NA,-8L),
      class = "data.frame"
    )

  expect_equivalent(df_standard,
                    df_test)

})

test_that("Tokyo 2020 Women 4 x 100m Free Heat 1", {
  skip_on_cran()

  file <-
    "https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73B1_SWMW4X100MFR----------HEAT000100--.pdf"

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
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
      row.names = c(NA, -7L),
      class = "data.frame"
    )

  expect_equivalent(df_standard,
                    df_test)

})

test_that("Tokyo 2020 Women 100 Breast Heats, with DNS", {
  skip_on_cran()

  file <-
    "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMW100MBR_HEAT.pdf"

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
               relay_swimmers = TRUE,
               typo = "RODRIGUEZ VILLANUEVA Byanca MelissaMEX",
               replacement = "RODRIGUEZ VILLANUEVA Byanca Melissa          MEX")

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
          "Heat_1",
          "Heat_1",
          "Heat_1",
          "Heat_1",
          "Heat_1",
          "Heat_1",
          "Heat_1",
          "Heat_2",
          "Heat_2",
          "Heat_2",
          "Heat_2",
          "Heat_2",
          "Heat_2",
          "Heat_2",
          "Heat_2",
          "Heat_3",
          "Heat_3",
          "Heat_3",
          "Heat_3",
          "Heat_3",
          "Heat_3",
          "Heat_3",
          "Heat_3",
          "Heat_4",
          "Heat_4",
          "Heat_4",
          "Heat_4",
          "Heat_4",
          "Heat_4",
          "Heat_4",
          "Heat_4",
          "Heat_5",
          "Heat_5",
          "Heat_5",
          "Heat_5",
          "Heat_5",
          "Heat_5",
          "Heat_5",
          "Heat_5",
          "Heat_6",
          "Heat_6",
          "Heat_6",
          "Heat_6",
          "Heat_6",
          "Heat_6",
          "Heat_6",
          "Heat_6"
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
          "SANTOS SILVA Emily M",
          "FISHER-MARSTERS Kirsten Andrea",
          "KOK SHUN Alicia",
          "VERDINO Claudia",
          "TETEREVKOVA Kotryna",
          "ELENDT Anna Charlott Darcel",
          "ATKINSON Alia",
          "SCHOUTEN Tes",
          "LECLUYSE Fanny",
          "RODRIGUEZ VILLANUEVA Byanca Melissa",
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
          "MEX",
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
          "0.65",
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

  file <-
    "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMM100MBF_SFNL.pdf"

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
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
      row.names = c(NA, -16L),
      class = "data.frame"
    )


  expect_equivalent(df_standard,
                    df_test)

})

test_that("Tokyo 2020 Women 400MR Finals, with record issues", {
  skip_on_cran()

  file <-
    "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMW4X100MMD_FNL.pdf"

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
               relay_swimmers = TRUE)

  df_standard <-
    structure(
      list(
        Place = c("1", "2", "3", "4", "5", "6", "7", "8"),
        Heat = rep("Final", 8),
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
      row.names = c(NA, -8L),
      class = "data.frame"
    )

  expect_equivalent(df_standard,
                    df_test)

})

test_that("Tokyo 2020 Mixed 4x100MR Finals, relay swimmer gender", {
  skip_on_cran()

  file <-
    "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMX4X100MMD_FNL.pdf"

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
               relay_swimmers = TRUE)

  df_standard <-
    structure(
      list(
        Place = c("1", "2", "3", "4", "5", "6", "7", "8"),
        Heat = rep("Final", 8),
        Lane = c("4", "3", "6", "2", "5", "7", "1", "8"),
        Team = c(
          "GBR - Great Britain",
          "CHN - People's Republic of China",
          "AUS - Australia",
          "ITA - Italy",
          "USA - United States of America",
          "NED - Netherlands",
          "ROC - ROC",
          "ISR - Israel"
        ),
        Finals_Time = c(
          "3:37.58",
          "3:38.86",
          "3:38.95",
          "3:39.28",
          "3:40.58",
          "3:41.25",
          "3:42.45",
          "3:44.77"
        ),
        DQ = c("0",
               "0", "0", "0", "0", "0", "0", "0"),
        Exhibition = c("0", "0",
                       "0", "0", "0", "0", "0", "0"),
        Event = c(
          "Mixed 4 x 100m Medley Relay",
          "Mixed 4 x 100m Medley Relay",
          "Mixed 4 x 100m Medley Relay",
          "Mixed 4 x 100m Medley Relay",
          "Mixed 4 x 100m Medley Relay",
          "Mixed 4 x 100m Medley Relay",
          "Mixed 4 x 100m Medley Relay",
          "Mixed 4 x 100m Medley Relay"
        ),
        Relay_Swimmer_1 = c(
          "DAWSON Kathleen",
          "XU Jiayu",
          "McKEOWN Kaylee",
          "CECCON Thomas",
          "MURPHY Ryan",
          "TOUSSAINT Kira",
          "RYLOV Evgeny",
          "GORBENKO Anastasia"
        ),
        Relay_Swimmer_1_Gender = c("F",
                                   "M", "F", "M", "M", "F", "M", "F"),
        Relay_Swimmer_2 = c(
          "PEATY Adam",
          "YAN Zibei",
          "STUBBLETY-COOK Izaac",
          "MARTINENGHI Nicolo",
          "JACOBY Lydia",
          "KAMMINGA Arno",
          "PRIGODA Kirill",
          "GOLDFADEN Itay"
        ),
        Relay_Swimmer_2_Gender = c("M",
                                   "M", "M", "M", "F", "M", "M", "M"),
        Relay_Swimmer_3 = c(
          "GUY James",
          "ZHANG Yufei",
          "TEMPLE Matthew",
          "di LIDDO Elena",
          "HUSKE Torri",
          "KORSTANJE Nyls",
          "CHIMROVA Svetlana",
          "COHEN GROUMI Gal"
        ),
        Relay_Swimmer_3_Gender = c("M",
                                   "F", "M", "F", "F", "M", "F", "M"),
        Relay_Swimmer_4 = c(
          "HOPKIN Anna",
          "YANG Junxuan",
          "McKEON Emma",
          "PELLEGRINI Federica",
          "DRESSEL Caeleb",
          "HEEMSKERK Femke",
          "KAMENEVA Mariia",
          "MUREZ Andrea"
        ),
        Relay_Swimmer_5_Gender = c("F",
                                   "F", "F", "F", "M", "F", "F", "F"),
        Split_50 = c(
          "28.35",
          "25.33",
          "28.46",
          "25.65",
          "25.09",
          "28.41",
          "26.06",
          "29.08"
        ),
        Split_100 = c(
          "58.80",
          "52.56",
          "58.14",
          "52.23",
          "52.23",
          "59.45",
          "52.79",
          "59.55"
        ),
        Split_150 = c(
          "26.18",
          "26.96",
          "27.60",
          "27.16",
          "29.99",
          "26.61",
          "27.40",
          "27.05"
        ),
        Split_200 = c(
          "56.78",
          "58.11",
          "58.82",
          "57.73",
          "1:05.09",
          "57.89",
          "59.15",
          "59.86"
        ),
        Split_250 = c(
          "23.22",
          "25.44",
          "23.55",
          "26.49",
          "25.41",
          "22.82",
          "26.06",
          "23.84"
        ),
        Split_300 = c(
          "50.00",
          "55.48",
          "50.26",
          "56.62",
          "56.27",
          "51.34",
          "56.95",
          "51.58"
        ),
        Split_350 = c(
          "24.66",
          "25.04",
          "24.88",
          "25.29",
          "22.15",
          "24.91",
          "25.74",
          "25.91"
        ),
        Split_400 = c(
          "52.00",
          "52.71",
          "51.73",
          "52.70",
          "46.99",
          "52.57",
          "53.56",
          "53.78"
        )
      ),
      row.names = c(NA,-8L),
      class = "data.frame"
    )

  expect_equivalent(df_standard,
                    df_test)

})

test_that("Tokyo 2020 Men 200FR Finals, need to capture swims", {
  skip_on_cran()

  file <-
    "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMM200MFR_FNL.pdf"

  df_test <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE,
               relay_swimmers = TRUE)

  df_standard <-
    structure(
      list(
        Place = c("1", "2", "3", "4", "5", "6", "7", "8"),
        Heat = rep("Final", 8),
        Lane = c("6", "4", "8", "1", "2", "5", "7", "3"),
        Name = c(
          "DEAN Tom",
          "SCOTT Duncan",
          "SCHEFFER Fernando",
          "POPOVICI David",
          "MALYUTIN Martin",
          "SMITH Kieran",
          "HWANG Sunwoo",
          "RAPSYS Danas"
        ),
        Team = c("GBR",
                 "GBR", "BRA", "ROU", "ROC", "USA", "KOR", "LTU"),
        Reaction_Time = c("0.64",
                          "0.66", "0.66", "0.68", "0.70", "0.69", "0.58", "0.65"),
        Finals_Time = c(
          "1:44.22",
          "1:44.26",
          "1:44.66",
          "1:44.68",
          "1:45.01",
          "1:45.12",
          "1:45.26",
          "1:45.78"
        ),
        DQ = c("0", "0", "0", "0", "0", "0", "0", "0"),
        Exhibition = c("0",
                       "0", "0", "0", "0", "0", "0", "0"),
        Event = c(
          "Men's 200m Freestyle",
          "Men's 200m Freestyle",
          "Men's 200m Freestyle",
          "Men's 200m Freestyle",
          "Men's 200m Freestyle",
          "Men's 200m Freestyle",
          "Men's 200m Freestyle",
          "Men's 200m Freestyle"
        ),
        Split_50 = c(
          "24.21",
          "24.81",
          "24.24",
          "24.23",
          "25.03",
          "24.58",
          "23.95",
          "24.96"
        ),
        Split_100 = c(
          "26.25",
          "26.57",
          "26.01",
          "26.50",
          "26.62",
          "26.36",
          "25.83",
          "27.00"
        ),
        Split_150 = c(
          "26.92",
          "26.42",
          "27.03",
          "27.24",
          "26.78",
          "26.87",
          "26.78",
          "26.87"
        ),
        Split_200 = c(
          "26.84",
          "26.46",
          "27.38",
          "26.71",
          "26.58",
          "27.31",
          "28.70",
          "26.95"
        )
      ),
      row.names = c(NA, -8L),
      class = "data.frame"
    )
  expect_equivalent(df_standard,
                    df_test)

})

test_that("Tokyo 2020 Men 4x200FR Finals, need to not capture splits as swims",
          {
            skip_on_cran()

            file <-
              "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMM4X200MFR_FNL.pdf"

            df_test <- file %>%
              read_results() %>%
              swim_parse(splits = TRUE,
                         relay_swimmers = TRUE)

            df_standard <-
              structure(
                list(
                  Place = c("1", "2", "3", "4", "5", "6", "7", "8"),
                  Heat = rep("Final", 8),
                  Lane = c("4", "6", "5", "2", "3", "7", "1", "8"),
                  Team = c(
                    "GBR - Great Britain",
                    "ROC - ROC",
                    "AUS - Australia",
                    "USA - United States of America",
                    "ITA - Italy",
                    "SUI - Switzerland",
                    "GER - Germany",
                    "BRA - Brazil"
                  ),
                  Finals_Time = c(
                    "6:58.58",
                    "7:01.81",
                    "7:01.84",
                    "7:02.43",
                    "7:03.24",
                    "7:06.12",
                    "7:06.51",
                    "7:08.22"
                  ),
                  DQ = c("0", "0",
                         "0", "0", "0", "0", "0", "0"),
                  Exhibition = c("0", "0", "0",
                                 "0", "0", "0", "0", "0"),
                  Event = c(
                    "Men's 4 x 200m Freestyle Relay",
                    "Men's 4 x 200m Freestyle Relay",
                    "Men's 4 x 200m Freestyle Relay",
                    "Men's 4 x 200m Freestyle Relay",
                    "Men's 4 x 200m Freestyle Relay",
                    "Men's 4 x 200m Freestyle Relay",
                    "Men's 4 x 200m Freestyle Relay",
                    "Men's 4 x 200m Freestyle Relay"
                  ),
                  Relay_Swimmer_1 = c(
                    "DEAN Tom",
                    "MALYUTIN Martin",
                    "GRAHAM Alexander",
                    "SMITH Kieran",
                    "BALLO Stefano",
                    "DJAKOVIC Antonio",
                    "MARTENS Lukas",
                    "SCHEFFER Fernando"
                  ),
                  Relay_Swimmer_2 = c(
                    "GUY James",
                    "GIREV Ivan",
                    "CHALMERS Kyle",
                    "KIBLER Drew",
                    "CIAMPI Matteo",
                    "LIESS Nils",
                    "ZELLMANN Poul",
                    "SETIN SARTORI Murilo"
                  ),
                  Relay_Swimmer_3 = c(
                    "RICHARDS Matthew",
                    "RYLOV Evgeny",
                    "INCERTI Zac",
                    "APPLE Zach",
                    "MEGLI Filippo",
                    "PONTI Noe",
                    "MUHLLEITNER Henning Bennet",
                    "CORREIA Breno"
                  ),
                  Relay_Swimmer_4 = c(
                    "SCOTT Duncan",
                    "DOVGALYUK Mikhail",
                    "NEILL Thomas",
                    "HAAS Townley",
                    "di COLA Stefano",
                    "MITYUKOV Roman",
                    "HEIDTMANN Jacob",
                    "MELO Luiz Altamir"
                  ),
                  Split_50 = c(
                    "24.55",
                    "24.77",
                    "24.94",
                    "24.37",
                    "24.98",
                    "24.89",
                    "25.20",
                    "24.70"
                  ),
                  Split_100 = c(
                    "51.26",
                    "51.62",
                    "51.65",
                    "50.62",
                    "51.68",
                    "51.81",
                    "52.14",
                    "51.08"
                  ),
                  Split_150 = c(
                    "1:18.38",
                    "1:18.81",
                    "1:18.80",
                    "1:17.59",
                    "1:18.76",
                    "1:19.22",
                    "1:19.58",
                    "1:18.31"
                  ),
                  Split_200 = c(
                    "1:45.72",
                    "1:45.69",
                    "1:46.00",
                    "1:44.74",
                    "1:45.77",
                    "1:45.77",
                    "1:46.68",
                    "1:45.93"
                  ),
                  Split_250 = c(
                    "23.84",
                    "24.26",
                    "24.01",
                    "24.29",
                    "24.62",
                    "24.17",
                    "24.23",
                    "24.63"
                  ),
                  Split_300 = c(
                    "50.31",
                    "50.96",
                    "50.82",
                    "50.95",
                    "51.58",
                    "51.03",
                    "51.34",
                    "51.61"
                  ),
                  Split_350 = c(
                    "1:17.31",
                    "1:18.20",
                    "1:17.95",
                    "1:18.18",
                    "1:18.78",
                    "1:19.08",
                    "1:19.06",
                    "1:18.78"
                  ),
                  Split_400 = c(
                    "1:44.40",
                    "1:45.63",
                    "1:45.35",
                    "1:45.51",
                    "1:45.88",
                    "1:47.74",
                    "1:46.30",
                    "1:46.09"
                  ),
                  Split_450 = c(
                    "23.88",
                    "24.19",
                    "24.13",
                    "23.80",
                    "24.20",
                    "24.66",
                    "25.21",
                    "24.63"
                  ),
                  Split_500 = c(
                    "50.70",
                    "51.06",
                    "50.84",
                    "50.54",
                    "50.78",
                    "51.85",
                    "52.52",
                    "51.93"
                  ),
                  Split_550 = c(
                    "1:18.14",
                    "1:18.54",
                    "1:18.37",
                    "1:18.31",
                    "1:18.26",
                    "1:19.63",
                    "1:20.20",
                    "1:20.04"
                  ),
                  Split_600 = c(
                    "1:45.01",
                    "1:45.26",
                    "1:45.75",
                    "1:47.31",
                    "1:45.33",
                    "1:46.93",
                    "1:48.04",
                    "1:48.11"
                  ),
                  Split_650 = c(
                    "23.64",
                    "23.86",
                    "23.93",
                    "23.82",
                    "23.92",
                    "24.11",
                    "24.39",
                    "23.77"
                  ),
                  Split_700 = c(
                    "49.86",
                    "50.14",
                    "50.49",
                    "50.04",
                    "50.41",
                    "51.01",
                    "50.99",
                    "50.47"
                  ),
                  Split_750 = c(
                    "1:16.65",
                    "1:17.47",
                    "1:17.65",
                    "1:17.40",
                    "1:17.80",
                    "1:18.48",
                    "1:17.99",
                    "1:18.45"
                  ),
                  Split_800 = c(
                    "1:43.45",
                    "1:45.23",
                    "1:44.74",
                    "1:44.87",
                    "1:46.26",
                    "1:45.68",
                    "1:45.49",
                    "1:48.09"
                  )
                ),
                row.names = c(NA, -8L),
                class = "data.frame"
              )
            expect_equivalent(df_standard,
                              df_test)

          })

test_that("Tokyo 2020 Women 4x200FR Heats, relay swimmers with punctuation in name",
          {
            skip_on_cran()

            file <-
              "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMW4X200MFR_HEAT.pdf"

            df_test <- file %>%
              read_results() %>%
              swim_parse(splits = TRUE,
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
                    "8",
                    "2"
                  ),
                  Heat = c(
                    "Heat_1",
                    "Heat_1",
                    "Heat_1",
                    "Heat_1",
                    "Heat_1",
                    "Heat_1",
                    "Heat_1",
                    "Heat_1",
                    "Heat_2",
                    "Heat_2",
                    "Heat_2",
                    "Heat_2",
                    "Heat_2",
                    "Heat_2",
                    "Heat_2",
                    "Heat_2"
                  ),
                  Lane = c(
                    "4",
                    "5",
                    "3",
                    "2",
                    "6",
                    "1",
                    "7",
                    "8",
                    "4",
                    "5",
                    "3",
                    "6",
                    "7",
                    "1",
                    NA,
                    NA
                  ),
                  Team = c(
                    "USA - United States of America",
                    "CHN - People's Republic of China",
                    "GER - Germany",
                    "FRA - France",
                    "JPN - Japan",
                    "RSA - South Africa",
                    "TUR - Turkey",
                    "KOR - Republic of Korea",
                    "AUS - Australia",
                    "CAN - Canada",
                    "ROC - ROC",
                    "HUN - Hungary",
                    "BRA - Brazil",
                    "NZL - New Zealand",
                    "HKG - Hong Kong, China",
                    "ITA - Italy"
                  ),
                  Finals_Time = c(
                    "7:47.57",
                    "7:48.98",
                    "7:52.06",
                    "7:55.05",
                    "7:58.39",
                    "8:01.56",
                    "8:10.96",
                    "8:11.16",
                    "7:44.61",
                    "7:51.52",
                    "7:52.04",
                    "7:56.16",
                    "7:59.50",
                    "8:06.16",
                    NA,
                    NA
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
                    "1",
                    "1"
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
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay",
                    "Women's 4 x 200m Freestyle Relay"
                  ),
                  Relay_Swimmer_1 = c(
                    "SIMS Arabella",
                    "TANG Muhan",
                    "GOSE Isabel",
                    "BONNET Charlotte",
                    "IGARASHI Chihiro",
                    "CANNY Aimee",
                    "GUNES Viktoria",
                    "JUNG Hyunyoung",
                    "O'CALLAGHAN Mollie",
                    "SAVARD Katerine",
                    "GUZHENKOVA Anastasia",
                    "JAKABOS Zsuzsanna",
                    "RODRIGUES Aline",
                    "FAIRWEATHER Erika",
                    NA,
                    "PIROZZI Stefania"
                  ),
                  Relay_Swimmer_2 = c(
                    "MADDEN Paige",
                    "ZHANG Yifan",
                    "KULLMANN Leonie Marlen",
                    "TOUATI Assia",
                    "SHIRAI Rio",
                    "MEDER Rebecca",
                    "BOCEKLER Beril",
                    "KIM Seoyeong",
                    "HARRIS Meg",
                    "SMITH Rebecca",
                    "SALAMATINA Valeriia",
                    "VERES Laura",
                    "OLIVEIRA Larissa",
                    "DOYLE Carina",
                    NA,
                    "MASCOLO Anna Chiara"
                  ),
                  Relay_Swimmer_3 = c(
                    "Mc LAUGHLIN Kathryn",
                    "DONG Jie",
                    "PIETRUSCHKA Marie",
                    "TESSARIOL Lucile",
                    "IKEMOTO Nagisa",
                    "COETZEE Dune",
                    "ERTAN Deniz",
                    "HAN Dakyung",
                    "THROSSELL Brianna",
                    "HARVEY Mary-Sophie",
                    "ANDRUSENKO Veronika",
                    "VERRASZTO Evelin",
                    "ALMEIDA Nathalia",
                    "THOMAS Eve",
                    NA,
                    "VETRANO Giulia"
                  ),
                  Relay_Swimmer_4 = c(
                    "FORDE Brooke",
                    "LI Bingjie",
                    "BRUHN Annika",
                    "FABRE Margaux",
                    "MASUDA Aoi",
                    "GALLAGHER Erin",
                    "TUNCEL Merve",
                    "AN Sehyeon",
                    "COOK Tamsin",
                    "PICKREM Sydney",
                    "EGOROVA Anna",
                    "KESELY Ajna",
                    "RONCATTO Gabrielle",
                    "GALYER Ali",
                    NA,
                    "PELLEGRINI Federica"
                  ),
                  Split_50 = c(
                    "27.56",
                    "27.13",
                    "27.59",
                    "27.42",
                    "27.52",
                    "27.66",
                    "28.37",
                    "28.09",
                    "27.25",
                    "27.38",
                    "27.73",
                    "28.18",
                    "28.04",
                    "27.30",
                    NA,
                    "28.16"
                  ),
                  Split_100 = c(
                    "57.35",
                    "56.68",
                    "57.13",
                    "57.46",
                    "57.66",
                    "57.40",
                    "59.39",
                    "58.78",
                    "56.94",
                    "57.31",
                    "57.45",
                    "58.47",
                    "57.99",
                    "56.97",
                    NA,
                    "58.36"
                  ),
                  Split_150 = c(
                    "1:28.05",
                    "1:26.86",
                    "1:27.33",
                    "1:27.81",
                    "1:28.17",
                    "1:27.38",
                    "1:31.71",
                    "1:30.08",
                    "1:26.70",
                    "1:27.28",
                    "1:27.36",
                    "1:28.78",
                    "1:29.13",
                    "1:27.34",
                    NA,
                    "1:29.74"
                  ),
                  Split_200 = c(
                    "1:58.59",
                    "1:57.29",
                    "1:57.29",
                    "1:57.61",
                    "1:57.87",
                    "1:58.41",
                    "2:04.42",
                    "2:01.27",
                    "1:55.11",
                    "1:58.18",
                    "1:57.26",
                    "1:59.19",
                    "2:00.15",
                    "1:57.38",
                    NA,
                    "2:01.64"
                  ),
                  Split_250 = c(
                    "27.01",
                    "26.60",
                    "27.59",
                    "27.46",
                    "27.45",
                    "27.35",
                    "28.33",
                    "27.69",
                    "25.87",
                    "26.59",
                    "27.70",
                    "27.17",
                    "27.29",
                    "28.01",
                    NA,
                    NA
                  ),
                  Split_300 = c(
                    "55.94",
                    "56.43",
                    "57.19",
                    "57.68",
                    "58.18",
                    "57.76",
                    "59.31",
                    "58.08",
                    "55.03",
                    "55.93",
                    "57.84",
                    "56.74",
                    "57.28",
                    "59.13",
                    NA,
                    NA
                  ),
                  Split_350 = c(
                    "1:25.60",
                    "1:27.17",
                    "1:27.69",
                    "1:28.27",
                    "1:29.10",
                    "1:28.84",
                    "1:30.91",
                    "1:28.87",
                    "1:25.70",
                    "1:25.87",
                    "1:28.78",
                    "1:27.11",
                    "1:28.93",
                    "1:30.57",
                    NA,
                    NA
                  ),
                  Split_400 = c(
                    "1:55.96",
                    "1:57.63",
                    "1:59.00",
                    "1:58.59",
                    "1:59.94",
                    "2:00.53",
                    "2:02.03",
                    "1:59.98",
                    "1:57.01",
                    "1:55.99",
                    "1:58.87",
                    "1:57.88",
                    "2:01.50",
                    "2:02.18",
                    NA,
                    NA
                  ),
                  Split_450 = c(
                    "26.45",
                    "27.15",
                    "27.65",
                    "27.93",
                    "26.88",
                    "27.32",
                    "28.48",
                    "28.21",
                    "26.11",
                    "26.65",
                    "27.34",
                    "27.94",
                    "27.62",
                    "28.64",
                    NA,
                    NA
                  ),
                  Split_500 = c(
                    "55.68",
                    "56.52",
                    "58.03",
                    "58.38",
                    "57.15",
                    "57.59",
                    "1:00.20",
                    "59.50",
                    "55.61",
                    "56.28",
                    "57.13",
                    "58.62",
                    "57.44",
                    "59.87",
                    NA,
                    NA
                  ),
                  Split_550 = c(
                    "1:25.66",
                    "1:26.74",
                    "1:28.45",
                    "1:28.79",
                    "1:28.43",
                    "1:28.46",
                    "1:32.34",
                    "1:32.01",
                    "1:25.91",
                    "1:26.66",
                    "1:27.81",
                    "1:29.56",
                    "1:28.39",
                    "1:30.62",
                    NA,
                    NA
                  ),
                  Split_600 = c(
                    "1:56.02",
                    "1:57.77",
                    "1:58.73",
                    "1:59.39",
                    "2:00.25",
                    "1:59.75",
                    "2:04.15",
                    "2:04.38",
                    "1:56.46",
                    "1:57.53",
                    "1:57.77",
                    "2:00.35",
                    "1:59.18",
                    "2:00.75",
                    NA,
                    NA
                  ),
                  Split_650 = c(
                    "27.52",
                    "26.92",
                    "26.58",
                    "27.07",
                    "27.06",
                    "27.16",
                    "28.54",
                    "28.67",
                    "26.77",
                    "27.27",
                    "27.24",
                    "26.79",
                    "27.53",
                    "28.44",
                    NA,
                    NA
                  ),
                  Split_700 = c(
                    "56.73",
                    "56.65",
                    "55.71",
                    "57.07",
                    "57.22",
                    "57.85",
                    "59.54",
                    "1:00.60",
                    "56.14",
                    "57.86",
                    "57.24",
                    "56.52",
                    "57.71",
                    "1:00.45",
                    NA,
                    NA
                  ),
                  Split_750 = c(
                    "1:26.75",
                    "1:26.76",
                    "1:26.14",
                    "1:28.21",
                    "1:28.56",
                    "1:29.78",
                    "1:30.43",
                    "1:33.35",
                    "1:26.08",
                    "1:29.54",
                    "1:27.56",
                    "1:27.17",
                    "1:28.31",
                    "1:33.23",
                    NA,
                    NA
                  ),
                  Split_800 = c(
                    "1:57.00",
                    "1:56.29",
                    "1:57.04",
                    "1:59.46",
                    "2:00.33",
                    "2:02.87",
                    "2:00.36",
                    "2:05.53",
                    "1:56.03",
                    "1:59.82",
                    "1:58.14",
                    "1:58.74",
                    "1:58.67",
                    "2:05.85",
                    NA,
                    NA
                  )
                ),
                row.names = c(NA,-16L),
                class = "data.frame"
              )


            expect_equivalent(df_standard,
                              df_test)

          })

test_that("Tokyo Men 50 Free Final, need to not pull out relay swimmers",
          {
            skip_on_cran()

            file <-
              "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMM50MFR_FNL.pdf"

            df_test <- file %>%
              read_results() %>%
              swim_parse(splits = TRUE,
                         relay_swimmers = TRUE)

            df_standard <-
              structure(
                list(
                  Place = c("1", "2", "3", "4", "5", "5", "7", "8"),
                  Heat = rep("Final", 8),
                  Lane = c("4", "5", "3", "7", "2", "6", "1", "8"),
                  Name = c(
                    "DRESSEL Caeleb",
                    "MANAUDOU Florent",
                    "FRATUS Bruno",
                    "ANDREW Michael",
                    "PROUD Benjamin",
                    "GKOLOMEEV Kristian",
                    "ZAZZERI Lorenzo",
                    "de BOER Thom"
                  ),
                  Team = c("USA",
                           "FRA", "BRA", "USA", "GBR", "GRE", "ITA", "NED"),
                  Reaction_Time = c("0.63",
                                    "0.61", "0.64", "0.66", "0.58", "0.63", "0.61", "0.63"),
                  Finals_Time = c(
                    "21.07",
                    "21.55",
                    "21.57",
                    "21.60",
                    "21.72",
                    "21.72",
                    "21.78",
                    "21.79"
                  ),
                  DQ = c("0", "0", "0", "0", "0", "0", "0", "0"),
                  Exhibition = c("0",
                                 "0", "0", "0", "0", "0", "0", "0"),
                  Event = c(
                    "Men's 50m Freestyle",
                    "Men's 50m Freestyle",
                    "Men's 50m Freestyle",
                    "Men's 50m Freestyle",
                    "Men's 50m Freestyle",
                    "Men's 50m Freestyle",
                    "Men's 50m Freestyle",
                    "Men's 50m Freestyle"
                  ),
                  Split_50 = c(
                    "21.07",
                    "21.55",
                    "21.57",
                    "21.60",
                    "21.72",
                    "21.72",
                    "21.78",
                    "21.79"
                  )
                ),
                row.names = c(NA,-8L),
                class = "data.frame"
              )


            expect_equivalent(df_standard,
                              df_test)

          })


# testthat::test_file("tests/testthat/test-omega.R")
