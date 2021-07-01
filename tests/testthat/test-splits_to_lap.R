test_that("cumulative splits 1500", {

  skip_on_cran() # due to length of test

  file <-
    system.file("extdata", "2018_jimi_flowers_PARA.pdf", package = "SwimmeR")

  df <- swim_parse(read_results(file),
                   splits = TRUE)

  df_test <- df %>%
    splits_to_lap()

  #### 1500 has lots of splits ####

  df_test_1500 <- df_test %>%
    filter(Event == "Women 1500 LC Meter Freestyle Multi-Class S14")

  df_standard_1500 <-
    structure(
      list(
        Place = 1,
        Name = "Nagy, Tessa M",
        Age = "18",
        Para = "S14",
        Team = "Unattached-UN",
        Prelims_Time = "26:19.38",
        Finals_Time = "27:16.60",
        DQ = 0,
        Exhibition = 0,
        Event = "Women 1500 LC Meter Freestyle Multi-Class S14",
        Split_50 = "46.78",
        Split_100 = "53.49",
        Split_150 = "52.78",
        Split_200 = "52.68",
        Split_250 = "54.73",
        Split_300 = "53.79",
        Split_350 = "54.73",
        Split_400 = "55.4",
        Split_450 = "53.04",
        Split_500 = "55.06",
        Split_550 = "53.88",
        Split_600 = "54.03",
        Split_650 = "53.88",
        Split_700 = "54.84",
        Split_750 = "56.38",
        Split_800 = "56.15",
        Split_850 = "59.48",
        Split_900 = "55.44",
        Split_950 = "55.35",
        Split_1000 = "55.1",
        Split_1050 = "55.63",
        Split_1100 = "55.66",
        Split_1150 = "55.59",
        Split_1200 = "54.81",
        Split_1250 = "55.96",
        Split_1300 = "53.65",
        Split_1350 = "55.48",
        Split_1400 = "55.06",
        Split_1450 = "55.47",
        Split_1500 = "52.28"
      ),
      row.names = c(NA,-1L),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_equivalent(df_test_1500, df_standard_1500)

  #### 50s shouldn't have splits ####

  df_test_50 <- df_test %>%
    filter(Event == "Women 50 LC Meter Freestyle Multi-Class S6")

  df_standard_50 <-
    structure(
      list(
        Place = c(1, 2, 3, 4),
        Name = c(
          "Nardella, Abigail M",
          "Shaffer, Casandra M",
          "Garcia, Julia",
          "Pfankuch, Emilynn N"
        ),
        Age = c("14", "18", "10", "12"),
        Para = c("S6", "S6", "S6",
                 "S6"),
        Team = c(
          "Chinook Aquatic Club-PN",
          "Bend Swim Club-OR",
          "Unattached-XX",
          "Blue Dolphins-OR"
        ),
        Prelims_Time = c("43.91",
                         "42.46", "1:18.35", "1:40.88"),
        Finals_Time = c("43.66", "44.44",
                        "1:18.78", "1:41.87"),
        DQ = c(0, 0, 0, 0),
        Exhibition = c(0,
                       0, 0, 0),
        Event = c(
          "Women 50 LC Meter Freestyle Multi-Class S6",
          "Women 50 LC Meter Freestyle Multi-Class S6",
          "Women 50 LC Meter Freestyle Multi-Class S6",
          "Women 50 LC Meter Freestyle Multi-Class S6"
        ),
        Split_50 = c(NA_character_,
                     NA_character_, NA_character_, NA_character_),
        Split_100 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_150 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_200 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_250 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_300 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_350 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_400 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_450 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_500 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_550 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_600 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_650 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_700 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_750 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_800 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_850 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_900 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_950 = c(NA_character_,
                      NA_character_, NA_character_, NA_character_),
        Split_1000 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_),
        Split_1050 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_),
        Split_1100 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_),
        Split_1150 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_),
        Split_1200 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_),
        Split_1250 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_),
        Split_1300 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_),
        Split_1350 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_),
        Split_1400 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_),
        Split_1450 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_),
        Split_1500 = c(NA_character_,
                       NA_character_, NA_character_, NA_character_)
      ),
      row.names = c(NA,-4L),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_equivalent(df_test_50, df_standard_50)

})

# testthat::test_file("tests/testthat/test-splits_to_lap.R")

