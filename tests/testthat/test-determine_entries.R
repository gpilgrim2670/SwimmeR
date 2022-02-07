test_that("determine entries RIT v IC", {

  # skip_on_cran()

  file <- system.file("extdata", "RIT_TopTimes_FS2021.pdf", package = "SwimmeR")

  #### test dfs ####
  RIT_TopTimes_2021 <- file %>%
    read_results() %>%
    swim_parse() %>%
    rename("Finals" = Result)

  RIT_IC <- "https://s3.amazonaws.com/sidearm.sites/bombers.ithaca.edu/documents/2021/11/20/11_20_21_RIT_IC_Results.pdf" %>%
    read_results() %>%
    swim_parse()

  IC <- RIT_IC %>%
    filter(Team == "Ithaca College-NI") %>%
    filter(str_detect(Event, "Div") == FALSE) %>%
    filter(str_detect(Event, "Relay") == FALSE) %>%
    mutate(Event = str_remove(Event, " Yard"),
           Event = str_remove(Event, "style"),
           Event = str_remove(Event, "stroke"),
           Event = str_replace(Event, "Butterfly", "Fly"))

  df_test <-
    determine_entries(
      df = RIT_TopTimes_2021,
      op_df = IC,
      point_values = "ncaa_six_lane",
      time_col = "Finals",
      events = NULL
    )

  df_standard <- structure(list(Rank = c(1, 2, 3, 2, 11, 3, 1, 2, 3, 1, 2, 6,
                                         3, 4, 2, 4, 5, 3, 1, 4, 2, 2, 3, 1, 1, 2, 4, 1, 4, 5, 1, 2, 3,
                                         2, 3, 4, 1, 6, 3, 2, 8, 4, 1, 8, 4, 3, 7, 2, 3, 5, 2, 2, 9, 3,
                                         1, 2, 3, 6, 8, 3, 4, 6, 3, 5, 7, 4, 2, 3, 4, 1, 8, 4), Finals = c("24.92",
                                                                                                           "25.01", "25.12", "54.08", "57.33", "54.73", "1:56.03", "2:00.75",
                                                                                                           "2:01.02", "5:17.21", "5:23.88", "5:34.28", "11:34.44", "11:45.27",
                                                                                                           "11:23.60", "1:01.04", "1:03.53", "1:00.83", "2:08.95", "2:16.17",
                                                                                                           "2:15.09", "1:07.16", "1:08.82", "1:05.85", "2:21.70", "2:23.11",
                                                                                                           "2:47.12", "1:00.67", "1:02.20", "1:03.45", "2:19.63", "2:24.96",
                                                                                                           "2:30.44", "2:10.53", "2:16.77", "2:19.66", "21.32", "22.04",
                                                                                                           "21.78", "46.83", "49.03", "48.06", "1:42.04", "1:50.09", "1:46.03",
                                                                                                           "4:48.49", "5:01.63", "4:46.97", "10:29.66", "11:11.81", "10:19.91",
                                                                                                           "52.14", "1:00.49", "54.13", "1:51.92", "1:53.08", "1:59.98",
                                                                                                           "1:01.37", "1:02.77", "59.12", "2:14.51", "2:20.72", "2:13.59",
                                                                                                           "52.57", "53.90", "52.10", "1:55.61", "1:55.77", "2:02.51", "1:53.57",
                                                                                                           "2:03.45", "1:59.06"), Name = c("Phoebe Huey", "Kimberly Dobie",
                                                                                                                                           "Sydney Burgard", "Kimberly Dobie", "Emma Thomas", "Khavy Sangasy",
                                                                                                                                           "Phoebe Huey", "Emma Thomas", "Devon Scott-Davis", "Quincy Cechini",
                                                                                                                                           "Keila Flewell", "Zoe Masters", "Rebecca McArthur", "Zoe Masters",
                                                                                                                                           "Quincy Cechini", "Mililani Rosare", "Amanda Macey", "Elle Holland",
                                                                                                                                           "Quincy Cechini", "Rebecca McArthur", "Elle Holland", "Anna Peshenko",
                                                                                                                                           "Samantha Russell", "Khavy Sangasy", "Khavy Sangasy", "Anna Peshenko",
                                                                                                                                           "Gabriella Guagliardo", "Audrey Wallach", "Kimberly Dobie", "Keila Flewell",
                                                                                                                                           "Audrey Wallach", "Keila Flewell", "Kiersten Winter", "Anna Peshenko",
                                                                                                                                           "Mililani Rosare", "Audrey Wallach", "Drew Scheib", "Kai Louie",
                                                                                                                                           "John Sapp", "John Sapp", "Zachary Claus", "Brendan Kapp", "Drew Scheib",
                                                                                                                                           "Connor Shriver", "Gerald Porretta", "Braden Feasley", "Mitchell Gumbert",
                                                                                                                                           "Alex Pecze", "Theoren Heilman", "Connor Donoghue", "Flynn Lundeen",
                                                                                                                                           "Alex Pecze", "Ronan Gorman", "Connor Shriver", "Alex Pecze",
                                                                                                                                           "Aidan Daudier", "Michael Pacholarz", "Samuel Hart", "Connor Donoghue",
                                                                                                                                           "Jonathan Costa", "Mitchell Gumbert", "Edrick Nguyen", "Samuel Hart",
                                                                                                                                           "Michael Atanasoff", "Ryan Sweet", "Jonathan Costa", "Kyle Emmerling",
                                                                                                                                           "Edrick Nguyen", "Samuel Sokalzuk", "Brendan Kapp", "Connor Shriver",
                                                                                                                                           "Kyle Emmerling"), Age = c("SR", "FR", "JR", "FR", "SO", "JR",
                                                                                                                                                                      "SR", "SO", "JR", "SO", "SO", "SO", "FR", "SO", "SO", "SR", "SO",
                                                                                                                                                                      "JR", "SO", "FR", "JR", "FR", "JR", "JR", "JR", "FR", "JR", "FR",
                                                                                                                                                                      "FR", "SO", "FR", "SO", "SO", "FR", "SR", "FR", "SO", "SO", "FR",
                                                                                                                                                                      "FR", "SO", "JR", "SO", "FR", "SO", "SR", "JR", "SO", "JR", "SO",
                                                                                                                                                                      "FR", "SO", "SO", "FR", "SO", "JR", "SO", "FR", "SO", "JR", "JR",
                                                                                                                                                                      "FR", "FR", "SO", "JR", "JR", "JR", "FR", "FR", "JR", "FR", "JR"
                                                                                                                                           ), Date = c("12/3/2021", "10/9/2021", "11/20/2021", "10/9/2021",
                                                                                                                                                       "11/6/2021", "11/6/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "12/3/2021", "11/20/2021", "10/20/2021",
                                                                                                                                                       "10/9/2021", "12/3/2021", "11/6/2021", "12/3/2021", "12/3/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "11/6/2021", "12/3/2021", "10/20/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "12/3/2021", "10/23/2021", "12/3/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "12/3/2021", "10/16/2021", "12/3/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021", "11/20/2021",
                                                                                                                                                       "10/9/2021", "10/23/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "12/3/2021", "10/9/2021", "12/3/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "10/16/2021", "10/9/2021", "12/3/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                                                       "12/3/2021", "12/3/2021", "12/3/2021"), Meet = c("Don Richards 2021",
                                                                                                                                                                                                        "RPI vs RIT", "Ithaca vs RIT 2021", "RPI vs RIT", "Gannon vs. RIT",
                                                                                                                                                                                                        "Gannon vs. RIT", "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Ithaca vs RIT 2021", "RIT-Oswego", "RPI vs RIT", "Don Richards 2021",
                                                                                                                                                                                                        "Gannon vs. RIT", "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Gannon vs. RIT", "Don Richards 2021", "RIT-Oswego", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Clarkson vs RIT",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "RIT vs Alfred University", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Ithaca vs RIT 2021", "RPI vs RIT", "Clarkson vs RIT", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "RPI vs RIT", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "RIT vs Alfred University",
                                                                                                                                                                                                        "RPI vs RIT", "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                                                        "Don Richards 2021", "Don Richards 2021", "Don Richards 2021"
                                                                                                                                                       ), Event = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
                                                                                                                                                                              4L, 4L, 5L, 5L, 5L, 8L, 8L, 8L, 9L, 9L, 9L, 10L, 10L, 10L, 11L,
                                                                                                                                                                              11L, 11L, 12L, 12L, 12L, 13L, 13L, 13L, 14L, 14L, 14L, 20L, 20L,
                                                                                                                                                                              20L, 21L, 21L, 21L, 22L, 22L, 22L, 23L, 23L, 23L, 24L, 24L, 24L,
                                                                                                                                                                              27L, 27L, 27L, 28L, 28L, 28L, 29L, 29L, 29L, 30L, 30L, 30L, 31L,
                                                                                                                                                                              31L, 31L, 32L, 32L, 32L, 33L, 33L, 33L), .Label = c("Women 50 Free",
                                                                                                                                                                                                                                  "Women 100 Free", "Women 200 Free", "Women 500 Free", "Women 1000 Free",
                                                                                                                                                                                                                                  "Women 1650 Free", "Women 50 Back", "Women 100 Back", "Women 200 Back",
                                                                                                                                                                                                                                  "Women 100 Breast", "Women 200 Breast", "Women 100 Fly", "Women 200 Fly",
                                                                                                                                                                                                                                  "Women 200 IM", "Women 400 IM", "Women 1 Meter 6 Dives", "Women 3 Meter 6 Dives",
                                                                                                                                                                                                                                  "Women 1 Meter 11 Dives", "Women 3 Meter 11 Dives", "Men 50 Free",
                                                                                                                                                                                                                                  "Men 100 Free", "Men 200 Free", "Men 500 Free", "Men 1000 Free",
                                                                                                                                                                                                                                  "Men 1650 Free", "Men 50 Back", "Men 100 Back", "Men 200 Back",
                                                                                                                                                                                                                                  "Men 100 Breast", "Men 200 Breast", "Men 100 Fly", "Men 200 Fly",
                                                                                                                                                                                                                                  "Men 200 IM", "Men 400 IM", "Men 1 Meter 6 Dives", "Men 3 Meter 6 Dives",
                                                                                                                                                                                                                                  "Men 1 Meter 11 Dives", "Men 3 Meter 11 Dives"), class = "factor"),
                                Time = c(24.92, 25.01, 25.12, 54.08, 57.33, 54.73, 116.03,
                                         120.75, 121.02, 317.21, 323.88, 334.28, 694.44, 705.27, 683.6,
                                         61.04, 63.53, 60.83, 128.95, 136.17, 135.09, 67.16, 68.82,
                                         65.85, 141.7, 143.11, 167.12, 60.67, 62.2, 63.45, 139.63,
                                         144.96, 150.44, 130.53, 136.77, 139.66, 21.32, 22.04, 21.78,
                                         46.83, 49.03, 48.06, 102.04, 110.09, 106.03, 288.49, 301.63,
                                         286.97, 629.66, 671.81, 619.91, 52.14, 60.49, 54.13, 111.92,
                                         113.08, 119.98, 61.37, 62.77, 59.12, 134.51, 140.72, 133.59,
                                         52.57, 53.9, 52.1, 115.61, 115.77, 122.51, 113.57, 123.45,
                                         119.06)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA,
                                                                                                                                -72L))

  expect_equivalent(df_test, df_standard)

})
# testthat::test_file("tests/testthat/test-determine_entries.R")
