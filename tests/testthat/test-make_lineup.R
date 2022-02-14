# testthat::test_file("tests/testthat/test-make_lineup.R")

#### RIT vs IC ####
test_that("determine entries RIT vs IC", {

  skip_on_cran()

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
    make_lineup(
      df = RIT_TopTimes_2021,
      op_df = IC,
      point_values = "ncaa_six_lane",
      result_col = "Finals",
      events = NULL
    )

  df_standard <-
    structure(list(Rank = c(1, 2, 3, 3, 4, 5, 1, 3, 4, 1, 2, 3, 1,
                            2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 2, 3, 4, 1, 2, 3, 1, 2, 4, 1,
                            3, 5, 2, 3, 5, 1, 2, 4, 1, 2, 3, 1, 2, 4, 1, 2, 3, 1, 2, 4, 1,
                            3, 4, 1, 2, 9, 2, 5, 6, 1, 4, 7, 1, 2, 3, 1, 2, 3), Finals = c("10:15.20",
                                                                                           "10:19.91", "10:29.66", "11:34.44", "11:45.27", "11:48.76", "1:42.04",
                                                                                           "1:45.71", "1:46.03", "1:56.03", "2:00.75", "2:01.02", "51.89",
                                                                                           "52.14", "54.13", "59.34", "1:00.35", "1:00.83", "56.98", "58.87",
                                                                                           "59.12", "1:05.85", "1:07.16", "1:08.82", "1:55.61", "1:55.77",
                                                                                           "2:02.51", "2:19.63", "2:24.96", "2:30.44", "21.32", "21.46",
                                                                                           "21.79", "24.92", "25.12", "25.39", "46.83", "47.50", "48.59",
                                                                                           "53.60", "54.08", "55.30", "1:51.92", "1:53.08", "1:59.98", "2:08.95",
                                                                                           "2:15.09", "2:16.17", "2:02.03", "2:11.21", "2:13.59", "2:21.70",
                                                                                           "2:23.11", "2:47.12", "4:45.89", "4:48.49", "4:49.73", "5:17.21",
                                                                                           "5:23.88", "6:10.05", "51.85", "52.57", "53.09", "1:00.67", "1:02.20",
                                                                                           "1:05.12", "1:53.57", "1:58.33", "1:58.46", "2:07.59", "2:10.53",
                                                                                           "2:16.77"), Name = c("Samuel Sokalzuk", "Flynn Lundeen", "Theoren Heilman",
                                                                                                                "Rebecca McArthur", "Zoe Masters", "Devon Scott-Davis", "Drew Scheib",
                                                                                                                "Michael Pacholarz", "Gerald Porretta", "Phoebe Huey", "Emma Thomas",
                                                                                                                "Devon Scott-Davis", "John Sapp", "Alex Pecze", "Connor Shriver",
                                                                                                                "Quincy Cechini", "Kimberly Dobie", "Elle Holland", "Brendan Kapp",
                                                                                                                "Matthew Ciminelli", "Jonathan Costa", "Khavy Sangasy", "Anna Peshenko",
                                                                                                                "Samantha Russell", "Kyle Emmerling", "Edrick Nguyen", "Samuel Sokalzuk",
                                                                                                                "Audrey Wallach", "Keila Flewell", "Kiersten Winter", "Drew Scheib",
                                                                                                                "Michael Atanasoff", "Jonathan Costa", "Phoebe Huey", "Sydney Burgard",
                                                                                                                "Elle Holland", "John Sapp", "Michael Atanasoff", "Kai Louie",
                                                                                                                "Phoebe Huey", "Kimberly Dobie", "Samantha Russell", "Alex Pecze",
                                                                                                                "Aidan Daudier", "Michael Pacholarz", "Quincy Cechini", "Elle Holland",
                                                                                                                "Rebecca McArthur", "Brendan Kapp", "Benjamin Sippel", "Samuel Hart",
                                                                                                                "Khavy Sangasy", "Anna Peshenko", "Gabriella Guagliardo", "Aidan Daudier",
                                                                                                                "Braden Feasley", "Gerald Porretta", "Quincy Cechini", "Keila Flewell",
                                                                                                                "Korey Hans", "John Sapp", "Michael Atanasoff", "Kai Louie",
                                                                                                                "Audrey Wallach", "Kimberly Dobie", "Korey Hans", "Brendan Kapp",
                                                                                                                "Edrick Nguyen", "Benjamin Sippel", "Khavy Sangasy", "Anna Peshenko",
                                                                                                                "Mililani Rosare"), Age = c("FR", "FR", "JR", "FR", "SO", "JR",
                                                                                                                                            "SO", "SO", "SO", "SR", "SO", "JR", "FR", "SO", "FR", "SO", "FR",
                                                                                                                                            "JR", "JR", "JR", "JR", "JR", "FR", "JR", "JR", "FR", "FR", "FR",
                                                                                                                                            "SO", "SO", "SO", "SO", "JR", "SR", "JR", "JR", "FR", "SO", "SO",
                                                                                                                                            "SR", "FR", "JR", "SO", "JR", "SO", "SO", "JR", "FR", "JR", "FR",
                                                                                                                                            "FR", "JR", "FR", "JR", "JR", "SR", "SO", "SO", "SO", "SO", "FR",
                                                                                                                                            "SO", "SO", "FR", "FR", "SO", "JR", "FR", "FR", "JR", "FR", "SR"
                                                                                                                ), Date = c("11/6/2021", "10/23/2021", "11/20/2021", "11/20/2021",
                                                                                                                            "10/20/2021", "11/6/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                            "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                            "12/3/2021", "12/3/2021", "11/20/2021", "12/3/2021", "12/3/2021",
                                                                                                                            "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                            "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                            "10/23/2021", "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                            "11/20/2021", "12/3/2021", "12/3/2021", "12/3/2021", "11/20/2021",
                                                                                                                            "12/3/2021", "10/9/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                            "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                            "12/3/2021", "10/9/2021", "12/3/2021", "12/3/2021", "11/6/2021",
                                                                                                                            "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                            "11/6/2021", "12/3/2021", "12/3/2021", "10/9/2021", "12/3/2021",
                                                                                                                            "10/20/2021", "12/3/2021", "12/3/2021", "12/3/2021", "12/3/2021",
                                                                                                                            "12/3/2021", "12/3/2021", "12/3/2021"), Meet = c("Gannon vs. RIT",
                                                                                                                                                                             "Clarkson vs RIT", "Ithaca vs RIT 2021", "Ithaca vs RIT 2021",
                                                                                                                                                                             "RIT-Oswego", "Gannon vs. RIT", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Ithaca vs RIT 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Clarkson vs RIT", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Ithaca vs RIT 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Ithaca vs RIT 2021", "Don Richards 2021", "RPI vs RIT", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "RPI vs RIT", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Gannon vs. RIT", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Gannon vs. RIT", "Don Richards 2021", "Don Richards 2021", "RPI vs RIT",
                                                                                                                                                                             "Don Richards 2021", "RIT-Oswego", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021", "Don Richards 2021",
                                                                                                                                                                             "Don Richards 2021", "Don Richards 2021"), Event = structure(c(1L,
                                                                                                                                                                                                                                            1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L,
                                                                                                                                                                                                                                            6L, 7L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L, 10L, 10L, 10L, 11L, 11L,
                                                                                                                                                                                                                                            11L, 12L, 12L, 12L, 13L, 13L, 13L, 14L, 14L, 14L, 15L, 15L, 15L,
                                                                                                                                                                                                                                            16L, 16L, 16L, 17L, 17L, 17L, 18L, 18L, 18L, 19L, 19L, 19L, 20L,
                                                                                                                                                                                                                                            20L, 20L, 21L, 21L, 21L, 22L, 22L, 22L, 23L, 23L, 23L, 24L, 24L,
                                                                                                                                                                                                                                            24L), .Label = c("Men 1000 Free", "Women 1000 Free", "Men 200 Free",
                                                                                                                                                                                                                                                             "Women 200 Free", "Men 100 Back", "Women 100 Back", "Men 100 Breast",
                                                                                                                                                                                                                                                             "Women 100 Breast", "Men 200 Fly", "Women 200 Fly", "Men 50 Free",
                                                                                                                                                                                                                                                             "Women 50 Free", "Men 100 Free", "Women 100 Free", "Men 200 Back",
                                                                                                                                                                                                                                                             "Women 200 Back", "Men 200 Breast", "Women 200 Breast", "Men 500 Free",
                                                                                                                                                                                                                                                             "Women 500 Free", "Men 100 Fly", "Women 100 Fly", "Men 200 IM",
                                                                                                                                                                                                                                                             "Women 200 IM"), class = "factor")), row.names = c(NA, -72L), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                     "tbl", "data.frame"))


    expect_equivalent(df_test, df_standard)

})



#### AU New vs AU Old ####

test_that("determine entries AU_Old vs AU_New", {

  skip_on_cran()

  OldAU <- readRDS(url("https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/SwimmeR%20Test%20Files/Old_AU.rds"))
  NewAU <- readRDS(url("https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/SwimmeR%20Test%20Files/AU_TopTimes_2021.rds"))
  df_standard <- readRDS(url("https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/SwimmeR%20Test%20Files/NewAUvsOldAU_Standard.rds"))

  df_test <- make_lineup(df = NewAU,
                           op_df = OldAU,
                           point_values = "ncaa_six_lane",
                           result_col = "Finals",
                           events = NULL,
                           max_entries = NULL)

  expect_equivalent(df_test, df_standard)


})
