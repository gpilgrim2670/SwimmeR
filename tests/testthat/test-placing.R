# testthat::test_file("tests/testthat/test-placing.R")

test_that("place", {
  df <- data.frame(
    Place = c(1, 1, 1, 1, 1, 1),
    Name = c("Sally Swimfast", "Bonnie Bubbles", "Kylie Kicker", "Riley Ripit", "Nathan Nosplash", "Tim Tuck"),
    Team = c("KVAC", "UBAM", "MERC", "Upstate Diving", "Nickel City Splash", "Finger Lakes Diving"),
    Event = c(rep("Women 200 Freestyle", 3), rep("Boys 1 mtr Diving", 3)),
    Prelims = c("2:00.00", "1:59.99", "2:01.50", "300.00", "305.00", "200.00"),
    Finals = c("1:58.00", "1:59.50", "2:00.50", "310.00", "307.00", "220.00"),
    Meet = c("Summer 2021", "Fall 2020", "Champs 2020","Regional Champs 2021", "Other Regional Champs 2021", "City Champs 2021" ))


  #### promise default ####
  df_promise_default_test <- df %>%
    place() %>%
    arrange(Event)

  df_promise_default_standard <-
    structure(list(Place = c(1L, 2L, 3L, 1L, 2L, 3L), Name = c("Riley Ripit",
                                                               "Nathan Nosplash", "Tim Tuck", "Sally Swimfast", "Bonnie Bubbles",
                                                               "Kylie Kicker"), Team = c("Upstate Diving", "Nickel City Splash",
                                                                                         "Finger Lakes Diving", "KVAC", "UBAM", "MERC"), Event = c("Boys 1 mtr Diving",
                                                                                                                                                   "Boys 1 mtr Diving", "Boys 1 mtr Diving", "Women 200 Freestyle",
                                                                                                                                                   "Women 200 Freestyle", "Women 200 Freestyle"), Prelims = c("300.00",
                                                                                                                                                                                                              "305.00", "200.00", "2:00.00", "1:59.99", "2:01.50"), Finals = c("310.00",
                                                                                                                                                                                                                                                                               "307.00", "220.00", "1:58.00", "1:59.50", "2:00.50"), Meet = c("Regional Champs 2021",
                                                                                                                                                                                                                                                                                                                                              "Other Regional Champs 2021", "City Champs 2021", "Summer 2021",
                                                                                                                                                                                                                                                                                                                                              "Fall 2020", "Champs 2020")), class = c("grouped_df", "tbl_df",
                                                                                                                                                                                                                                                                                                                                                                                      "tbl", "data.frame"), row.names = c(NA, -6L), groups = structure(list(
                                                                                                                                                                                                                                                                                                                                                                                        Event = c("Boys 1 mtr Diving", "Women 200 Freestyle"), .rows = structure(list(
                                                                                                                                                                                                                                                                                                                                                                                          1:3, 4:6), ptype = integer(0), class = c("vctrs_list_of",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "vctrs_vctr", "list"))), row.names = c(NA, -2L), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "tbl", "data.frame"), .drop = TRUE))
  expect_equivalent(df_promise_default_test, df_promise_default_standard)

  #### promise non-default ####
  df_promise_nondefault_test <- df %>%
    place(result_col = Prelims) %>%
    arrange(Event)

  df_promise_nondefault_standard <-
    structure(list(Place = c(1L, 2L, 3L, 1L, 2L, 3L), Name = c("Nathan Nosplash",
                                                               "Riley Ripit", "Tim Tuck", "Bonnie Bubbles", "Sally Swimfast",
                                                               "Kylie Kicker"), Team = c("Nickel City Splash", "Upstate Diving",
                                                                                         "Finger Lakes Diving", "UBAM", "KVAC", "MERC"), Event = c("Boys 1 mtr Diving",
                                                                                                                                                   "Boys 1 mtr Diving", "Boys 1 mtr Diving", "Women 200 Freestyle",
                                                                                                                                                   "Women 200 Freestyle", "Women 200 Freestyle"), Prelims = c("305.00",
                                                                                                                                                                                                              "300.00", "200.00", "1:59.99", "2:00.00", "2:01.50"), Finals = c("307.00",
                                                                                                                                                                                                                                                                               "310.00", "220.00", "1:59.50", "1:58.00", "2:00.50"), Meet = c("Other Regional Champs 2021",
                                                                                                                                                                                                                                                                                                                                              "Regional Champs 2021", "City Champs 2021", "Fall 2020", "Summer 2021",
                                                                                                                                                                                                                                                                                                                                              "Champs 2020")), class = c("grouped_df", "tbl_df", "tbl", "data.frame"
                                                                                                                                                                                                                                                                                                                                              ), row.names = c(NA, -6L), groups = structure(list(Event = c("Boys 1 mtr Diving",
                                                                                                                                                                                                                                                                                                                                                                                                           "Women 200 Freestyle"), .rows = structure(list(1:3, 4:6), ptype = integer(0), class = c("vctrs_list_of",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "vctrs_vctr", "list"))), row.names = c(NA, -2L), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "tbl", "data.frame"), .drop = TRUE))

  expect_equivalent(df_promise_nondefault_test,
                    df_promise_nondefault_standard)


  #### string non-default ####
  df_string_nondefault_test <- df %>%
    place(result_col = "Prelims") %>%
    arrange(Event)

  expect_equivalent(df_string_nondefault_test, df_promise_nondefault_standard)


})

test_that("swim_place", {
  df <- data.frame(
    Place = c(1, 1, 1),
    Name = c("Sally Swimfast", "Bonnie Bubbles", "Kylie Kicker"),
    Team = c("KVAC", "UBAM", "MERC"),
    Event = rep("Womens 200 Freestyle", 3),
    Prelims = c("2:00.00", "1:59.99", "2:01.50"),
    Finals = c("1:58.00", "1:59.50", "2:00.50"),
    Meet = c("Summer 2021", "Fall 2020", "Champs 2020")
  )


  #### promise default ####
  df_promise_default_test <- df %>%
    swim_place()

  df_promise_default_standard <-
    structure(
      list(
        Place = 1:3,
        Name = c("Sally Swimfast", "Bonnie Bubbles",
                 "Kylie Kicker"),
        Team = c("KVAC", "UBAM", "MERC"),
        Event = c(
          "Womens 200 Freestyle",
          "Womens 200 Freestyle",
          "Womens 200 Freestyle"
        ),
        Prelims = c("2:00.00",
                         "1:59.99", "2:01.50"),
        Finals = c("1:58.00", "1:59.50",
                        "2:00.50"),
        Meet = c("Summer 2021", "Fall 2020", "Champs 2020")
      ),
      class = c("grouped_df", "tbl_df", "tbl", "data.frame"),
      row.names = c(NA,-3L),
      groups = structure(
        list(
          Event = "Womens 200 Freestyle",
          .rows = structure(
            list(1:3),
            ptype = integer(0),
            class = c("vctrs_list_of",
                      "vctrs_vctr", "list")
          )
        ),
        row.names = c(NA,-1L),
        class = c("tbl_df",
                  "tbl", "data.frame"),
        .drop = TRUE
      )
    )
  expect_equivalent(df_promise_default_test, df_promise_default_standard)

  #### promise non-default ####
  df_promise_nondefault_test <- df %>%
    swim_place(time_col = Prelims)

  df_promise_nondefault_standard <-
    structure(
      list(
        Place = 1:3,
        Name = c("Bonnie Bubbles", "Sally Swimfast",
                 "Kylie Kicker"),
        Team = c("UBAM", "KVAC", "MERC"),
        Event = c(
          "Womens 200 Freestyle",
          "Womens 200 Freestyle",
          "Womens 200 Freestyle"
        ),
        Prelims = c("1:59.99",
                         "2:00.00", "2:01.50"),
        Finals = c("1:59.50", "1:58.00",
                        "2:00.50"),
        Meet = c("Fall 2020", "Summer 2021", "Champs 2020")
      ),
      class = c("grouped_df", "tbl_df", "tbl", "data.frame"),
      row.names = c(NA,-3L),
      groups = structure(
        list(
          Event = "Womens 200 Freestyle",
          .rows = structure(
            list(1:3),
            ptype = integer(0),
            class = c("vctrs_list_of",
                      "vctrs_vctr", "list")
          )
        ),
        row.names = c(NA,-1L),
        class = c("tbl_df",
                  "tbl", "data.frame"),
        .drop = TRUE
      )
    )
  expect_equivalent(df_promise_nondefault_test,
                    df_promise_nondefault_standard)


  #### string non-default ####
  df_string_nondefault_test <- df %>%
    swim_place(time_col = "Prelims")

  df_string_nondefault_standard <-
    structure(
      list(
        Place = 1:3,
        Name = c("Bonnie Bubbles", "Sally Swimfast",
                 "Kylie Kicker"),
        Team = c("UBAM", "KVAC", "MERC"),
        Event = c(
          "Womens 200 Freestyle",
          "Womens 200 Freestyle",
          "Womens 200 Freestyle"
        ),
        Prelims = c("1:59.99",
                         "2:00.00", "2:01.50"),
        Finals = c("1:59.50", "1:58.00",
                        "2:00.50"),
        Meet = c("Fall 2020", "Summer 2021", "Champs 2020")
      ),
      class = c("grouped_df", "tbl_df", "tbl", "data.frame"),
      row.names = c(NA,-3L),
      groups = structure(
        list(
          Event = "Womens 200 Freestyle",
          .rows = structure(
            list(1:3),
            ptype = integer(0),
            class = c("vctrs_list_of",
                      "vctrs_vctr", "list")
          )
        ),
        row.names = c(NA,-1L),
        class = c("tbl_df",
                  "tbl", "data.frame"),
        .drop = TRUE
      )
    )
  expect_equivalent(df_string_nondefault_test, df_string_nondefault_standard)
})

#### diving ####
test_that("dive_place", {
  df <- data.frame(
    Place = c(1, 1, 1),
    Name = c("Riley Ripit", "Tanya Tuck", "Irene Inward"),
    Team = c("Upstate", "Finger Lakes", "Nickel City"),
    Event = rep("1m Diving", 3),
    Prelims = c("400", "410", "350"),
    Finals = c("410", "370", "360"),
    Meet = c("Summer 2021", "Fall 2020", "Champs 2020")
  )


  #### promise default ####
  df_promise_default_test <- df %>%
    dive_place()

  df_promise_default_standard <-
    structure(list(Place = 1:3, Name = c("Riley Ripit", "Tanya Tuck",
                                         "Irene Inward"), Team = c("Upstate", "Finger Lakes", "Nickel City"
                                         ), Event = c("1m Diving", "1m Diving", "1m Diving"), Prelims = c("400",
                                                                                                               "410", "350"), Finals = c("410", "370", "360"), Meet = c("Summer 2021",
                                                                                                                                                                             "Fall 2020", "Champs 2020")), class = c("grouped_df", "tbl_df",
                                                                                                                                                                                                                     "tbl", "data.frame"), row.names = c(NA, -3L), groups = structure(list(
                                                                                                                                                                                                                       Event = "1m Diving", .rows = structure(list(1:3), ptype = integer(0), class = c("vctrs_list_of",
                                                                                                                                                                                                                                                                                                       "vctrs_vctr", "list"))), row.names = c(NA, -1L), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                                                  "tbl", "data.frame"), .drop = TRUE))
  expect_equivalent(df_promise_default_test, df_promise_default_standard)

  #### promise non-default ####
  df_promise_nondefault_test <- df %>%
    dive_place(score_col = Prelims)

  df_promise_nondefault_standard <-
    structure(list(Place = 1:3, Name = c("Tanya Tuck", "Riley Ripit",
                                         "Irene Inward"), Team = c("Finger Lakes", "Upstate", "Nickel City"
                                         ), Event = c("1m Diving", "1m Diving", "1m Diving"), Prelims = c("410",
                                                                                                               "400", "350"), Finals = c("370", "410", "360"), Meet = c("Fall 2020",
                                                                                                                                                                             "Summer 2021", "Champs 2020")), class = c("grouped_df", "tbl_df",
                                                                                                                                                                                                                       "tbl", "data.frame"), row.names = c(NA, -3L), groups = structure(list(
                                                                                                                                                                                                                         Event = "1m Diving", .rows = structure(list(1:3), ptype = integer(0), class = c("vctrs_list_of",
                                                                                                                                                                                                                                                                                                         "vctrs_vctr", "list"))), row.names = c(NA, -1L), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                                                    "tbl", "data.frame"), .drop = TRUE))

  expect_equivalent(df_promise_nondefault_test,
                    df_promise_nondefault_standard)


  #### string non-default ####
  df_string_nondefault_test <- df %>%
    dive_place(score_col = "Prelims")

  df_string_nondefault_standard <-
    structure(list(Place = 1:3, Name = c("Tanya Tuck", "Riley Ripit",
                                         "Irene Inward"), Team = c("Finger Lakes", "Upstate", "Nickel City"
                                         ), Event = c("1m Diving", "1m Diving", "1m Diving"), Prelims = c("410",
                                                                                                               "400", "350"), Finals = c("370", "410", "360"), Meet = c("Fall 2020",
                                                                                                                                                                             "Summer 2021", "Champs 2020")), class = c("grouped_df", "tbl_df",
                                                                                                                                                                                                                       "tbl", "data.frame"), row.names = c(NA, -3L), groups = structure(list(
                                                                                                                                                                                                                         Event = "1m Diving", .rows = structure(list(1:3), ptype = integer(0), class = c("vctrs_list_of",
                                                                                                                                                                                                                                                                                                         "vctrs_vctr", "list"))), row.names = c(NA, -1L), class = c("tbl_df",
                                                                                                                                                                                                                                                                                                                                                                    "tbl", "data.frame"), .drop = TRUE))

  expect_equivalent(df_string_nondefault_test, df_string_nondefault_standard)

})
