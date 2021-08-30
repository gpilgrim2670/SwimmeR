test_that("Singapore results, splits in parenthesis", {

  file <- system.file("extdata", "s2-results.pdf", package = "SwimmeR")

  df <- swim_parse(
    read_results(file),
    avoid = c("MR:"),
    typo = c(
      "Swim\\s{2,}Club",
      "Performance\\s{2,}Swim",
      "Swimming\\s{2,}Club",
      "Stamford\\s{2,}American\\s{2,}Internationa",
      "Uwcsea\\s{2,}Phoenix-ZZ",
      "AquaTech\\s{2,}Swimming",
      "Chinese\\s{2,}Swimming",
      "Aquatic\\s{2,}Performance",
      "SwimDolphia\\s{2}Aquatic School"
    ),
    replacement = c(
      "Swim Club",
      "Performance Swim",
      "Swimming Club",
      "Stamford American International",
      "Uwcsea Phoenix-ZZ",
      "AquaTech Swimming",
      "Chinese Swimming",
      "Aquatic Performance",
      "SwimDolphia Aquatic School"
    ),
    splits = TRUE
  ) %>%
    splits_reform()

  match_sum <- sum(df$not_matching, na.rm = TRUE) # should be zero

  expect_equivalent(match_sum, 0)

})


test_that("NYS results, multiple lines of splits with different lengths, has parenthesis", {

  skip_on_cran() # due to risk of external resources failing

  file <- "http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm"

  if(is_link_broken(file) == TRUE){
    warning("Link to external data is broken")
  } else {
  df <- swim_parse(
    read_results(file),
    typo = c("-1NORTH ROCKL"),
    replacement = c("1-NORTH ROCKL"),
    splits = TRUE
  ) %>%
    splits_reform()

  match_sum <- sum(df$not_matching, na.rm = TRUE)
  # should be 75.  Three swimmers in the 200 IM did not record splits,
  # 20 200MRs did not have splits reported and the 32 400 free relays report splits by swimmer
  # so the total does not sum to the total time for 75 errors

  expect_equivalent(match_sum, 75)
  }

})

test_that("USA Swimming results, splits don't have parenthesis, some splits longer than 59.99", {

  file <- system.file("extdata", "jets08082019_067546.pdf", package = "SwimmeR")
  df <- file %>%
    read_results() %>%
    swim_parse(splits = TRUE) %>%
    splits_reform()

  match_sum <- sum(df$not_matching, na.rm = TRUE) # should be 11 because 10 swimmers and finished legally but did not record splits and 1 relay was DQ'd without splits

  expect_equivalent(match_sum, 11)

})

test_that("ISL results", {

  skip_on_cran() # due to risk of external resources failing

  file <- "https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/ISL/Season_1_2019/ISL_19102019_Lewisville_Day_1.pdf"

  if(is_link_broken(file) == TRUE){
    warning("Link to external data is broken")
  } else {
    df <- swim_parse_ISL(read_results(file), splits = TRUE) %>%
      # dplyr::rename("Finals_Time" = Time) %>%
      splits_reform()

  match_sum <- sum(df$not_matching, na.rm = TRUE) # should be 24 due to 24 relays

  expect_equivalent(match_sum, 24)
  }

})

test_that("multiple splits below 59.99 in parens and out", {

  skip_on_cran() # due to risk of external resources failing

  file <- "https://data.ohiostatebuckeyes.com/livestats/m-swim/210302F001.htm"

  if(is_link_broken(file) == TRUE){
    warning("Link to external data is broken")
  } else {
    df <-
      swim_parse(
        read_results(file),
        splits = TRUE,
        relay_swimmers = TRUE,
        split_length = 25
      ) %>%
      dplyr::mutate(dplyr::across(Split_25:Split_200, as.numeric)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        total = sum(Split_50, Split_100, Split_150, Split_200),
        F_sec = sec_format(Finals_Time),
        not_matching = dplyr::case_when(round(F_sec - total, 2) == 0 ~ FALSE, # does total match finals time?
                                        round(F_sec - total, 2) != 0 ~ TRUE)
      )

    match_sum <- sum(df$not_matching, na.rm = TRUE) # should be 0 because they all should be correct

    expect_equivalent(match_sum, 0)
  }

})

test_that("correct split distances", {

  # goal here is to convert the Men's 50 Free in df_test to splitting by 25 rather than by 50

  df_standard <-
    data.frame(
      Name = c("Lilly King", "Caeleb Dressel", "Mallory Comerford"),
      Event = as.factor(
        c(
          "Women 100 Meter Breaststroke",
          "Men 50 Yard Freestyle",
          "Women 200 Yard Freestyle"
        )
      ),
      Split_50 = c(NA, "8.48", NA),
      Split_50 = c("29.80", "9.15", "23.90"),
      Split_100 = c("34.33", NA, "25.52"),
      Split_150 = c(NA, NA, "25.13"),
      Split_200 = c(NA, NA, "25.25"),
      stringsAsFactors = FALSE
    )

  df_test <- data.frame(
    Name = c("Lilly King", "Caeleb Dressel", "Mallory Comerford"),
    Event = c(
      "Women 100 Meter Breaststroke",
      "Men 50 Yard Freestyle",
      "Women 200 Yard Freestyle"
    ),
    Split_50 = c("29.80", "8.48", "23.90"),
    Split_100 = c("34.33", "9.15", "25.52"),
    Split_150 = c(NA, NA, "25.13"),
    Split_200 = c(NA, NA, "25.25"),
    stringsAsFactors = FALSE
  )

  df_test <- df_test %>% correct_split_distance(new_split_length = 25,
                                      events = c("Men 50 Yard Freestyle"))

    expect_equivalent(df_test, df_standard)


})

# testthat::test_file("tests/testthat/test-splits.R")
