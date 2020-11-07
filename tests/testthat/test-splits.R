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
  )
  df <- df %>%
    dplyr::mutate(F_sec = sec_format(Finals_Time)) %>% # finals time in seconds
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), ~sec_format(.x))) %>% # all splits in seconds to account for splits over 59.99
    dplyr::mutate(total = dplyr::select(., Split_50:length(df)) %>% rowSums(na.rm = TRUE)) %>% # total up splits
    dplyr::mutate(not_matching = case_when(round(F_sec - total, 2) == 0 ~ FALSE, # does total match finals time?
                                    round(F_sec - total, 2) != 0 ~ TRUE))

  match_sum <- sum(df$not_matching, na.rm = TRUE)

  expect_equivalent(match_sum, 0)

  # df_splits_sum <-
  #   sum(df[names(df) %in% names(df)[grep("^Split", names(df))]], na.rm = TRUE)

  # expect_equivalent(df_splits_sum, 54448.73)

})


test_that("NYS results, multiple lines of splits with different lengths, has parenthesis", {
  file <- "http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm"
  df <- swim_parse(
    read_results(file, node = "pre"),
    typo = c("-1NORTH ROCKL"),
    replacement = c("1-NORTH ROCKL"),
    splits = TRUE
  )

  df <- df %>%
    filter(DQ != 1,
           stringr::str_detect(Event, "Diving") == FALSE, # diving does not have splits
           # stringr::str_detect(Event, "Relay") == FALSE, # relays now do have splits
           stringr::str_detect(Event, "\\s50\\s") == FALSE) %>% # 50s do not have splits
    dplyr::mutate(F_sec = sec_format(Finals_Time)) %>% # finals time in seconds
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), ~ sec_format(.x))) %>% # all splits in seconds to account for splits over 59.99
    dplyr::mutate(total = dplyr::select(., Split_50:length(df)) %>% rowSums(na.rm = TRUE)) %>% # total up splits
    dplyr::mutate(not_matching = dplyr::case_when(round(F_sec - total, 2) == 0 ~ FALSE, # does total match finals time?
                                    round(F_sec - total, 2) != 0 ~ TRUE))

  match_sum <- sum(df$not_matching, na.rm = TRUE) # should be 75.  Three swimmers in the 200 IM did not record splits, 20 200MRs did not have splits reported and the 32 400 free relays report splits by swimmer so the total does not sum to the total time for 75 errors

  expect_equivalent(match_sum, 75)

})

test_that("USA results, splits don't have parenthesis, some splits longer than 59.99", {
  file <- system.file("extdata", "jets08082019_067546.pdf", package = "SwimmeR")
  df <- swim_parse(
    read_results(file, node = "pre"),
    splits = TRUE
  )

  df <- df %>%
    filter(DQ != 1,
           stringr::str_detect(Event, "Relay") == FALSE, # relays do not have splits
           stringr::str_detect(Event, "\\s50\\s") == FALSE) %>% # 50s do not have splits
    dplyr::mutate(F_sec = sec_format(Finals_Time)) %>% # finals time in seconds
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), ~ sec_format(.x))) %>% # all splits in seconds to account for splits over 59.99
    dplyr::mutate(total = dplyr::select(., Split_50:length(df)) %>% rowSums(na.rm = TRUE)) %>% # total up splits
    dplyr::mutate(not_matching = dplyr::case_when(round(F_sec - total, 2) == 0 ~ FALSE, # does total match finals time?
                                                  round(F_sec - total, 2) != 0 ~ TRUE))

  match_sum <- sum(df$not_matching, na.rm = TRUE) # should be 10 because 10 swimmers finished legally but did not record splits

  expect_equivalent(match_sum, 10)

})

# test_file("tests/testthat/test-splits.R")
