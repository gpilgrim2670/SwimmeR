test_that("Singapore results", {
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
  df_splits_sum <-
    sum(df[names(df) %in% names(df)[grep("^\\d", names(df))]], na.rm = TRUE)

  expect_equivalent(df_splits_sum, 54448.73)

})


test_that("NYS results, multiple lines of splits with different lengths", {
  file <- "http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm"
  df <- swim_parse(
    read_results(file, node = "pre"),
    typo = c("-1NORTH ROCKL"),
    replacement = c("1-NORTH ROCKL"),
    splits = TRUE
  )

  df_splits_sum <-
    sum(df[names(df) %in% names(df)[grep("^\\d", names(df))]], na.rm = TRUE)

  expect_equivalent(df_splits_sum, 53290.22)

})
