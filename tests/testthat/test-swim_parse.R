test_that("read_results_flag within swim_parse", {
  file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")

  expect_error(swim_parse(file), regexp = "Please run read_results on file prior to running swim_parse.")

})

test_that("places with parens '1)' etc", {

  skip_on_cran() # due to risk of external resources failing

  # build test df

  file <- "https://swimswam.com/wp-content/uploads/2018/08/2004-Division-I-NCAA-Championships-Women-results1.pdf"

  df <- file %>%
    read_results() %>%
    swim_parse()

  df_test <- df %>%
    filter(Name == "JOYCE, KARA LYNN")

  # build standard df

  df_standard <-
    structure(
      list(
        Place = c(1, 1),
        Name = c("JOYCE, KARA LYNN",
                 "JOYCE, KARA LYNN"),
        Age = c("FR", "FR"),
        Team = c("GEORGIA",
                 "GEORGIA"),
        Prelims_Time = c("24.21", "54.40"),
        Finals_Time = c("24.24",
                        "53.15"),
        DQ = c(0, 0),
        Exhibition = c(0, 0),
        Event = c(
          "Event 4 WOMEN's 50 Meter Freestyle",
          "Event 17 WOMEN's 100 Meter Freestyle"
        )
      ),
      row.names = c(NA,-2L),
      class = "data.frame"
    )

  # compare test and standard

  expect_equivalent(df_test, df_standard)

})


test_that("swim_parse works", {
  file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")

  df <- swim_parse(
    read_results(file)
    )

  expect_match(df[102, 2], "Lilly King") # for swim_parse post v0.6.0 because scratch lines are now included

})


test_that("swim_parse works para", {

  file <-
    system.file("extdata", "2018_jimi_flowers_PARA.pdf", package = "SwimmeR")

  df <- swim_parse(
    read_results(file)
  )

  expect_equivalent(
    unique(df$Para),
    c(
      "S3",
      "S6",
      "S7",
      "S8",
      "S9" ,
      "S10",
      "S11",
      "S12",
      "S14",
      NA,
      "SB7",
      "SB9",
      "SB12",
      "SB14",
      "S5",
      "SB3",
      "SB5",
      "SB6",
      "SB8",
      "SB11",
      "SM6",
      "SM7",
      "SM8",
      "SM9",
      "SM10",
      "SM11"
    )
  )

})

test_that("swim_parse_2 works 2", {
  file <-
    system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")

  df <- swim_parse(
    read_results(file)
  )

  expect_match(df$Finals_Time[257], # for swim_parse_2 because scratch lines are now included
               "2:01.78")
})

test_that("swim_parse_2 works 3", {

  skip_on_cran() # due to risk of external resources failing

  file <- "http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm"

  if(is_link_broken(file) == TRUE){
    warning("Link to external data is broken")
  } else {

    df <- swim_parse(
      read_results(file),
      typo = c("-1NORTH ROCKL"),
      replacement = c("1-NORTH ROCKL")
    )

    expect_equivalent(sum(df[,1], na.rm = TRUE), 16235) # works with swim_parse_2
  }

})

test_that("swim_parse works USMS", {
  file <- system.file("extdata", "11102019roc.pdf", package = "SwimmeR")
  expect_match(swim_parse(
    read_results(file)
  )[109, 6],
  "51.90")

})

test_that("swim_parse works USA", {
  file <- system.file("extdata", "jets08082019_067546.pdf", package = "SwimmeR")

  df <- file %>%
    read_results() %>%
    swim_parse()

  sum_df <- sum(df[1], na.rm = TRUE)

  expect_equivalent(sum_df,
  3109)
})

test_that("swim_parse works Brit ID", {
  file <- system.file("extdata", "1500m_mixed_090220_Brit_ID.pdf", package = "SwimmeR")

  df_test <- swim_parse(read_results(file), splits = TRUE, split_length = 50)

  expect_equivalent(dim(df_test), c(33, 41))

  expect_equal(sum(is.na(df_test)), 1, tolerance = 1e-2) # one missing split in row 31

})

test_that("swim_parse works list", {

  skip_on_cran() # due to time, risk of external resources failing

  # import standard
  # df_standard <- read.csv(system.file("extdata", "df_standard.csv", package = "SwimmeR"), stringsAsFactors = FALSE, colClasses=c("numeric", rep("character", 6), "numeric", "numeric", "character"))
  df_standard <- readRDS(url("https://github.com/gpilgrim2670/Pilgrim_Data/blob/master/SwimmeR%20Test%20Files/df_standard.rds?raw=true"))


  # import test files
  file_1 <- system.file("extdata", "jets08082019_067546.pdf", package = "SwimmeR")
  file_2 <- system.file("extdata", "11102019roc.pdf", package = "SwimmeR")
  file_3 <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  file_4 <- system.file("extdata", "s2-results.pdf", package = "SwimmeR")
  file_5 <- system.file("extdata", "ttsc10262018results-rev2.pdf", package = "SwimmeR") # https://www.teamunify.com/eznslsc/UserFiles/File/Meet-Results/2018-2019/ttsc10262018results-rev2.pdf # potential addition, lot of df_7 stuff
  url91 <- "http://www.section11swim.com/Results/GirlsHS/2016/League1/Single.htm" # numbers as grades still attached to schools - fixed
  url92 <- "http://www.section1swim.com/Results/BoysHS/2020/Sec1/Single.htm" # schools are NA - fixed
  url93 <- "http://www.section2swim.com/Results/BoysHS/2004/Sec2/A/Single.htm" # schools as SR
  # url94 <- "http://www.section6swim.com/Results/GirlsHS/2012/NFL/Single.htm" # schools named NT - not going to support
  url97 <- "http://www.section3swim.com/Results/BoysHS/2020/Sec3/BC/Single.htm" # events errors - fixed - events as Class B, Class C etc
  url98 <- "http://www.section5swim.com/Results/BoysHS/2013/HAC/Single.htm" # 'A' 'B' strings
  url101 <- "http://www.section5swim.com/Results/GirlsHS/2000/Sec5/B/Single.htm" # empty '' strings where 'A', 'B' would go for relays

  if(sum(sapply(c(url91, url92, url93, url97, url98, url101), is_link_broken)) > 0.9){
    warning("A link to external data is broken")
  }

  sources <- c(file_1,
               file_2,
               file_3,
               file_4,
               file_5,
               url91,
               url92,
               url93,
               # url94,
               url97,
               url98,
               url101)

  # helper function to apply read_results across list of links
  Read_Map <- function(links) {

    scrape_test_all <-
      purrr::map(links, read_results, node = "pre")

    names(scrape_test_all) <- links
    return(scrape_test_all)

  }

  # helper function to apply swim_parse across all files
  Parse_Map <- function(links) {

    all_results <-
      purrr::map(
        links,
        swim_parse,
        typo = c(
          "\n",
          # "Greece  Athena",
          "Newburgh Free  9",
          # "FAYETTEVILLE MAN  ",
          # "CICERO NORTH SYR  ",
          " - ",
          "Vineland  \\(Boy\\'s\\)",
          "\\(Kp\\)",
          "\\(Mc\\)",
          "\\(P",
          "  Psal",
          " Brian\\t A",
          "Williamsville E ",
          " B-AAB",
          "Section  X I",
          "Mexico  -B",
          "Nottingham  -A",
          # "Bronxville  High School",
          "A A",
          ",  CT",
          ",  MA",
          "-1NORTH ROCKL",
          # "QUEENSBURY  HIGH",
          # "Indiana  University",
          ", University of",
          "Sugrue_Neuendorf,",
          # "Swim\\s{2,}Club",
          # "Performance\\s{2,}Swim",
          # "Swimming\\s{2,}Club",
          "Stamford\\s{2,}American\\s{2,}Internationa",
          # "Uwcsea\\s{2,}Phoenix-ZZ",
          # "AquaTech\\s{2,}Swimming",
          # "Chinese\\s{2,}Swimming",
          # "Aquatic\\s{2,}Performance",
          # "SwimDolphia\\s{2}Aquatic School",
          "Young-Mandiak, Atticus F 11",
          "Molina Ayquipa, Santiago 12"
        ),

        replacement = c(
          "",
          # "Greece Athena",
          "Newburgh Free-9",
          # "FAYETTEVILLE MAN ",
          # "CICERO NORTH SYR ",
          "-",
          "Vineland",
          "",
          "",
          "",
          "-Psal",
          "Brian A",
          "Williamsville East ",
          "B-AAB",
          "Section XI",
          "Mexico",
          "Nottingham",
          # "Bronxville",
          "AA",
          "-CT",
          "-MA",
          "1-NORTH ROCKL",
          # "QUEENSBURY",
          # "Indiana University",
          "",
          "Neuendorf, Sugrue",
          # "Swim Club",
          # "Performance Swim",
          # "Swimming Club",
          "Stamford American International",
          # "Uwcsea Phoenix-ZZ",
          # "AquaTech Swimming",
          # "Chinese Swimming",
          # "Aquatic Performance",
          # "SwimDolphia Aquatic School",
          "Young-Mandiak, Atticus F   11",
          "Molina Ayquipa, Santiago   12"
        )
      )

    return(all_results)

  }

  # get test data to compare with standard
  df_test <- Read_Map(sources)
  df_test <- Parse_Map(df_test)
  # df_test_2 <- dplyr::bind_rows(df_test, .id = "source")
  df_test <- dplyr::bind_rows(df_test, .id = "source") %>%
    dplyr::select(-source, -Reaction_Time)

  # df <- dplyr::anti_join(df_standard, df_test)

  # to regenerate df_standard if df_test is more correct
  # readr::write_rds(df_test, "~/Pilgrim_Data/SwimmeR Test Files/df_standard.rds")

  # compare standard to test
  expect_equivalent(df_standard,
                    df_test)
})

# testthat::test_file("tests/testthat/test-swim_parse.R")
