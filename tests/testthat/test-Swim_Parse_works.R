
test_that("swim_parse works", {
  file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  expect_match(swim_parse(
    read_results(file),
    typo =  c("\n", "Indiana  University", ", University of"),
    replacement = c("\n", "Indiana University", "")
  )[100, 2],
  "Lilly King")

})

test_that("swim_parse works 2", {
  file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  expect_match(swim_parse(
    read_results(file),
    typo =  c("\n", "Indiana  University", ", University of"),

    replacement = c("\n", "Indiana University", "")
  )[252, 6],
  "2:01.78")

})

test_that("swim_parse works 3", {

  file <- "http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm"

  if(is_link_broken(file) == TRUE){
    warning("Link to external data is broken")
  } else {

  expect_equivalent(sum(swim_parse(
    read_results(
      file
    ),
    typo = c("-1NORTH ROCKL", "\\s\\d{1,2}\\s{2,}"),
    replacement = c("1-NORTH ROCKL", "  ")
  )[,1], na.rm = TRUE),
  16235)
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
  expect_equivalent(sum(swim_parse(
    read_results(file)
  )[1], na.rm = TRUE),
  3091)
})

test_that("swim_parse works list", {

  # import standard
  # df_standard <- read.csv(system.file("extdata", "df_standard.csv", package = "SwimmeR"), stringsAsFactors = FALSE, colClasses=c("numeric", rep("character", 6), "numeric", "numeric", "character"))
  df_standard <- results <- readRDS(system.file("extdata", "df_standard.rds", package = "SwimmeR"))


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
      purrr::map(links, swim_parse, typo = c("\n", "Greece  Athena", "Newburgh Free  9", "FAYETTEVILLE MAN  ", "CICERO NORTH SYR  ", " - ", "Vineland  \\(Boy\\'s\\)",
                                      "\\(Kp\\)", "\\(Mc\\)", "\\(P", "  Psal", " Brian\\t A", "Williamsville E ", " B-AAB", "Section  X I", "Mexico  -B",
                                      "Nottingham  -A", "Bronxville  High School", "A A", ",  CT", ",  MA", "-1NORTH ROCKL", "QUEENSBURY  HIGH", "Indiana  University", ", University of", "Sugrue_Neuendorf,",
                                      "Swim\\s{2,}Club",
                                      "Performance\\s{2,}Swim",
                                      "Swimming\\s{2,}Club",
                                      "Stamford\\s{2,}American\\s{2,}Internationa",
                                      "Uwcsea\\s{2,}Phoenix-ZZ",
                                      "AquaTech\\s{2,}Swimming",
                                      "Chinese\\s{2,}Swimming",
                                      "Aquatic\\s{2,}Performance",
                                      "SwimDolphia\\s{2}Aquatic School",
                                      "Young-Mandiak, Atticus F 11",
                                      "Molina Ayquipa, Santiago 12"),

          replacement = c("", "Greece Athena", "Newburgh Free-9", "FAYETTEVILLE MAN ", "CICERO NORTH SYR ", "-", "Vineland",
                          "", "", "", "-Psal", "Brian A", "Williamsville East ", "B-AAB", "Section XI", "Mexico",
                          "Nottingham", "Bronxville", "AA", "-CT", "-MA", "1-NORTH ROCKL", "QUEENSBURY", "Indiana University", "", "Neuendorf, Sugrue",
                          "Swim Club",
                          "Performance Swim",
                          "Swimming Club",
                          "Stamford American International",
                          "Uwcsea Phoenix-ZZ",
                          "AquaTech Swimming",
                          "Chinese Swimming",
                          "Aquatic Performance",
                          "SwimDolphia Aquatic School",
                          "Young-Mandiak, Atticus F   11",
                          "Molina Ayquipa, Santiago   12"))

    return(all_results)

  }

  # get test data to compare with standard
  df_test <- Read_Map(sources)
  df_test <- Parse_Map(df_test)
  df_test <- dplyr::bind_rows(df_test, .id = "source") %>%
    dplyr::select(-source)

  # to regenerate df_standard if df_test is more correct
  # readr::write_rds(df_test, "~/SwimmeR/inst/extdata/df_standard.rds")

  # compare standard to test
  expect_equivalent(df_standard,
                    df_test)
})

# testthat::test_file("tests/testthat/test-swim_parse_works.R")
