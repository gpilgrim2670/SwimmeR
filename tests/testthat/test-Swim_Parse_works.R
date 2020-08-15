library(rvest)
library(pdftools)

test_that("Swim_Parse works", {
  file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  expect_match(Swim_Parse(
    Read_Results(file),
    typo =  c("\n", "Indiana  University", ", University of"),
    replacement = c("\n", "Indiana University", "")
  )[67, 1],
  "Lilly King")

})

test_that("swim_parse works 2", {
  # file <- test_file("Texas-Florida-Indiana.pdf")
  file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  expect_match(swim_parse(
    Read_Results(file),
    typo =  c("\n", "Indiana  University", ", University of"),

    replacement = c("\n", "Indiana University", "")
  )[190, 6],
  "2:01.78")

})

test_that("Swim_Parse works 3", {
  expect_equivalent(sum(Swim_Parse(
    Read_Results(
      "http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm",
      node = "pre"
    ),
    typo = c("-1NORTH ROCKL"),
    replacement = c("1-NORTH ROCKL")
  )[,2]),
  16235)

})


test_that("Swim_Parse works USMS", {
  file <- system.file("extdata", "11102019roc.pdf", package = "SwimmeR")
  expect_match(Swim_Parse(
    Read_Results(file)
  )[107, 6],
  "51.90")

})



test_that("Swim_Parse works USA", {
  file <- system.file("extdata", "jets08082019_067546.pdf", package = "SwimmeR")
  expect_equivalent(sum(Swim_Parse(
    Read_Results(file)
  )[1]),
  3091)
})

test_that("Swim_Parse works list", {
  file_1 <- system.file("extdata", "jets08082019_067546.pdf", package = "SwimmeR")
  file_2 <- system.file("extdata", "11102019roc.pdf", package = "SwimmeR")
  file_3 <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  url91 <- "http://www.section11swim.com/Results/GirlsHS/2016/League1/Single.htm" # numbers as grades still attached to schools - fixed
  url92 <- "http://www.section1swim.com/Results/BoysHS/2020/Sec1/Single.htm" # schools are NA - fixed
  url93 <- "http://www.section2swim.com/Results/BoysHS/2004/Sec2/A/Single.htm" # schools as SR
  url94 <- "http://www.section6swim.com/Results/GirlsHS/2012/NFL/Single.htm" # schools as SR
  url97 <- "http://www.section3swim.com/Results/BoysHS/2020/Sec3/BC/Single.htm" # events errors - fixed
  url98 <- "http://www.section5swim.com/Results/BoysHS/2013/HAC/Single.htm"
  url99 <- "http://www.section1swim.com/Results/BoysHS/2020/Sec1/Single.htm"
  url100 <- "http://www.section9swim.com/Results/GirlsHS/2000/Sec9/Single.htm"
  url101 <- "http://www.section5swim.com/Results/GirlsHS/2000/Sec5/B/Single.htm"
  sources <- c(file_1,
               file_2,
               file_3,
               url91,
               url92,
               url93,
               url94,
               url97,
               url98,
               url99,
               url100,
               url101)

  df_standard <- read.csv(system.file("extdata", "df_standard.csv", package = "SwimmeR"), stringsAsFactors = FALSE, colClasses=c("character", "numeric", rep("character", 7)))
  df_standard <- df_standard %>%
    select(-X)

  Read_Map <- function(links) {

    scrape_test_all <-
      map(links, Read_Results, node = "pre")

    names(scrape_test_all) <- links
    return(scrape_test_all)

  }

  Parse_Map <- function(links) {

    all_results <-
      map(links, Swim_Parse, typo = c("\n", "Greece  Athena", "Newburgh Free  9", "FAYETTEVILLE MAN  ", "CICERO NORTH SYR  ", " - ", "Vineland  \\(Boy\\'s\\)",
                                      "\\(Kp\\)", "\\(Mc\\)", "\\(P", "  Psal", " Brian\\t A", "Williamsville E ", " B-AAB", "Section  X I", "Mexico  -B",
                                      "Nottingham  -A", "Bronxville  High School", "A A", ",  CT", ",  MA", "-1NORTH ROCKL", "QUEENSBURY  HIGH", "Indiana  University", ", University of", "Sugrue_Neuendorf,"),

          replacement = c("", "Greece Athena", "Newburgh Free-9", "FAYETTEVILLE MAN ", "CICERO NORTH SYR ", "-", "Vineland",
                          "", "", "", "-Psal", "Brian A", "Williamsville East ", "B-AAB", "Section XI", "Mexico",
                          "Nottingham", "Bronxville", "AA", "-CT", "-MA", "1-NORTH ROCKL", "QUEENSBURY", "Indiana University", "", "Neuendorf, Sugrue"))

    # names(scrape_test_all) <- links
    return(all_results)

  }

  df_test <- Read_Map(sources)
  df_test <- Parse_Map(df_test)
  df_test <- dplyr::bind_rows(df_test, .id = "column_label") %>%
    select(-column_label)
  expect_equivalent(df_standard,
                    df_test)
})

