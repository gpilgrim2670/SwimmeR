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

test_that("Swim_Parse works 2", {
  # file <- test_file("Texas-Florida-Indiana.pdf")
  file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  expect_match(Swim_Parse(
    Read_Results(file),
    typo =  c("\n", "Indiana  University", ", University of"),

    replacement = c("\n", "Indiana University", "")
  )[164, 6],
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
<<<<<<< HEAD


test_that("Swim_Parse works USA", {
  file <- system.file("extdata", "jets08082019_067546.pdf", package = "SwimmeR")
  expect_equivalent(sum(Swim_Parse(
    Read_Results(file)
  )[1]),
  3091)
})


=======
>>>>>>> parent of 98df11c... Added testing for USA meet
