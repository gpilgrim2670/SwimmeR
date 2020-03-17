library(rvest)
library(pdftools)

test_that("Swim_Parse works", {
  expect_match(Swim_Parse(
    Read_Results("Texas-Florida-Indiana.pdf"),
    typo =  c("\n", "Indiana  University", ", University of"),
    replacement = c("\n", "Indiana University", "")
  )[67, 1],
  "Lilly King")

})

test_that("Swim_Parse works 2", {
  expect_match(Swim_Parse(
    Read_Results("Texas-Florida-Indiana.pdf"),
    typo =  c("\n", "Indiana  University", ", University of"),

    replacement = c("\n", "Indiana University", "")
  )[164, 6],
  "2:01.78")

})

test_that("Swim_Parse works 3", {
  expect_equivalent(sum(Swim_Parse(
    Read_Results(
      "2008 NYSPHAA Federation Championship - 2_29_2008 to 3_1_2008.html",
      node = "pre"
    ),
    typo = c("-1NORTH ROCKL"),
    replacement = c("1-NORTH ROCKL")
  )[,2]),
  16235)

})
