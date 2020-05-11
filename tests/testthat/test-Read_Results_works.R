library(rvest)
library(pdftools)

test_that("Read_Result PDF works", {
  file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  expect_match(
    Read_Results(file)[298],
    "\n    1 Lilly King                                21 Indiana University                         NT               59.46   B"
  )

})

test_that("Read_Result HTML works", {
  expect_match(
    Read_Results("http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm", node = "pre")[683],
    "\n  1 Ricky Henahan       12 5-WEST IRONDEQUO       50.36      49.76 AAA     24"
  )

})
