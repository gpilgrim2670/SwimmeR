test_that("read_results PDF works", {
  file <-
    system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  file <- read_results(file)
  expect_match(paste(str_extract_all(file[298], "\\d")[[1]], collapse = ""),
               "1215946")

})

test_that("read_results HTML works", {
  file <- "http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm"

  if (is_link_broken(file) == TRUE) {
    warning("Link to external data is broken")
  } else {
    expect_match(read_results(file)[683],
                 "\n  1 Ricky Henahan       12 5-WEST IRONDEQUO       50.36      49.76 AAA     24")
  }

})

# test_file("tests/testthat/test-read_results_works.R")
