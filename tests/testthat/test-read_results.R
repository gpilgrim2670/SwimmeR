test_that("read_results PDF works", {
  file <-
    system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  file <- read_results(file)
  expect_match(paste(str_extract_all(file[299], "\\d")[[1]], collapse = ""),
               "1215946")

})

test_that("read_results HTML works", {

  skip_on_cran() # due to risk of external resources failing

  file <- "http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm"

  if (is_link_broken(file) == TRUE) {
    warning("Link to external data is broken")
  } else {
    expect_match(read_results(file)[684],
                 "\n  1 Ricky Henahan       12 5-WEST IRONDEQUO       50.36      49.76 AAA     24")
  }

})

test_that("read_results_flag PDF works", {
  file <-
    system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
  file <- read_results(file)
  expect_match(file[1],
               "read_results_flag")

})

# test_file("tests/testthat/test-read_results.R")
