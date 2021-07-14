test_that("hy3_parse_works", {

  skip_on_cran()

  file <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/SwimmeR%20Test%20Files/2020_NI_Champs_Qualifier_UNAC.hy3"

  results <- read_results(file)
  results <- SwimmeR:::hy3_parse(results)
  ### still working on .hy3 parsing, hence minimal testing standard
  expect_equal(sum(results$DQ, na.rm = TRUE), 23)
})

# testthat::test_file("tests/testthat/test-hy3_parse.R")
