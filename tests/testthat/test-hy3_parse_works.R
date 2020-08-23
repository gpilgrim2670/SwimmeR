test_that("hy3_parse_works", {
  file <-
    system.file("extdata", "2020_NI_Champs_Qualifier_UNAC.hy3", package = "SwimmeR")
  results <- read_results(file)
  results <- SwimmeR:::parse_hy3(results)
  ### still working on .hy3 parsing, hence minimal testing standard
  expect_equal(sum(results$DQ, na.rm = TRUE), 23)
})
