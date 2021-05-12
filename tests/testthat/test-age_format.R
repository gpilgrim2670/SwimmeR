test_that("multiplication works", {

  ages_standard <- c("13.5", "25.25", NA, "SR")

  ages_test <- age_format(c("13-06", "25-03", NA, "SR"))

  expect_equivalent(ages_standard, ages_test)
})

# testthat::test_file("tests/testthat/test-age_format.R")
