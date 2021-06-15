test_that("format ages", {

  ages_standard <- c("13", "25", NA, "SR", "27")

  ages_test <- age_format(c("13-06", "25-03", NA, "SR", "27-94"))

  expect_equivalent(ages_standard, ages_test)
})

# testthat::test_file("tests/testthat/test-age_format.R")
