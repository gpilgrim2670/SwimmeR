test_that("sec_format", {
  sec_list_test <- sec_format(c("1:35.93", "16:45.19", NA, "25.43", ":55.62", "`1:35.93"))
  sec_list_standard <- c(95.93, 1005.19, NA, 25.43, 55.62, 95.93)

  expect_equivalent(sec_list_test, sec_list_standard)

})

# testthat::test_file("tests/testthat/test-sec_format.R")
