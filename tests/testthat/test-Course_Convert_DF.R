test_that("course_convert_DF_works", {

  df_test <- course_convert_DF(
    time = 53.89,
    event = "100 Fly",
    course = "SCY",
    course_to = "LCM"
  )

  df_standard <- data.frame(
    Time = c("53.89"),
    Course = c("SCY"),
    Course_To = c("LCM"),
    Event = c("100 Fly"),
    Time_Converted_sec = c(61.20),
    Time_Converted_mmss = c("1:01.22")
  )

  expect_equal(df_test, df_standard, 0.001)
})

# testthat::test_file("tests/testthat/test-course_convert_DF.R")
