test_that("course_convert_works 100 fly", {
  converted_time <- course_convert(
    time = 53.89,
    event = "100 Fly",
    course = "SCY",
    course_to = "LCM"
  )

  expect_equivalent(converted_time,
                    "1:01.22")
})

test_that("course_convert_works 400-500", {
  converted_time <- course_convert(
    time = "4:45.00",
    event = "500 Free",
    course = "SCY",
    course_to = "LCM"
  )

  expect_equivalent(converted_time,
                    "4:14.36")
})


test_that("course_convert verbose works", {

  df_test <- course_convert(
    time = 53.89,
    event = "100 Fly",
    course = "SCY",
    course_to = "LCM",
    verbose = TRUE
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


test_that("course_convert verbose works 200 breast", {

  df_test <- course_convert(
    time = "2:31.31",
    event = "200 Breast",
    course = "LCM",
    course_to = "SCY",
    verbose = TRUE
  )

  df_standard <- data.frame(
    Time = c("2:31.31"),
    Course = c("LCM"),
    Course_To = c("SCY"),
    Event = c("200 Breast"),
    Time_Converted_sec = c(132.71),
    Time_Converted_mmss = c("2:12.71")
  )

  expect_equal(df_test, df_standard, 0.001)
})

# testthat::test_file("tests/testthat/test-course_convert.R")
