test_that("course_convert_DF_works", {
  expect_equivalent(round(
    course_convert_DF(
      time = 53.89,
      event = "100 Fly",
      course = "SCY",
      course_to = "LCM"
    )[1, 5],
    1
  ),  round(61.2, 1))
})
