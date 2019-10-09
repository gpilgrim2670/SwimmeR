test_that("course_convert_works", {
  expect_equivalent(course_convert(time = 53.89, event = "100 Fly", course = "SCY", course_to = "LCM"), "1:01.22")
})
