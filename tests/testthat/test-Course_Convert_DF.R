test_that("multiplication works", {
  expect_equivalent(round(as.numeric(course_convert_DF(53.89, "SCY", "LCM", "100 Fly")[1,5]),  1),  round(61.2) ,1)
})
