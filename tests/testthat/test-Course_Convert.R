test_that("multiplication works", {
  expect_equivalent(course_convert(53.89, "SCY", "LCM", "100 Fly"), "1:01.22")
})
