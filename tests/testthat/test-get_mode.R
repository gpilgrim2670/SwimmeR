test_that("get_mode works character", {
  a <- c("a", "a", "b", NA, NA, NA, "c")
  expect_equal(get_mode(a), "a")
})


test_that("get_mode works numeric", {
  a <- c(NA, NA, 100, 100, 100, 13, 2900)
  expect_equal(get_mode(a), 100)
})
