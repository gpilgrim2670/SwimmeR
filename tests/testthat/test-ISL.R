test_that("Lilly King Score ISL", {
  file <- system.file("extdata", "ISL_2020_Match_1.pdf", package = "SwimmeR")
  df <- swim_parse_ISL(
    Read_Results(file))
  expect_equal(sum(unique(df[df$Name == "KING Lilly",])$Points, na.rm = TRUE),
  108)

})

test_that("CAC Score ISL", {
  file <- system.file("extdata", "ISL_2020_Match_1.pdf", package = "SwimmeR")
  df <- swim_parse_ISL(
    Read_Results(file))
  expect_equal(sum(unique(df[df$Team == "CAC",])$Points, na.rm = TRUE),
               567)

})
