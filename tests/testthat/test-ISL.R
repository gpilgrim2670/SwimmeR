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

test_that("Lilly King Times with score", {
  file <- system.file("extdata", "ISL_2020_Match_1.pdf", package = "SwimmeR")
  df <- swim_parse_ISL(
    Read_Results(file))
  expect_equivalent(df[which(df$Name == "KING Lilly"),]$Time,
               c("2:17.11", "28.86", "1:03.16", "29.16", "29.25", "28.90"))

})

test_that("Lilly King Times without score", {
  file <- system.file("extdata", "ISL_2019_CollegePark_Day_1.pdf", package = "SwimmeR")
  df <- swim_parse_ISL(
    Read_Results(file))
  expect_equivalent(df[which(df$Name == "KING Lilly"),]$Time,
               c("29.00", "2:17.78"))

})

# testthat::test_file("tests/testthat/test-ISL.R")
