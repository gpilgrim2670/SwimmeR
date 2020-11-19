

test_that("Lilly King Score ISL", {
  file <- "https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/ISL/Season_2_2020/ISL_16102020_Budapest_Match_1.pdf"

  if(is_link_broken(file) == TRUE){
    warning("Link to external data is broken")
  } else {
  df <- swim_parse_ISL(
    read_results(file))
  expect_equal(sum(unique(df[df$Name == "KING Lilly",])$Points, na.rm = TRUE),
  108)
  }

})

test_that("CAC Score ISL", {
  file <- "https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/ISL/Season_2_2020/ISL_16102020_Budapest_Match_1.pdf"

  if(is_link_broken(file) == TRUE){
    warning("Link to external data is broken")
  } else {
  df <- swim_parse_ISL(
    read_results(file))
  expect_equal(sum(unique(df[df$Team == "CAC",])$Points, na.rm = TRUE),
               567)
  }

})

test_that("Lilly King Times with score", {
  file <- "https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/ISL/Season_2_2020/ISL_16102020_Budapest_Match_1.pdf"

  if(is_link_broken(file) == TRUE){
    warning("Link to external data is broken")
  } else {
  df <- swim_parse_ISL(
    read_results(file))
  expect_equivalent(df[which(df$Name == "KING Lilly"),]$Time,
               c("2:17.11", "28.86", "1:03.16", "29.16", "29.25", "28.90"))
  }

})

test_that("Lilly King Times without score", {
  file <- "https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/ISL/Season_1_2019/ISL_16112019_CollegePark_Day_1.pdf"

  if(is_link_broken(file) == TRUE){
    warning("Link to external data is broken")
  } else {
  df <- swim_parse_ISL(
    read_results(file))
  expect_equivalent(df[which(df$Name == "KING Lilly"),]$Time,
               c("29.00", "2:17.78"))
  }

})

# testthat::test_file("tests/testthat/test-ISL.R")
