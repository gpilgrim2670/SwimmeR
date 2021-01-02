test_that("multiplication works", {
  url_1 <- "http://www.pacswim.org/userfiles/meets/documents/1360/0719resl.htm"
  url_2 <- "http://www.pacswim.org/userfiles/meets/documents/1585/0220cruz.htm" # no places, timed finals
  url_3 <- "http://www.pacswim.org/userfiles/meets/documents/1675/0220tera.htm" # a few extra results stuck on the beginning
  url_4 <- "http://www.pacswim.org/userfiles/meets/documents/1637/0220bac.htm" # has splits
  url_5 <- "http://www.pacswim.org/userfiles/meets/documents/1521/0220acsc.htm" # has lots of stuff, splits, missing finals places, all kinds of things

  expect_equal(2 * 2, 4)
})

# df <- swim_parse(read_results(url_1))
# View(df)

# testthat::test_file("tests/testthat/test-samms_works.R")
