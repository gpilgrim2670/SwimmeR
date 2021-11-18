test_that("samms_works_1", {

  skip_on_cran() # due to risk of external resources failing

  # url_1 <- "http://www.pacswim.org/userfiles/meets/documents/1360/0719resl.htm"
  # url_2 <- "http://www.pacswim.org/userfiles/meets/documents/1585/0220cruz.htm" # no places, timed finals
  # url_3 <- "http://www.pacswim.org/userfiles/meets/documents/1675/0220tera.htm" # a few extra results stuck on the beginning
  # url_4 <- "http://www.pacswim.org/userfiles/meets/documents/1637/0220bac.htm" # has splits
  url_5 <- "http://www.pacswim.org/userfiles/meets/documents/1521/0220acsc.htm" # has lots of stuff, splits, missing finals places, all kinds of things
  # url_6 <- "https://www.swimmingworldmagazine.com/results/pdf/2015-sac-joaquin-section-swimming-and-diving-championships-high-school-2015-05-16.pdf" # has diving

  if(is_link_broken(url_5) == TRUE){
    warning("Link to external data is broken")
  } else {

    df <- swim_parse(read_results(url_5))


    # should be 113 DQs + 54 NO SWIMS, total is 167 in the DQ column
    expect_equal(sum(df$DQ == 1), 167)

  }
})

test_that("samms_works_2", {

  skip_on_cran() #  I am removing this test so as to avoid that issue, but I am not happy about it

  # url_1 <- "http://www.pacswim.org/userfiles/meets/documents/1360/0719resl.htm"
  # url_2 <- "http://www.pacswim.org/userfiles/meets/documents/1585/0220cruz.htm" # no places, timed finals
  # url_3 <- "http://www.pacswim.org/userfiles/meets/documents/1675/0220tera.htm" # a few extra results stuck on the beginning
  # url_4 <- "http://www.pacswim.org/userfiles/meets/documents/1637/0220bac.htm" # has splits
  # url_5 <- "http://www.pacswim.org/userfiles/meets/documents/1521/0220acsc.htm" # has lots of stuff, splits, missing finals places, all kinds of things
  url_6 <- "https://www.swimmingworldmagazine.com/results/pdf/2015-sac-joaquin-section-swimming-and-diving-championships-high-school-2015-05-16.pdf" # has diving

  if(is_link_broken(url_6) == TRUE){
    warning("Link to external data is broken")
  } else {

    df <- swim_parse(read_results(url_6))

    expect_equal(sum(is.na(df$Finals)), sum(df$DQ == 1)) # had problem on debian builds, likely due to pdf issues
    # should be 42 DQs, 7 No Shows but one DQ (OAKM, A in the Womens 400 Free Relay is cut off by pdf tools)
    # test is 48 total NAs in finals = 41 DQs + 7 no shows
  }
})

# testthat::test_file("tests/testthat/test-samms_works.R")
