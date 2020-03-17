test_that("Read_Result PDF works", {
  expect_match(
    Read_Results("Texas-Florida-Indiana.pdf")[298],
    "\n    1 Lilly King                                21 Indiana University                         NT               59.46   B"
  )

})

test_that("Read_Result HTML works", {
  expect_match(
    Read_Results("2008 NYSPHAA Federation Championship - 2_29_2008 to 3_1_2008.html", node = "pre")[683],
    "\n  1 Ricky Henahan       12 5-WEST IRONDEQUO       50.36      49.76 AAA     24"
  )

})
