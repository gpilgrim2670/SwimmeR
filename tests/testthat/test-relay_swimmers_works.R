test_that("relay swimmers one line works", {

  relay_swimmers_standard <- c("Morgan Scott", "Lilly King", "Christie Jensen", "Shelby Koontz")

  df <-
    swim_parse(read_results(
      system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
    ), relay_swimmers = TRUE)

  relay_swimmers_test <- df %>%
    dplyr::select(Relay_Swimmer_1, Relay_Swimmer_2, Relay_Swimmer_3, Relay_Swimmer_4) %>%
    head(1) %>%
    as.list() %>%
    unname()
  expect_equivalent(relay_swimmers_test, relay_swimmers_standard)
})

test_that("relay swimmers two lines works", {
  file <-
    "https://cdn.swimswam.com/wp-content/uploads/2018/08/2004-Division-I-NCAA-Championships-Men-results1.pdf"

  if (is_link_broken(file) == TRUE) {
    warning("Link to external data is broken")
  } else {
    relay_swimmers_standard <-
      c("BOVELL, GEORGE",
        "GIBB, DEREK",
        "WOCHOMURKA, RYAN",
        "BOUSQUET, FRED")

    df <- swim_parse(read_results(file), relay_swimmers = TRUE)

    relay_swimmers_test <- df %>%
      dplyr::select(Relay_Swimmer_1,
                    Relay_Swimmer_2,
                    Relay_Swimmer_3,
                    Relay_Swimmer_4) %>%
      head(1) %>%
      as.list() %>%
      unname()

    expect_equivalent(relay_swimmers_test, relay_swimmers_standard)
  }

})

# testthat::test_file("tests/testthat/test-relay_swimmers_works.R")
