# # file <- read_results("~/SwimmeR/inst/extdata/s2-results.pdf")
# df <- swim_parse(
#   read_results("inst/extdata/s2-results.pdf"),
#   avoid = c("MR:"),
#   typo =
#     c(
#       "Swim\\s{2,}Club",
#       "Performance\\s{2,}Swim",
#       "Swimming\\s{2,}Club",
#       "Stamford\\s{2,}American\\s{2,}Internationa",
#       "Uwcsea\\s{2,}Phoenix-ZZ",
#       "AquaTech\\s{2,}Swimming",
#       "Chinese\\s{2,}Swimming",
#       "Aquatic\\s{2,}Performance",
#       "SwimDolphia\\s{2}Aquatic School"
#     ),
#   replacement =
#     c(
#       "Swim Club",
#       "Performance Swim",
#       "Swimming Club",
#       "Stamford American International",
#       "Uwcsea Phoenix-ZZ",
#       "AquaTech Swimming",
#       "Chinese Swimming",
#       "Aquatic Performance",
#       "SwimDolphia Aquatic School"
#     ),
#   splits = TRUE
# )
# str_extract_all(text[167], "[\\(\\s)]\\d\\d\\.\\d\\d[\\)\\s]")
# str_extract_all(text[167], "^\\s+\\d\\d\\.\\d\\d|\\(\\d\\d\\.\\d\\d\\)")

# file <- system.file("extdata", "Texas-Florida-Indiana.pdf", package = "SwimmeR")
# df <- swim_parse(
#   read_results(file),
#   typo =  c("\n", "Indiana  University", ", University of"),
#
#   replacement = c("\n", "Indiana University", ""),
#   splits = TRUE
# )

