#' Pulls out heat labels from text
#'
#' Locates heat labels in text of results output from \code{read_results} and
#' their associated row numbers.  The resulting data frame is joined back into
#' results to include heat numbers
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom stringr str_replace
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param text output from \code{read_results} followed by
#'   \code{add_row_numbers}
#' @return returns a data frame with heat names and row numbers to eventually
#'   be recombined with swimming results inside \code{swim_parse_omega}
#'
#' @seealso \code{heat_parse_omega} is a helper function inside
#'   \code{\link{swim_parse_omega}}

heat_parse_omega <- function(text) {
  # file <-
  #   system.file("extdata", "Omega_OT_400m_Finals_2021.pdf", package = "SwimmeR")
  #
  # text <- file %>%
  #   read_results() %>%
  #   add_row_numbers()
  #
  # text <- as_lines_list_2

  heat_string <- "Heat\\s\\d{1,}\\sof\\s\\d{1,}|Semifinal\\s+\\d{1,}|Final|Heats(?![:alpha:])"

  heats <- text %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     heat_string)]

  if (length(heats) > 0) {
    # if heat names are recognized clean them up and determine row ranges
    heat_rows <- heats %>%
      stringr::str_extract("\\d{1,}$")

    heats <- heats %>%
      stringr::str_extract(heat_string) %>%
      stringr::str_remove("\\\nHeat?\\s") %>%
      stringr::str_remove("\\sof\\s\\d{1,}") %>%
      stringr::str_replace("Semifinal\\s+", "Semi_") %>%
      stringr::str_replace("Heat\\s+", "Heat_") %>%
      trimws()

    heats <- paste(heats, heat_rows, sep = "   ")

    heats <-
      unlist(purrr::map(heats, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    # dataframe for heats with names and row number ranges
    heats <- heats %>%
      list_transform() %>%
      dplyr::mutate(
        Heat = V1,
        Heat_Row_Min = as.numeric(V2),
        Heat_Row_Max = dplyr::lead(Heat_Row_Min, 1L, default = length(text)) - 1,
        V1 = NULL,
        V2 = NULL
      )
  } else{
    # if no heat names are recognized deploy dummy dataframe with heat name "NA"
    heats <- data.frame(
      Heat = "NA",
      Heat_Row_Min = 1,
      Heat_Row_Max = length(text) - 1,
      stringsAsFactors = FALSE
    )

  }
  return(heats)

}
