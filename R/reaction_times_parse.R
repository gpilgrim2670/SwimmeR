#' Pulls out reaction times from text
#'
#' Locates reaction times in text of results output from \code{read_results} and
#' their associated row numbers.  The resulting data frame is joined back into
#' results to include reaction times
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
#' @return returns a data frame with reaction times and row numbers to
#'   eventually be recombined with swimming results inside \code{swim_parse}
#'
#' @seealso \code{reaction_times_parse} is a helper function inside
#'   \code{\link{swim_parse}}

reaction_times_parse <- function(text) {
  # text <- as_lines_list_2

  reaction_time_string <- "r\\:\\+?\\-?\\s?\\d?\\d\\.\\d\\d\\s*\\d"


  reaction_times_raw <- text %>%
    .[stringr::str_detect(.,
                     reaction_time_string)]

  if (length(reaction_times_raw) > 0) {
    # if reaction times are recognized clean them up and determine row ranges
    reaction_times <- reaction_times_raw %>%
      stringr::str_extract(reaction_time_string) %>%
      stringr::str_remove("r\\:\\+?\\s?") %>%
      stringr::str_remove("\\s*\\d$") %>%
      trimws()

    row_numbs <- reaction_times_raw %>%
      stringr::str_extract("\\d{1,}$") %>%
      trimws()

    reaction_times <- paste(reaction_times, row_numbs, sep = "   ")


    reaction_times <-
      unlist(purrr::map(reaction_times, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    # dataframe for reaction times with names and row number ranges
    reaction_times <- reaction_times %>%
      list_transform() %>%
      dplyr::mutate(
        Reaction_Time = V1,
        # Reaction_Time_Row_Min = as.numeric(V2) - 1,
        # Reaction_Time_Row_Max = dplyr::lead(Reaction_Time_Row_Min, 1L, default = length(text)),
        Reaction_Time_Row_Numb = as.numeric(V2) - 1,
        V1 = NULL,
        V2 = NULL
      )
  } else{
    # if no reaction times are recognized deploy dummy data frame with reaction time "NA"
    reaction_times <- data.frame(
      Reaction_Time = "NA",
      Reaction_Time_Row_Numb = 1,
      stringsAsFactors = FALSE
    )

  }
  return(reaction_times)

}
