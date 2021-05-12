#' Adds places to diving results
#'
#' Places are awarded on the basis of score, with highest score winning.  Ties
#' are placed as ties (both athletes get 2nd etc.)
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_lower
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr desc
#'
#' @param df a data frame with results from \code{swim_parse}, including only
#'   diving results (not swimming)
#' @param max_place highest place value that scores
#' @return data frame modified so that places have been appended based on diving
#'   score
#'
#' @seealso \code{dive_place} is a helper function used inside of
#'   \code{results_score}


dive_place <- function(df, max_place) {
  df <- df %>%
    dplyr::filter(stringr::str_detect(str_to_lower(Event), "diving") == TRUE) %>%
    dplyr::group_by(Event, Name) %>%
    dplyr::slice(1) %>% # first instance of every diver
    dplyr::ungroup() %>%
    dplyr::group_by(Event) %>%
    dplyr::mutate(Finals_Time = as.numeric(Finals_Time)) %>%
    dplyr::mutate(
      Place = rank(desc(Finals_Time), ties.method = "min"),
      # again, highest score gets rank 1
      Finals_Time = as.character(Finals_Time)
    ) %>%
    dplyr::filter(Place <= max_place)
  return(df)
}
