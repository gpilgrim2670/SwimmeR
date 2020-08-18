#' Adds places to swimming results
#'
#' Places are awarded on the basis of time, with fastest (lowest) time winning.  Ties are placed as ties (both athletes get 2nd etc.)
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#'
#' @param df a dataframe with results from \code{swim_parse}, including only swimming results (not diving)
#' @param max_place highest place value that scores
#' @return df modified so that places have been appended based on swimming time
#'
#' @seealso \code{swim_place} is a helper function used inside of \code{results_score}

swim_place <- function(df, max_place) {
  df <- df %>%
    dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "diving") == FALSE) %>%
    dplyr::slice(1) %>% # first instance of every swimmer or team (for relays)
    dplyr::ungroup() %>%
    dplyr::group_by(Event) %>%
    dplyr::mutate(Finals_Time_sec = SwimmeR::sec_format(Finals_Time)) %>% # time as seconds
    dplyr::mutate(
      # Place = as.numeric(Place),
      # Place = case_when(
      #   is.na(Place) == TRUE ~ as.numeric(rank(Finals_Time_sec, ties.method = "min") + lanes),
      #   is.na(Place) == FALSE ~ Place
      # ),
      Place = rank(Finals_Time_sec, ties.method = "min")) %>%
    dplyr::arrange(Place) %>%
    mutate(Place = Place - cumsum(DQ)) %>% # places, low number wins
    dplyr::filter(Place <= max_place)
  return(df)
}
