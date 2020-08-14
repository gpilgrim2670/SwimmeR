#' Rescore to account for ties
#'
#' Places are awarded on the basis of time, with fastest (lowest) time winning.  Ties are placed as ties (both ahtletes get 2nd etc.)
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#'
#' @param df a dataframe with results from \code{swim_parse}, with places from \code{swim_place} and/or \code{dive_place}
#' @param point_values a named list of point values for each scoring place
#' @return df modified so that places have been appended based on swimming time
#'
#' @seealso \code{tie_rescore} is a helper function used inside of \code{results_score}
#'
#'
#'
tie_rescore <- function(df, point_values) {
  results <- df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Event) %>%
    dplyr::mutate(New_Place = rank(Place, ties.method = "first"),
                  Points = point_values[New_Place]) %>%
    dplyr::group_by(Place, Event) %>%
    dplyr::summarize(Points = mean(Points)) %>%
    dplyr::inner_join(df) %>%
    dplyr::mutate(Points = dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(Event), "relay") == TRUE ~ Points * 2,
      TRUE ~ Points
    )) %>%
    dplyr::ungroup()
  return(results)
}
