#' Rescore to account for ties
#'
#' Rescoring to average point values for ties.  Ties are placed as ties (both
#' athletes get 2nd etc.)
#'
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom dplyr summarize
#' @importFrom dplyr inner_join
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_detect
#'
#' @param df a data frame with results from \code{swim_parse}, with places from
#'   \code{swim_place} and/or \code{dive_place}
#' @param point_values a named list of point values for each scoring place
#' @param lanes number of scoring lanes in the pool
#' @return df modified so that places have been appended based on swimming time
#'
#' @seealso \code{tie_rescore} is a helper function used inside of
#'   \code{results_score}

tie_rescore <- function(df, point_values, lanes) {

  # df <- results

  results <- df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Event, Heat) %>%
    dplyr::mutate(New_Place = rank(Place, ties.method = "first") + ((Heat - 1)*lanes) - cumsum(DQ),
                  Points = point_values[New_Place]) %>%
    dplyr::group_by(Place, Event) %>%
    dplyr::summarize(Points = mean(Points, na.rm = TRUE)) %>%
    dplyr::inner_join(df) %>%
    dplyr::mutate(Points = dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(Event), "relay") == TRUE ~ Points * 2,
      TRUE ~ Points
    )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Points = dplyr::case_when(DQ == 1 ~ 0,
                                     DQ == 0 ~ Points))
  return(results)
}
