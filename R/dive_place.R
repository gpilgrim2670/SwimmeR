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
#' @importFrom dplyr enquo
#'
#' @param df a data frame with results from \code{swim_parse}, including only
#'   diving results (not swimming)
#' @param max_place highest place value that scores #' @param score_col the name
#'   of a column in \code{df} containing scores on which to place (order)
#'   performances
#' @param score_col the name of a column in \code{df} containing scores on which
#'   to place (order) performances
#' @param keep_nonscoring are athletes in places greater than \code{max_place}
#'   be retained in the data frame.  Either \code{TRUE} or \code{FALSE}
#' @return data frame modified so that places have been appended based on diving
#'   score
#'
#' @seealso \code{dive_place} is a helper function used inside of
#'   \code{results_score}
#'
#' @export


dive_place <- function(df, max_place, score_col = Finals_Time, keep_nonscoring = TRUE) {


  if(as.character(dplyr::ensym(score_col)) %!in% names(df)){
    stop("score_col must be a column of times in df")
  }

  if(any(!is.logical(keep_nonscoring), is.na(keep_nonscoring)) == TRUE) {
    stop("keep_nonscoring must be logical, either TRUE or FALSE")
  }

  if("Event" %!in% names(df)){
    stop("df must contain a column named Event")
  }

  if("Name" %!in% names(df)){
    stop("df must contain a column named Name")
  }

  score_col <- dplyr::enquo(score_col)

  df <- df %>%
    dplyr::filter(stringr::str_detect(str_to_lower(Event), "diving") == TRUE) %>%
    dplyr::group_by(Event, Name) %>%
    dplyr::slice(1) %>% # first instance of every diver
    dplyr::ungroup() %>%
    dplyr::group_by(Event) %>%
    dplyr::mutate(!!score_col := as.numeric(!!score_col)) %>%
    dplyr::mutate(
      Place = rank(desc(!!score_col), ties.method = "min"),
      # again, highest score gets rank 1
      !!score_col := as.character(!!score_col)
    ) %>%
    dplyr::mutate(Place = Place - cumsum(DQ)) %>% # take out DQs
    {if(keep_nonscoring == FALSE)
    dplyr::filter(., Place <= max_place) else .}
  return(df)
}
