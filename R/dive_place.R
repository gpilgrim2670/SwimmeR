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
#' @importFrom dplyr ensym
#'
#' @param df a data frame with results from \code{swim_parse}, including only
#'   diving results (not swimming)
#' @param score_col the name of a column in \code{df} containing scores on which
#'   to place (order) performances
#' @param max_place highest place value that scores #' @param score_col the name
#'   of a column in \code{df} containing scores on which to place (order)
#'   performances
#' @param keep_nonscoring are athletes in places greater than \code{max_place}
#'   be retained in the data frame.  Either \code{TRUE} or \code{FALSE}
#' @return data frame modified so that places have been appended based on diving
#'   score
#'
#' @seealso \code{dive_place} is a helper function used inside of
#'   \code{results_score}
#'
#' @export


dive_place <- function(df, score_col = Finals, max_place = NULL, keep_nonscoring = TRUE) {

  #### regularize score_col ####
  score_col <- dplyr::ensym(score_col)

  #### keep_nonscoring and max place ####
  if(any(!is.logical(keep_nonscoring), is.na(keep_nonscoring)) == TRUE) {
    stop("keep_nonscoring must be logical, either TRUE or FALSE")
  }

  if(keep_nonscoring == FALSE & is.null(max_place)){
    stop("If keep_nonscoring = FALSE then max_place must be specified")
  }

  if(all(!is.null(max_place), max_place %% 1 < 0)){
    stop("max_place must be an integer value greater than 0 or NULL")
  }
  max_place <- as.integer(max_place)

  #### required columns ####
  if(as.character(score_col) %!in% names(df)){
    stop("score_col must be a column of scores in df")
  }

  if("Name" %!in% names(df)){
    stop("df must contain a column named Name")
  }

  if("Event" %!in% names(df)){
    stop("df must contain a column named Event")
  }

  #### must have diving ####
  if(any(stringr::str_detect(stringr::str_to_lower(df$Event), "diving")) == FALSE){
    stop("df must contain a calumn called Event with some rows containing 'Diving' or 'diving'")
  }

  df <- df %>%
    dplyr::filter(stringr::str_detect(str_to_lower(Event), "diving") == TRUE) %>%
    dplyr::group_by(Event, Name) %>%
    dplyr::slice(1) %>% # first instance of every diver
    dplyr::ungroup() %>%
    dplyr::group_by(Event) %>%
    dplyr::mutate(Score = {{score_col}}) %>%
    dplyr::mutate(Score = as.numeric(Score)) %>%
    dplyr::mutate(Place = rank(desc(Score), ties.method = "min")) %>%
    dplyr::select(-Score) %>%
    {
      if ("DQ" %in% names(df))
        dplyr::mutate(., Place = Place - cumsum(DQ))
      else
        .
    } %>% # take out DQs
    {
      if (keep_nonscoring == FALSE)
        dplyr::filter(., Place <= max_place)
      else
        .
    } %>%
    dplyr::arrange(Place)

  return(df)
}
