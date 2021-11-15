#' Adds places to swimming results
#'
#' Places are awarded on the basis of time, with fastest (lowest) time winning.
#' Ties are placed as ties (both athletes get 2nd etc.)
#'
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr ensym
#' @importFrom dplyr enquo
#'
#' @param df a data frame with results from \code{swim_parse}, including only
#'   swimming results (not diving)
#' @param max_place highest place value that scores
#' @param time_col the name of a column in \code{df} containing times on which
#'   to place (order) performances
#' @param event_type either \code{"ind"} for individual or \code{"relay"} for
#'   relays
#' @param max_relays_per_team an integer value denoting the number of relays a
#'   team may score (usually 1)
#' @param keep_nonscoring are athletes in places greater than \code{max_place}
#'   be retained in the data frame.  Either \code{TRUE} or \code{FALSE}
#' @return a data frame modified so that places have been appended based on
#'   swimming time
#'
#' @seealso \code{swim_place} is a helper function used inside of
#'   \code{results_score}
#'
#' @export

swim_place <- function(df,
                       max_place,
                       time_col = Finals_Time,
                       event_type = "ind",
                       max_relays_per_team = 1,
                       keep_nonscoring = TRUE
                       ) {

  if(max_relays_per_team %% 1 > 0){
    stop("max_relays_per_team must be an integer value greater than 0")
  }

  if(any(!is.logical(keep_nonscoring), is.na(keep_nonscoring)) == TRUE) {
    stop("keep_nonscoring must be logical, either TRUE or FALSE")
  }

  if(event_type %!in% c("ind", "relay")){
  stop("event_type must be either 'ind' or 'relay'")}

  if(all(event_type == "relay", max_relays_per_team < 1)){
    stop("if event_type == 'relay' then max_relays_per_team must be an integer greater than or equal to 1")}

  if(as.character(dplyr::ensym(time_col)) %!in% names(df)){
    stop("time_col must be a column of times in df")
  }

  if("DQ" %!in% names(df)){
    df$DQ <- 0
  }

  if("Event" %!in% names(df)){
    stop("df must contain a column named Event")
  }

  if(all(event_type == "ind", "Name" %!in% names(df))){
    stop("df must contain a column named Name if event_type = 'ind'")
  }

  if(all(event_type == "relay", "Team" %!in% names(df))){
    stop("df must contain a column named Team if event_type = 'relay'")
  }

  time_col <- dplyr::enquo(time_col)
  max_relays_per_team <- as.integer(max_relays_per_team)


  df <- df %>%
    {if(event_type == "ind")
      dplyr::group_by(., Event, Name) else dplyr::group_by(., Event, Team)} %>%
    dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "diving") == FALSE) %>%
    {if(event_type == "ind")
    dplyr::slice(., 1) else dplyr::slice(., max_relays_per_team)} %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Event) %>%
    # dplyr::mutate(Time_sec = SwimmeR::sec_format(Finals_Time)) %>% # time as seconds
    dplyr::mutate(Time_sec = SwimmeR::sec_format(!!time_col)) %>% # time as seconds
    dplyr::mutate(Place = rank(Time_sec, ties.method = "min")) %>% # places, low number wins
    dplyr::select(-Time_sec) %>%
    dplyr::arrange(Place) %>%
    dplyr::mutate(Place = Place - cumsum(DQ)) %>% # take out DQs
    {if(keep_nonscoring == FALSE)
      dplyr::filter(., Place <= max_place) else .}
    # dplyr::filter(Place <= max_place) # cannot place because of DQ in slot ahead

  return(df)
}
