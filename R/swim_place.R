#' Add places to swimming results
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
#'
#' @param df a data frame with results from \code{swim_parse}, including only
#'   swimming results (not diving)
#' @param time_col the name of a column in \code{df} containing times on which
#'   to place (order) performances.  Default is \code{Finals}
#' @param max_place highest place value that scores
#' @param event_type either \code{"ind"} for individual or \code{"relay"} for
#'   relays
#' @param max_relays_per_team an integer value denoting the number of relays a
#'   team may score (usually 1)
#' @param keep_nonscoring are athletes in places greater than \code{max_place}
#'   be retained in the data frame.  Either \code{TRUE} or \code{FALSE}
#' @param verbose should warning messages be posted.  Default is \code{TRUE} and
#'   should rarely be changed.
#' @return a data frame modified so that places have been appended based on
#'   swimming time
#'
#' @seealso \code{swim_place} is a helper function used inside of
#'   \code{results_score}
#'
#' @examples
#'   df <- data.frame(Place = c(1, 1, 1),
#'                Name = c("Sally Swimfast", "Bonnie Bubbles", "Kylie Kicker"),
#'                Team = c("KVAC", "UBAM", "MERC"),
#'                Event = rep("Women 200 Freestyle", 3),
#'                Prelims = c("2:00.00", "1:59.99", "2:01.50"),
#'                Finals = c("1:58.00", "1:59.50", "2:00.50"),
#'                Meet = c("Summer 2021", "Fall 2020", "Champs 2020"))
#'
#' df %>%
#'   swim_place()
#'
#' df %>%
#'   swim_place(time_col = Prelims)
#'
#' df %>%
#'   swim_place(time_col = "Prelims")
#'
#' @export

swim_place <- function(df,
                       time_col = Finals,
                       max_place = NULL,
                       event_type = "ind",
                       max_relays_per_team = 1,
                       keep_nonscoring = TRUE,
                       verbose = TRUE
                       ) {

  #### regularize time_col ####
  time_col <- dplyr::ensym(time_col)

  #### max relays ####
  if(max_relays_per_team %% 1 > 0){
    stop("max_relays_per_team must be an integer value greater than 0")
  }
  max_relays_per_team <- as.integer(max_relays_per_team)

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

  #### event_type ####
  if (event_type %!in% c("ind", "relay")) {
    stop("event_type must be either 'ind' or 'relay'")
  }

  if (all(event_type == "relay", max_relays_per_team < 1)) {
    stop(
      "if event_type = 'relay' then max_relays_per_team must be an integer greater than or equal to 1"
    )
  }

  if(all(event_type == "ind", "Name" %!in% names(df))){
    stop("df must contain a column named Name if event_type = 'ind'")
  }

  if(all(event_type == "relay", "Team" %!in% names(df))){
    stop("df must contain a column named Team if event_type = 'relay'")
  }

  #### required columns ####
  if(as.character(time_col) %!in% names(df)){
    stop("time_col must be a column of times in df")
  }

  if("Event" %!in% names(df)){
    stop("df must contain a column named Event")
  }

  #### actual function ####
  if(any(stringr::str_detect(stringr::str_to_lower(as.character(df$Event)), "diving")) == FALSE){

  df <- df %>%
    {
      if (event_type == "ind")
        dplyr::group_by(., Event, Name)
      else
        dplyr::group_by(., Event, Team)
    } %>%
    dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "div(e|ing)") == FALSE) %>%
    {
      if (event_type == "ind")
        dplyr::slice(., 1)
      else
        dplyr::slice(., max_relays_per_team)
    } %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Event) %>%
    dplyr::mutate(Time_sec = {{time_col}}) %>%
    dplyr::mutate(Time_sec = sec_format(Time_sec)) %>%
    dplyr::mutate(Place = rank(Time_sec, ties.method = "min")) %>% # places, low number wins
    dplyr::select(-Time_sec) %>%
    dplyr::arrange(Place) %>%
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
    }
  } else if (verbose == TRUE) {
    message("df does not have column called Event.  No places determined.")
  }

  return(df)
}
