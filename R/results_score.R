#' Scores a swim meet
#'
#' Used to add a \code{Points} column with point values for each place.  Can either score "timed finals" type meets where any athlete can get any place, or "prelims-finals", type meets, where placing is restricted by prelim performance.
#'
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom dplyr inner_join
#' @importFrom dplyr summarize
#' @importFrom dplyr bind_rows
#' @importFrom dplyr matches
#' @importFrom dplyr everything
#' @importFrom dplyr distinct
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_lower
#'
#' @param results an output from \code{swim_parse}
#' @param events list of events
#' @param meet_type how to score based on \code{timed_finals}, where any place is possible, or \code{prelims_finals} where athletes are locked into heats for scoring purposes
#' @param lanes number of lanes in to the pool, for purposes of heat
#' @param scoring_heats number of heats which score (if 1 only A final scores, if 2 A and B final score etc.)
#' @param point_values a list of point values for each scoring place
#'
#' @return results with point values in a column called \code{Points}
#'
#' @examples \dontrun{
#' file <-
#' system.file("extdata", "BigTen_WSWIM_2018.pdf", package = "SwimmeR")
#' BigTenRaw <- read_results(file)
#'
#' BigTen <- swim_parse(
#'   BigTenRaw,
#'   typo = c(
#'     "^\\s{1,}\\*",
#'     "^\\s{1,}(\\d{1,2})\\s{2,}",
#'     ",\\s{1,}University\\s{1,}of",
#'     "University\\s{1,}of\\s{1,}",
#'     "\\s{1,}University",
#'     "SR\\s{2,}",
#'     "JR\\s{2,}",
#'     "SO\\s{2,}",
#'     "FR\\s{2,}"
#'   ),
#'   replacement = c(" ",
#'                   "  \\1 ",
#'                   "", "", "",
#'                   "SR ",
#'                   "JR ",
#'                   "SO ",
#'                   "FR "),
#'   avoid = c("B1G", "Pool")
#' )
#'
#' BigTen <- BigTen %>%
#'   dplyr::filter(
#'     stringr::str_detect(Event, "Time Trial") == FALSE,
#'     stringr::str_detect(Event, "Swim-off") == FALSE
#'   ) %>%
#'   dplyr::mutate(Team = dplyr::case_when(Team == "Wisconsin, Madi" ~ "Wisconsin",
#'                                           TRUE ~ Team))
#'
#' # begin results_score portion
#' df <- BigTen %>%
#'   results_score(
#'     events = unique(BigTen$Event),
#'     meet_type = "prelims_finals",
#'     lanes = 8,
#'     scoring_heats = 3,
#'     point_values = c(
#'       32, 28, 27, 26, 25, 24, 23, 22, 20, 17, 16, 15, 14, 13, 12, 11, 9, 7, 6, 5, 4, 3, 2, 1)
#'   )
#'  }
#'
#' @export

results_score <-
  function(results,
           events,
           meet_type = c("timed_finals", "prelims_finals"),
           lanes = c(4, 6, 8, 10),
           scoring_heats = c(1, 2, 3),
           point_values) {
    events <- stringr::str_to_lower(events)

    max_place <- length(point_values)

    # add 0 to list of point values
    point_values <- sort(point_values, decreasing = TRUE)
    point_values <- c(point_values, 0)

    # name point values for their respective places
    names(point_values) <- 1:length(point_values)

    ex_results <- results %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event),
                                        paste(events, collapse = "|")) == TRUE) %>%
      dplyr::select(dplyr::everything(), -dplyr::matches("Points")) %>%
      dplyr::filter(Exhibition == 1)

    results <- results %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event),
                                        paste(events, collapse = "|")) == TRUE) %>%
      dplyr::select(dplyr::everything(), -dplyr::matches("Points")) %>%
      dplyr::filter(Exhibition == 0)

    ind_results <- results %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "relay") == FALSE) %>%
      dplyr::distinct(Event, Name, Team, .keep_all = TRUE) %>%
      unique()

    relay_results <- results %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "relay") == TRUE)

    results <- dplyr::bind_rows(ind_results, relay_results)

    if (meet_type == "timed_finals") {
      # diving
      if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "diving"))) {
        diving_results <- results %>%
          dive_place(max_place = max_place)

      } else {
        diving_results  <- results[0,]
      }

      # relays
      if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "relay"))) {
        relay_results <- results %>%
          dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "relay") == TRUE) %>% # only want relays
          dplyr::group_by(Event, Team) %>%
          swim_place(max_place = max_place)

      } else {
        relay_results  <- results[0,]
      }

      # individual swimming results
      if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "diving|relay") == FALSE)) {
        ind_results <- results %>%
          dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "diving|relay") == FALSE) %>%
          dplyr::group_by(Event, Name) %>%
          swim_place(max_place = max_place)

      } else {
        ind_results  <- results[0,]
      }

      # collecting all results
      results <-
        dplyr::bind_rows(ind_results, diving_results, relay_results)  %>%
        dplyr::mutate(Heat = 1)

      # rescoring to deal with ties
      results <- results %>%
        tie_rescore(point_values = point_values, lanes = lanes) %>%
        select(-Heat) %>%
        left_join(ex_results)

      return(results)
      ### prelims_finals ####
    } else if (meet_type == "prelims_finals") {
      ### one heat ###
      if (scoring_heats == 1) {
        results <- results %>%
          dplyr::group_by(Event) %>%
          dplyr::slice(1:max_place)

        if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "diving|relay") == FALSE)) {
          ind_results <- results %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "diving|relay") == FALSE) %>%
            dplyr::group_by(Event, Name) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Heat = 1)

        } else {
          ind_results  <- results[0,]
        }

        if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "relay"))) {
          relay_results <- results %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "relay") == TRUE) %>% # only want relays
            dplyr::group_by(Event, Team) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Heat = 1)

        } else {
          relay_results  <- results[0,]
        }

        if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "diving"))) {
          diving_results <- results %>%
            dive_place(max_place = max_place) %>%
            dplyr::mutate(Heat = 1)

        } else {
          diving_results  <- results[0,]
        }
        ### two heats ###
      } else if (scoring_heats == 2) {
        results_1 <- results %>%
          dplyr::group_by(Event) %>%
          dplyr::slice(1:lanes)

        results_2 <- results %>%
          dplyr::group_by(Event) %>%
          dplyr::slice((lanes + 1):max_place)

        if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "diving|relay") == FALSE)) {
          ind_results_1 <- results_1 %>%
            # filter(Event == "Boys 500 Yard Freestyle") %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "diving|relay") == FALSE) %>%
            dplyr::group_by(Event, Name) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Heat = 1)

          ind_results_2 <- results_2 %>%
            # filter(Event == "Boys 500 Yard Freestyle") %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "diving|relay") == FALSE) %>%
            dplyr::group_by(Event, Name) %>%
            swim_place(max_place = max_place) %>%
            mutate(Place = Place + lanes) %>%
            dplyr::mutate(Heat = 2)

          ind_results <-
            dplyr::bind_rows(ind_results_1, ind_results_2)

        } else {
          ind_results  <- results[0,]
        }

        if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "relay"))) {
          relay_results_1 <- results_1 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "relay") == TRUE) %>% # only want relays
            dplyr::group_by(Event, Team) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Heat = 1)

          relay_results_2 <- results_2 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "relay") == TRUE) %>% # only want relays
            dplyr::group_by(Event, Team) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Place = Place + lanes) %>%
            dplyr::mutate(Heat = 2)

          relay_results <-
            dplyr::bind_rows(relay_results_1, relay_results_2)

        } else {
          relay_results  <- results[0,]
        }

        if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "diving"))) {
          diving_results_1 <- results_1 %>%
            dive_place(max_place = max_place) %>%
            dplyr::mutate(Heat = 1)

          diving_results_2 <- results_2 %>%
            dive_place(max_place = max_place) %>%
            dplyr::mutate(Place = Place + lanes) %>%
            dplyr::mutate(Heat = 2)

          diving_results <-
            dplyr::bind_rows(diving_results_1, diving_results_2)

        } else {
          diving_results  <- results[0,]
        }
        ### three heats ####
      } else if (scoring_heats == 3) {
        results_1 <- results %>%
          dplyr::group_by(Event) %>%
          dplyr::slice(1:lanes)

        results_2 <- results %>%
          group_by(Event) %>%
          dplyr::slice((lanes + 1):(lanes * 2))

        results_3 <- results %>%
          dplyr::group_by(Event) %>%
          dplyr::slice((1 + (lanes * 2)):max_place)

        if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "diving|relay") == FALSE)) {
          ind_results_1 <- results_1 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "diving|relay") == FALSE) %>%
            dplyr::group_by(Event, Name) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Heat = 1)

          ind_results_2 <- results_2 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "diving|relay") == FALSE) %>%
            dplyr::group_by(Event, Name) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Place = Place + lanes) %>%
            dplyr::mutate(Heat = 2)

          ind_results_3 <- results_3 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "diving|relay") == FALSE) %>%
            dplyr::group_by(Event, Name) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Place = Place + (2 * lanes)) %>%
            dplyr::mutate(Heat = 3)

          ind_results <-
            dplyr::bind_rows(ind_results_1, ind_results_2, ind_results_3)

        } else {
          ind_results  <- results[0,]
        }

        if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "diving"))) {
          diving_results_1 <- results_1 %>%
            dive_place(max_place = max_place) %>%
            dplyr::mutate(Heat = 1)

          diving_results_2 <- results_2 %>%
            dive_place(max_place = max_place) %>%
            dplyr::mutate(Place = Place + lanes) %>%
            dplyr::mutate(Heat = 2)

          diving_results_3 <- results_3 %>%
            dive_place(max_place = max_place) %>%
            dplyr::mutate(Place = Place + (2 * lanes)) %>%
            dplyr::mutate(Heat = 3)

          diving_results <-
            dplyr::bind_rows(diving_results_1, diving_results_2,
                             diving_results_3)

        } else {
          diving_results  <- results[0,]
        }

        if (any(stringr::str_detect(stringr::str_to_lower(results$Event), "relay"))) {
          relay_results_1 <- results_1 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "relay") == TRUE) %>% # only want relays
            dplyr::group_by(Event, Team) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Heat = 1)

          relay_results_2 <- results_2 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "relay") == TRUE) %>% # only want relays
            dplyr::group_by(Event, Team) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Place = Place + lanes) %>%
            dplyr::mutate(Heat = 2)

          relay_results_3 <- results_3 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(Event), "relay") == TRUE) %>% # only want relays
            dplyr::group_by(Event, Team) %>%
            swim_place(max_place = max_place) %>%
            dplyr::mutate(Place = Place + (2 * lanes)) %>%
            dplyr::mutate(Heat = 3)

          relay_results <-
            dplyr::bind_rows(relay_results_1, relay_results_2, relay_results_3)

        } else {
          relay_results  <- results[0,]
        }

      }
      results <-
        dplyr::bind_rows(ind_results, diving_results, relay_results)

      results <- results %>%
        tie_rescore(point_values = point_values, lanes = lanes) %>%
        select(-Heat) %>%
        left_join(ex_results)
    }

  }
