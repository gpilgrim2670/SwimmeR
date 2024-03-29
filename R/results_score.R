#' Scores a swim meet
#'
#' Used to add a \code{Points} column with point values for each place.  Can
#' either score "timed finals" type meets where any athlete can get any place,
#' or "prelims-finals", type meets, where placing is restricted by prelim
#' performance.
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
#' @param meet_type how to score based on \code{timed_finals}, where any place
#'   is possible, or \code{prelims_finals} where athletes are locked into heats
#'   for scoring purposes
#' @param lanes number of lanes in to the pool, for purposes of heat
#' @param scoring_heats number of heats which score (if 1 only A final scores,
#'   if 2 A and B final score etc.)
#' @param point_values Either a list of point values for each scoring place or
#'   one of the following recognized strings: \code{"hs_four_lane"},
#'   \code{"hs_six_lane"}, \code{"ncaa_six_lane"},
#'   \code{"championship_8_lane_2_heat"} or \code{"championship_8_lane_3_heat"}
#' @param max_relays_per_team the number of relays a team is allowed to score
#'   (usually 1)
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
#'       32, 28, 27, 26, 25, 24, 23, 22, 20, 17, 16, 15, 14, 13, 12, 11, 9, 7,
#'        6, 5, 4, 3, 2, 1)
#'   )
#'  }
#'
#' @export

results_score <-
  function(results,
           events = NULL,
           meet_type = c("timed_finals", "prelims_finals"),
           lanes = c(4, 6, 8, 10),
           scoring_heats = c(1, 2, 3),
           point_values,
           max_relays_per_team = 1) {

    #### testing ####
    # results <- BigTen
    # events = unique(BigTen$Event)
    # meet_type = "prelims_finals"
    # lanes = 8
    # scoring_heats = 3
    # point_values = c(
    #   32, 28, 27, 26, 25, 24, 23, 22, 20, 17, 16, 15, 14, 13, 12, 11, 9, 7, 6, 5, 4, 3, 2, 1
    # )
    # max_relays_per_team = 1

    # results = C_Comp_Prelims
    # events = unique(as.character(results$Event))
    # meet_type = "timed_finals"
    # lanes = 8
    # scoring_heats = 2
    # point_values = c(20, 17, 16, 15, 14, 13, 12, 11, 9, 7, 6, 5, 4, 3, 2, 1)
    # max_relays_per_team = 1



    # 2/14 issue with timed_finals from test - ind non diving events are being ranked upside down
    #### timed finals test ####
    # results = df_test
    # events = unique(df_test$Event)
    # meet_type = "timed_finals"
    # lanes = 8
    # scoring_heats = 2
    # point_values = c(20, 17, 16, 15, 14, 13, 12, 11, 9, 7, 6, 5, 4, 3, 2, 1)
    # max_relays_per_team = 1


    #### errors ####
    if(is.data.frame(results) == FALSE){
      stop("results must be a data frame")
    }

    if("Event" %!in% names(results)){
      stop("the results data frame must contain a called called 'Event'")
    }

    #### regularize inputs ####

    if(is.null(events)){
      event_order <- unique(results$Event)
      events <- event_order # work with events later, but need to maintain order and capitalization

      if(length(events) < 1){
        stop("No events available.  Please provide either a column named 'Event' in results data frame.")
      }
    } else {
      event_order <- events
    }

      recognized_strings <- c("hs_four_lane",
                              "hs_six_lane",
                              # "hs_eight_lane",
                              "ncaa_six_lane",
                              "championship_8_lane_2_heat",
                              "championship_8_lane_3_heat")

      point_values_list <- list(
        "hs_four_lane" = c(4, 3, 1, 0),
        "hs_six_lane" = c(6, 4, 3, 2, 1, 0),
        # "hs_eight_lane" = c(),
        "ncaa_six_lane" = c(9, 4, 3, 2, 1, 0),
        "championship_8_lane_2_heat" = c(20, 17, 16, 15, 14, 13, 12, 11, 9, 7, 6, 5, 4, 3, 2, 1),
        "championship_8_lane_3_heat" = c(32, 28, 27, 26, 25, 24, 23, 22, 20, 17, 16, 15, 14, 13, 12, 11, 9, 7, 6, 5, 4, 3, 2, 1)
      )

      if (all(length(point_values) == 1, any(point_values %in% recognized_strings))) {
        point_values <- point_values_list[point_values][[1]]

        # last point value should be 0
        if (tail(point_values, 1) != 0) {
          point_values <- c(point_values, 0)
          point_values <- sort(point_values, decreasing = TRUE)
        }

      } else if (all(is.numeric(point_values)) == TRUE) {
        point_values <- sort(point_values, decreasing = TRUE)


      } else if (all(is.numeric(point_values)) == FALSE) {
        stop(
          "point_values must be a list of numeric values or a recognized string.\n
      See ?results_score() for a list of recognized strings"
        )
      }

    #### begin actual function ####
    events <- stringr::str_to_lower(as.character(events))

    max_place <- length(point_values)

    # add 0 to list of point values
    # point_values <- sort(point_values, decreasing = TRUE)
    point_values <- c(point_values, 0)

    # name point values for their respective places
    names(point_values) <- 1:length(point_values)

    raw_results <- results
    results_cols <- names(results)

    if("Exhibition" %in% names(results)){
    ex_results <- results %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)),
                                        paste(events, collapse = "|")) == TRUE) %>%
      dplyr::select(dplyr::everything(), -dplyr::matches("Points")) %>%
      dplyr::filter(Exhibition == 1)
    } else {
      ex_results <- results[0,]
    }

    results <- results %>%
      dplyr::filter(stringr::str_detect(
        stringr::str_to_lower(as.character(Event)),
        paste(events, collapse = "|")
      ) == TRUE) %>%
      dplyr::select(dplyr::everything(),-dplyr::matches("Points")) %>%
      {
        if ("Exhibition" %in% names(results))
          dplyr::filter(., Exhibition == 0)
        else
          .
      }


    ind_results <- results %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == FALSE) %>%
      dplyr::distinct(Event, Name, Team, .keep_all = TRUE) %>%
      unique()

    relay_results <- results %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == TRUE)

    results <- dplyr::bind_rows(ind_results, relay_results)

    if (meet_type == "timed_finals") {

      # relays
      if (any(stringr::str_detect(stringr::str_to_lower(as.character(results$Event)), "relay"))) {
        relay_results <- results %>%
          dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == TRUE) %>% # only want relays
          place(
            max_place = max_place,
            event_type = "relay",
            max_relays_per_team = max_relays_per_team,
            verbose = FALSE
          )

      } else {
        relay_results  <- results[0, ]
      }

      # individual swimming results
      if (any(stringr::str_detect(stringr::str_to_lower(as.character(results$Event)), "relay") == FALSE)) {
        ind_results <- results %>%
          dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == FALSE) %>%
          place(max_place = max_place, verbose = FALSE)

      } else {
        ind_results  <- results[0, ]
      }

      # collecting all results
      results <-
        dplyr::bind_rows(ind_results,
                         # diving_results,
                         relay_results)  %>%
        dplyr::mutate(Heat = 1)

      # rescoring to deal with ties
      suppressMessages(
      results <- results %>%
        tie_rescore(point_values = point_values, lanes = lanes) %>%
        dplyr::select(-Heat) %>%
        dplyr::left_join(ex_results) %>%
        dplyr::arrange(factor(Event, levels = event_order))
      )

      results <- results[c(names(results), setdiff(results_cols, names(results)))]

      return(results)
      ### prelims_finals ####
    } else if (meet_type == "prelims_finals") {
      ### one heat ###
      if (scoring_heats == 1) {
        results <- results %>%
          dplyr::group_by(Event) %>%
          dplyr::slice(1:max_place)

        #### ind one heat ####
        if (any(stringr::str_detect(stringr::str_to_lower(as.character(results$Event)), "relay") == FALSE)) {
          ind_results <- results %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == FALSE) %>%
            place(max_place = max_place, verbose = FALSE) %>%
            dplyr::mutate(Heat = 1)

        } else {
          ind_results  <- results[0,]
        }

        #### relay one heat ####
        if (any(stringr::str_detect(stringr::str_to_lower(as.character(results$Event)), "relay"))) {
          relay_results <- results %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == TRUE) %>% # only want relays
            place(
              max_place = max_place,
              event_type = "relay",
              max_relays_per_team = max_relays_per_team,
              verbose = FALSE
            ) %>%
            dplyr::mutate(Heat = 1)

        } else {
          relay_results  <- results[0, ]
        }

        ### two heats ###
      } else if (scoring_heats == 2) {
        results_1 <- results %>%
          dplyr::group_by(Event) %>%
          dplyr::slice(1:lanes)

        results_2 <- results %>%
          dplyr::group_by(Event) %>%
          dplyr::slice((lanes + 1):max_place)

        #### ind two heats ####
        if (any(stringr::str_detect(stringr::str_to_lower(as.character(results$Event)), "relay") == FALSE)) {
          ind_results_1 <- results_1 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == FALSE) %>%
            place(max_place = max_place, verbose = FALSE) %>%
            dplyr::mutate(Heat = 1)

          ind_results_2 <- results_2 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == FALSE) %>%
            place(max_place = max_place, verbose = FALSE) %>%
            dplyr::mutate(Place = Place + lanes) %>%
            dplyr::mutate(Heat = 2)

          ind_results <-
            dplyr::bind_rows(ind_results_1, ind_results_2)

        } else {
          ind_results  <- results[0,]
        }

        #### relay two heats ####
        if (any(stringr::str_detect(stringr::str_to_lower(as.character(results$Event)), "relay"))) {
          relay_results_1 <- results_1 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == TRUE) %>% # only want relays
            place(
              max_place = max_place,
              event_type = "relay",
              max_relays_per_team = max_relays_per_team,
              verbose = FALSE
            ) %>%
            dplyr::mutate(Heat = 1)

          relay_results_2 <- results_2 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == TRUE) %>% # only want relays
            place(
              max_place = max_place,
              event_type = "relay",
              max_relays_per_team = max_relays_per_team,
              verbose = FALSE
            ) %>%
            dplyr::mutate(Place = Place + lanes) %>%
            dplyr::mutate(Heat = 2)

          relay_results <-
            dplyr::bind_rows(relay_results_1, relay_results_2)

        } else {
          relay_results  <- results[0, ]
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

        #### ind three heats ####
        if (any(stringr::str_detect(stringr::str_to_lower(as.character(results$Event)), "relay") == FALSE)) {
          ind_results_1 <- results_1 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == FALSE) %>%
            place(max_place = max_place, verbose = FALSE) %>%
            dplyr::mutate(Heat = 1)

          ind_results_2 <- results_2 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == FALSE) %>%
            place(max_place = max_place, verbose = FALSE) %>%
            dplyr::mutate(Place = Place + lanes) %>%
            dplyr::mutate(Heat = 2)

          ind_results_3 <- results_3 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == FALSE) %>%
            place(max_place = max_place, verbose = FALSE) %>%
            dplyr::mutate(Place = Place + (2 * lanes)) %>%
            dplyr::mutate(Heat = 3)

          ind_results <-
            dplyr::bind_rows(ind_results_1, ind_results_2, ind_results_3)

        } else {
          ind_results  <- results[0,]
        }

        #### relay three heats ####
        if (any(stringr::str_detect(stringr::str_to_lower(as.character(results$Event)), "relay"))) {
          relay_results_1 <- results_1 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == TRUE) %>% # only want relays
            place(
              max_place = max_place,
              event_type = "relay",
              max_relays_per_team = max_relays_per_team,
              verbose = FALSE
            ) %>%
            dplyr::mutate(Heat = 1)

          relay_results_2 <- results_2 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == TRUE) %>% # only want relays
            place(
              max_place = max_place,
              event_type = "relay",
              max_relays_per_team = max_relays_per_team,
              verbose = FALSE
            ) %>%
            dplyr::mutate(Place = Place + lanes) %>%
            dplyr::mutate(Heat = 2)

          relay_results_3 <- results_3 %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == TRUE) %>% # only want relays
            place(
              max_place = max_place,
              event_type = "relay",
              max_relays_per_team = max_relays_per_team,
              verbose = FALSE
            ) %>%
            dplyr::mutate(Place = Place + (2 * lanes)) %>%
            dplyr::mutate(Heat = 3)

          relay_results <-
            dplyr::bind_rows(relay_results_1, relay_results_2, relay_results_3)

        } else {
          relay_results  <- results[0, ]
        }

      }

      results_new <-
        dplyr::bind_rows(ind_results,
                         # diving_results,
                         relay_results)

      suppressMessages(
      results_old <- raw_results %>%
        dplyr::anti_join(results_new, by = c("Name", "Team", "Event")) %>%
        dplyr::anti_join(ex_results, by = c("Name", "Team", "Event"))
      )

      results_old_swimming_ind <- results_old %>%
        dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay") == FALSE) %>%
        place()

      results_old_swimming_relay <- results_old %>%
        dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "relay")) %>%
        place()

      # results_old_diving <- results_old %>%
      #   dplyr::filter(stringr::str_detect(stringr::str_to_lower(as.character(Event)), "diving")) %>%
      #   dive_place(verbose = FALSE)

      results_old <-
        dplyr::bind_rows(results_old_swimming_ind,
                         results_old_swimming_relay
                         # results_old_diving
                         ) %>%
        dplyr::mutate(Place = Place + max_place) %>%
        dplyr::mutate(Points = NA)

      suppressMessages(
      results_new <- results_new %>%
        tie_rescore(point_values = point_values, lanes = lanes) %>%
        dplyr::select(-Heat)
      )

      results_new <- results_new %>%
        dplyr::bind_rows(results_old, ex_results)
    }

    results_new <- results_new %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.nan(.), NA))) %>%
      dplyr::select(
        Place,
        contains("Name"),
        contains("Age"),
        contains("Team"),
        contains("Prelims"),
        Finals,
        Points,
        dplyr::everything()
      ) %>%
      dplyr::arrange(factor(Event, levels = event_order))
    # dplyr::distinct(Name, Team, Event, .keep_all = TRUE)

    return(results_new)

  }
