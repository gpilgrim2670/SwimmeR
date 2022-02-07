# In progress.  The for loop below does work but obviously needs to be cleaned
# up and sent to map.  Need to break events into Event_A, Event_B etc. for
# actual use.  Will also need to accept scoring system.  Do keywords for scoring
# systems, for rescoring functions as well.  Maybe an events-back-to-back
# protocol as well.  Also something to exclude a particular athlete from a
# particular event

# file <- system.file("extdata", "RIT_TopTimes_FS2021.pdf", package = "SwimmeR")
#
#  #### test dfs ####
# RIT_TopTimes_2021 <- file %>%
#   read_results() %>%
#   swim_parse() %>%
#   rename(Finals = Result)
#
# RIT_IC <- "https://s3.amazonaws.com/sidearm.sites/bombers.ithaca.edu/documents/2021/11/20/11_20_21_RIT_IC_Results.pdf" %>%
#   read_results() %>%
#   swim_parse()
#
# IC <- RIT_IC %>%
#   filter(Team == "Ithaca College-NI") %>%
#   filter(str_detect(Event, "Div") == FALSE) %>%
#   filter(str_detect(Event, "Relay") == FALSE) %>%
#   mutate(Event = str_remove(Event, " Yard"),
#          Event = str_remove(Event, "style"),
#          Event = str_remove(Event, "stroke"),
#          Event = str_replace(Event, "Butterfly", "Fly"))

#' Determine optimal entries against a given opponent lineup
#'
#' @importFrom dplyr add_row
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr group_modify
#' @importFrom dplyr group_split
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom stringr str_detect
#'
#' @param df a data frame of times for the team to be entered.  Must contain
#'   column \code{Event} with the same event naming convention as \code{op_df},
#'   a column with name matching \code{time_col} containing times or diving
#'   scores, and a column called \code{Name} containing athlete names
#' @param op_df a data frame containing the opponent lineup.  Must contain
#'   column \code{Event} with the same event naming convention as \code{df},
#'   a column with name matching \code{time_col} containing times or diving
#'   scores, and a column called \code{Name} containing athlete names
#' @param point_values either a recognized string or a list of numeric values
#'   containing the points awarded by place.  Recognized strings are
#'   \code{"hs_four_lane"}, \code{"hs_six_lane"}, \code{"ncaa_six_lane"}
#' @param time_col the name of a column, present in both \code{df} and
#'   \code{op_df} that contains times and/or diving scores
#' @param events a list of events.  If no list is entered then \code{events}
#'   will be taken from \code{unique(op_df$Event)}
#' @return a data frame of optimal entries based on \code{df} and \code{op_df}

determine_entries <- function(df, op_df, point_values, time_col, events = NULL, max_entries = NULL){

  # df = RIT_TopTimes_2021
  # op_df = IC
  # point_values = "ncaa_six_lane"
  # time_col = "Finals"
  # events = NULL
  # max_entries = NULL

  #### regularize time_col ####
  time_col <- dplyr::ensym(time_col)

  recognized_strings <- c("hs_four_lane",
                          "hs_six_lane",
                          # "hs_eight_lane",
                          "ncaa_six_lane")

  point_values_list <- list(
    "hs_four_lane" = c(4, 3, 1, 0),
    "hs_six_lane" = c(6, 4, 3, 2, 1, 0),
    # "hs_eight_lane" = c(),
    "ncaa_six_lane" = c(9, 4, 3, 2, 1, 0)
  )

  if (point_values %in% recognized_strings) {
    point_values <- point_values_list[point_values][[1]]

    # last point value should be 0
    if (tail(point_values, 1) != 0) {
      point_values <- c(point_values, 0)
    }

    # add check for even length of point_value following zero addition

  } else if (all(is.numeric(point_values)) == TRUE) {
    point_values <- sort(point_values, decreasing = TRUE)


  } else if (all(is.numeric(point_values)) == FALSE) {
    stop(
      "point_values must be a list of numeric values or a recognized string.\n
      See ?determine_entries() for a list of recognized strings"
    )
  }

  if (is.null(events)) {
    if ("Event" %!in% names(op_df)) {
      stop(
        "Either specify a list of events to the events argument or provide a column in op_df called 'Event'"
      )
    }
    events <- data.frame(Event = unique(op_df$Event))
  } else {
    events <- data.frame(Event = unique(events))
  }

  #### setup ####

  df <- df %>%
    dplyr::filter(is.na(Name) == FALSE) %>%
    dplyr::filter(Event %!in% events) %>%
    dplyr::mutate(Time = {{time_col}}) %>%
    dplyr::mutate(Time = sec_format(Time))

  op_df <- op_df %>%
    dplyr::filter(is.na(Name) == FALSE) %>%
    dplyr::filter(Event %!in% events) %>%
    dplyr::mutate(Time = {{time_col}}) %>%
    dplyr::mutate(Time = sec_format(Time))

  event_order <- unique(op_df$Event)

  #### actual function ####

  Point_Values <- data.frame(Points = point_values[c(TRUE, FALSE)])

  #### make sure all events in op_df have the same number of entries, equal to
  #### the maximum number of entries in any event
  op_df_split <- op_df %>%
    {
      if ("Exhibition" %in% names(op_df))
        dplyr::filter(., Exhibition == 0)
    } %>%
    dplyr::group_split(Event)

  if (is.null(max_entries)) {
    max_entries <- max(unlist(purrr::map(op_df_split, nrow)))
  }

  to_add <-
    op_df_split[unlist(map(op_df_split, nrow)) < max_entries]

  if (length(to_add) > 0) {
    to_add <- to_add %>%
      purrr::map(function(x)
        x %>%
          dplyr::group_by(Event) %>%
          dplyr::group_modify(~ dplyr::add_row(.x, Time = 99)))

    no_add <-
      op_df_split[unlist(purrr::map(op_df_split, nrow)) == max_entries]

    op_df <- dplyr::bind_rows(c(to_add, no_add))
  }

  op_df <- events %>%
    dplyr::right_join(op_df, by  = "Event") %>%
    dplyr::select(-dplyr::contains("Points")) %>%
    dplyr::filter(Exhibition < 1) %>%
    dplyr::group_by(Event) %>%
    dplyr::mutate(Points = Point_Values$Points) %>%
    dplyr::arrange(dplyr::desc(Points), Time) %>%
    dplyr::mutate(Event_Points = paste(Event, Points, sep = "_"))

  Entries <- list()

  competitors <- unique(df$Name)

  Times_Competed <- list_to_list_names(competitors, 0)

  Events_Competed <- list_to_list_names(competitors, c("X", "X", "X"))

  test <- determine_entries_helper(
    i,
    df_helper = df,
    op_df_helper = op_df,
    end_seq = length(op_df$Event_Points),
    max_ind_entries = max_entries,
    time_col_helper = time_col
  )

  Events_Competed <- test[[length(test)]]

  test <- test[1:length(op_df$Event_Points)]

  test <- test %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(Event, Time) %>%
    dplyr::mutate(Rank = as.numeric(Rank))

  test <- test %>%
    dplyr::mutate(Event = factor(Event, levels = event_order)) %>%
    dplyr::arrange(Event)

  athletes_remaining <- Events_Competed[which(purrr::map(purrr::map(Events_Competed, stringr::str_detect, "X"), any) == TRUE)]

  test <- determine_entries_helper_2(
    i,
    df_helper = df,
    in_progress_entries_df = test,
    events_competed_helper = Events_Competed,
    max_ind_entries = max_entries
  )

  test <- test %>%
    dplyr::select(-Time) %>%
    dplyr::mutate(Event = factor(Event, levels = event_order)) %>%
    dplyr::arrange(Event, Rank)

  return(test)

}

#### Helper 1 - Min Power ####
#' Determine optimal entries against a given opponent lineup
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom purrr map
#'
#' @param i a sequential list of numbers incremented by 1.  Used to index
#'   function.
#' @param df_helper a data frame of times for the team to be entered.  Must
#'   contain column \code{Event} with the same event naming convention as
#'   \code{op_df}, a column with name matching \code{time_col} containing times
#'   or diving scores, and a column called \code{Name} containing athlete names
#' @param op_df_helper a data frame containing the opponent lineup.  Must
#'   contain column \code{Event} with the same event naming convention as
#'   \code{df}, a column with name matching \code{time_col} containing times or
#'   diving scores, and a column called \code{Name} containing athlete names
#' @param max_ind_entries a numeric value denoting the maximum number of
#'   individual events that may be entered by a single athlete
#' @return xxx

determine_entries_helper <-
  function(i,
           df_helper,
           op_df_helper,
           end_seq,
           max_ind_entries = 2,
           time_col_helper = time_col) {

  # df_helper <- df
  # op_df_helper <- op_df
  # max_ind_entries <- 3
  # i <- 1

  Entries <- list()

  competitors <- unique(df_helper$Name)

  Times_Competed <- list_to_list_names(competitors, 0)

  Events_Competed <- list_to_list_names(competitors, c("X", "X", "X"))

   for (i in 1:end_seq){

  e <- op_df_helper$Event_Points[i]
  e_no_points <- stringr::str_remove(e, "_.*")

  op_result <- op_df_helper %>%
    dplyr::filter(Event_Points == e) %>%
    dplyr::pull({{time_col_helper}}) %>%
    sec_format()

  entry <- df_helper %>%
    dplyr::filter(Name %!in% names(Times_Competed[which(Times_Competed > max_ind_entries - 1)])) %>%
    dplyr::filter(Name %!in% stringr::str_remove(names(which(
      unlist(
        purrr::map(Events_Competed, stringr::str_detect, e_no_points)
      )
    )), "\\d.*$")) %>%
    dplyr::rowwise() %>%
    dplyr::filter(Event == e_no_points, Time < op_result) %>%
    dplyr::ungroup()

  if (nrow(entry) > 1) {
    entry <-  entry %>%
      dplyr::filter(Name %!in% names(Times_Competed[which(Times_Competed > max_ind_entries - 1)])) %>%
      dplyr::filter(Name %!in% stringr::str_remove(names(which(
        unlist(
          purrr::map(Events_Competed, stringr::str_detect, e_no_points)
        )
      )), "\\d.*$")) %>%
      dplyr::ungroup() %>%
      dplyr::slice(which.max(Time))
  }

  if (nrow(entry) == 0) {
    entry <- df_helper %>%
      dplyr::filter(Event == e_no_points) %>%
      dplyr::filter(Name %!in% names(Times_Competed[which(Times_Competed > max_ind_entries - 1)])) %>%
      dplyr::filter(Name %!in% stringr::str_remove(names(which(
        unlist(
          purrr::map(Events_Competed, stringr::str_detect, e_no_points)
        )
      )), "\\d.*$")) %>%
      dplyr::ungroup() %>%
      dplyr::slice(which.max(Time))
  }

  if(Times_Competed[entry$Name][[1]] > max_ind_entries - 1){

    entry <- df_helper %>%
      dplyr::filter(Name != entry$Name[1]) %>%
      dplyr::filter(Name %!in% stringr::str_remove(names(which(
        unlist(
          purrr::map(Events_Competed, stringr::str_detect, e_no_points)
        )
      )), "\\d.*$")) %>%
      dplyr::ungroup() %>%
      dplyr::rowwise() %>%
      dplyr::filter(Event == e_no_points, Time < op_result) %>%
      dplyr::ungroup()
  }

  if (nrow(entry) > 1) {
    entry <-  entry %>%
      dplyr::filter(Name %!in% names(Times_Competed[which(Times_Competed > max_ind_entries - 1)])) %>%
      dplyr::filter(Name %!in% stringr::str_remove(names(which(
        unlist(
          purrr::map(Events_Competed, stringr::str_detect, e_no_points)
        )
      )), "\\d.*$")) %>%
      dplyr::slice(which.max(Time))
  }

  if (nrow(entry) == 0) {
    entry <- df_helper %>%
      dplyr::filter(Event == e_no_points) %>%
      dplyr::filter(Name %!in% names(Times_Competed[which(Times_Competed > max_ind_entries - 1)])) %>%
      dplyr::filter(Name %!in% stringr::str_remove(names(which(
        unlist(
          purrr::map(Events_Competed, stringr::str_detect, e_no_points)
        )
      )), "\\d.*$")) %>%
      dplyr::ungroup() %>%
      dplyr::slice(which.max(Time))
  }

  if(nrow(entry) == 0){
    entry <- data.frame(Name = paste0("NA", i),
                        Time = NA,
                        Event = e_no_points)
  }


  Times_Competed[entry$Name][[1]] <- Times_Competed[entry$Name][[1]] + 1
  Events_Competed[entry$Name][[1]][min(which(stringr::str_detect(Events_Competed[entry$Name][[1]], "X") == TRUE))] <-
    e_no_points

  Entries[[i]] <- entry %>%
    dplyr::mutate(Counter = i)
   }

  Entries[[i + end_seq]] <- Events_Competed

return(Entries)
return(Events_Competed)
return(Times_Competed)

# }
  }

#### Helper 2 - Overpowered ####

#' Assign overpowered entries
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom purrr map
#'
#' @param i a sequential list of numbers incremented by 1.  Used to index
#'   function.
#' @param df_helper a data frame of all times to be entered for a given team.
#'   Must contain column \code{Event} with the same event naming convention as
#'   \code{op_df}, a column with name matching \code{time_col} containing times
#'   or diving scores, and a column called \code{Name} containing athlete names
#' @param in_progress_entries_df a data frame containing the output of
#'   \code{determine_entries_helper}, which is the minimum power set of entries
#' @param events_competed_helper a list of lists containing all the events a
#'   given athlete is competing in.  Sub-lists are named with the athlete name.
#' @param max_ind_entries a numeric value denoting the maximum number of
#'   individual events that may be entered by a single athlete
#' @return a data frame containing entries updated to be as powerful as possible

determine_entries_helper_2 <-
  function(i,
           df_helper,
           in_progress_entries_df,
           # athlete_ranks_helper,
           events_competed_helper = Events_Competed,
           max_ind_entries = 3) {

    # df_helper <- df
    # in_progress_entries_df <- test
    # athletes_remaining_helper <- athletes_remaining
    # events_competed_helper <- Events_Competed
    # max_ind_entries <- 3

    Entries <- list()

    athlete_ranks <- df_helper %>%
      dplyr::select(Rank, Name, Event) %>%
      dplyr::mutate(Rank = as.numeric(Rank))

    athletes_remaining_helper <-
      events_competed_helper[which(purrr::map(purrr::map(events_competed_helper, stringr::str_detect, "X"), any) == TRUE)]

    events_remaining <- stack(purrr::map(purrr::map(events_competed_helper, stringr::str_count, "X"), sum)) %>%
      dplyr::rename(Events_Remaining = values, Name = ind)

    rank_helper <- athlete_ranks %>%
      dplyr::filter(Name %in% names(athletes_remaining_helper)) %>%
      dplyr::mutate(Event = factor(Event, levels = unique(in_progress_entries_df$Event))) %>%
      dplyr::left_join(events_remaining, by = "Name")

    rank_helper <- rank_helper %>%
      dplyr::filter(is.na(Event) == FALSE) %>%
      dplyr::group_by(Name) %>%
      dplyr::summarize(Ranks = list(Rank)) %>%
      dplyr::right_join(rank_helper, by = "Name") %>%
      dplyr::arrange(Rank)

    end_seq <- length(unique(in_progress_entries_df$Event))

    for (i in 1:end_seq){
    # for (i in 17:24){

    e <- unique(in_progress_entries_df$Event)[i]
    # print(paste("interation", i))
    # print(e)

    e_df <- in_progress_entries_df %>%
      dplyr::filter(Event == e) %>%
      dplyr::arrange(dplyr::desc(Rank))

    e_rank_helper <- rank_helper %>%
      dplyr::filter(Name %in% names(athletes_remaining_helper)) %>%
      dplyr::filter(Name %!in% c(e_df$Name)) %>%
      dplyr::filter(Event == e) %>%
      dplyr::rowwise() %>%
      dplyr::filter(max(sort(Ranks)[1:Events_Remaining]) >= Rank) %>%
      dplyr::arrange(Rank) %>%
      ungroup() %>%
      head(max_ind_entries)

    # e_df
    # e_rank_helper

    ### remove elements of e_df as things progress and add check in the if statements for e_df length ###

    if (nrow(e_rank_helper) == 0 & i > 1) {
      Entries[[i]] <- Entries[[i - 1]]

    } else if (nrow(e_rank_helper) == 0 & i == 1) {
      Entries[[i]] <- in_progress_entries_df

    } else {
      ##### first replacement #######
      if (all(length(e_rank_helper$Rank) >= 1 ,
              e_df$Rank[1] > e_rank_helper$Rank[1])) {
        # need this as number of team entries

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 1,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 1,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i - 1]] %>%
            replacement_entries(
              j = 1,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-1, ]

        events_competed_helper[e_rank_helper$Name[1]][[1]][min(which(
          stringr::str_detect(events_competed_helper[e_rank_helper$Name[1]][[1]], "X") == TRUE
        ))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 1,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      } else if (all(length(e_rank_helper$Rank) >= 1 ,
                     e_df$Rank[2] > e_rank_helper$Rank[1])) {
        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 1,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 2,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i - 1]] %>%
            replacement_entries(
              j = 2,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-2, ]

        events_competed_helper[e_rank_helper$Name[1]][[1]][min(which(
          stringr::str_detect(events_competed_helper[e_rank_helper$Name[1]][[1]], "X") == TRUE
        ))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 1,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      } else if (all(length(e_rank_helper$Rank) >= 1 ,
                     e_df$Rank[3] > e_rank_helper$Rank[1])) {
        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 1,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 3,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i-1]] %>%
            replacement_entries(
              j = 3,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-3, ]

        events_competed_helper[e_rank_helper$Name[1]][[1]][min(which(
          stringr::str_detect(events_competed_helper[e_rank_helper$Name[1]][[1]], "X") == TRUE
        ))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 1,
            e_helper = e,
            events_remaining_helper = events_remaining
          )
      }

      # print("completed first replacement")

      ######### second replacement: k = 2 ##########

      if (all(
        length(e_rank_helper$Rank) >= 2,
        length(e_df$Rank) >= 1,
        e_df$Rank[1] > e_rank_helper$Rank[2]
      )) {
        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 2,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 1,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 1,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-1, ]

        events_competed_helper[e_rank_helper$Name[2]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[2]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 2,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      } else if (all(
        length(e_rank_helper$Rank) >= 2,
        length(e_df$Rank) >= 2,
        e_df$Rank[length(e_df$Rank) - 1] > e_rank_helper$Rank[2]
      )) {

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 2,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 2,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 2,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-2, ]

        events_competed_helper[e_rank_helper$Name[2]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[2]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 2,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      } else if (all(
        length(e_rank_helper$Rank) >= 2,
        length(e_df$Rank) >= 3,
        e_df$Rank[3] > e_rank_helper$Rank[2]
      )) {

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 2,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 3,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 3,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df

            )
        }

        e_df <- e_df[-3, ]

        events_competed_helper[e_rank_helper$Name[2]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[2]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 2,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      } else if (all(
        length(e_rank_helper$Rank) >= 2,
        length(e_df$Rank) >= 4,
        e_df$Rank[3] > e_rank_helper$Rank[2]
      )) {

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 2,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 4,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 4,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-4, ]

        events_competed_helper[e_rank_helper$Name[2]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[2]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 2,
            e_helper = e,
            events_remaining_helper = events_remaining
          )


      } else {
        e_rank_helper <- data.frame(Rank = 0)
      }

      # print("completed second replacement")

      ######### third replacement: k = 3 ##########

      if (all(
        length(e_rank_helper$Rank) >= 3,
        length(e_df$Rank) >= 1,
        e_df$Rank[1] > e_rank_helper$Rank[3]
      )) {

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 3,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 1,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )

        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 1,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-1, ]

        events_competed_helper[e_rank_helper$Name[3]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[3]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 3,
            e_helper = e,
            events_remaining_helper = events_remaining
          )
      } else if (all(
        length(e_rank_helper$Rank) >= 3,
        length(e_df$Rank) >= 2,
        e_df$Rank[2] > e_rank_helper$Rank[3]
      )) {

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 3,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 2,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 2,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-2, ]

        events_competed_helper[e_rank_helper$Name[3]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[3]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 3,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      } else if (all(
        length(e_rank_helper$Rank) >= 3,
        length(e_df$Rank) >= 3,
        e_df$Rank[1] > e_rank_helper$Rank[3]
      )) {

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 3,
                              e_helper = e)


        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 3,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 3,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-3, ]

        events_competed_helper[e_rank_helper$Name[3]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[3]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 3,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      } else if (all(
        length(e_rank_helper$Rank) >= 3,
        length(e_df$Rank) >= 4,
        e_df$Rank[1] > e_rank_helper$Rank[3]
      )) {
        # need this as number of team entries

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 3,
                              e_helper = e)


        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 4,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 4,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-4, ]

        events_competed_helper[e_rank_helper$Name[3]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[3]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 3,
            e_helper = e,
            events_remaining_helper = events_remaining
          )


      # print("completed third replacement")

    }

    ######### forth replacement: k = 4 ##########


      if (all(
        length(e_rank_helper$Rank) >= 4,
        length(e_df$Rank) >= 1,
        e_df$Rank[1] > e_rank_helper$Rank[4]
      )) {
        # need this as number of team entries

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 4,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 1,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 1,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-1, ]

        events_competed_helper[e_rank_helper$Name[4]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[4]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 4,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      } else if (all(
        length(e_rank_helper$Rank) >= 4,
        length(e_df$Rank) >= 2,
        e_df$Rank[2] > e_rank_helper$Rank[4]
      )) {

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 4,
                              e_helper = e)

        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 2,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 2,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-2, ]

        events_competed_helper[e_rank_helper$Name[4]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[3]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 4,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      } else if (all(
        length(e_rank_helper$Rank) >= 4,
        length(e_df$Rank) >= 3,
        e_df$Rank[1] > e_rank_helper$Rank[4]
      )) {

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 4,
                              e_helper = e)


        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 3,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 3,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-3, ]

        events_competed_helper[e_rank_helper$Name[4]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[3]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 4,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      } else if (all(
        length(e_rank_helper$Rank) >= 4,
        length(e_df$Rank) >= 4,
        e_df$Rank[1] > e_rank_helper$Rank[4]
      )) {

        row_to_add <- df_helper %>%
          generate_row_to_add(e_rank_helper_2 = e_rank_helper,
                              k = 4,
                              e_helper = e)


        if (is.null(Entries[i][[1]]) & i == 1) {
          Entries[[i]] <- in_progress_entries_df %>%
            replacement_entries(
              j = 4,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        } else {
          Entries[[i]] <- Entries[[i]] %>%
            replacement_entries(
              j = 4,
              row_to_add_replacement = row_to_add,
              e_df_replacement = e_df
            )
        }

        e_df <- e_df[-4, ]

        events_competed_helper[e_rank_helper$Name[4]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[3]][[1]], "X") == TRUE))] <-
          paste(e)

        athletes_remaining_helper <-
          events_competed_helper[which(purrr::map(
            purrr::map(events_competed_helper, stringr::str_detect, "X"),
            any
          ) == TRUE)]

        events_remaining <-
          stack(purrr::map(
            purrr::map(events_competed_helper, stringr::str_count, "X"),
            sum
          )) %>%
          dplyr::rename(Events_Remaining = values, Name = ind)

        rank_helper <- rank_helper %>%
          update_rank_helper(
            e_rank_helper_2 = e_rank_helper,
            k = 4,
            e_helper = e,
            events_remaining_helper = events_remaining
          )

      }

      # print("completed forth replacement")

    }

    if(length(Entries) == 0 & i == 1){
      Entries[[1]] <- in_progress_entries_df
    } else if(length(Entries) == 0){
      Entries[[i]] <- Entries[[i-1]]
    }

    #### remove empty data frames ####
    if(is.null(Entries[i][[1]]) & i > 1){
      Entries[i][[1]] <- Entries[i-1][[1]]
    } else if(is.null(Entries[i][[1]]) & i == 1){
      Entries[i][[1]] <- in_progress_entries_df
    }

    # dplyr::all_equal(Entries_Test, Entries[i][[1]])
    # View(Entries[i][[1]])

    }

    Entries <- Entries[length(unique(in_progress_entries_df$Event))][[1]] %>%
      dplyr::select(-Counter)

    return(Entries)
  }

#### Replacement Entries Function ####

#' Replaces superseded rows
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr rowwise
#' @importFrom dplyr ungroup
#'
#' @param x a data frame of entries, either df_helper_2 or Entries
#' @param j an integer denoting which element of e_df_replacement is
#'   under test for removal.  Should be 1, 2, 3 or 4 depending on the minimum
#'   number of entries
#' @param row_to_add_replacement a row containing an improved entry that should
#'   be added to x
#' @param e_df_replacement a data frame of entries that may be replaced
#' @return a data frame containing entries updated to include new rows from
#'   row_to_add_replacement and to not contain rows from e_df_replacement, based
#'   on j

replacement_entries <- function(x, j_helper, row_to_add_replacement, e_df_replacement){

  x <- x %>%
      dplyr::rowwise() %>%
      dplyr::filter(!all(Event == as.character(e_df_replacement$Event), Name == e_df_replacement$Name[j_helper])) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(row_to_add_replacement) %>%
      dplyr::arrange(Event)

  return(x)

}

#### generate_add_row ####

#' Create a one-line data frame containing an entry to be appended to an
#' in-progress data frame of all entries
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#'
#' @param df_helper_2 a  master data frame of athlete ranks by event
#' @param e_rank_helper a data frame of candidate athlete entries to add to a
#'   given event
#' @param k an integer denoting which element of e_rank_helper is under
#'   evaluation for addition.  Should be 1, 2, 3 or 4 depending on the minimum
#'   number of entries
#' @param e_helper the event for which entries are being evaluated
#' @return a one row data frame containing an improved entry

generate_row_to_add <- function(df_helper_2, e_rank_helper_2, k, e_helper){

  x <- df_helper_2 %>%
    dplyr::mutate(Rank = as.numeric(Rank)) %>%
    dplyr::filter(Name == e_rank_helper_2$Name[k],
           Event == e_helper)

  return(x)
}

#### update_rank_helper ####

#' Create a one-line data frame containing an entry to be appended to an
#' in-progress data frame of all entries
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr right_join
#' @importFrom dplyr summarize
#' @importFrom purrr reduce
#'
#' @param x a  master data frame of athlete ranks by event
#' @param e_rank_helper a data frame of candidate athlete entries to add to a
#'   given event
#' @param k an integer denoting which element of e_rank_helper is under
#'   evaluation for addition.  Should be 1, 2, 3 or 4 depending on the minimum
#'   number of entries
#' @param e_helper the event for which entries are being evaluated
#' @param events_remaining_helper a data frame with two columns, \code{Name} and
#'   \code{Events_Remaining}
#' @return a one row data frame containing an improved entry

update_rank_helper <-
  function(rank_helper_2,
           e_rank_helper_2,
           k,
           e_helper,
           events_remaining_helper) {

    join_names <-
      purrr::reduce(list(
        names(rank_helper_2),
        names(events_remaining_helper),
        c("Name", "Events_Remaining", "Event")
      ), intersect)

    x <- rank_helper_2 %>%
      dplyr::ungroup() %>%
      dplyr::rowwise() %>%
      dplyr::filter(!all(Name == e_rank_helper_2$Name[k] &
                           Event == e_helper)) %>%
      dplyr::left_join(events_remaining_helper, by = join_names) %>%
      dplyr::arrange(Event)

    x_sum <- x %>%
      dplyr::group_by(Name) %>%
      dplyr::summarize(Ranks = list(Rank))

    join_names_2 <-
      purrr::reduce(list(names(x), names(x_sum), c("Name", "Ranks", "Event")), intersect)

    x <- x_sum %>%
      dplyr::right_join(x, by = join_names_2) %>%
      dplyr::ungroup() %>%
      dplyr::filter(is.na(Event) == FALSE,
                    is.null(Ranks) == FALSE)


    return(x)
  }



  # if (is.null(Entries[index_number][[1]]) & i == 1) {
  #   Entries[[index_number]] <- in_progress_entries_df %>%
  #     replacement_entries(
  #       j_helper = j,
  #       row_to_add_replacement = row_to_add,
  #       e_df_replacement = e_df
  #     )
  # } else if (index_number == 1) {
  #   Entries[[index_number]] <- Entries[[index_number]] %>%
  #     replacement_entries(
  #       j_helper = j,
  #       row_to_add_replacement = row_to_add,
  #       e_df_replacement = e_df
  #     )
  # } else {
  #   Entries[[index_number]] <- Entries[[index_number]] %>%
  #     replacement_entries(
  #       j_helper = j,
  #       row_to_add_replacement = row_to_add,
  #       e_df_replacement = e_df
  #     )
  # }

#   e_df <- e_df[-j,]
#
#   events_competed_helper[e_rank_helper$Name[k]][[1]][min(which(stringr::str_detect(events_competed_helper[e_rank_helper$Name[k]][[1]], "X") == TRUE))] <-
#     e
#
#   athletes_remaining_helper <-
#     events_competed_helper[which(purrr::map(purrr::map(events_competed_helper, stringr::str_detect, "X"), any) == TRUE)]
#
#   events_remaining <- stack(purrr::map(purrr::map(events_competed_helper, stringr::str_count, "X"), sum)) %>%
#     rename(Events_Remaining = values, Name = ind)
#
#   rank_helper <- rank_helper %>%
#     dplyr::rowwise() %>%
#     dplyr::filter(!all(Name == e_rank_helper$Name[k] & Event == e)) %>%
#     dplyr::left_join(events_remaining) %>%
#     dplyr::arrange(Event)
#
#   rank_helper <- rank_helper %>%
#     dplyr::group_by(Name) %>%
#     dplyr::summarize(Ranks = list(Rank)) %>%
#     dplyr::right_join(rank_helper)
#
#   x <- c()
#
#   return(x)
#
# }

