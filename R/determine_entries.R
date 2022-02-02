# In progress.  The for loop below does work but obviously needs to be cleaned
# up and sent to map.  Need to break events into Event_A, Event_B etc. for
# actual use.  Will also need to accept scoring system.  Do keywords for scoring
# systems, for rescoring functions as well.  Maybe an events-back-to-back
# protocol as well.  Also something to exclude a particular athlete from a
# particular event

# file <- system.file("extdata", "RIT_TopTimes_FS2021.pdf", package = "SwimmeR")
#
# #### test dfs ####
# data <- file %>%
#   read_results() %>%
#   swim_parse()
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
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom purrr map
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
#' @return xxx

determine_entries <- function(df, op_df, point_values, time_col, events = NULL){

  # df = RIT_TopTimes_2021
  # op_df = IC
  # point_values = "ncaa_six_lane"
  # time_col = "Finals"
  # events = NULL

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

  #### actual function ####

  Point_Values <- data.frame(Points = point_values[c(TRUE, FALSE)])

  #### make sure all events in op_df have the same number of entries, equal to
  #### the maximum number of entries in any event
  op_df_split <- op_df %>%
    {
      if ("Exhibition" %in% names(op_df))
        filter(., Exhibition == 0)
    } %>%
    dplyr::group_split(Event)

  max_entries <- max(unlist(map(op_df_split, nrow)))

  to_add <-
    op_df_split[unlist(map(op_df_split, nrow)) < max_entries]

  if (length(to_add) > 0) {
    to_add <- to_add %>%
      map(function(x)
        x %>% dplyr::group_by(Event) %>% dplyr::group_modify( ~ dplyr::add_row(.x, Time = 99)))

    no_add <-
      op_df_split[unlist(map(op_df_split, nrow)) == max_entries]

    op_df <- dplyr::bind_rows(c(to_add, no_add))

  }

  op_df_2 <- events %>%
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

  # test <- purrr::map(seq(1, length(op_df_2$Event_Points), 1),
  #            determine_entries_helper,
  #            df_helper = df,
  #            op_df_helper = op_df_2,
  #            Times_Competed = Times_Competed,
  #            events_competed_helper = Events_Competed,
  #            max_ind_entries = max_entries,
  #            time_col_helper = time_col)


  test <- determine_entries_helper(
    i,
    df_helper = df,
    op_df_helper = op_df_2,
    end_seq = length(op_df_2$Event_Points),
    # Times_Competed = Times_Competed,
    # events_competed_helper = Events_Competed,
    max_ind_entries = max_entries,
    time_col_helper = time_col
  )

  ### need to reconstruct Events_Competed from test 2/2/22

  test <- test %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(Event, Time) %>%
    dplyr::mutate(Rank = as.numeric(Rank))

  athletes_remaining <- Events_Competed[which(str_detect(Events_Competed, "X") == TRUE)]

  athlete_ranks <- df %>%
    dplyr::select(Rank, Name, Event) %>%
    dplyr::mutate(Rank = as.numeric(Rank))

  test_2 <- purrr::map(seq(1, length(unique(test$Event)), 1),
             determine_entries_helper_2,
             df_helper = df,
             df_2_helper = test,
             athlete_ranks_helper = athlete_ranks,
             athletes_remaining_helper = athletes_remaining,
             events_competed_helper = Events_Competed,
             max_ind_entries = 3)

  test_3 <- test_2[length(unique(test$Event))][[1]]

  return(test_3)


  # determine_entries_helper(
  #   i = 8,
  # df_helper = df,
  # op_df_helper = op_df_2,
  # Times_Competed = Times_Competed,
  # events_competed_helper = Events_Competed,
  # max_ind_entries = 3)

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
           # events_helper,
           # Times_Competed = Times_Competed,
           # events_competed_helper = Events_Competed,
           max_ind_entries = 2,
           time_col_helper = time_col) {

  # df_helper <- df
  # op_df_helper <- op_df_2
  # max_ind_entries <- 3
  # i <- 1
  #
  Entries <- list()

  competitors <- unique(df_helper$Name)

  Times_Competed <- list_to_list_names(competitors, 0)

  Events_Competed <- list_to_list_names(competitors, c("X", "X", "X"))

   for (i in 1:end_seq){

  e <- op_df_helper$Event_Points[i]
  e_no_points <- stringr::str_remove(e, "_.*")

  op_result <- op_df_helper %>%
    filter(Event_Points == e) %>%
    # mutate(Time = {{time_col}}) %>%
    dplyr::pull({{time_col_helper}}) %>%  ### need to set as time_col
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
    paste(e_no_points, i, sep = "_")

  Entries[[i]] <- entry %>%
    dplyr::mutate(Counter = i)
   }

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

determine_entries_helper_2 <-
  function(i,
           df_helper,
           df_2_helper,
           athlete_ranks_helper,
           athletes_remaining_helper,
           events_competed_helper = Events_Competed,

           max_ind_entries = 3) {

    # df_helper <- df
    # df_2_helper <- test
    # athlete_ranks_helper <- athlete_ranks
    # athletes_remaining_helper <- athletes_remaining
    # events_competed_helper <- Events_Competed
    # max_ind_entries <- 3
    # Entries <- list()

    rank_helper <- athlete_ranks_helper %>%
      filter(Name %in% names(athletes_remaining_helper)) %>%
      arrange(Rank)

    rank_helper_2 <- rank_helper %>%
      mutate(Event = factor(Event, levels = unique(athlete_ranks_helper$Event)))

    df_2_helper <- df_2_helper %>%
      mutate(Event = factor(Event, levels = unique(athlete_ranks_helper$Event))) %>%
      arrange(Event)

    # i <- 13
    # athletes_remaining_2 <- athletes_remaining
    # Events_Competed_2 <- Events_Competed

    e <- unique(df_2_helper$Event)[i]
    print(paste("interation", i))
    print(e)

    e_df <- df_2_helper %>%
      filter(Event == e) %>%
      arrange(desc(Rank))

    e_rank_helper <- rank_helper_2 %>%
      filter(Name %in% names(athletes_remaining)) %>%
      filter(Name %!in% c(e_df$Name)) %>%
      filter(Event == e) %>%
      arrange(Rank) %>%
      head(max_ind_entries)

    e_df
    e_rank_helper

    ### remove elements of e_df as things progress and add check in the if statements for e_df length ###

    if (nrow(e_rank_helper) == 0 & i > 1) {
      Entries[[i]] <- Entries[[i - 1]]

    } else if (nrow(e_rank_helper) == 0 & i == 1) {
      Entries[[i]] <<- df_2_helper

    } else {


      ##### first replacement #######
      if(all(length(e_rank_helper$Rank) >= 1 ,
             e_df$Rank[1] > e_rank_helper$Rank[1])) {
        # need this as number of team entries

        row_to_add <- df_helper %>%
          mutate(Event = factor(Event, levels = unique(rank_helper$Event))) %>%
          mutate(Rank = as.numeric(Rank)) %>%
          filter(Name == e_rank_helper$Name[1],
                 Event == e)

        if(is.null(Entries[i][[1]]) & i == 1){
          Entries[[i]] <<- df_2_helper %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[1])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else {
          Entries[[i]] <<- Entries[[i-1]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[1])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        }

        e_df <- e_df[-1,]

        Events_Competed[e_rank_helper$Name[1]][[1]][min(which(str_detect(Events_Competed[e_rank_helper$Name[1]][[1]], "X") == TRUE))] <<-
          paste(e, i, sep = "_")

        athletes_remaining <<- Events_Competed[which(str_detect(Events_Competed, "X") == TRUE)]

        rank_helper_2 <- rank_helper_2 %>%
          rowwise() %>%
          filter(!all(Name == e_rank_helper$Name[1] & Event == e)) %>%
          arrange(Event)

      } else if(all(length(e_rank_helper$Rank) >= 1 ,
                    e_df$Rank[2] > e_rank_helper$Rank[1])){

        row_to_add <- df_helper %>%
          mutate(Event = factor(Event, levels = unique(rank_helper$Event))) %>%
          mutate(Rank = as.numeric(Rank)) %>%
          filter(Name == e_rank_helper$Name[1],
                 Event == e)

        if(is.null(Entries[i][[1]]) & i == 1){
          Entries[[i]] <<- df_2_helper %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[2])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else if(i==1) {
          Entries[[i]] <<- Entries[[i]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[2])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else {
          Entries[[i]] <<- Entries[[i-1]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[2])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        }

        e_df <- e_df[-2,]

        Events_Competed[e_rank_helper$Name[1]][[1]][min(which(str_detect(Events_Competed[e_rank_helper$Name[1]][[1]], "X") == TRUE))] <<-
          paste(e, i, sep = "_")

        athletes_remaining <<- Events_Competed[which(str_detect(Events_Competed, "X") == TRUE)]

        rank_helper_2 <- rank_helper_2 %>%
          rowwise() %>%
          filter(!all(Name == e_rank_helper$Name[1] & Event == e)) %>%
          arrange(Event)

      } else if (all(length(e_rank_helper$Rank) >= 1 ,
                     e_df$Rank[3] > e_rank_helper$Rank[1])){

        row_to_add <- df_helper %>%
          mutate(Event = factor(Event, levels = unique(rank_helper$Event))) %>%
          mutate(Rank = as.numeric(Rank)) %>%
          filter(Name == e_rank_helper$Name[1],
                 Event == e)

        if(is.null(Entries[i][[1]]) & i == 1){
          Entries[[i]] <<- df_2_helper %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[3])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else if(i==1) {
          Entries[[i]] <<- Entries[[i]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[3])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else {
          Entries[[i]] <<- Entries[[i-1]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[3])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        }

        e_df <- e_df[-1,]

        Events_Competed[e_rank_helper$Name[1]][[1]][min(which(str_detect(Events_Competed[e_rank_helper$Name[1]][[1]], "X") == TRUE))] <<-
          paste(e, i, sep = "_")

        athletes_remaining <<- Events_Competed[which(str_detect(Events_Competed, "X") == TRUE)]

        rank_helper_2 <- rank_helper_2 %>%
          rowwise() %>%
          filter(!all(Name == e_rank_helper$Name[1] & Event == e)) %>%
          arrange(Event)
      }

      print("completed first replacement")

      ######### second replacement ##########

      if (all(length(e_rank_helper$Rank) >= 2,
              length(e_df$Rank) >= 1,
              e_df$Rank[1] > e_rank_helper$Rank[2])) {
        # need this as number of team entries

        row_to_add <- df_helper %>%
          mutate(Event = factor(Event, levels = unique(rank_helper$Event))) %>%
          mutate(Rank = as.numeric(Rank)) %>%
          filter(Name == e_rank_helper$Name[2],
                 Event == e)

        if(is.null(Entries[i][[1]]) & i == 1){
          Entries[[i]] <<- df_2_helper %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[1])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else if(i==1) {
          Entries[[i]] <<- Entries[[i]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[1])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else {
          Entries[[i]] <<- Entries[[i-1]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[1])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        }

        e_df <- e_df[-1,]

        Events_Competed[e_rank_helper$Name[2]][[1]][min(which(str_detect(Events_Competed[e_rank_helper$Name[2]][[1]], "X") == TRUE))] <<-
          paste(e, i, sep = "_")

        athletes_remaining <<- Events_Competed[which(str_detect(Events_Competed, "X") == TRUE)]

        rank_helper_2 <- rank_helper_2 %>%
          rowwise() %>%
          filter(!all(Name == e_rank_helper$Name[2] & Event == e)) %>%
          arrange(Event)

      } else if (all(length(e_rank_helper$Rank) >= 2,
                     length(e_df$Rank) >= 2,
                     e_df$Rank[length(e_df$Rank)-1] > e_rank_helper$Rank[2])) {
        # need this as number of team entries

        row_to_add <- df_helper %>%
          mutate(Event = factor(Event, levels = unique(rank_helper$Event))) %>%
          mutate(Rank = as.numeric(Rank)) %>%
          filter(Name == e_rank_helper$Name[2],
                 Event == e)

        if(is.null(Entries[i][[1]]) & i == 1){
          Entries[[i]] <<- df_2_helper %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[2])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else if(i==1) {
          Entries[[i]] <<- Entries[[i]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[2])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else {
          Entries[[i]] <<- Entries[[i-1]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[2])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        }

        e_df <- e_df[-2,]

        Events_Competed[e_rank_helper$Name[2]][[1]][min(which(str_detect(Events_Competed[e_rank_helper$Name[2]][[1]], "X") == TRUE))] <<-
          paste(e, i, sep = "_")

        athletes_remaining <<- Events_Competed[which(str_detect(Events_Competed, "X") == TRUE)]

        rank_helper_2 <- rank_helper_2 %>%
          rowwise() %>%
          filter(!all(Name == e_rank_helper$Name[2] & Event == e)) %>%
          arrange(Event)

      } else if (all(length(e_rank_helper$Rank) > 2,
                     length(e_df$Rank) >= 3,
                     e_df$Rank[3] > e_rank_helper$Rank[2])) {
        # need this as number of team entries

        row_to_add <- df_helper %>%
          mutate(Event = factor(Event, levels = unique(rank_helper$Event))) %>%
          mutate(Rank = as.numeric(Rank)) %>%
          filter(Name == e_rank_helper$Name[2],
                 Event == e)

        if(is.null(Entries[i][[1]]) & i == 1){
          Entries[[i]] <<- df_2_helper %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[3])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else if(i==1) {
          Entries[[i]] <<- Entries[[i]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[3])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else {
          Entries[[i]] <- Entries[[i - 1]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[3])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)


          e_df <- e_df[-3,]

          Events_Competed[e_rank_helper$Name[2]][[1]][min(which(str_detect(Events_Competed[e_rank_helper$Name[2]][[1]], "X") == TRUE))] <<-
            paste(e, i, sep = "_")

          athletes_remaining <-
            Events_Competed[which(str_detect(Events_Competed, "X") == TRUE)]

          rank_helper_2 <- rank_helper_2 %>%
            rowwise() %>%
            filter(!all(Name == e_rank_helper$Name[2] &
                          Event == e)) %>%
            arrange(Event)

        }

      } else {
        e_df <- data.frame(Rank = 0)
      }

      print("completed second replacement")

      ######### third replacement ##########

      if (all(length(e_rank_helper$Rank) >= 3,
              length(e_df$Rank) >= 1,
              e_df$Rank[1] > e_rank_helper$Rank[3])) {
        # need this as number of team entries

        row_to_add <- df_helper %>%
          mutate(Event = factor(Event, levels = unique(rank_helper$Event))) %>%
          mutate(Rank = as.numeric(Rank)) %>%
          filter(Name == e_rank_helper$Name[3],
                 Event == e)

        if(is.null(Entries[i][[1]]) & i == 1){
          Entries[[i]] <<- df_2_helper %>%
            replacement_entries(index_number = 1, row_to_add_replacement = row_to_add, e_df_replacement = e_df)


        } else if(i==1) {
          Entries[[i]] <<- Entries[[i]] %>%
            replacement_entries(index_number = 1, row_to_add_replacement = row_to_add, e_df_replacement = e_df)

        } else {
          Entries[[i]] <<- Entries[[i-1]] %>%
            replacement_entries(index_number = 1, row_to_add_replacement = row_to_add, e_df_replacement = e_df)
        }


        # if(is.null(Entries[i][[1]]) & i == 1){
        #   Entries[[i]] <<- df_2_helper %>%
        #     rowwise() %>%
        #     filter(!all(Event == e, Name == e_df$Name[1])) %>%
        #     ungroup() %>%
        #     bind_rows(row_to_add) %>%
        #     arrange(Event)
        # } else if(i==1) {
        #   Entries[[i]] <<- Entries[[i]] %>%
        #     rowwise() %>%
        #     filter(!all(Event == e, Name == e_df$Name[1])) %>%
        #     ungroup() %>%
        #     bind_rows(row_to_add) %>%
        #     arrange(Event)
        # } else {
        #   Entries[[i]] <<- Entries[[i-1]] %>%
        #     rowwise() %>%
        #     filter(!all(Event == e, Name == e_df$Name[1])) %>%
        #     ungroup() %>%
        #     bind_rows(row_to_add) %>%
        #     arrange(Event)
        # }

        e_df <- e_df[-1,]

        Events_Competed[e_rank_helper$Name[3]][[1]][min(which(str_detect(Events_Competed[e_rank_helper$Name[3]][[1]], "X") == TRUE))] <<-
          paste(e, i, sep = "_")

        athletes_remaining <<- Events_Competed[which(str_detect(Events_Competed, "X") == TRUE)]

        rank_helper_2 <- rank_helper_2 %>%
          rowwise() %>%
          filter(!all(Name == e_rank_helper$Name[3] & Event == e)) %>%
          arrange(Event)

      } else if (all(length(e_rank_helper$Rank) >= 3,
                     length(e_df$Rank) >= 2,
                     e_df$Rank[2] > e_rank_helper$Rank[3])) {
        # need this as number of team entries

        row_to_add <- df_helper %>%
          mutate(Event = factor(Event, levels = unique(rank_helper$Event))) %>%
          mutate(Rank = as.numeric(Rank)) %>%
          filter(Name == e_rank_helper$Name[3],
                 Event == e)

        if(is.null(Entries[i][[1]]) & i == 1){
          Entries[[i]] <<- df_2_helper %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[2])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else if(i==1) {
          Entries[[i]] <<- Entries[[i]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[2])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else {
          Entries[[i]] <<- Entries[[i-1]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[2])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        }

        e_df <- e_df[-2,]

        Events_Competed[e_rank_helper$Name[3]][[1]][min(which(str_detect(Events_Competed[e_rank_helper$Name[3]][[1]], "X") == TRUE))] <<-
          paste(e, i, sep = "_")

        athletes_remaining <<- Events_Competed[which(str_detect(Events_Competed, "X") == TRUE)]

        rank_helper_2 <- rank_helper_2 %>%
          rowwise() %>%
          filter(!all(Name == e_rank_helper$Name[3] & Event == e)) %>%
          arrange(Event)

      } else if (all(length(e_rank_helper$Rank) >= 3,
                     length(e_df$Rank) >= 3,
                     e_df$Rank[1] > e_rank_helper$Rank[3])) {
        # need this as number of team entries

        row_to_add <- df_helper %>%
          mutate(Event = factor(Event, levels = unique(rank_helper$Event))) %>%
          mutate(Rank = as.numeric(Rank)) %>%
          filter(Name == e_rank_helper$Name[3],
                 Event == e)

        if(is.null(Entries[i][[1]]) & i == 1){
          Entries[[i]] <<- df_2_helper %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[3])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else if(i==1) {
          Entries[[i]] <<- Entries[[i]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[3])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        } else {
          Entries[[i]] <<- Entries[[i-1]] %>%
            rowwise() %>%
            filter(!all(Event == e, Name == e_df$Name[3])) %>%
            ungroup() %>%
            bind_rows(row_to_add) %>%
            arrange(Event)
        }

        e_df <- e_df[-3,]

        Events_Competed[e_rank_helper$Name[3]][[1]][min(which(str_detect(Events_Competed[e_rank_helper$Name[3]][[1]], "X") == TRUE))] <<-
          paste(e, i, sep = "_")

        athletes_remaining <<- Events_Competed[which(str_detect(Events_Competed, "X") == TRUE)]

        rank_helper_2 <- rank_helper_2 %>%
          rowwise() %>%
          filter(!all(Name == e_rank_helper$Name[3] & Event == e)) %>%
          arrange(Event)

      }

      print("completed third replacement")

    }

    # Entries_Test <- Entries[i][[1]]
    #
    # Entries[i][[1]]

    if(length(Entries) == 0 & i == 1){
      Entries[[1]] <<- df_2_helper
    } else if(length(Entries) == 0){
      Entries[[i]] <<- Entries[[i-1]]
    }

    #### remove empty data frames ####
    if(is.null(Entries[i][[1]]) & i > 1){
      Entries[i][[1]] <<- Entries[i-1][[1]]
    } else if(is.null(Entries[i][[1]]) & i == 1){
      Entries[i][[1]] <<- df_2_helper
    }

    # dplyr::all_equal(Entries_Test, Entries[i][[1]])
    # Entries[i][[1]]

    return(Entries[[i]])

    # return(Entries)

    # }
  }

#### Replacement Entries Function ####

#' Replaces superseded rows
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

replacement_entries <- function(x, index_number, row_to_add_replacement, e_df_replacement){

  x <- x %>%
      dplyr::rowwise() %>%
      dplyr::filter(!all(Event == as.character(e_df_replacement$Event), Name == e_df_replacement$Name[index_number])) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(row_to_add_replacement) %>%
      dplyr::arrange(Event)

}
