#' Changes lengths associated with splits to new values
#'
#' Useful for dealing with meets where some events are split by 50 and others by 25.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr starts_with
#' @importFrom stringr str_sort
#' @importFrom purrr map
#'
#' @param df a data frame having some split columns (Split_50, Split_100 etc.)
#' @param new_split_length split length to rename split columns based on
#' @param events list of events to correct splits for
#' @return a data frame where all events named in the \code{events} parameter have their split column labels adjusted to reflect \code{new_split_length}
#'
#' @examples df <- data.frame(Name = c("Lilly King", "Caeleb Dressel"),
#' Event = c("Women 100 Meter Breaststroke", "Men 50 Yard Freestyle"),
#' Split_50 = c("29.80", "8.48"),
#' Split_100 = c("34.33", "9.15"))
#'
#' df %>% correct_split_distance(
#'  new_split_length = 25,
#'  events = c("Men 50 Yard Freestyle")
#' )
#'
#' @export


correct_split_distance <- function(df, new_split_length, events){

  #### conditionals for parameters ####
  if(is.data.frame(df) == FALSE){
    stop("`df` must be a data frame with a column named 'Event' and at least one column beginning with 'Split_'" )
  }

  if(is.numeric(new_split_length) == FALSE){
    stop("`new_split_length` must be numeric")
  }

  if(any(str_detect(names(df), "^Split_")) == FALSE){
    stop("the data must contain at least one split column, with a name begining with 'Split_'.")
  }

  if("Event" %in% names(df) == FALSE){
    stop("data must contain a column named 'Event'.")
  }

  if(all(events %in% unique(df$Event)) == FALSE){
    stop("all of the events named in `events` must appear in the data frame, in a column named 'Event'.")
  }

  #### function proper ####
  df_split <- df %>%
    split(f = as.factor(.$Event)) # split df by event

  event_order <- unique(df$Event) # get order of events to help with reassembling whole df

  df_split <- df_split[names(df_split) %in% events] # only want events named in `events` parameter

  df_split <- df_split %>%
    purrr::map(correct_split_distance_helper, new_split_length_helper = new_split_length) %>% # helper function to actually change split distances for each element of df_split
    dplyr::bind_rows() # df_split back to single data frame

  suppressMessages( # to suppress join by message
    df <- df %>%
      dplyr::filter(Event %!in% events) %>% # only events that did not have splits corrected
      dplyr::full_join(df_split) %>% # join in events that did have splits corrected
      dplyr::mutate(Event = factor(Event, levels = event_order)) %>% # restore event order (1)
      dplyr::arrange(Event) # restore event order (2)
  )

  df <- df %>% # orders split columns by distance (Split_25 first, Split_50)
    dplyr::select(!dplyr::starts_with("Split"), stringr::str_sort(names(.), numeric = TRUE))


  return(df)

}

#' @rdname correct_split_distance
#' @export
correct_split_length <- correct_split_distance


#' Changes lengths associated with splits to new values
#'
#' Useful for dealing with meets where some events are split by 50 and others by 25.
#'
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract_all
#'
#' @param df_helper a data frame having some split columns (Split_50, Split_100 etc.)
#' @param new_split_length_helper split length to rename split columns based on
#' @return a data frame where all values have been pushed left, replacing `NA`s, and all columns containing only `NA`s have been removed
#'
#' @seealso \code{correct_split_distance_helper} is a helper function inside \code{correct_split_distance}



correct_split_distance_helper <- function(df_helper, new_split_length_helper){

#### collect names of split columns ####
  old_split_cols <- names(df_helper)[stringr::str_detect(names(df_helper), "Split")]
  old_split_distances <- as.numeric(stringr::str_extract_all(old_split_cols, "\\d{1,}"))

  #### determine multiplier to change old split distances into new split distance
  split_distances_multiplier <- new_split_length_helper/min(old_split_distances, na.rm = TRUE)

  #### create new split columns
  new_split_distances <- old_split_distances * split_distances_multiplier
  new_split_cols <- paste0("Split_", new_split_distances)

  #### rename columns as needed ####
  df_helper <- df_helper %>%
    dplyr::rename_with(~ new_split_cols[which(old_split_cols == .x)], .cols = old_split_cols) %>%
    dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) # remove empty columns

  return(df_helper)

}
