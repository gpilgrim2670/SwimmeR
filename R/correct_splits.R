#' Changes lengths associated with splits to new values
#'
#' Useful for dealing with meets where some events are split by 50 and others by 25.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param df a dataframe having some split columns (Split_50, Split_100 etc.)
#' @param new_split_length split length to rename split columns based on
#' @param events list of events to correct splits for
#' @return a dataframe where all values have been pushed left, replacing `NA`s, and all columns containing only `NA`s have been removed
#'
#' @seealso \code{fill_left} is a helper function inside \code{lines_sort} and \code{splits_parse}
#'
#' @export


correct_splits <- function(df, new_split_length, events){

  # events <- c("Women 50 Yard Freestyle")

  df_split <- df %>%
    split(f = as.factor(.$Event))

  event_order <- unique(df$Event)

  df_split <- df_split[names(df_split) %in% events]

  df_split <- df_split %>%
    purrr::map(correct_splits_helper, new_split_length_helper = new_split_length) %>%
    dplyr::bind_rows()

  df <- df %>%
    dplyr::filter(Event %!in% events) %>%
    dplyr::full_join(df_split) %>%
    dplyr::mutate(Event = factor(Event, levels = event_order)) %>%
    dplyr::arrange(Event)

  return(df)


}
