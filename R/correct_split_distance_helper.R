#' Changes lengths associated with splits to new values
#'
#' Useful for dealing with meets where some events are split by 50 and others by 25.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract_all
#'
#' @param df_helper a dataframe having some split columns (Split_50, Split_100 etc.)
#' @param new_split_length_helper split length to rename split columns based on
#' @return a dataframe where all values have been pushed left, replacing `NA`s, and all columns containing only `NA`s have been removed
#'
#' @seealso \code{correct_split_distance_helper} is a helper function inside \code{correct_split_distance}



correct_split_distance_helper <- function(df_helper, new_split_length_helper){

  # new_split_length <- 25
  #
  # df_helper <- df

  old_split_cols <- names(df_helper)[stringr::str_detect(names(df_helper), "Split")]
  old_split_distances <- as.numeric(stringr::str_extract_all(split_cols, "\\d{1,}"))

  split_distances_multiplier <- new_split_length/min(old_split_distances, na.rm = TRUE)

  new_split_distances <- old_split_distances * split_distances_multiplier
  new_split_cols <- paste0("Split_", new_split_distances)

  df_helper <- df_helper %>%
    # dplyr::filter(Event == "Women 50 Yard Freestyle") %>%
    dplyr::rename_with(~ new_split_cols[which(old_split_cols == .x)], .cols = old_split_cols) %>%
    dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) # remove empty columns

  return(df_helper)

}
