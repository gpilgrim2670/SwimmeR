#' Collects Splash format splits
#'
#' Collects splits and breaks them into a distance and a time, with a
#' corresponding row number
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_replace
#'
#' @param df a data frame containing two columns, \code{V1} is row numbers and
#'   \code{Dummy} as a string combining split distance and split time
#' @return a data frame with three columns, \code{V1}, \code{Split_Distance} and
#'   \code{Split}


splash_collect_splits <- function(df) {
  df <- df %>%
    dplyr::mutate(
      Split_Distance = stringr::str_split_fixed(Dummy, " ", n = 2)[, 1],
      Split = stringr::str_split_fixed(Dummy, " ", n = 2)[, 2]
    ) %>%
    dplyr::mutate(Split_Distance = stringr::str_replace(Split_Distance, "(\\d{1,})m", "_\\1")) %>%
    dplyr::select("Row_Numb" = V1, Split_Distance, Split)

  return(df)
}
