#' Sorts and collects lines by performance and row number
#'
#' Collects all lines, (for example containing splits or relay swimmers)
#' associated with a particular performance (a swim) into a data frame with the
#' appropriate row number for that performance
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lag
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom dplyr across
#' @importFrom dplyr summarise
#' @importFrom dplyr starts_with
#' @importFrom dplyr ungroup
#' @importFrom stats reshape
#'
#' @param x a list of character strings including performances, with tow numbers
#'   added by \code{add_row_numbers}
#' @param min_row the lowest row number
#' @param to_wide should the data frame x be converted to wide format?  Default
#'   is \code{TRUE} as used in Hytek and Omega results.  Use \code{FALSE} in
#'   Splash results
#' @return a data frame with \code{Row_Numb} as the first column.  Other columns
#'   are performance elements, like splits or relay swimmers, both in order of
#'   occurrence left to right
#'
#' @seealso \code{lines_sort} is a helper function inside \code{splits_parse}
#'   and \code{swim_parse_ISL}

lines_sort <- function(x, min_row = minimum_row, to_wide = TRUE) {

  #### testing ####
  # x <- data_splits
  # min_row <- 13
  # to_wide <- FALSE


  #### actual function ####

  min_row <- as.numeric(min_row)

  x <- x %>%
    dplyr::mutate(Row_Numb = as.numeric(V1)) %>%
    dplyr::arrange(Row_Numb) %>%
    dplyr::mutate(
      Row_Numb_2 = dplyr::case_when(
        Row_Numb - dplyr::lag(Row_Numb, default = min_row) == 0 ~ "Different",
        Row_Numb - dplyr::lag(Row_Numb, default = min_row) <= 1 ~ "Same",
        Row_Numb - dplyr::lag(Row_Numb, default = min_row) > 1 ~ "Different",
        TRUE ~ "Different"
      )
    ) %>%
    dplyr::mutate(
      Row_Fill = dplyr::case_when(Row_Numb_2 == "Different" ~ Row_Numb,
                                  Row_Numb_2 == "Same" ~ 0),
      Row_Fill = as.character(Row_Fill)
    ) %>%
    na_if_numeric(0) %>%
    dplyr::mutate(Row_Fill = fill_down(Row_Fill)) %>%

    dplyr::select(-V1, -Row_Numb, -Row_Numb_2) %>%
    dplyr::mutate(row_index = 1:dplyr::n()) %>%
    dplyr::relocate(Row_Fill)

  if (to_wide == TRUE) {
    x <- x %>%
      stats::reshape(direction = "wide",
                     idvar = "Row_Fill",
                     timevar = "row_index") %>%
      fill_left()

    names(x)[1] <- "Row_Numb"
  }

  if (to_wide == FALSE) {
    x <- x %>%
      dplyr::select(-row_index) %>%
      dplyr::group_by(Row_Fill) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), ~ sec_format(.x))) %>%
      dplyr::summarise(dplyr::across(dplyr::starts_with("Split"), ~ sum(.x, na.rm = TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), ~ mmss_format(.x))) %>%
      na_if_character("00.00") %>%
      dplyr::rename("Row_Numb" = Row_Fill) %>%
      dplyr::relocate(Row_Numb)

  }

  return(x)
}
