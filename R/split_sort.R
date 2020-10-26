#' Sorts splits by performance row number
#'
#' Collects all splits associated with a paritcular performance (a swim) into a dataframe with the appropriate row number for that performance
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lag
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @importFrom stats reshape
#'
#' @param x a list of splits
#' @return a dataframe with \code{Row_Numb} as the first column.  Other columns are splits, by distance
#'
#' @seealso \code{split_sort} is a helper function
#'

split_sort <- function(x) {
  x <- x %>%
    dplyr::mutate(Row_Numb = as.numeric(V1)) %>%
    dplyr::mutate(
      Row_Numb_2 = dplyr::case_when(
        Row_Numb - dplyr::lag(Row_Numb, default = minimum_row) == 0 ~ "Different",
        Row_Numb - dplyr::lag(Row_Numb, default = minimum_row) <= 1 ~ "Same",
        Row_Numb - dplyr::lag(Row_Numb, default = minimum_row) > 1 ~ "Different",
        TRUE ~ "Different"
      )
    ) %>%
    dplyr::mutate(
      Row_Fill = dplyr::case_when(Row_Numb_2 == "Different" ~ Row_Numb - 1,
                           Row_Numb_2 == "Same" ~ 0),
      Row_Fill = as.character(Row_Fill)
    ) %>%
    dplyr::na_if(0) %>%
    dplyr::mutate(Row_Fill = fill_down(Row_Fill)) %>%

    dplyr::select(-V1, -Row_Numb, -Row_Numb_2) %>%
    dplyr::mutate(row_index = 1:dplyr::n()) %>%
    stats::reshape(direction = "wide",
            idvar = "Row_Fill",
            timevar = "row_index") %>%
    fill_left()

  names(x)[1] <- "Row_Numb"

  return(x)
}
