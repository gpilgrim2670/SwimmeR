#' Shifts non-NA values to left in data frame
#'
#' Moves non-NA data left into NA spaces, then removes all columns that contain
#' only NA values
#'
#' @param df a data frame having some `NA` values
#' @return a data frame where all values have been pushed left, replacing `NA`s,
#'   and all columns containing only `NA`s have been removed
#'
#' @seealso \code{fill_left} is a helper function inside \code{lines_sort} and
#'   \code{splits_parse}

fill_left <- function(df) {
  df <- as.data.frame(t(apply(df, 1, function(x) {
    return(c(x[!is.na(x)], x[is.na(x)]))
  })))

  df <- Filter(function(x)
    ! all(is.na(x)), df)


  # not sure if I want to implement this or not 3/31/21
  # names(df)[names(df) == "V1"] <- "Row_Numb"

  return(df)

}
