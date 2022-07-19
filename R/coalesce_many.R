#' Combined paired sets of columns following a join operation
#'
#' @importFrom dplyr left_join
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom purrr reduce
#'
#' @param df a data frame following a join and thereby containing paired columns
#'   of the form Col_1.x, Col_1.y
#' @return returns a data frame with all sets of paired columns combined into
#'   single columns and named as, for example, Col_1, Col_2 etc.
#'
#' @seealso \code{coalesce_many} runs inside
#'   \code{\link{swim_parse_splash}}

coalesce_many <- function(df){

  # df <- data_ind

  new_split_names <- names(df) %>%
    .[stringr::str_detect(., "\\.[x|y]")] %>%
    stringr::str_remove("\\.[x|y]") %>%
    unique()

  if(length(new_split_names) > 0){

  i <- seq(1, length(new_split_names), 1)

  df_test <- i %>%
    purrr::map(coalesce_many_helper, df = df, new_split_names = new_split_names) %>%
    purrr::reduce(dplyr::left_join) %>%
    unique()
  } else {
    df_test <- df
  }


  return(df_test)
}


#' Combined paired sets of columns following a join operation
#'
#' This function is intended to be mapped over a sequence \code{i} inside the
#' function \code{\link{coalesce_many}}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr sym
#' @importFrom dplyr coalesce
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#'
#' @param df a data frame following a join and thereby containing paired columns
#'   of the form Col_1.x, Col_1.y
#' @param new_split_names a list of desired column names, e.g. Col_1, Col_2
#' @param i a number between 1 and the length of \code{new_split_names}
#' @return returns a data frame with one set of paired columns combined into a
#'   single column and named based on \code{new_split_names}
#'
#' @seealso \code{coalesce_many_helper} runs inside
#'   \code{\link{coalesce_many}}
#'
coalesce_many_helper <- function(df, new_split_names, i) {

  x_cols <- paste0(new_split_names, ".x")
  y_cols <- paste0(new_split_names, ".y")

  df_test <- df %>%
    dplyr::mutate(!!dplyr::sym(new_split_names[i]) := dplyr::coalesce(!!dplyr::sym(x_cols[i]) ,!!dplyr::sym(y_cols[i]))) %>%
    dplyr::select(-dplyr::all_of(c(x_cols, y_cols)))

  return(df_test)
}
