#' Converts splits from lap to cumulative format
#'
#' Cumulative splits are when each split is the total elapsed time at a given
#' distance.  For example, if an athlete swims the first 50 of a 200 yard race
#' in 25.00 seconds (lap and cumulative split), and the second 50 (i.e. the 100
#' lap split) in 30.00 seconds the cumulative 100 split is 25.00 + 30.00 =
#' 55.00.  Some swimming results are reported with lap splits (preferred), but
#' others use cumulative splits.  This function converts lap splits to
#' cumulative splits.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom stringr str_sort
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#'
#' @param df a data frame containing results with splits in lap format.  Must
#'   be formatted in a "normal" SwimmeR fashion - see vignette
#' @param threshold a numeric value above which a split is taken to be
#'   cumulative.  Default is \code{Inf}
#' @return a data frame with all splits in lap form
#' @examples \dontrun{
#'df <- data.frame(Place = rep(1, 2),
#'                 Name = c("Lenore Lap", "Casey Cumulative"),
#'                 Team = rep("KVAC", 2),
#'                 Event = rep("Womens 200 Freestyle", 2),
#'                 Finals = rep("1:58.00", 2),
#'                 Split_50 = rep("28.00", 2),
#'                 Split_100 = c("31.00", "59.00"),
#'                 Split_150 = c("30.00", "1:29.00"),
#'                 Split_200 = c("29.00", "1:58.00")
#'                )
#'
#'  # since one entry is in lap time and the other is cumulative, need to
#'  # set threshold value
#'
#'  # not setting threshold will produce bad results by attempting to convert
#'  # Casey Cumulative's splits, which are already in cumulative
#'  # format, into cumulative format again
#'
#'  df %>%
#'    splits_to_cumulative()
#'
#'  df %>%
#'    splits_to_cumulative(threshold = 20)
#'
#'  }
#' @seealso \code{splits_to_cumulative} is the reverse of
#'   \code{\link{splits_to_lap}}
#' @export

splits_to_cumulative <- function(df, threshold = Inf) {

  #### Error Messages ####

  if(is.data.frame(df) == FALSE) {
    stop("df must be a data frame")
  }

  if(is.numeric(threshold) == FALSE) {
    stop("threshold must be numeric")
  }

  #### testing ####
  #
  # threshold <- Inf


  #### Actual Function ####

  #### Remove Empty Columns ####
  df <- df[, colSums(is.na(df)) != nrow(df)]

  #### Locate Split Columns ####
  split_cols <- names(df)[stringr::str_detect(names(df), "Split")]
  split_cols <- stringr::str_sort(split_cols, numeric = TRUE)
  i <- seq(1, length(split_cols) -1, 1)

  #### Run Helper Function ####
  suppressMessages(
    df_corrected <- purrr::map(
      i,
      splits_to_cumulative_helper_recalc,
      df = df,
      split_cols = split_cols,
      threshold = threshold
    ) %>%
      purrr::reduce(dplyr::left_join) %>%
      unique()
  )

  names(df_corrected) <- stringr::str_remove(names(df_corrected), "_new")

  if (any(df_corrected %>%
          dplyr::select(
            dplyr::contains("Split")) %>%
          purrr::map(.,
                     ~ stringr::str_detect(., '\\-\\d')) %>% purrr::reduce(., `|`), na.rm = TRUE
  ) == TRUE) {
    warning(
      "Negative split values produced.  Please check source data and/or consider setting the `threshold` parameter."
    )
  }

  return(df_corrected)
}

#' Helper function for converting lap splits to cumulative splits
#'
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr starts_with
#' @importFrom dplyr any_of
#' @importFrom dplyr select
#' @importFrom stringr str_remove
#' @importFrom stringr str_detect
#'
#' @param df a data frame containing splits in lap format
#' @param i list of values to iterate along
#' @param split_cols list of columns containing splits
#' @param threshold a numeric value below which a split is taken to be
#'   lap
#' @return a list of data frames with all splits in cumulative format for a
#'   particular event, each with a single split column converted to cumulative
#'   format

splits_to_cumulative_helper_recalc <- function(df, i, split_cols = split_cols, threshold = threshold) {

  #### testing ####

  # threshold <- 0
  # split_cols <- names(df)[stringr::str_detect(names(df), "Split")]
  # split_cols <- stringr::str_sort(split_cols, numeric = TRUE)
  # i <- seq(1, length(split_cols) -1)
  # i <- max(i)
  #
  #### actual function ####

  split_cols_new <- paste0(split_cols, "_new")[-1]
  j <- i + 1

  # define positions of column ranges
  col_vars <- names(df)
  col_start <- match(split_cols[1], col_vars)
  col_end <- match(split_cols[j], col_vars)

  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), sec_format)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), as.numeric)) %>%
    dplyr::mutate(!!paste0(split_cols[1], "_new") := !!as.name(split_cols[1])) %>%
    dplyr::mutate(
      !!as.name(split_cols_new[i]) := dplyr::case_when(
        is.na(!!as.name(split_cols[j])) == FALSE &
          (!!as.name(split_cols[j]) - !!as.name(split_cols[i]) < threshold) ~
          rowSums(.[, col_start:col_end]),
        TRUE ~ !!as.name(split_cols[j])
      )
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), round, 2)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), mmss_format)) %>%
    dplyr::select(-dplyr::any_of(split_cols))

  df <- df[, colSums(is.na(df)) != nrow(df)]

  return(df)
}
