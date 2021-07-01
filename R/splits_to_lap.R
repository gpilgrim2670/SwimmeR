#' Converts splits from cumulative to lap format
#'
#' Cumulative splits are when each split is the total elapsed time at a given
#' distance.  For example, if an athlete swims the first 50 of a 200 yard race
#' in 25.00 seconds (lap and cumulative split), and the second 50 (i.e. the 100
#' lap split) in 30.00 seconds the cumulative 100 split is 25.00 + 30.00 =
#' 55.00.  Some swimming results are reported with lap splits (preferred), but
#' others use cumulative splits.  This function converts cumulative splits to
#' lap splits.
#'
#' @importFrom dplyr left_join
#' @importFrom purrr reduce
#' @importFrom stringr str_sort
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#'
#' @param df a data frame containing splits in cumulative format results.  Must
#'   be formatted in a "normal" fashion - see vignette
#' @param threshold a numeric value above which a split is taken to be
#'   cumulative.  Default is \code{0}
#' @return a data frame with all splits in lap form
#' @examples \dontrun{
#'df <- data.frame(Place = 1,
#'                 Name = "Sally Swimfast",
#'                 Team = "KVAC",
#'                 Event = "Womens 200 Freestyle",
#'                 Finals_Time = "1:58.00",
#'                 Split_50 = "28.00",
#'                 Split_100 = "59.00",
#'                 Split_150 = "1:31.00",
#'                 Split_200 = "1:58.00")
#'
#'df %>%
#'  splits_to_lap
#'  }
#' @export

splits_to_lap <- function(df, threshold = 0) {

  #### Error Messages ####

  if(is.data.frame(df) == FALSE) {
    stop("df must be a data frame")
  }

  if(is.numeric(threshold) == FALSE) {
    stop("threshold must be numeric")
  }

  #### testing ####

  # file <-
  #   system.file("extdata", "2018_jimi_flowers_PARA.pdf", package = "SwimmeR")
  #
  # df <- swim_parse(read_results(file),
  #                  splits = TRUE)

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
      splits_to_lap_helper_recalc,
      df = df,
      split_cols = split_cols,
      threshold = threshold
    ) %>%
      purrr::reduce(dplyr::left_join) %>%
      unique()
  )

  names(df_corrected) <- stringr::str_remove(names(df_corrected), "_new")

  return(df_corrected)
}

#' Helper function for converting cumulative splits to lap splits
#'
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr starts_with
#' @importFrom dplyr rowwise
#' @importFrom dplyr any_of
#' @importFrom dplyr select
#' @importFrom stringr str_remove
#' @importFrom stringr str_detect
#'
#' @param df a data frame containing splits in cumulative format
#' @param i list of values to iterate along
#' @param split_cols list of columns containing splits
#' @param threshold a numeric value above which a split is taken to be
#'   cumulative
#' @return a list of data frames with all splits in lap format for a particular
#'   event, each with a single split column converted to lap format

splits_to_lap_helper_recalc <- function(df, i, split_cols = split_cols, threshold = threshold) {

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

  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), sec_format)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), as.numeric)) %>%
    dplyr::mutate(!!paste0(split_cols[1], "_new") := !!as.name(split_cols[1])) %>%
    dplyr::mutate(
      !!as.name(split_cols_new[i]) := dplyr::case_when(
        is.na(!!as.name(split_cols[j])) == FALSE &
          !!as.name(split_cols[j]) > threshold ~ !!as.name(split_cols[j]) -!!as.name(split_cols[i]),
        TRUE ~ !!as.name(split_cols[j])
      )
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), round, 2)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), mmss_format)) %>%
    dplyr::select(-dplyr::any_of(split_cols))

  df <- df[, colSums(is.na(df)) != nrow(df)]

  return(df)
}
