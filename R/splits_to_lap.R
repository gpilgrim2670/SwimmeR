#' Converts splits from cumulative to lap format
#'
#' Cumulative splits are when each split is the total elapsed time at a given
#' distance.  For example, if an athlete swims the first 50 of a 200 yard race
#' in 25.00 seconds (alp and cumulative split), and the second 50 (i.e. the 100
#' lap split) in 30.00 seconds the cumulative 100 split is 25.00 + 30.00 =
#' 55.00.  Some swimming results are reported with lap splits (preferred), but
#' others use cumulative splits.  This function converts cumulative splits to
#' lap splits.
#'
#' @importFrom dplyr group_split
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_df
#' @importFrom stringr str_sort
#' @importFrom stringr str_detect
#'
#' @param df a data frame containing splits in cumulative format results.  Must
#'   be formatted in a "normal" fashion - see vignette
#' @param threshold a numeric value above which a split is taken to be
#'   cumulative.  Default is \code{0}
#' @param messages should progress messages be displayed?  Default is
#'   \code{FALSE}.  Converting large (> ~5) events at once will take several
#'   minutes.
#' @return a data frame with all splits in lap form
#'
#' @export

splits_to_lap <- function(df, threshold = 0, messages = FALSE) {

  #### Error Messages ####

  if(is.data.frame(df) == FALSE) {
    stop("df must be a data frame")
  }

  if(is.numeric(threshold) == FALSE) {
    stop("threshold must be numeric")
  }

  if(any(!is.logical(messages) & is.na(messages)) == TRUE) {
    stop("messages must be logical, either TRUE or FALSE")
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

  #### Establish ordering over events ####

  df_split <- df %>%
    dplyr::mutate(Event = factor(Event, levels = unique(df$Event))) %>%
    dplyr::group_split(Event)

  if(length(df_split) > 5){
    message("Correcting splits for large numbers of events can take ~1-2 minutes.  Set `messages = TRUE` to see progress.")
  }

  #### Map and Reassemble Data Frame

  if(messages == TRUE){
  df_corrected <-
    purrr::map_df(df_split, splits_to_lap_helper_2, i = i, split_cols = split_cols, threshold = threshold)

  } else {
    suppressMessages(
      df_corrected <-
        purrr::map_df(df_split, splits_to_lap_helper_2, i = i, split_cols = split_cols, threshold = threshold)
    )
  }

  df_corrected <- df_corrected %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(Event) %>%
    dplyr::mutate(Event = as.character(Event))


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

splits_to_lap_helper <- function(df, i, split_cols = split_cols, threshold = threshold) {

  #### testing ####

  # threshold <- 0
  # df <- df_split[[115]]
  # split_cols <- names(df)[stringr::str_detect(names(df), "Split")]
  # split_cols <- stringr::str_sort(split_cols, numeric = TRUE)
  # i <- seq(1, length(split_cols) -1)
  # i <- max(i)
  #
  #### actual function ####

  # df <- df[, colSums(is.na(df)) != nrow(df)]
  #
  # if("Split_50" %in% names(df) == FALSE){
  #   df$Split_50 <- NA
  # }
  #
  #
  # if (length(split_cols) == sum(stringr::str_count(names(df), "Split"))) {
  #   split_cols <- names(df)[stringr::str_detect(names(df), "Split")]
  # } else {
  #   split_cols <- split_cols[1:sum(stringr::str_count(names(df), "Split")) + 1]
  # }
  #
  # if(length(split_cols) > 1){
  # i <- i[1:length(split_cols) -1]
  # } else {
  #   i <- i[1:length(split_cols)]
  # }

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
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), as.character)) %>%
    dplyr::select(-dplyr::any_of(split_cols))

  df <- df[, colSums(is.na(df)) != nrow(df)]

  return(df)
}

#' Helper function for converting cumulative splits to lap splits
#'
#' @importFrom dplyr select
#' @importFrom dplyr matches
#' @importFrom dplyr left_join
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom stringr str_remove
#'
#' @param df a data frame containing splits in cumulative format
#' @param i list of values to iterate along
#' @param split_cols list of columns containing splits
#' @param threshold a numeric value above which a split is taken to be
#'   cumulative
#' @return a data frame with all splits in lap format

splits_to_lap_helper_2 <- function(df, i, split_cols = split_cols, threshold = threshold) {

  df_new <-
    purrr::map(
      i,
      splits_to_lap_helper,
      df = df,
      split_cols = split_cols,
      threshold = threshold
    ) %>%
    purrr::reduce(dplyr::left_join) %>%
    unique()
    # dplyr::select(-dplyr::matches("\\d$"))

  names(df_new) <- stringr::str_remove(names(df_new), "_new")

  return(df_new)
}
