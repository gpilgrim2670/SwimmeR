#' #' Converts splits from cumulative to lap format
#' #'
#' #'
#' #' Cumulative splits are when each split is the total elapsed time at a given
#' #' distance.  For example, if an athlete swims the first 50 of a 200 yard race
#' #' in 25.00 seconds (alp and cumulative split), and the second 50 (i.e. the 100
#' #' lap split) in 30.00 seconds the cumulative 100 split is 25.00 + 30.00 =
#' #' 55.00.  Some swimming results are reported with lap splits (preferred), but
#' #' others use cumulative splits.  This function converts cumulative splits to
#' #' lap splits.
#' #'
#' #' @importFrom dplyr group_split
#' #' @importFrom dplyr mutate
#' #' @importFrom dplyr arrange
#' #' @importFrom dplyr bind_rows
#' #' @importFrom purrr map
#' #'
#' #' @param df a data frame containing splits in cumulative format
#' #'   results.  Must be formatted in a "normal" fashion - see vignette
#' #' @param threshold a numeric value above which a split is taken to be cumulative
#' #' @return a data frame with all splits in lap form
#' #'
#' #' @export
#'
#' splits_to_lap <- function(df, threshold = 0) {
#'   #### testing ####
#'
#'   # file <-
#'   #   system.file("extdata", "2018_jimi_flowers_PARA.pdf", package = "SwimmeR")
#'   #
#'   # df <- swim_parse(read_results(file),
#'   #                  splits = TRUE)
#'
#'   #### actual function ####
#'
#'   split_cols <- names(df)[stringr::str_detect(names(df), "Split")]
#'   i <- seq(1, length(split_cols) -1, 1)
#'
#'   df_split <- df %>%
#'     dplyr::mutate(Event = factor(Event, levels = unique(df$Event))) %>%
#'     dplyr::group_split(Event)
#'
#'   df_corrected <-
#'     purrr::map_df(df_split, splits_to_lap_helper, i = i, split_cols_helper = split_cols, threshold_helper = threshold) %>%
#'     dplyr::bind_rows() %>%
#'     dplyr::arrange(Event) %>%
#'     dplyr::mutate(Event = as.character(Event))
#'
#'   return(df_corrected)
#' }
#'
#'
#' #' Helper function for converting cumulative splits to lap splits
#' #'
#' #' @importFrom stringr str_detect
#' #' @importFrom dplyr across
#' #' @importFrom dplyr mutate
#' #' @importFrom dplyr case_when
#' #' @importFrom dplyr starts_with
#' #' @importFrom dplyr rowwise
#' #' @importFrom stringr str_remove
#' #'
#' #' @param df a data frame containing splits in cumulative format
#' #' @param threshold_helper a numeric value above which a split is taken to be cumulative
#' #' @return a data frame with all splits in lap format
#'
#' splits_to_lap_helper <- function(df, i, split_cols_helper = split_cols, threshold_helper = threshold) {
#'
#'   j <- i + 1
#'
#'   # split_cols_helper <- names(df)[stringr::str_detect(names(df), "Split")]
#'
#'   df <- df %>%
#'     dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), sec_format)) %>%
#'     dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), as.numeric)) %>%
#'     # dplyr::rowwise() %>%
#'     dplyr::mutate(
#'       !!as.name(split_cols_helper[j]) := dplyr::case_when(
#'         is.na(!!as.name(split_cols_helper[j])) == FALSE &
#'           !!as.name(split_cols_helper[j]) > threshold_helper ~ !!as.name(split_cols_helper[j]) - !!as.name(split_cols_helper[i]),
#'         TRUE ~ !!as.name(split_cols_helper[j])
#'       )
#'     ) %>%
#'     dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), mmss_format))
#'
#'   return(df)
#' }
#'
#' # df_test <- df %>%
#' #   splits_to_lap()
