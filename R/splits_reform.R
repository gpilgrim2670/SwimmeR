#' Adds together splits and compares to listed finals time to see if they match.
#'
#' Used in testing the workings for \code{split_parse} inside test-splits.R.  Note that even properly handled splits may not match the finals time due to issues in the source material.  Sometimes splits aren't fully recorded in the source.  Some relays also will not match due to the convetiontin of reporting splits by swimmer (see vignette for more details).
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr c_across
#' @importFrom dplyr starts_with
#' @importFrom dplyr case_when
#' @importFrom dplyr rowwise
#' @importFrom stringr str_detect
#'
#' @param df a dataframe output from \code{swim_parse} creates with \code{splits = TRUE}
#' @return a dataframe with a column \code{not_matching} containing \code{TRUE} if the splits for that swim match the finals time and \code{FALSE} if they do not

splits_reform <- function(df){

  df <- df %>%
    dplyr::filter(DQ != 1,
           stringr::str_detect(Event, "Diving") == FALSE, # diving does not have splits
           stringr::str_detect(Event, "\\s50\\s|\\s50m\\s") == FALSE) %>% # 50s do not have splits
    dplyr::mutate(F_sec = sec_format(Finals_Time)) %>% # finals time in seconds
    dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), ~ sec_format(.x))) %>% # all splits in seconds to account for splits over 59.99
    dplyr::rowwise() %>%
    dplyr::mutate(total = sum(dplyr::c_across(Split_50:length(df)), na.rm = TRUE)) %>%
    dplyr::mutate(not_matching = dplyr::case_when(round(F_sec - total, 2) == 0 ~ FALSE, # does total match finals time?
                                                  round(F_sec - total, 2) != 0 ~ TRUE))

  return(df)
}
