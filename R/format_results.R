#' Formats data for analysis within \code{swim_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{swim_parse},
#' removes "special" strings like DQ and SCR from results, replacing them with
#' NA.  Also ensures that all athletes have a Finals_Time, by moving over
#' Prelims_Time.  This makes later analysis much easier.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr coalesce
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#'
#' @param df a data frame of results at the end of \code{swim_parse}
#' @return returns a formatted data frame
#'
#' @export
#'
#' @seealso \code{splits_parse} runs inside \code{\link{swim_parse}} on the
#'   output of \code{\link{read_results}} with row numbers from
#'   \code{\link{add_row_numbers}}

format_results <- function(df){
  df <- df %>%
    dplyr::mutate(Finals_Time = replace(Finals_Time, which(Finals_Time %in% c(
      "NT", "NS", "DQ", "SCR", "NP"
    )), NA)) %>%
    dplyr::mutate(Prelims_Time = replace(Prelims_Time, which(
      Prelims_Time %in% c("NT", "NS", "DQ", "SCR", "NP")
    ), NA)) %>%
    dplyr::mutate(Switch = dplyr::case_when(
      is.na(Prelims_Time) == FALSE &
        is.na(Finals_Time) == TRUE & DQ == 0 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::mutate(Finals_Time = dplyr::case_when(
      Switch == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
      TRUE ~ Finals_Time
    )) %>%
    dplyr::mutate(
      Prelims_Time = dplyr::case_when(Switch == TRUE ~ "NA",
                                      TRUE ~ Prelims_Time),
      Prelims_Time = dplyr::na_if(Prelims_Time, "NA")
    ) %>%
    dplyr::select(-Switch)

  return(df)
}

