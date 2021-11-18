#' Formats data for analysis within \code{swim_parse}
#'
#' Takes the output of \code{read_results} and, inside of \code{swim_parse},
#' removes "special" strings like DQ and SCR from results, replacing them with
#' NA.  Also ensures that all athletes have a Finals, by moving over
#' Prelims.  This makes later analysis much easier.
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

  if("Prelims" %in% names(df) == FALSE){
    df$Prelims <- "NA"
  }

  specials <- c("NT", "NS", "DNS", "DQ", "DSQ", "SCR", "NP")

  df <- df %>%
    dplyr::mutate(Finals = replace(Finals, which(Finals %in% specials), NA)) %>%
    dplyr::mutate(Prelims = replace(Prelims, which(
      Prelims %in% specials
    ), NA)) %>%
    dplyr::mutate(Switch = dplyr::case_when(
      is.na(Prelims) == FALSE &
        is.na(Finals) == TRUE & DQ == 0 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::mutate(Finals = dplyr::case_when(
      Switch == TRUE ~ dplyr::coalesce(Finals, Prelims),
      TRUE ~ Finals
    )) %>%
    dplyr::mutate(
      Prelims = dplyr::case_when(Switch == TRUE ~ "NA",
                                      TRUE ~ Prelims),
      Prelims = dplyr::na_if(Prelims, "NA")
    ) %>%
    dplyr::select(-Switch)

  return(df)
}

