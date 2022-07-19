#' Regularizes event names
#'
#' XXX
#'
#' @importFrom stringr str_replace
#'
#' @param x a character vector of event names
#' @return a character vector of event names with naming conventions enforced to
#'   regularize event names
#'
#' @export

clean_events <- function(x) {

  x <- x %>%
    stringr::str_replace("Fly$", "Butterfly") %>%
    stringr::str_replace("Back$", "Backstroke") %>%
    stringr::str_replace("Breast$", "Breaststroke") %>%
    stringr::str_replace("Free(?=$|\\s)", "Freestyle") %>%
    stringr::str_replace("FL(\\s|$)", "Butterfly\\1") %>%
    stringr::str_replace("BK(\\s|$)", "Backstroke\\1") %>%
    stringr::str_replace("BR(\\s|$)", "Breaststroke\\1") %>%
    stringr::str_replace("FR(\\s|$)", "Freestyle\\1") %>%
    stringr::str_replace("IM(\\s|$)", "Individual Medley\\1")

  return(x)

}


# events_to_test <- unique(df_standard$Event)
# events_to_test <- unique(c(events_to_test, unique(RIT_TopTimes_2021$Event)))

# events_to_test %>%
#   clean_events()
