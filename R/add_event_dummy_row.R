#' Add dummy entry rows
#'
#' If a team does not have a full compliment, defined by \code{max_entries}, of
#' athletes in a given event then dummy rows containing blank entries need to be
#' added to that event
#'
#' @importFrom dplyr add_row
#' @importFrom dplyr group_by
#' @importFrom dplyr group_modify
#'
#' @param x a list of data frames containing event results that need dummy
#'   entries added
#' @return returns a list of data frames each with a dummy entry row added


add_event_dummy_row <- function(x) {

  x <- x %>%
    dplyr::group_by(Event) %>%
    {if("Exhibition" %!in% names(x[[1]])) mutate(., Exhibition = 0)} %>%
    dplyr::group_modify( ~ dplyr::add_row(.x, Result_numeric = 99999, Exhibition = 0, Name = paste0(.y, "_Dummy_4X"))) %>%
    dplyr::group_modify( ~ dplyr::add_row(.x, Result_numeric = 99998, Exhibition = 0, Name = paste0(.y, "_Dummy_3X"))) %>%
    dplyr::group_modify( ~ dplyr::add_row(.x, Result_numeric = 99997, Exhibition = 0, Name = paste0(.y, "_Dummy_2X"))) %>%
    dplyr::group_modify( ~ dplyr::add_row(.x, Result_numeric = 99996, Exhibition = 0, Name = paste0(.y, "_Dummy_1X")))

return(x)

}
