#' Pulls out event labels from text
#'
#' Locates event labels in text of results output from \code{read_results} and their associated row numbers.  The resulting dataframe is joined back into results to include event names
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param text output from \code{read_results}
#' @return returns a dataframe with event names and row numbers to eventually be recombined with swimming results inside \code{swim_parse}

#' @seealso \code{event_parse} is a helper function inside \code{\link{swim_parse}}

event_parse <- function(text){

events <- text %>%
  .[purrr::map_lgl(
    .,
    stringr::str_detect,
    "Event \\d{1,}|Women .* Yard|Women .* Meter|Girls .* Yard|Girls .* Meter|Men .* Yard|Men .* Meter|Boys .* Yard|Boys .* Meter|Mixed .* Yard|Mixed .* Meter"
  )]
events <- stringr::str_replace(events, ".*Event \\d{1,4} ", "")
events <- stringr::str_replace(events, "Open  ", "") ## Addition
events <-
  stringr::str_replace(events, "1 M  ", "1 M ") ## Addition
events <- stringr::str_replace(events, "([^1])0  ", "\\10 ")
events <- stringr::str_replace(events, " Class [:alpha:]", "")
events <- events %>% # Addition
  .[purrr::map_lgl(., stringr::str_detect, "[[:alpha:]]")] %>%
  stringr::str_replace_all("\\\n", "") %>%
  stringr::str_replace_all("\\(", "") %>%
  stringr::str_replace_all("\\)", "") %>%
  trimws()

events <-
  unlist(purrr::map(events, stringr::str_split, "\\s{2,}"),
         recursive = FALSE)

# dataframe for events with names and row number ranges
events <- as.data.frame(t(as.data.frame(events)),
                        row.names = FALSE,
                        stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    Event = stringr::str_extract(V1, "[[:graph:] ]*"),
    Event_Row_Min = as.numeric(V2),
    Event_Row_Max = dplyr::lead(Event_Row_Min, 1L, default = length(text)) - 1,
    V1 = NULL,
    V2 = NULL
  )
return(events)
}
