#' Pulls out event labels from text
#'
#' Locates event labels in text of results output from \code{read_results} and their associated row numbers.  The resulting dataframe is joined back into results to include event names
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param text output from \code{read_results}
#' @return returns a dataframe with event names and row numbers to eventually be recombined with swimming results inside \code{swim_parse}

#' @seealso \code{event_parse} is a helper function inside \code{\link{swim_parse}}

event_parse_ISL <- function(text){

  events <- text %>%
  .[purrr::map_lgl( # new 10/16
    .,
    stringr::str_detect,
    "Event \\d{1,}|Women .* Yard|Women .* Meter|Women.*\\d{2,4}\\s*[:alpha:]+|Girls .* Yard|Girls .* Meter|Girl.*\\d{2,4}\\s*[:alpha:]+|Men .* Yard|Men .* Meter|Men.*\\d{2,4}\\s*[:alpha:]+|Boys .* Yard|Boys .* Meter|Boy.*\\d{2,4}\\s*[:alpha:]+|Mixed .* Yard|Mixed .* Meter|Mixed.*\\d{2,4}\\s*[:alpha:]+"
  )]
  events <- events %>%
    .[purrr::map_lgl(., stringr::str_detect, "[[:alpha:]]")] %>%
    stringr::str_remove_all("\\\n") %>%
    stringr::str_remove_all("Event \\d+") %>%
    stringr::str_replace_all("([:alpha:])\\s{2,}([:alpha:])", "\\1 \\2") %>% # collapse events ending with final or round together into one string, like for skins rounds
    stringr::str_remove_all("\\(") %>%
    stringr::str_remove_all("\\)") %>%
    stringr::str_replace(".*(?=(Wom|Men|Boy|Girl|Mixed))", "") %>% # new 10/16
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
