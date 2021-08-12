#' Pulls out event labels from text
#'
#' Locates event labels in text of results output from \code{read_results} and
#' their associated row numbers.  The resulting data frame is joined back into
#' results to include event names
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
#' @param text output from \code{read_results} followed by
#'   \code{add_row_numbers}
#' @return returns a data frame with event names and row numbers to eventually
#'   be recombined with swimming results inside \code{swim_parse}
#'
#' @seealso \code{event_parse} is a helper function inside
#'   \code{\link{swim_parse}}

event_parse <- function(text) {
  # text <- as_lines_list_2

  omega_headers_string <- "Records Set"

  olympics_string <- "Men.* 4?\\s?x?\\s?\\d{1,}m.*|Women.* 4?\\s?x?\\s?\\d{1,}m.*|Mixed.* 4?\\s?x?\\s?\\d{1,}m.*"

  event_string <-
    "Event\\:?\\s?#?\\s+\\d{1,}|EVENT\\:?\\s?#?\\s+\\d{1,}|Women.* Yard|Women.* Meter|Women.* Metre|Girls.* Yard|Girls.* Meter|Girls.* Metre|Men.* Yard|Men.* Meter|Men.* Metre|Boys.* Yard|Boys.* Meter|Boys.* Metre|Mixed.* Yard|Mixed.* Meter|Mixed.* Metre"

  event_string <- paste(event_string, olympics_string, omega_headers_string, sep = "|")

  events <- text %>%
    .[purrr::map_lgl(.,
                     stringr::str_detect,
                     event_string)] %>% # new 12/15 for older NCAA results
    .[purrr::map_lgl(., ~ !any(stringr::str_detect(., "\\.\\.\\.")))] %>%  # removes subheaders like in OT results "Semi-Finals ... (women...)" etc.
    .[purrr::map_lgl(., stringr::str_detect, "\\d{2}\\.\\d{2}", negate = TRUE)] %>%
    .[purrr::map_lgl(., stringr::str_detect, "\\d{2} [:upper:]{3} \\d{4} GOLD", negate = TRUE)]

  if (length(events) > 0) {
    #if event names are recognized clean them up and determine row ranges
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
      stringr::str_replace_all("\\s{2,}", " ") %>%
      stringr::str_replace(" (\\d{1,})$", "   \\1") %>% # new 12/15 for older NCAA results
      stringr::str_remove("\\sCONT\\s?.*") %>%
      # stringr::str_replace(".*(?=(Wom|Men|Boy|Girl))", "") %>% # new 10/16
      trimws()

    events <-
      unlist(purrr::map(events, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    # dataframe for events with names and row number ranges
    events <- events %>%
      list_transform() %>%
      dplyr::mutate(
        Event = stringr::str_extract(V1, "[[:graph:] ]*"),
        Event_Row_Min = as.numeric(V2),
        Event_Row_Max = dplyr::lead(Event_Row_Min, 1L, default = length(text)) - 1,
        V1 = NULL,
        V2 = NULL
      )
  } else{
    # if no event names are recognized deploy dummy dataframe with event name "unknown" and post warning
    events <- data.frame(
      Event = "Unknown",
      Event_Row_Min = 1,
      Event_Row_Max = length(text) - 1,
      stringsAsFactors = FALSE
    )
    warning("No event names recognized - defaulting to NA")
  }
  return(events)

}
