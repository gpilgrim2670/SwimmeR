#' Helper for reading prelims and finals places from Hy-Tek .hy3 files
#'
#' Used to pull prelims and finals places from .hy3 files as part of parsing them.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_length
#' @importFrom stringr str_split
#' @importFrom stringr str_remove
#' @importFrom purrr map

#' @param file an output of read_results, from an .hy3 file
#' @param type type of times, either "prelims", "relay_prelims", "finals" or "relay_finals"
#' @return a dataframe where column 1 is times and column 2 is row number
#'
#' @seealso \code{hy3_places} is run inside of \code{\link{parse_hy3}}
#'

hy3_places <-
  function(file,
           type = c("prelims", "relay_prelims", "finals", "relay_finals")) {

    codes <- c("^E2P.*", "^F2P.*", "^E2F.*", "^F2F.*")
    result_types <-
      c("prelims", "relay_prelims", "finals", "relay_finals")
    names <-
      c("Prelims_Place",
        "Prelims_Place",
        "Finals_Place",
        "Finals_Place")
    codes_df <- data.frame(
      codes = codes,
      result_types = result_types,
      names = names,
      stringsAsFactors = FALSE
    )

    code <- codes_df$code[result_types == type]
    name <- codes_df$names[result_types == type]

    results <- file %>%
      stringr::str_extract_all(code) %>%
      .[purrr::map(., length) > 0] %>%
      stringr::str_replace_all("([:alpha:]{1,})\\s{1,}([:alpha:]{1,})", "\\1\\2") %>%
      .[purrr::map(., stringr::str_length) > 1] %>%
      trimws()

    results <-
      unlist(purrr::map(results, stringr::str_split, "\\s{1,}"),
             recursive = FALSE)

    places_rows <-
      results %>% # get row numbers, which are in the last column
      purrr::map(tail, 1) %>%
      unlist()

    places <- results %>%
      purrr::map(tail,-6) %>% # remove first column, which has hy-tek codes
      purrr::map(head, 1) # only want new first column, which has places

    if(length(places) == 0){ # if there are no places, like in meets that don't have prelims, this will make a dummy list
      places <- rep(NA, length(places_rows))
    }

    places <-
      data.frame(unlist(places),
                 Row_Numb = as.numeric(places_rows),
                 stringsAsFactors = FALSE)
    colnames(places) <- c(name, "Row_Numb")
    places <- places %>%
      dplyr::mutate((!! name) := stringr::str_remove((!!as.name(name)), "[:alpha:]$"))

    return(places)
  }
