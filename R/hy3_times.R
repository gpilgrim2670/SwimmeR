#' Helper for reading prelims and finals times from Hy-Tek .hy3 files
#'
#' Used to pull prelims and finals times from .hy3 files as part of parsing them.
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
#' @return a data frame where column 1 is times and column 2 is row number
#'
#' @seealso \code{hy3_times} is run inside of \code{\link{hy3_parse}}
#'

hy3_times <-
  function(file,
           type = c("prelims", "relay_prelims", "finals", "relay_finals")) {

    codes <- c("^E2P.*", "^F2P.*", "^E2F.*", "^F2F.*")
    time_types <-
      c("prelims", "relay_prelims", "finals", "relay_finals")
    names <-
      c("Prelims_Time",
        "Prelims_Time",
        "Finals_Time",
        "Finals_Time")

    codes_df <- data.frame(
      codes = codes,
      time_types = time_types,
      names = names,
      stringsAsFactors = FALSE
    )

    code <- codes_df$code[time_types == type]
    name <- codes_df$names[time_types == type]

    times <- file %>%
      stringr::str_extract_all(code) %>%
      .[purrr::map(., length) > 0] %>%
      stringr::str_replace_all("([:alpha:]{1,})\\s{1,}([:alpha:]{1,})", "\\1\\2") %>%
      .[purrr::map(., stringr::str_length) > 1] %>%
      trimws()

    times <-
      unlist(purrr::map(times, stringr::str_split, "\\s{1,}"),
             recursive = FALSE)

    times_rows <-
      times %>% # get row numbers, which are in the last column
      purrr::map(tail, 1) %>%
      unlist()

    times <- times %>%
      purrr::map(tail, -1) %>% # remove first column, which has hytek codes
      purrr::map(head, 1) # only want new first column, which has times

    if (length(times) == 0) {
      # if there are no times, like in meets that don't have prelims, this will make a dummy list
      times <- rep(NA, length(times_rows))
    }

    times <-
      data.frame(unlist(times),
                 Row_Numb = as.numeric(times_rows),
                 stringsAsFactors = FALSE)
    colnames(times) <- c(name, "Row_Numb")
    # times <- times %>%
    # dplyr::mutate(DQ = case_when(str_detect((!!as.name(name)), "Q") == TRUE ~ 1,
    #                              TRUE ~ 0),
    # (!! name) := stringr::str_remove((!!as.name(name)), "[:alpha:]$"))

    return(times)
  }
