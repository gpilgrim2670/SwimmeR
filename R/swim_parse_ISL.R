#' Formats swimming results from the International Swim Leage ('ISL') read with \code{read_results} into a dataframe
#'
#' Takes the output of \code{read_results} and cleans it, yielding a dataframe of 'ISL' swimming results
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lag
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#'
#' @param file output from \code{read_results}
#' @return returns a dataframe with columns \code{Place}, \code{Lane}, \code{Name}, \code{Team}, \code{Time}, \code{Points}, \code{Event} & \code{DQ}.
#'
#' @examples \dontrun{
#' swim_parse_ISL(
#' read_results(
#' "https://isl.global/wp-content/uploads/2019/11/isl_college_park_results_day_2.pdf"))
#'  }
#' @seealso \code{swim_parse_ISL} must be run on the output of \code{\link{read_results}}
#'

swim_parse_ISL <-
  function(file) {
    #  file <- read_results("https://swimswam.com/wp-content/uploads/2020/10/ISL-Budapest-2020-Meet-1-Day-1-CAC-NYB-ENS-LAC.pdf")

    # file <- read_results("https://isl.global/wp-content/uploads/2019/11/isl_college_park_results_day_2.pdf")

    # file <- read_results(md1)

    as_lines_list_2 <- add_row_numbers(text = file)

    # Pulls out event labels from text
    events <- event_parse_ISL(as_lines_list_2)

    # clean input data
    suppressWarnings(
      data_1 <- as_lines_list_2 %>%
        stringr::str_replace_all("\\*(\\d{1,})", replacement = "\\1") %>%  # removes * placed in front of place number in ties
        .[purrr::map(., length) > 0] %>%
        .[purrr::map_lgl(., stringr::str_detect, "\\.\\d\\d|DSQ")] %>% # must have \\.\\d\\d because all swimming and diving times do
        .[purrr::map_lgl(., stringr::str_detect, "Reaction Time", negate = TRUE)] %>% # removes header row
        .[purrr::map_lgl(., stringr::str_detect, "[:alpha:]{2,}")] %>% # must have at least two letters in a row
        stringr::str_remove_all("\n") %>%
        stringr::str_replace_all("(\\d|\\))\\s", "\\1   ") %>%
        stringr::str_replace_all("^\\s+(\\d)\\s{2,}\\d\\s{2,}(\\d)", "\\1    \\2") %>%
        stringr::str_remove_all("\\+\\d{1}\\.\\d{2}") %>% # removes the +0.07 or whatever, distance behind leader column
        stringr::str_remove_all("\\=") %>% # removes the = used to denote ties
        stringr::str_remove_all("\\(\\d\\)") %>% # removes the split placing
        trimws()
    )

    #### splits data into variables by splitting at multiple (>= 2) spaces ####
    data_1 <-
      unlist(purrr::map(data_1, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    #### breaks data into subsets based on swim type ####
    is_digit <-
      purrr::map(data_1, stringr::str_detect, "^\\d") # is first element of list a digit - no for relay swimmers, yes for everyone else
    data_relay_swimmer <-
      data_1[purrr::map(is_digit, 1) == FALSE] # pull out relay swimmers, not sure what to do with them yet
    data_entry <-
      data_1[purrr::map(is_digit, 1) == TRUE] # all entries (ie not relay swimmers, which are a kind of subentry)

    #### ind swimmer list ####
    suppressWarnings(data_ind_swimmer <-
                       data_entry[stringr::str_detect(data_entry, "[:upper:]{3}\\s-\\s[:upper:]{1,2}", negate = TRUE)] %>%  # removes relays coded as CLB - Club
    map(stringr::str_remove_all, "^\\+?\\d{1}\\.\\d{2}$") %>%  # replaces time behind times with empty strings ("")
      map(function(z){ z[!is.na(z) & z != ""]})) # removes empty strings from list# relays
    suppressWarnings(data_ind_swimmer <-
                       data_ind_swimmer[stringr::str_detect(data_ind_swimmer, "DSQ", negate = TRUE)]) # removes DQs

    #### relay list ####
    suppressWarnings(data_relay <-
                       data_entry[stringr::str_detect(data_entry, "[:upper:]{3}\\s-\\s[:upper:]{1,2}", negate = FALSE)] %>%
      map(stringr::str_remove_all, "^\\d{1,2}\\.\\d{2}$") %>%  # replaces time behind times with empty strings ("")
      map(function(z){ z[!is.na(z) & z != ""]})) # removes empty strings from list# relays
    suppressWarnings(
    data_relay <- data_relay[stringr::str_detect(data_relay, "DSQ", negate = TRUE)]) # removes DQs


    #### DQ swimmer list ####
    suppressWarnings(data_DSQ <-
                       data_1[stringr::str_detect(data_1, "DSQ") == TRUE]) # pull out DQ swimmers, called DSQ by ISL

    #### individual swimmers df ####
    if (length(data_ind_swimmer) > 0) {
      left_side <- map(data_ind_swimmer, head, 4)
      right_side <- map(data_ind_swimmer, tail, 3)

      df_left <-
        as.data.frame(t(as.data.frame(left_side)),
                      row.names = FALSE,
                      stringsAsFactors = FALSE) %>%
        dplyr::rename(
          "Place" = V1,
          "Lane" = V2,
          "Name" = V3,
          "Team" = V4
        )

      df_right <-
        as.data.frame(t(as.data.frame(right_side)),
                      row.names = FALSE,
                      stringsAsFactors = FALSE) %>%
        dplyr::rename("Time" = V1,
                      "Points" = V2,
                      "Row_Numb" = V3)

      df_ind_swimmer <- dplyr::bind_cols(df_left, df_right) %>%
        # dplyr::filter(Time != "DSQ") %>%
        dplyr::mutate(Time = dplyr::case_when(stringr::str_detect(Points, "\\d{2}\\.\\d{2}") == TRUE & stringr::str_detect(Time, "\\d{2}\\.\\d{2}", negate = TRUE) == TRUE ~ Points,
                                       TRUE ~ Time),
                      Points = dplyr::case_when(Time == Points ~ "NA",
                                                str_detect(Points, "\\d{2}\\.\\d{2}") == TRUE ~ "NA",
                                         TRUE ~ Points)) %>%
        na_if("NA")

    } else {
      df_ind_swimmer <- data.frame(
        Place = character(),
        Lane = character(),
        Name = character(),
        Team = character(),
        Time = character(),
        Points = character(),
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### relays df ####
    if (length(data_relay) > 0) {
      df_relay <-
        as.data.frame(t(as.data.frame(data_relay)),
                      row.names = FALSE,
                      stringsAsFactors = FALSE)

      if (ncol(df_relay) == 5) {
        df_relay <- df_relay %>%
          dplyr::mutate(Points = NA) %>%
          dplyr::rename(
            "Place" = V1,
            "Lane" = V2,
            "Team" = V3,
            "Time" = V4,
            "Row_Numb" = V5
          )
      } else {
        df_relay <- df_relay %>%
          dplyr::rename(
            "Place" = V1,
            "Lane" = V2,
            "Team" = V3,
            "Time" = V4,
            "Points" = V5,
            "Row_Numb" = V6
          )
      }

    } else {
      df_relay <- data.frame(
        Place = character(),
        Lane = character(),
        Team = character(),
        Time = character(),
        Points = character(),
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### DQ df ####

    data_DSQ_4 <- data_DSQ[purrr::map(data_DSQ, length) == 4]
    data_DSQ_5 <- data_DSQ[purrr::map(data_DSQ, length) == 5]
    data_DSQ_6 <- data_DSQ[purrr::map(data_DSQ, length) == 6]

    if (length(data_DSQ_4) > 0) {
        df_DSQ_4 <-
          as.data.frame(
            t(as.data.frame(data_DSQ_4)),
            row.names = FALSE,
            stringsAsFactors = FALSE
          ) %>%
            dplyr::mutate(Points = NA) %>%
            dplyr::rename(
              "Lane" = V1,
              "Team" = V2,
              "Time" = V3,
              "Row_Numb" = V4
            )

    } else {
      df_DSQ_4 <- data.frame(
        Lane = character(),
        Name = character(),
        Team = character(),
        Time = character(),
        Points = character(),
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    if (length(data_DSQ_5) > 0) {
      df_DSQ_5 <-
        as.data.frame(
          t(as.data.frame(data_DSQ_5)),
          row.names = FALSE,
          stringsAsFactors = FALSE
        ) %>%
        dplyr::mutate(Points = NA) %>%
        dplyr::rename(
          "Lane" = V1,
          "Name" = V2,
          "Team" = V3,
          "Time" = V4,
          "Row_Numb" = V5
        )

    } else {
      df_DSQ_5 <- data.frame(
        Lane = character(),
        Name = character(),
        Team = character(),
        Time = character(),
        Points = character(),
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    if (length(data_DSQ_6) > 0) {
        df_DSQ_5 <-
          as.data.frame(
            t(as.data.frame(data_DSQ_6)),
            row.names = FALSE,
            stringsAsFactors = FALSE
          ) %>%
          dplyr::rename(
            "Lane" = V1,
            "Name" = V2,
            "Team" = V3,
            "Time" = V4,
            "Points" = V5,
            "Row_Numb" = V6
          )

    } else {
      df_DSQ_6 <- data.frame(
        Lane = character(),
        Name = character(),
        Team = character(),
        Time = character(),
        Points = character(),
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### Rejoin dataframes from each number of variables ####
    Min_Row_Numb <- min(events$Event_Row_Min)
    suppressWarnings(
      data <- dplyr::bind_rows(df_ind_swimmer, df_relay) %>%
        dplyr::bind_rows(df_DSQ_4) %>%
        dplyr::bind_rows(df_DSQ_5) %>%
        dplyr::bind_rows(df_DSQ_6) %>%
        dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
        dplyr::arrange(Row_Numb) %>%
        dplyr::mutate(
          Place = as.numeric(Place),
          Place = dplyr::case_when(
            is.na(dplyr::lag(Place)) == TRUE ~ Place,
            dplyr::lag(Place) == Place ~ Place + 0.1,
            dplyr::lag(Place) != Place ~ Place
          ),
          Place = as.character(Place),
          Row_Numb = as.numeric(Row_Numb)
        ) %>%
        dplyr::filter(Row_Numb >= Min_Row_Numb)
    )

    #### add in events based on row number ranges ####
    data  <-
      transform(data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)])
    data$Row_Numb <- NULL

    #### cleaning up final results ####

    suppressWarnings(
      data <- data %>%
        ### deal with no-point swims  - covert to 0 point swims ###
        dplyr::mutate(Points = dplyr::case_when(Points == "-" ~ "0",
                                                Points != "-" ~ Points)) %>%
        ### deal with DQs ###
        dplyr::mutate(DQ = case_when(Time == "DSQ" ~ 1,
                                     TRUE ~ 0)) %>%
        na_if("DSQ") %>%
        ### clean up relay names ###
        dplyr::rowwise() %>%
        dplyr::mutate(Team = stringr::str_split_fixed(Team, "\\s", 2)[1]) %>%
        dplyr::mutate(
          Points = as.numeric(Points),
          Lane = as.numeric(Lane),
          Place = round(as.numeric(Place)),
          Event = as.character(Event)
        )

    )

    return(data)
  }

#' @rdname swim_parse_ISL
#' @export
Swim_Parse_ISL <- swim_parse_ISL
