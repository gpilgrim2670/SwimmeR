#' Formats swimming results from the International Swim League ('ISL') read with
#' \code{read_results} into a data frame
#'
#' Takes the output of \code{read_results} and cleans it, yielding a data frame
#' of 'ISL' swimming results
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
#' @importFrom dplyr rowwise
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom purrr map
#'
#' @param file output from \code{read_results}
#' @param splits should splits be included, default is \code{FALSE}
#' @param relay_swimmers should relay swimmers be included as separate columns,
#'   default is \code{FALSE}
#'
#' @return returns a data frame of ISL results
#' @examples \dontrun{
#' swim_parse_ISL(
#' read_results(
#' "https://isl.global/wp-content/uploads/2019/11/isl_college_park_results_day_2.pdf"),
#' splits = TRUE,
#' relay_swimmers = TRUE)
#'  }
#' @seealso \code{swim_parse_ISL} must be run on the output of
#'   \code{\link{read_results}}

swim_parse_ISL <-
  function(file, splits = FALSE, relay_swimmers = FALSE) {


    #### testing ####
    # file <- "https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/ISL/Season_2_2020/ISL_01112020_Budapest_Match_6.pdf"
    # file <- "https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/ISL/Season_2_2020/ISL_18102020_Budapest_Match_2.pdf"
    # file <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/ISL/Season_2_2020/ISL_05112020_Budapest_Match_8.pdf"
    # file <- "https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/ISL/Season_2_2020/ISL_24102020_Budapest_Match_3.pdf"
    # file <- "https://github.com/gpilgrim2670/Pilgrim_Data/raw/master/ISL/Season_1_2019/ISL_16112019_CollegePark_Day_1.pdf"
    # file <- read_results(file)
    # splits = TRUE
    # relay_swimmers = TRUE

    #### Begin Actual Function ####
    if(is.logical(splits) == FALSE) {
      stop("splits must be logical, either TRUE or FALSE")
    }

    if(is.logical(relay_swimmers) == FALSE) {
      stop("relay_swimmers must be logical, either TRUE or FALSE")
    }

    #### Add row numbers ####
    as_lines_list_2 <- add_row_numbers(text = file)

    #### Pulls out event labels from text ####
    events <- event_parse_ISL(as_lines_list_2)

    #### Define min row number ####
    Min_Row_Numb <- min(events$Event_Row_Min)

    #### clean input data ####
    suppressWarnings(
      data_cleaned <- as_lines_list_2 %>%
        stringr::str_replace_all("\\*(\\d{1,})", replacement = "\\1") %>%  # removes * placed in front of place number in ties
        .[purrr::map(., length) > 0] %>%
        .[stringr::str_detect(., "\\d\\d\\.\\d\\d|DSQ|[:upper:]{2,}\\s[:upper:][:lower:]")] %>% # must have \\.\\d\\d because all swimming and diving times do
        .[stringr::str_detect(., "Reaction Time", negate = TRUE)] %>% # removes header row
        .[stringr::str_detect(., "[:alpha:]{2,}")] %>% # must have at least two letters in a row
        stringr::str_remove_all("\n") %>%
        stringr::str_replace_all("(\\d|\\))\\s", "\\1   ") %>%
        stringr::str_replace_all("^\\s+(\\d)\\s{2,}\\d\\s{2,}(\\d)", "\\1    \\2") %>%
        stringr::str_remove_all("\\+\\d{1,2}\\.\\d{2}") %>% # removes the +0.07 or whatever, distance behind leader column
        stringr::str_replace_all("\\s+Q\\s+", "   ") %>% # removes Q used to denote qualifying for skins rounds
        stringr::str_remove_all("\\=") %>% # removes the = used to denote ties
        stringr::str_remove_all("\\(\\d\\)") %>% # removes the split placing
        # stringr::str_replace_all("MUNOZ del CAMPO Lidon AQC", "MUNOZ del CAMPO Lidon   AQC") %>% # season 2 match 5 issue, would prefer general solution
        stringr::str_replace_all("(?<=[:lower:]\\s)AQC", "   AQC") %>%
        stringr::str_replace_all("(?<=[:lower:]\\s)CAC", "   CAC") %>%
        stringr::str_replace_all("(?<=[:lower:]\\s)ENS", "   ENS") %>%
        stringr::str_replace_all("(?<=[:lower:]\\s)DCT", "   DCT") %>%
        stringr::str_replace_all("(?<=[:lower:]\\s)LON", "   LON") %>%
        stringr::str_replace_all("(?<=[:lower:]\\s)LAC", "   LAC") %>%
        stringr::str_replace_all("(?<=[:lower:]\\s)TOK", "   TOK") %>%
        stringr::str_replace_all("(?<=[:lower:]\\s)IRO", "   IRO") %>%
        stringr::str_replace_all("(?<=[:lower:]\\s)NYB", "   NYB") %>%
        stringr::str_replace_all("(?<=[:lower:]\\s)TOR", "   TOR") %>%
        trimws()
    )

    #### if data_cleaned is empty ####
    if(!length(data_cleaned) > 0){
      message("No results found in file")

    } else {

    #### splits data into variables by splitting at multiple (>= 2) spaces ####
    data_cleaned <-
      unlist(purrr::map(data_cleaned, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    #### breaks data into subsets based on swim type ####
    is_digit <-
      purrr::map(data_cleaned, stringr::str_detect, "^\\d") # is first element of list a digit - no for relay swimmers, yes for everyone else

    if(relay_swimmers == TRUE){
    data_relay_swimmer <-
      data_cleaned[purrr::map(is_digit, 1) == FALSE] # pull out relay swimmers
    } else {
      data_relay_swimmer <- NULL
    }

    data_entry <-
      data_cleaned[purrr::map(is_digit, 1) == TRUE] # all entries (ie not relay swimmers, which are a kind of sub entry)

    #### ind swimmer list ####
    suppressWarnings(data_ind_swimmer <-
                       data_entry[stringr::str_detect(data_entry, "[:upper:]{3}\\s-\\s[:upper:]{1,2}", negate = TRUE)] %>%  # removes relays coded as CLB - Club
    map(stringr::str_remove_all, "^\\+?1?\\d{1}\\.\\d{2}$") %>%  # replaces time behind times with empty strings ("")
      map(function(z){ z[!is.na(z) & z != ""]})) # removes empty strings from list# relays
    suppressWarnings(data_ind_swimmer <-
                       data_ind_swimmer[stringr::str_detect(data_ind_swimmer, "(DSQ)|(DNS)", negate = TRUE)]) # removes DQs

    #### relay list ####
    suppressWarnings(data_relay <-
                       data_entry[stringr::str_detect(data_entry, "[:upper:]{3}\\s-\\s[:upper:]{1,2}", negate = FALSE)] %>%
      map(stringr::str_remove_all, "^\\d{1,2}\\.\\d{2}$") %>%  # replaces time behind times with empty strings ("")
      map(function(z){ z[!is.na(z) & z != ""]})) # removes empty strings from list# relays
    suppressWarnings(data_relay_NoDQ <-
                       data_relay[stringr::str_detect(data_relay, "DSQ", negate = TRUE)]) # removes DQs
    suppressWarnings(data_relay_DQ <-
                       data_relay[stringr::str_detect(data_relay, "DSQ")]) # collects DQs
    data_relay_DQ <- data_relay_DQ %>%
      map(~ c("10000", .x))

    data_relay <- append(data_relay_NoDQ, data_relay_DQ)


    #### DQ swimmer list ####
    suppressWarnings(data_DSQ <-
                       data_cleaned[stringr::str_detect(data_cleaned, "(DSQ)|(DNS)") == TRUE]) # pull out DQ swimmers, called DSQ by ISL

    #### individual swimmers df ####
    if (length(data_ind_swimmer) > 0) {
      left_side <- map(data_ind_swimmer, head, 4)
      right_side <- map(data_ind_swimmer, tail, 3)

      df_left <- left_side %>%
        list_transform() %>%
        dplyr::rename(
          "Place" = V1,
          "Lane" = V2,
          "Name" = V3,
          "Team" = V4
        )

      df_right <- right_side %>%
        list_transform() %>%
        dplyr::rename("Finals_Time" = V1,
                      "Points" = V2,
                      "Row_Numb" = V3)

      df_ind_swimmer <- dplyr::bind_cols(df_left, df_right) %>%
        dplyr::mutate(
          Finals_Time = dplyr::case_when(
            stringr::str_detect(Points, "\\d{2}\\.\\d{2}") == TRUE ~ Points,
            TRUE ~ Finals_Time
          ),
          Points = dplyr::case_when(
            Finals_Time == Points ~ "NA",
            str_detect(Points, "\\d{2}\\.\\d{2}") == TRUE ~ "NA",
            TRUE ~ Points
          )
        ) %>%
        na_if("NA")

    } else {
      df_ind_swimmer <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### relays df ####
    if (length(data_relay) > 0) {
      df_relay <- data_relay %>%
        list_transform()

      if (ncol(df_relay) == 5) {
        df_relay <- df_relay %>%
          dplyr::mutate(Points = NA) %>%
          dplyr::rename(
            "Place" = V1,
            "Lane" = V2,
            "Team" = V3,
            "Finals_Time" = V4,
            "Row_Numb" = V5
          )
      } else {
        df_relay <- df_relay %>%
          dplyr::rename(
            "Place" = V1,
            "Lane" = V2,
            "Team" = V3,
            "Finals_Time" = V4,
            "Points" = V5,
            "Row_Numb" = V6
          )
      }

    } else {
      df_relay <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### relay swimmers df ####
    if (length(data_relay_swimmer) > 0) {
      left_side <- map(data_relay_swimmer, head, 1)
      right_side <- map(data_relay_swimmer, tail, 1)

      df_left <- left_side %>%
        list_transform() %>%
        dplyr::rename(
          "Name" = V1
        )

      df_right <- right_side %>%
        list_transform()

      df_relay_swimmer <- dplyr::bind_cols(df_left, df_right) %>%
        dplyr::na_if("NA")

      suppressWarnings(
      df_relay_swimmer <- df_relay_swimmer %>%
        lines_sort(min_row = min(as.numeric(df_relay_swimmer$V1))) %>%
        dplyr::mutate(Row_Numb = as.numeric(Row_Numb) - 1) %>%
        dplyr::mutate(Row_Numb = as.character(Row_Numb)) %>%
        dplyr::rename("Relay_Swimmer_1" = V2,
               "Relay_Swimmer_2" = V3,
               "Relay_Swimmer_3" = V4,
               "Relay_Swimmer_4" = V5)
      )

    } else {
      df_relay_swimmer <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### DQ df ####
    data_DSQ_4 <- data_DSQ[purrr::map(data_DSQ, length) == 4]
    data_DSQ_5 <- data_DSQ[purrr::map(data_DSQ, length) == 5]
    data_DSQ_6 <- data_DSQ[purrr::map(data_DSQ, length) == 6]

    # if (length(data_DSQ_4) > 0) {
    #     df_DSQ_4 <- data_DSQ_4 %>%
    #       list_transform() %>%
    #         dplyr::mutate(Points = NA) %>%
    #         dplyr::rename(
    #           "Lane" = V1,
    #           "Team" = V2,
    #           "Finals_Time" = V3,
    #           "Row_Numb" = V4
    #         )
    #
    # } else {
    #   df_DSQ_4 <- data.frame(
    #     Row_Numb = character(),
    #     stringsAsFactors = FALSE
    #   )
    # }

    if (length(data_DSQ_5) > 0) {
      df_DSQ_5 <- data_DSQ_5 %>%
        list_transform() %>%
        dplyr::rename("Lane" = V1,
                      "Row_Numb" = V5) %>%
        dplyr::mutate(Name = dplyr::case_when(stringr::str_detect(V4, "DSQ") == TRUE ~ V2)) %>%
        dplyr::mutate(Team = dplyr::case_when(stringr::str_detect(V3, "DSQ") == TRUE ~ V2,
                                              TRUE ~ V3)) %>%
        dplyr::mutate(
          Finals_Time = dplyr::case_when(
            stringr::str_detect(V3, "DSQ") == TRUE ~ V3,
            stringr::str_detect(V4, "DSQ") == TRUE ~ V4
          )
        ) %>%
        dplyr::mutate(Points = dplyr::case_when(stringr::str_detect(V3, "DSQ") == TRUE ~ V4,
                                                TRUE ~ "NA")) %>%
        dplyr::na_if("NA") %>%
        dplyr::select(Lane, Name, Team, Finals_Time, Row_Numb)

    } else {
      df_DSQ_5 <- data.frame(Row_Numb = character(),
                             stringsAsFactors = FALSE)
    }

    if (length(data_DSQ_6) > 0) {
        df_DSQ_6 <- data_DSQ_6 %>%
          list_transform() %>%
          dplyr::rename(
            "Lane" = V1,
            "Name" = V2,
            "Team" = V3,
            "Finals_Time" = V4,
            "Points" = V5,
            "Row_Numb" = V6
          )

    } else {
      df_DSQ_6 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### Join up relay and relay swimmers data frames ####
    df_relay <- dplyr::left_join(df_relay, df_relay_swimmer)

    #### Rejoin data frames from each number of variables ####
    suppressWarnings(
      data <- dplyr::bind_rows(df_ind_swimmer, df_relay) %>%
        # dplyr::bind_rows(df_DSQ_4) %>%
        dplyr::bind_rows(df_DSQ_5) %>%
        dplyr::bind_rows(df_DSQ_6) %>%
        dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
        dplyr::arrange(Row_Numb) %>%
        dplyr::na_if("10000") %>%
        unique() %>%
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

    data <- data %>%
      filter(Event != "Results Summary")

    #### cleaning up final results ####
    suppressWarnings(
      data <- data %>%
        ### deal with no-point swims  - covert to 0 point swims ###
        dplyr::mutate(Points = dplyr::case_when(Points == "-" ~ "0",
                                                Points != "-" ~ Points)) %>%
        ### deal with DQs ###
        dplyr::mutate(DQ = case_when(Finals_Time == "DSQ" ~ 1,
                                     Finals_Time == "DNS" ~ 1,
                                     TRUE ~ 0)) %>%
        dplyr::na_if("DSQ") %>%
        ### clean up relay names ###
        dplyr::rowwise() %>%
        dplyr::mutate(Team = stringr::str_split_fixed(Team, "\\s", 2)[1]) %>%
        dplyr::mutate(
          Points = as.numeric(Points),
          Lane = as.numeric(Lane),
          Place = round(as.numeric(Place)),
          Event = as.character(Event)
        ) %>%
        dplyr::filter(stringr::str_detect(Event, "Club Standing") == FALSE) %>%
        dplyr::filter(stringr::str_detect(Team, "\\d") == FALSE) # removes summary stats the ISL appends to bottom of meet
    )

    if(relay_swimmers == TRUE){
    data <- data %>%
      dplyr::select(Row_Numb, Place, Lane, Name, Team, Finals_Time, Event, Points, DQ, Relay_Swimmer_1, Relay_Swimmer_2, Relay_Swimmer_3, Relay_Swimmer_4)
    # data <- data %>%
    #   filter(is.na(Relay_Swimmer_1) & str_detect(Event, "\\d\\s?x\\s?\\d"))
    } else {
      data <- data %>%
        dplyr::select(Row_Numb, Place, Lane, Name, Team, Finals_Time, Event, Points, DQ)
    }

    #### adding splits back in ####
    if (splits == TRUE) {
      splits_df <- splits_parse_ISL(as_lines_list_2)

      data <- data %>%
        dplyr::left_join(splits_df, by = 'Row_Numb')

      #### calculate 50 split since it's blank coming in from  split_parse_ISL due to row mismatches
      suppressWarnings(data <- data %>%
        dplyr::mutate(Split_50 = as.numeric(Split_50)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Split_50 = dplyr::case_when(is.na(Split_100) == FALSE & str_detect(Event, "\\d\\s?x\\s?\\d") == FALSE ~ round(
          sec_format(Finals_Time) - sum(as.numeric(
            c(
              Split_100,
              Split_150,
              Split_200,
              Split_250,
              Split_300,
              Split_350,
              Split_400
            )
          ), na.rm = TRUE), 2
        ),
        TRUE ~ Split_50)) %>%
        dplyr::mutate(Split_50 = sprintf("%.2f", Split_50)) %>% # to keep trailing zero e.g. in 24.50
        na_if("NA")
      )
    }

    ### remove empty columns (all values are NA) ###
    data <- Filter(function(x)
      !all(is.na(x)), data)

    data$Row_Numb <- NULL
    return(data)
    }
  }

#' @rdname swim_parse_ISL
#' @export
Swim_Parse_ISL <- swim_parse_ISL
