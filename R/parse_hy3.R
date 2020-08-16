#' Parses Hy-Tek .hy3 files
#'
#' Helper function used inside `swim_parse` for dealing with Hy-Tek .hy3 files.  Can have more columns than other `swim_parse` outputs, because .hy3 files can contain more data
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr rowwise
#' @importFrom dplyr group_by
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_detect
#' @importFrom stringr str_length
#' @importFrom stringr str_split
#' @importFrom purrr map
#' @importFrom utils head
#' @importFrom utils tail
#'
#' @param file output from \code{read_results}
#' @param avoid a list of strings.  Rows in \code{x} containing these strings will not be included. For example "Pool:", often used to label pool records, could be passed to \code{avoid}.  The default is \code{avoid_default}, which contains many strings similar to "Pool:", such as "STATE:" and "Qual:".  Users can supply their own lists to \code{avoid}.
#' @param typo a list of strings that are typos in the original results.  \code{swim_parse} is particularly sensitive to accidental double spaces, so "Central  High School", with two spaces between "Central" and "High" is a problem, which can be fixed.  Pass "Central High School" to \code{typo}.  Unexpected commas as also an issue, for example "Texas, University of" should be fixed using \code{typo} and \code{replacement}
#' @param replacement a list of fixes for the strings in \code{typo}.  Here one could pass "Central High School" (one space between "Central" and "High") and "Texas" to \code{replacement} fix the issues described in \code{typo}
#' @return returns a dataframe with columns \code{Name}, \code{Place}, \code{Grade}, \code{School}, \code{Prelims_Time}, \code{Finals_Time}, & \code{Event}.  May also contain \code{Seed_Time}, \code{USA_ID}, and/or \code{Birthdate}.  Note all swims will have a \code{Finals_Time}, even if that time was actually swam in the prelims (i.e. a swimmer did not qualify for finals).  This is so that final results for an event can be generated from just one column.
#'
#' @seealso \code{parse_hy3} must be run on the output of \code{\link{read_results}}
#' @seealso \code{parse_hy3} runs inside of \code{\link{swim_parse}}

parse_hy3 <- function(file, avoid = avoid_minimal, typo = typo_default, replacement = replacement_default) {

  avoid_minimal <- c("Sammy Steroids")

  typo_default <- c("typo")

  replacement_default <- c("typo")

   file <- file %>%
    .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid)))] %>%
    stringr::str_replace_all(stats::setNames(replacement, typo))
# data beginning with E1M or E1F contains results from each swim (male and female respectively)
entry <- file %>%
  stringr::str_extract_all("^E1M.*|^E1F.*") %>%
  .[purrr::map(., length)>0] %>%
  stringr::str_replace_all("([:alpha:]{1,})\\s{1,}([:alpha:]{1,})", "\\1\\2") %>%
  trimws()

entry <-
  unlist(purrr::map(entry, stringr::str_split, "\\s{1,}"), recursive = FALSE) %>%
  .[purrr::map(., length)>2]

entry_rows <- entry %>%
  map(tail, 1) %>%
  unlist()

entry <- entry %>%
  # map(tail, -1) %>%
  map(head, 10)

entry <- data.frame(entry, stringsAsFactors = FALSE) %>%
  t()
rownames(entry) <- NULL

entry <- data.frame(entry, stringsAsFactors = FALSE)
entry <- entry[c("X2","X3","X9")]
colnames(entry) <- c("ID", "Event", "Seed_Time")
entry$Row_Numb <- as.numeric(entry_rows)

# entries are doubled in the case of prelims/finals
# need to collect both prelim and final entry into one row
entry <- entry %>%
  group_by(ID, Event, Seed_Time) %>%
  dplyr::summarise(Row_Numb = min(as.numeric(Row_Numb))) %>%
  dplyr::arrange(Row_Numb) %>%
  dplyr::ungroup()

entry <- entry %>%
  dplyr::mutate(ID_Numb = stringr::str_extract(ID, "^\\d{1,}"),
         Row_Numb = as.numeric(Row_Numb)) %>%
  dplyr::mutate(Gender = dplyr::case_when(stringr::str_detect(ID, "MB$|MM$") ~ "M",
                            stringr::str_detect(ID, "FG$|FW$") ~ "F")) %>%
  dplyr::mutate(Course = stringr::str_extract(Seed_Time, "[:alpha:]$"),
         Course = dplyr::case_when(Course == "Y" ~ "Yard",
                            Course == "M" ~ "Meter"),
         Event = dplyr::case_when(Event == "25A" ~ paste("25", Course,  "Freestyle"),
                           Event == "50A" ~ paste("50", Course,  "Freestyle"),
                           Event == "100A" ~ paste("100", Course,  "Freestyle"),
                           Event == "200A" ~ paste("200", Course,  "Freestyle"),
                           Event == "400A" ~ paste("400", Course,  "Freestyle"),
                           Event == "500A" ~ paste("500", Course,  "Freestyle"),
                           Event == "800A" ~ paste("800", Course,  "Freestyle"),
                           Event == "1000A" ~ paste("1000", Course,  "Freestyle"),
                           Event == "1500A" ~ paste("1500", Course,  "Freestyle"),
                           Event == "1650A" ~ paste("1650", Course,  "Freestyle"),
                           Event == "25B" ~ paste("25", Course,  "Backstroke"),
                           Event == "50B" ~ paste("50", Course,  "Backstroke"),
                           Event == "100B" ~ paste("100", Course,  "Backstroke"),
                           Event == "200B" ~ paste("200", Course,  "Backstroke"),
                           Event == "25C" ~ paste("25", Course,  "Breaststroke"),
                           Event == "50C" ~ paste("50", Course,  "Breaststroke"),
                           Event == "100C" ~ paste("100", Course,  "Breaststroke"),
                           Event == "200C" ~ paste("200", Course,  "Breaststroke"),
                           Event == "25D" ~ paste("25", Course,  "Butterfly"),
                           Event == "50D" ~ paste("50", Course,  "Butterfly"),
                           Event == "100D" ~ paste("100", Course,  "Butterfly"),
                           Event == "200D" ~ paste("200", Course,  "Butterfly"),
                           Event == "100E" ~ paste("100", Course,  "Individual Medley"),
                           Event == "200E" ~ paste("200", Course,  "Individual Medley"),
                           Event == "400E" ~ paste("400", Course,  "Individual Medley"),
                           Event == "6F" ~ "1 mtr Diving (6 dives)",
                           Event == "11F" ~ "1 mtr Diving (11 dives)"),
         Seed_Time = stringr::str_remove(Seed_Time, "[:alpha:]$")) %>%
  ungroup() %>%
  dplyr::mutate(Row_Min = as.numeric(Row_Numb),
         Row_Max = dplyr::lead(Row_Min, 1L, default = length(file) - 1),
         Row_Min = Row_Min - 0.1) %>%
  dplyr::mutate(Finals_Time = NA,
         Prelims_Time = NA) %>%
  dplyr::select(-ID, -Course) %>%
  arrange(Row_Min)

finals <- hy3_times(file = file, type = "finals")
prelims <- hy3_times(file = file, type = "prelims")

entry <- interleave_times(results = entry, times = finals, type = "individual")
entry <- interleave_times(results = entry, times = prelims, type = "individual")

entry <- entry %>%
  dplyr::mutate(
    Seed_Time = stringr::str_remove(Seed_Time, "[A-Z]{1,}"),
    Prelims_Time = stringr::str_remove(Prelims_Time, "[A-Z]{1,}"),
    Finals_Time = stringr::str_remove(Finals_Time, "[A-Z]{1,}")
  ) %>%
  dplyr::mutate(
    Seed_Time = dplyr::case_when(stringr::str_detect(Event, "Diving") == FALSE ~ mmss_format(as.numeric(Seed_Time)),
                          TRUE ~ Seed_Time),
    Prelims_Time = dplyr::case_when(stringr::str_detect(Event, "Diving") == FALSE ~ mmss_format(as.numeric(Prelims_Time)),
                            TRUE ~ Prelims_Time),
    Finals_Time = dplyr::case_when(stringr::str_detect(Event, "Diving") == FALSE ~ mmss_format(as.numeric(Finals_Time)),
                           TRUE ~ Finals_Time)
  )

# data beginning with D1M contains swimmer info (M for male, F for female)
swimmer <- file %>%
  stringr::str_extract_all("^D1M.*|^D1F.*") %>%
  .[purrr::map(., length)>0] %>%
  str_replace_all("([:alpha:]{1,})\\s([:alpha:]{1,})", "\\1\\2") %>%
  trimws()

swimmer <-
  unlist(purrr::map(swimmer, stringr::str_split, "\\s{1,}"), recursive = FALSE)


swimmer_rows <- swimmer %>%
  map(tail, 1) %>%
  unlist()

swimmer <- swimmer %>%
  # map(tail, -1) %>%
  map(unique) %>%
  map(head, 7)

swimmer <- data.frame(swimmer, stringsAsFactors = FALSE) %>%
  t()
rownames(swimmer) <- NULL

swimmer <- data.frame(swimmer, stringsAsFactors = FALSE)

swimmer <- swimmer %>%
  dplyr::mutate(ID = X2,
         First = X3,
         USA_ID = dplyr::case_when(stringr::str_detect(X4, "[A-Z]{3,}") ~ X4,
                            stringr::str_detect(X5, "[A-Z]{3,}") ~ X5),
         Birthdate = dplyr::case_when(stringr::str_detect(X5, "[A-Z]{3,}") == FALSE & stringr::str_length(X5) >= 6 ~ X5,
                               stringr::str_detect(X6, "[A-Z]{3,}") == FALSE & stringr::str_length(X6) >= 6 ~ X6),
         Grade = dplyr::case_when(stringr::str_length(X5) < 6 & stringr::str_length(X5) >= 1 & X5 != "0" & stringr::str_detect(X4, "[A-Z]{3,}") == FALSE ~ X5,
                           stringr::str_length(X6) < 6 & stringr::str_length(X6) >= 1 & X6 != "0" ~ X6,
                           stringr::str_length(X6) >= 6 & stringr::str_length(X7) < 6 & X7 != "0" ~ X7)) %>%
  dplyr::mutate(Row_Numb = as.numeric(swimmer_rows)) %>%
  dplyr::select(ID, First, USA_ID, Birthdate, Grade, Row_Numb)

swimmer <- swimmer %>%
  dplyr::mutate(ID_Numb = stringr::str_extract(ID, "^\\d{1,}"),
         Row_Numb = as.numeric(Row_Numb),
         Last_Name = stringr::str_remove(ID, ID_Numb),
         Name = paste0(Last_Name, ", ", First)) %>%
  dplyr::select(-Last_Name, -First, -ID)


# data beginning with C1 contains team info
team <- file %>%
  stringr::str_extract_all("^C1.*") %>%
  .[purrr::map(., length)>0] %>%
  str_replace_all("\\s{2,}Unattached", " Unattached") %>%
  trimws()

team <-
  unlist(purrr::map(team, stringr::str_split, "\\s{2,}"), recursive = FALSE) %>%
  map(unique)

team_rows <- team %>%
  map(tail, 1) %>%
  unlist()

team <- team %>%
  map(head, 1) %>%
  map(paste, collapse = " ")

team <- data.frame(School = unlist(team), Row_Numb = as.numeric(team_rows)) %>%
  dplyr::mutate(School = stringr::str_remove(School, "^C1[A-Z]{1,} ")) %>%
  dplyr::mutate(Row_Min = as.numeric(Row_Numb),
         Row_Max = dplyr::lead(Row_Min, 1L, default = length(file)) - 1,)


# data beginning with F1 contains relay info
relay <- file %>%
  stringr::str_extract_all("^F1.*") %>%
  .[purrr::map(., length)>0] %>%
  # str_replace_all("([:alpha:]{1,})\\s{2,}([:alpha:]{1,})", "\\1\\2") %>%
  trimws()

relay <-
  unlist(purrr::map(relay, stringr::str_split, "\\s{1,}"), recursive = FALSE) %>%
  map(unique)

relay_rows <- relay %>%
  map(tail, 1) %>%
  unlist()

relay <- relay %>%
  # map(tail, -1) %>%
  map(unique) %>%
  map(head, 9)

relay <- data.frame(relay, stringsAsFactors = FALSE) %>%
  t()

rownames(relay) <- NULL

relay <- data.frame(relay, stringsAsFactors = FALSE)

relay <- relay[c("X1", "X2", "X3", "X4","X8", "X9")]

colnames(relay) <- c("Team", "Relay_Rank", "ID", "Event", "X8", "X9")
relay$Row_Numb <- as.numeric(relay_rows)

relay <- relay %>%
  dplyr::mutate(Seed_Time = dplyr::case_when(stringr::str_detect(X8, "\\d{2,}\\.\\d{2,}") ~ X8,
                               stringr::str_detect(X9, "\\d{2,}\\.\\d{2,}") ~ X9,
                               TRUE ~ "NA")) %>%
  dplyr::na_if("^NA$") %>%
  dplyr::select(-X8, -X9)

relay <- relay %>%
  dplyr::group_by(Team, Relay_Rank, Event, Seed_Time, ID) %>%
  dplyr::summarise(Row_Numb = min(as.numeric(Row_Numb), na.rm = TRUE)) %>%
  dplyr::arrange(Row_Numb) %>%
  dplyr::ungroup()

relay <- relay %>%
  dplyr::mutate(Gender = dplyr::case_when(stringr::str_detect(ID, "MB$|MM$|MXX$") ~ "M",
                            stringr::str_detect(ID, "FG$|FW$|FXX$") ~ "F")) %>%
  dplyr::mutate(Course = stringr::str_extract(Seed_Time, "[:alpha:]$"),
         Course = dplyr::case_when(Course == "Y" ~ "Yard",
                            Course == "M" ~ "Meter"),
         Event = dplyr::case_when(Event == "200E" ~ paste("200", Course,  "Medley Relay"),
                           Event == "400E" ~ paste("400", Course,  "Medley Relay"),
                           Event == "200A" ~ paste("200", Course,  "Freesytle Relay"),
                           Event == "400A" ~ paste("400", Course,  "Freesytle Relay"),
                           Event == "800A" ~ paste("800", Course,  "Freesytle Relay")),
         Seed_Time = stringr::str_remove(Seed_Time, "[:alpha:]$")) %>%
  dplyr::mutate(Row_Min = as.numeric(Row_Numb),
         Row_Max = dplyr::lead(Row_Min, 1L, default = length(file) - 1),
         Row_Min = Row_Min - 0.1) %>%
  dplyr::mutate(Finals_Time = NA,
         Prelims_Time = NA) %>%
  dplyr::select(-Course, -Team, -Relay_Rank, -ID)


relay_finals <- hy3_times(file = file, type = "relay_finals")
relay_prelims <- hy3_times(file = file, type = "relay_prelims")

relay <- interleave_times(results = relay, times = relay_finals, type = "relay")
relay <- interleave_times(results = relay, times = relay_prelims, type = "relay")

relay <- relay %>%
  dplyr::mutate(
    Seed_Time = stringr::str_remove(Seed_Time, "[A-Z]{1,}"),
    Prelims_Time = stringr::str_remove(Prelims_Time, "[A-Z]{1,}"),
    Finals_Time = stringr::str_remove(Finals_Time, "[A-Z]{1,}")
  ) %>%
  dplyr::mutate(
    Seed_Time = mmss_format(as.numeric(Seed_Time)),
    Prelims_Time = mmss_format(as.numeric(Prelims_Time)),
    Finals_Time = mmss_format(as.numeric(Finals_Time))
  )

#### Binding up data
data <- dplyr::left_join(swimmer, entry, by = "ID_Numb") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Row_Numb = min(c(Row_Numb.x, Row_Numb.y), na.rm = TRUE)) %>%
  dplyr::select(-Row_Numb.x, -Row_Numb.y)

data <- dplyr::bind_rows(data, relay)

data  <-
  transform(data, School = team$School[findInterval(Row_Numb, team$Row_Min)])

data <- data %>%
  dplyr::mutate(Finals_Time = dplyr::case_when(stringr::str_detect(Finals_Time, "[:alpha:]") ~ "Bad Entry",
                                 TRUE ~ Finals_Time),
         Prelims_Time = dplyr::case_when(stringr::str_detect(Prelims_Time, "[:alpha:]") ~ "Bad Entry",
                                  TRUE ~ Prelims_Time)) %>%
  dplyr::na_if("Bad Entry") %>%
  dplyr::mutate(Finals_Time = dplyr::case_when((is.na(Prelims_Time) == FALSE & is.na(Finals_Time) == TRUE) ~ Prelims_Time,
                                 TRUE ~ Finals_Time)) %>%
  dplyr::mutate(Birthdate = stringr::str_extract(USA_ID, "\\d{6,8}"),
         USA_ID = dplyr::case_when(stringr::str_length(USA_ID) < 8 ~ "Bad Entry",
                            TRUE ~ USA_ID)) %>%
  dplyr::na_if("Bad Entry") %>%
  dplyr::select(-Row_Min, -Row_Max, -Row_Numb, -ID_Numb)

## cleaning up data
data <- data %>%
  dplyr::mutate(Finals_Time = dplyr::case_when(is.na(Finals_Time) == TRUE & is.na(Prelims_Time) == FALSE ~ Prelims_Time,
                                               TRUE ~ Finals_Time)) %>%
  dplyr::na_if("00.00")


return(data)

}
