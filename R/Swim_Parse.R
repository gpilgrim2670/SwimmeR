#' Formats swimming and diving data read in with Read_Results into dataframe
#'
#' Takes the output of Read_Results and cleans it.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @import dplyr
#' @import stringr
#' @import purrr
#'
#' @param x the output or Read_Results
#' @param avoid a list of strings.  Rows in \code{x} containing these strings will not be included. For eample "Pool:", often used to label pool records, could be passed to \code{avoid}
#' @param typo a list of strings that are typos in the original results.  \code{Swim_Parse} is particularly sensitive to accidental double spaces, so "Central  High School", with two spaces between "Central" and "High" is a problem, which can be fixed.  Pass "Centeral High School" to \code{typo}.
#' @param replacement a list of fixes for the strings in \code{typo}.  Here one could pass "Central High School" (one space between "Central" and "High") to \code{replacement} fix the issue described in \code{typo}
#' @return returns a dataframe with columns \code{Name}, \code{Place}, \code{Grade}, \code{School}, \code{Prelims_Time}, \code{Finals_Time}, \code{Points}, & \code{Event}.  Note all swims will have a \code{Finals_Time}, even if that time was actually swam in the prelims (i.e. a swimmer did not qualify for finals).  This is so that final results for an event can be generated from just one column.
#'
#' @examples Swim_Parse(Read_Results("2008 NYSPHAA Federation Championship - 2_29_2008 to 3_1_2008.html", node = "pre"))
#' Swim_Parse(Read_Results("Texas-Florida-Indiana.pdf"))
#' @seealso \code{Swim_Parse} must be run on the output of \code{\link{Read_Results}}
#'
#' @export


Swim_Parse <- function(x, avoid = avoid_2, typo = typo_2, replacement = replacement_2) {

  # as_lines <- str_extract_all(x, "\n.*")
  # as_lines_list_2 <- unlist(as_lines, recursive = FALSE)

  row_numbs <- seq(1, length(x), 1)
  as_lines_list_2 <- paste(x, row_numbs, sep = "  ")

  events <- as_lines_list_2 %>%
    .[map_lgl(., str_detect, "Event \\d{1,}")]
  events <- str_replace(events, ".*Event \\d{1,4} ", "")
  events <- str_replace(events, "Open  ", "") ## Addition
  events <- str_replace(events, "1 M  ", "1 M ") ## Addition
  events <- str_replace(events, "([^1])0  ", "\\10 ")
  events <- str_replace(events, " Class [:alpha:]", "")
  events <- events %>% # Addition
    .[map_lgl(., str_detect, "[[:alpha:]]")] %>%
    str_replace_all("\\\n", "") %>%
    trimws()

  events <-
    unlist(map(events, str_split, "\\s{2,}"), recursive = FALSE)

  events <- as.data.frame(t(as.data.frame(events)),
                          row.names = FALSE,
                          stringsAsFactors = FALSE) %>%
    mutate(
      Event = str_extract(V1, "[[:alnum:] ]*"),
      Event_Row_Min = as.numeric(V2),
      Event_Row_Max = lead(Event_Row_Min, 1L, default = length(as_lines_list_2)) - 1,
      V1 = NULL,
      V2 = NULL
    )

  data_1 <- as_lines_list_2 %>%
    str_extract_all("\n\\s*\\d* [:alpha:].*|\n\\s*\\d* \\d*\\-[:alpha:].*") %>%
    .[map(., length)>0] %>% #data_2
    .[map(., str_length)>50] %>%
    .[map_lgl(., str_detect, "\\.\\d\\d")] %>%
    .[map_lgl(., str_detect, "[:alpha:]{2,}")] %>%
    .[map_lgl(., ~ !any(str_detect(., avoid)))] %>%
    str_replace_all(setNames(replacement, typo)) %>%
    trimws() #data_8

  data_1 <-
    unlist(map(data_1, str_split, "\\s{2,}"), recursive = FALSE)

  # data_length_2 <- data_1_test[map(data_1_test, length)==2]
  data_length_3 <- data_1[map(data_1, length) == 3]
  data_length_4 <- data_1[map(data_1, length) == 4]
  data_length_5 <- data_1[map(data_1, length) == 5]
  data_length_6 <- data_1[map(data_1, length) == 6]
  data_length_7 <- data_1[map(data_1, length) == 7]

  if(length(data_length_7) > 0){
    df_7 <- #url76, url14, url78
      as.data.frame(t(as.data.frame(data_length_7)),
                    row.names = FALSE,
                    stringsAsFactors = FALSE) %>%
      mutate(
        Place = str_split_fixed(V1, " ", n = 2)[, 1]) %>%
      mutate(
        Name = case_when(
          str_detect(V1, ",") == TRUE ~ str_split_fixed(V1, " ", n = 2)[, 2],
          str_detect(V1, "[:alpha:] [:alpha:]") == TRUE & str_detect(V2, "^\\'[:upper:]\\'$") == FALSE ~ str_split_fixed(V1, " ", n = 2)[, 2],
          str_detect(V1, ",") == FALSE &
            str_detect(V2, ",") == TRUE ~ V2
        )) %>%
      mutate(
        Grade = case_when(
          any(str_detect(
            V2, "^SR$|^JR$|^SO$|^FR$|^10$|^11$|^12$"
          )) == TRUE ~ V2,
          any(str_detect(
            V2, "^SR |^JR |^SO |^FR |^10 |^11 |^12 "
          )) == TRUE ~ str_extract(V2, "^SR |^JR |^SO |^FR |^10 |^11 |^12 |^9 |^8 |^7 "),
          any(str_detect(
            V2, "^SR$|^JR$|^SO$|^FR$|^10$|^11$|^12$"
          )) == FALSE &
            any(str_detect(V3, "^SR$|^JR$|^SO$|^FR$")) == TRUE ~ V3
        ),
        Grade = trimws(Grade)) %>%
      mutate(
        School = case_when(
          str_detect(V1, Name) & str_detect(V2, Grade) & str_detect(V3, "\\.\\d\\d") == FALSE ~ V3,
          str_detect(V1, ",") == FALSE &
            str_detect(V1, "[:alpha:] [:alpha:]") == FALSE ~ str_split_fixed(V1, " ", n = 2)[, 2],
          str_detect(V2, "^\\'[:upper:]\\'$") == TRUE ~ str_split_fixed(V1, " ", n = 2)[, 2],
          str_detect(V2, "^\\'[:upper:]\\'$") == FALSE ~ str_split_fixed(V2, " ", n = 2)[, 2],
          # str_detect(V1, Name) & str_detect(V2, Grade) ~ V3,
          TRUE ~ V3
        )) %>%
      mutate(
        Prelims_Time = case_when(any(str_detect(V3, "\\:\\d\\d")) == TRUE & any(str_detect(V4, "\\:\\d\\d")) == TRUE ~ V3,
                                 any(str_detect(V4, "\\:\\d\\d")) == TRUE & any(str_detect(V5, "\\:\\d\\d")) == TRUE ~ V4,
                                 any(str_detect(V4, "SEC")) == TRUE ~ V5)) %>%
      mutate(
        Finals_Time = case_when(
          any(str_detect(V3, "\\:\\d\\d")) == TRUE &
            any(str_detect(V4, "\\:\\d\\d")) == TRUE ~ V4,
          any(str_detect(V4, "\\:\\d\\d")) == TRUE &
            any(str_detect(V5, "\\:\\d\\d")) == TRUE ~ V5,
          any(str_detect(V4, "SEC")) == TRUE ~ V6
        )) %>%
      mutate(
        Points = case_when(Finals_Time != V6 ~ V6,
                           Finals_Time == V6 ~ ""),
        Points = case_when(is.numeric(Points) == FALSE ~ "",
                           TRUE ~ Points)) %>%
      na_if("") %>%
      mutate(
        Row_Numb = V7,
        V1 = NULL,
        V2 = NULL,
        V3 = NULL,
        V4 = NULL,
        V5 = NULL,
        V6 = NULL,
        V7 = NULL
      ) %>%
      # select(
      #   Name,
      #   School,
      #   Place,
      #   Grade,
      #   Prelims_Time = V3,
      #   Finals_Time = V4,
      #   Points = V6,
      #   Row_Numb = V7
      # ) %>%
      na_if("") %>%
      na_if("''") %>%
      mutate(
        Finals_Time = replace(Finals_Time, str_length(Finals_Time) < 3, NA),
        Finals_Time = str_replace(Finals_Time, "J", ""),
        Finals_Time = case_when(
          Place >= 1 &
            is.na(Finals_Time) == TRUE ~ coalesce(Finals_Time, Prelims_Time),
          Place >= 1 &
            is.na(Finals_Time) != TRUE ~ Finals_Time
        ),
        School = case_when(
          is.na(School) == TRUE &
            str_detect(Grade, "[[:alpha:]]") == TRUE ~ Grade,
          is.na(School) == FALSE &
            str_detect(Grade, "[[:alpha:]]") == FALSE ~ School,
          is.na(School) == FALSE ~ School
        ),
        School = case_when(
          str_length(School) < 1 | is.na(School) == TRUE ~ Name,
          str_length(School) >= 1 ~ School
        ),
        School = case_when(
          str_detect(School, "\\'[[:upper:]]\\'") == TRUE &
            is.na(Name) == FALSE ~ Name,
          str_detect(School, "^[:upper:]$") == TRUE &
            is.na(Name) == FALSE ~ Name,
          str_detect(School, "^-[:upper:]$") == TRUE &
            is.na(Name) == FALSE ~ Name,
          # (str_detect(School, "^[:upper:]$") == FALSE & str_detect(School, "^-[:upper:]$") == FALSE) | is.na(Name) == TRUE ~ School
          TRUE ~ School
        ),
        Grade = replace(Grade, Grade == School, NA),
        School = case_when(str_length(Grade) > 2 & is.na(Grade) == FALSE & str_detect(Grade, "\\'[[:alpha:]]\\'") == FALSE & str_detect(Grade, "\\.\\d") == FALSE ~ paste(Grade, School, sep = " "),
                           str_length(Grade) <= 2 | is.na(Grade) == TRUE | str_detect(Grade, "\\'[[:alpha:]]\\'") == TRUE | str_detect(Grade, "\\.\\d") == TRUE ~ School),
        Grade = replace(Grade, (str_length(Grade) > 2 & is.na(Grade) == FALSE), NA),
        Grade = case_when(str_detect(Grade, "^-[:upper:]$") == TRUE ~ "",
                          TRUE ~ Grade),
        Name = replace(Name, Name == School, NA),
        Finals_Time = str_replace(Finals_Time, "x", ""),
        Finals_Time = str_replace(Finals_Time, "X", ""),
        Finals_Time = str_extract(Finals_Time, "[[:digit:][\\.\\:]]*"),
        Prelims_Time = str_replace(Prelims_Time, "x", ""),
        Prelims_Time = str_replace(Prelims_Time, "X", ""),
        Prelims_Time = str_extract(Prelims_Time, "[[:digit:][\\.\\:]]*")
      ) %>%
      na_if("") %>%
      mutate(Prelims_Time = case_when(
        all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
        all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
      )) %>%
      mutate(School = case_when(
        str_detect(School, "\\'[[:alpha:]]\\'") == TRUE &
          is.na(Name) == FALSE ~ Name,
        str_detect(School, "\\'[[:alpha:]]\\'") == FALSE ~ School,
        str_detect(School, "^A$") == TRUE &
          is.na(Name) == FALSE ~ Name,
        str_detect(School, "^A$") == FALSE ~ School
      ),
      School = str_replace(School, "^SR |^JR |^SO |^FR |^10 |^11 |^12 |^9 |^8 |^7 ", ""),
      Name = str_replace(Name, "^-", ""),
      Name = replace(Name, Name == School, NA))

  } else {
    df_7 <- data.frame(Name = character(),
                       Place = character(),
                       Grade = character(),
                       School = character(),
                       Prelims_Time = character(),
                       Finals_Time = character(),
                       Points = character(),
                       Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if(length(data_length_6) > 0){
    df_6 <-
      as.data.frame(t(as.data.frame(data_length_6)),
                    row.names = FALSE,
                    stringsAsFactors = FALSE) %>%
      mutate(
        Place = str_split_fixed(V1, " ", n = 2)[, 1]) %>%
      mutate(
        Name = case_when(str_detect(V1, ",") == TRUE ~ str_extract(V1, "[:alpha:]+'?[:alpha:]+, [:alpha:]+"),
                         str_detect(V2, ",") == TRUE ~ str_extract(V2, "[:alpha:]+'?[:alpha:]+, [:alpha:]+"),
                         TRUE ~ str_split_fixed(V1, " ", n = 2)[, 2]),
        V1 = str_replace(V1, Place, ""),
        V1 = trimws(V1)) %>%
      mutate(
        Grade = case_when(
          str_length(str_split_fixed(V2, " ", n = 2)[, 1]) <= 2 & str_detect(V2, "SR|JR|SO|FR|12|11|10|9|8|7|^\\d\\d ") ~ str_split_fixed(V2, " ", n = 2)[, 1],
          str_length(V2) <= 2 & str_detect(V2, "[[:alpha:]\\.]") == FALSE ~ V2,
          TRUE ~ ""
        ),
        # V2 = str_replace(V2, Grade, ""),
        Grade = trimws(Grade)) %>%
      na_if("") %>%
      mutate(
        School = case_when(
          V2 == Grade ~ V3,
          any(str_detect(V3, "SEC")) == TRUE ~ str_replace(V1, Name, ""),
          is.na(Grade) & str_detect(V2, "\\.\\d\\d") == FALSE ~ V2,
          TRUE ~ str_split_fixed(V2, " ", n = 2)[, 2])) %>%
      # mutate(
      #   Grade = case_when(str_detect(V3, "SEC") == TRUE ~ V2,
      #                     V2 == Name ~ V3,
      #     TRUE ~ str_split_fixed(V2, " ", n = 2)[, 1])) %>%
      mutate(
        Prelims_Time = case_when(str_detect(V4, "\\.\\d\\d") == TRUE & str_detect(V5, "\\.\\d\\d") == TRUE ~ V4,
                                 str_detect(V4, "\\.\\d\\d") == FALSE & str_detect(V5, "\\.\\d\\d") == TRUE ~ "",
                                 TRUE ~ V3)) %>%
      mutate(
        Finals_Time = case_when(str_detect(V4, "\\.\\d\\d") == TRUE & str_detect(V5, "\\.\\d\\d") == TRUE ~ V5,
                                str_detect(V4, "\\.\\d\\d") == FALSE & str_detect(V5, "\\.\\d\\d") == TRUE ~ V5,
                                TRUE ~ V4)) %>%
      mutate(
        Points = case_when(Finals_Time == V5 ~ "",
                           TRUE ~ V5),
        Points = case_when(is.numeric(Points) == FALSE ~ "",
                           TRUE ~ Points)) %>%
      na_if("") %>%
      mutate(
        V1 = NULL,
        V2 = NULL,
        V3 = NULL,
        V4 = NULL,
        V5 = NULL,
      ) %>%
      select(
        Name,
        Place,
        Grade,
        School,
        Prelims_Time,
        Finals_Time,
        Points,
        # Prelims_Time = V3,
        # Finals_Time = V4,
        # Points = V5,
        Row_Numb = V6
      ) %>%
      na_if("") %>%
      na_if("''") %>%
      # na_if("NT") %>%
      mutate(
        Finals_Time = replace(Finals_Time, str_length(Finals_Time) < 3, NA),
        Finals_Time = case_when(
          Place >= 1 &
            is.na(Finals_Time) == TRUE ~ coalesce(Finals_Time, Prelims_Time),
          Place >= 1 &
            is.na(Finals_Time) != TRUE ~ Finals_Time
        ),
        School = case_when(
          is.na(School) == TRUE &
            str_detect(Grade, "[[:alpha:]]") == TRUE ~ Grade,
          is.na(School) == FALSE &
            str_detect(Grade, "[[:alpha:]]") == FALSE ~ School,
          is.na(School) == FALSE ~ School
        ),
        School = case_when(
          str_length(School) < 1 | is.na(School) == TRUE ~ Name,
          str_length(School) >= 1 ~ School
        ),
        Grade = replace(Grade, Grade == School, NA),
        School = case_when(
          # str_length(Grade) > 2 &
          is.na(Grade) == FALSE &
            str_detect(Grade, "\\'[[:alpha:]]\\'") == FALSE &
            str_detect(Grade, "\\.\\d") == FALSE &
            str_detect(Grade, "\\d") == FALSE &
            Grade %in% c("SR", "JR", "SO", "FR") == FALSE &
            Grade != School ~ paste(Grade, School, sep = " "),
          # str_length(Grade) <= 2 |
          #   is.na(Grade) == TRUE |
          #   str_detect(Grade, "\\'[[:alpha:]]\\'") == TRUE |
          #   str_detect(Grade, "\\.\\d") == TRUE ~ School
          TRUE ~ School),
        # Grade = replace(Grade, Grade == School, NA),
        Grade = replace(Grade, (str_length(Grade) > 2 & is.na(Grade) == FALSE), NA),
        # Name = case_when(identical(Name, School) == TRUE ~ NA,
        #                  identical(Name, School) == FALSE ~ Name)
        Name = replace(Name, Name == School, NA),
        Finals_Time = str_replace(Finals_Time, "x", ""),
        Finals_Time = str_replace(Finals_Time, "X", ""),
        Finals_Time = str_extract(Finals_Time, "[[:digit:][\\.\\:]]*"),
        Prelims_Time = str_replace(Prelims_Time, "x", ""),
        Prelims_Time = str_replace(Prelims_Time, "X", ""),
        Prelims_Time = str_extract(Prelims_Time, "[[:digit:][\\.\\:]]*")
      ) %>%
      na_if("") %>%
      mutate(Prelims_Time = case_when(
        all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
        all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
      )) %>%
      mutate(School = case_when(
        str_detect(School, "\\'[[:alpha:]]\\'") == TRUE &
          is.na(Name) == FALSE ~ Name,
        str_detect(School, "\\'[[:alpha:]]\\'") == FALSE ~ School,
        str_detect(School, "^[:upper:]$") == TRUE &
          is.na(Name) == FALSE ~ Name,
        str_detect(School, "^-[:upper:]$") == TRUE & is.na(Name) == FALSE ~ Name,
        TRUE ~ School
      ),
      Name = str_replace(Name, "^-", ""),
      Name = replace(Name, Name == School, NA))

  } else {
    df_6 <- data.frame(Name = character(),
                       Place = character(),
                       Grade = character(),
                       School = character(),
                       Prelims_Time = character(),
                       Finals_Time = character(),
                       Points = character(),
                       Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if(length(data_length_5) > 0) {
    df_5 <- as.data.frame(t(as.data.frame(data_length_5)),
                          row.names = FALSE,
                          stringsAsFactors = FALSE) %>%
      # mutate(V3 = case_when(any(str_detect(V3, "\\:\\d\\d")) == TRUE ~ str_replace(V3, "^NT$", ""))) %>%
      # na_if("") %>%
      mutate(
        Place = str_split_fixed(V1, " ", n = 2)[, 1]) %>%
      mutate(
        Name = case_when(
          any(str_detect(V1, ",")) == TRUE &
            str_detect(V1, ",") == TRUE ~ str_extract(V1, "[:alpha:]+'?[:alpha:]+, [:alpha:]+"),
          any(str_detect(V1, ",")) == TRUE &
            str_detect(V1, ",") == FALSE ~ "",
          str_detect(V2, ",") == TRUE ~ str_extract(V2, "[:alpha:]+'?[:alpha:]+, [:alpha:]+"),
          str_detect(V1, ",") == FALSE & str_detect(V2, "\\.\\d\\d") == TRUE ~ "",
          TRUE ~ str_split_fixed(V1, " ", n = 2)[, 2]
        )) %>%
      na_if("") %>%
      # na_if("''") %>%
      mutate(
        # V1 = NULL,
        V1 = case_when(is.na(Place) == FALSE ~ str_replace(V1, Place, ""),
                       TRUE ~ V1),
        V1 = case_when(is.na(Name) == FALSE ~ str_replace(V1, Name, ""),
                       TRUE ~ V1),
        V1 = trimws(V1),
        Name = trimws(Name)) %>%
      mutate(
        Grade = case_when(
          str_length(str_split_fixed(V2, " ", n = 2)[, 1]) <= 2 & str_detect(V2, "SR|JR|SO|FR|12|11|10|9|8|7|^\\d\\d ") ~ str_split_fixed(V2, " ", n = 2)[, 1],
          TRUE ~ ""
        ),
        # V2 = str_replace(V2, Grade, ""),
        Grade = trimws(Grade)) %>%
      na_if("") %>%
      # na_if("''") %>%
      mutate(
        School = case_when(
          V2 == Grade & any(str_detect(V3, "^SEC \\d+$")) == FALSE ~ V3,
          V2 == Grade & any(str_detect(V3, "^SEC \\d+$")) == TRUE ~ V1,
          (V2 != Grade | is.na(Grade)) & str_detect(V3, "\\.\\d\\d|^NT$|^NP$") & str_detect(V2, "'[:upper:]'|^[:upper:]$|^\\'\\'$") == FALSE & str_detect(V2, "\\.\\d\\d") == FALSE ~ V2,
          (V2 != Grade | is.na(Grade)) & str_detect(V3, "\\.\\d\\d|^NT$|^NP$") & str_detect(V2, "'[:upper:]'|^[:upper:]$|'[:upper:]'|^\\'\\'$") == TRUE & (V1 != Name | is.na(Name)) ~ V1,
          (V1 == Name | is.na(Name)) & V2 != Grade ~ str_split_fixed(V2, " ", n = 2)[, 2],
          # any(str_detect(V3, "SEC")) == TRUE & is.na(Name) == FALSE &
          #   str_detect(V3, "\\:\\d\\d|^NT$|^NP$") == FALSE ~ str_replace(V1, Name, ""),
          str_detect(V2, "\\:\\d\\d|^NT$|^NP$") == TRUE ~ V1,
          TRUE ~ str_split_fixed(V2, " ", n = 2)[, 2]
        ),
        School = case_when(str_detect(School, Grade) == TRUE & is.na(School) == FALSE ~ str_replace(School, Grade, ""),
                           TRUE ~ School),
        School = trimws(School)) %>%
      # School = str_split_fixed(V2, " ", n = 2)[, 2],
      # V2 = NULL,
      mutate(
        Prelims_Time = case_when(
          str_detect(V2, "\\.\\d\\d") == TRUE &
            str_detect(V3, "\\.\\d\\d") == TRUE ~ V2,
          str_detect(V3, "\\.\\d\\d") == TRUE &
            str_detect(V4, "\\.\\d\\d") == TRUE ~ V3,
          str_detect(V3, "\\.\\d\\d") == FALSE &
            str_detect(V4, "\\.\\d\\d") == TRUE ~ "",
          TRUE ~ V2
        )) %>%
      # mutate(School = str_replace(School, Grade, ""),
      #        # School = str_replace(School, Grade, ""),
      #        School = trimws(School)) %>%
      mutate(
        Finals_Time = case_when(
          str_detect(V3, "\\.\\d\\d") == TRUE &
            str_detect(V4, "\\.\\d\\d") == TRUE ~ V4,
          str_detect(V3, "\\.\\d\\d") == FALSE &
            str_detect(V4, "\\.\\d\\d") == TRUE ~ V4,
          TRUE ~ V3
        )) %>%
      mutate(
        Points = case_when(Finals_Time == V3 ~ V4,
                           Finals_Time == V4 ~ "",
                           TRUE ~ ""),
        Points = case_when(is.numeric(Points) == FALSE ~ "",
                           TRUE ~ Points),
        Points = case_when(is.numeric(Points) == FALSE ~ "",
                           TRUE ~ Points)) %>%
      na_if("") %>%
      mutate(
        V1 = NULL,
        V2 = NULL,
        V3 = NULL,
        V4 = NULL,
      ) %>%
      select(
        Name,
        Place,
        Grade,
        School,
        Prelims_Time,
        Finals_Time,
        Points,
        # Prelims_Time = V2,
        # Finals_Time = V3,
        # Points = V4,
        Row_Numb = V5
      ) %>%
      na_if("") %>%
      na_if("''") %>%
      mutate(Finals_Time = replace(Finals_Time, n_distinct(Finals_Time) <= 2, ""),
             Prelims_Time = replace(Prelims_Time, n_distinct(Prelims_Time) <= 2, "")) %>%
      na_if("") %>%
      na_if("''") %>%
      # na_if("^NT$") %>%
      mutate(Finals_Time = case_when(sum(str_detect(Finals_Time, "\\.\\d")) >= 1 ~ str_replace(Finals_Time, "NT", ""),
                                     sum(str_detect(Finals_Time, "\\.\\d")) < 1 ~ Finals_Time)) %>%
      na_if("") %>%
      mutate(
        School = case_when(is.na(School) == TRUE & str_detect(Finals_Time, "\\.\\d") == FALSE ~ Finals_Time,
                           is.na(School) == FALSE | str_detect(Finals_Time, "\\.\\d") == TRUE ~ School),
        # School = replace(School, (is.na(School) == TRUE & str_detect(Finals_Time, "\\.") == FALSE), Finals_Time),
        Finals_Time = replace(Finals_Time, str_detect(Finals_Time, "^NT$") == TRUE, NA),
        Finals_Time = replace(Finals_Time, School == Finals_Time, NA),
        Finals_Time = replace(Finals_Time, str_length(Finals_Time) < 3, NA),
        Finals_Time = case_when(
          Place >= 1 &
            is.na(Finals_Time) == TRUE ~ coalesce(Finals_Time, Prelims_Time),
          Place >= 1 &
            is.na(Finals_Time) != TRUE ~ Finals_Time
        ),
        Finals_Time = str_replace(Finals_Time, "x", ""),
        Finals_Time = str_replace(Finals_Time, "X", ""),
        Finals_Time = str_extract(Finals_Time, "[[:digit:][\\.\\:]]*"),
        Finals_Time = str_replace(Finals_Time, "x", ""),
        Finals_Time = str_replace(Finals_Time, "X", ""),
        Finals_Time = str_extract(Finals_Time, "[[:digit:][\\.\\:]]*"),
        Prelims_Time = str_replace(Prelims_Time, "x", ""),
        Prelims_Time = str_replace(Prelims_Time, "X", ""),
        Prelims_Time = str_extract(Prelims_Time, "[[:digit:][\\.\\:]]*")
      ) %>%
      na_if("") %>%
      mutate(Prelims_Time = case_when(
        all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
        all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
      )) %>%
      mutate(
        Points_2 = case_when(
          str_detect(Points, "\\:\\d\\d") == TRUE ~ "",
          as.numeric(Points) > 20 & str_detect(Points, "\\.\\d\\d") == TRUE ~ "",
          str_detect(Points, "\\:\\d\\d") == FALSE ~ Points
        ),
        Prelims_Time = case_when(
          str_detect(Points, "\\:\\d\\d") == TRUE ~ Finals_Time,
          as.numeric(Points) > 20 & str_detect(Points, "\\.\\d\\d") == TRUE ~ Finals_Time,
          TRUE ~ Prelims_Time
        ),
        Finals_Time = case_when(
          str_detect(Points, "\\:\\d\\d") == TRUE ~ Points,
          as.numeric(Points) > 20 & str_detect(Points, "\\.\\d\\d") == TRUE ~ Points,
          TRUE ~ Finals_Time
        )) %>%
      na_if("") %>%
      mutate(
        Points = case_when(is.na(Points_2) == FALSE ~ Points_2,
                           Points == Finals_Time ~ "",
                           TRUE ~ Points),
        Points_2 = NULL,
        Finals_Time = str_replace(Finals_Time, "x", ""),
        Finals_Time = str_replace(Finals_Time, "X", ""),
        Finals_Time = str_extract(Finals_Time, "[[:digit:][\\.\\:]]*"),
        Prelims_Time = str_replace(Prelims_Time, "x", ""),
        Prelims_Time = str_replace(Prelims_Time, "X", ""),
        Prelims_Time = str_extract(Prelims_Time, "[[:digit:][\\.\\:]]*"),
        Prelims_Time = replace(Prelims_Time, str_detect(Prelims_Time, "\\.") == FALSE, NA)
      ) %>%
      na_if("") %>% # Fine
      mutate(
        School = replace(School, str_detect(School, "\\.\\d") == TRUE, NA),
        School = case_when(
          is.na(School) == TRUE &
            Grade %!in% c("FR", "SO", "JR", "SR", "7", "8", "05", "06", "07", "08", "09", as.character(seq(10,25,1))) & str_detect(Grade, "\\'[[:alpha:]]\\'") == FALSE & str_detect(Grade, "\\.\\d") == FALSE ~ Grade,
          is.na(School) == FALSE |
            Grade %in% c("FR", "SO", "JR", "SR", "7", "8", "05", "06", "07", "08", "09", as.character(seq(10,25,1))) | str_detect(Grade, "\\'[[:alpha:]]\\'") == TRUE | str_detect(Grade, "\\.\\d") == FALSE ~ School,
          is.na(School) == FALSE ~ School
        ), #fine
        School = case_when(
          str_length(School) < 1 | is.na(School) == TRUE ~ Name,
          str_length(School) >= 1 ~ School
        ),
        School = case_when(
          # str_length(Grade) > 2 &
          is.na(Grade) == FALSE &
            str_detect(Grade, "\\'[[:alpha:]]\\'") == FALSE &
            str_detect(Grade, "\\.\\d") == FALSE &
            str_detect(Grade, "\\d") == FALSE &
            Grade %in% c("SR", "JR", "SO", "FR") == FALSE &
            Grade != School ~ paste(Grade, School, sep = " "),
          # str_length(Grade) <= 2 |
          #   is.na(Grade) == TRUE |
          #   str_detect(Grade, "\\'[[:alpha:]]\\'") == TRUE |
          #   str_detect(Grade, "\\.\\d") == TRUE ~ School
          TRUE ~ School
        ),
        School = case_when(
          str_detect(School, "\\'[[:upper:]]\\'") == TRUE &
            is.na(Name) == FALSE ~ Name,
          str_detect(School, "^[:upper:]$") == TRUE &
            is.na(Name) == FALSE ~ Name,
          str_detect(School, "^-[:upper:]$") == TRUE &
            is.na(Name) == FALSE ~ Name,
          # (str_detect(School, "^[:upper:]$") == FALSE & str_detect(School, "^-[:upper:]$") == FALSE) | is.na(Name) == TRUE ~ School
          TRUE ~ School
        ),
        Grade = replace(Grade, Grade == School, NA),
        Grade = replace(Grade, str_detect(Grade, "\\.\\d") == TRUE, NA),
        Name = str_replace(Name, "^-", ""),
        Name = replace(Name, Name == School, NA),
        # Prelims_Time = case_when(is.na(Name) == TRUE ~ Grade,
        #                          is.na(Name) == FALSE ~ Prelims_Time),
        Grade = replace(Grade, is.na(Name) == TRUE &
                          is.na(School) == FALSE, NA),

        Grade = replace(Grade, Grade == School, NA),
        Grade = replace(Grade, (str_length(Grade) > 2 & is.na(Grade) == FALSE), NA)

      )
  } else {
    df_5 <- data.frame(Name = character(),
                       Place = character(),
                       Grade = character(),
                       School = character(),
                       Prelims_Time = character(),
                       Finals_Time = character(),
                       Points = character(),
                       Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if(length(data_length_4) > 0){
    df_4 <-
      as.data.frame(t(as.data.frame(data_length_4)),
                    row.names = FALSE,
                    stringsAsFactors = FALSE) %>%
      mutate(
        Place = str_split_fixed(V1, " ", n = 2)[, 1]) %>%
      mutate(
        Name = case_when(
          any(str_detect(V1, ",")) == TRUE &
            str_detect(V1, ",") == TRUE ~ str_extract(V1, "[:alpha:]+'?[:alpha:]+, [:alpha:]+"),
          any(str_detect(V1, ",")) == TRUE &
            str_detect(V1, ",") == FALSE ~ "",
          str_detect(V2, ",") == TRUE ~ str_extract(V2, "[:alpha:]+'?[:alpha:]+, [:alpha:]+"),
          str_detect(V1, ",") == FALSE & str_detect(V2, "\\.\\d\\d") == TRUE ~ "",
          TRUE ~ str_split_fixed(V1, " ", n = 2)[, 2]
        )) %>%
      na_if("") %>%
      # na_if("''") %>%
      mutate(
        # V1 = NULL,
        V1 = case_when(is.na(Place) == FALSE ~ str_replace(V1, Place, ""),
                       TRUE ~ V1),
        V1 = case_when(is.na(Name) == FALSE ~ str_replace(V1, Name, ""),
                       TRUE ~ V1),
        V1 = trimws(V1),
        Name = trimws(Name)) %>%
      mutate(
        Grade = case_when(
          str_length(str_split_fixed(V2, " ", n = 2)[, 1]) <= 2 & str_detect(V2, "SR|JR|SO|FR|12|11|10|9|8|7|^\\d\\d ") ~ str_split_fixed(V2, " ", n = 2)[, 1],
          TRUE ~ ""
        ),
        # V2 = str_replace(V2, Grade, ""),
        Grade = trimws(Grade)) %>%
      na_if("") %>%
      # na_if("''") %>%
      mutate(
        School = case_when(
          V2 == Grade & any(str_detect(V3, "^SEC \\d+$")) == FALSE ~ V3,
          V2 == Grade & any(str_detect(V3, "^SEC \\d+$")) == TRUE ~ V1,
          (V2 != Grade | is.na(Grade)) & str_detect(V3, "\\.\\d\\d|^NT$|^NP$") & str_detect(V2, "'[:upper:]'|^[:upper:]$|^\\'\\'$") == FALSE & str_detect(V2, "\\.\\d\\d") == FALSE ~ V2,
          (V2 != Grade | is.na(Grade)) & str_detect(V3, "\\.\\d\\d|^NT$|^NP$") & str_detect(V2, "'[:upper:]'|^[:upper:]$|'[:upper:]'|^\\'\\'$") == TRUE & (V1 != Name | is.na(Name)) ~ V1,
          (V1 == Name | is.na(Name)) & V2 != Grade ~ str_split_fixed(V2, " ", n = 2)[, 2],
          # any(str_detect(V3, "SEC")) == TRUE & is.na(Name) == FALSE &
          #   str_detect(V3, "\\:\\d\\d|^NT$|^NP$") == FALSE ~ str_replace(V1, Name, ""),
          str_detect(V2, "\\:\\d\\d|^NT$|^NP$") == TRUE ~ V1,
          TRUE ~ str_split_fixed(V2, " ", n = 2)[, 2]
        ),
        School = case_when(str_detect(School, Grade) == TRUE & is.na(School) == FALSE ~ str_replace(School, Grade, ""),
                           TRUE ~ School),
        School = trimws(School)) %>%
      # School = str_split_fixed(V2, " ", n = 2)[, 2],
      # V2 = NULL,
      mutate(
        Prelims_Time = case_when(
          str_detect(V2, "\\.\\d\\d") == TRUE &
            str_detect(V3, "\\.\\d\\d") == TRUE ~ V2,
          str_detect(V3, "\\.\\d\\d") == TRUE &
            str_detect(V4, "\\.\\d\\d") == TRUE ~ V3,
          str_detect(V3, "\\.\\d\\d") == FALSE &
            str_detect(V4, "\\.\\d\\d") == TRUE ~ "",
          TRUE ~ V2
        )) %>%
      # mutate(School = str_replace(School, Grade, ""),
      #        # School = str_replace(School, Grade, ""),
      #        School = trimws(School)) %>%
      mutate(
        Finals_Time = case_when(
          str_detect(V3, "\\.\\d\\d") == TRUE &
            str_detect(V4, "\\.\\d\\d") == TRUE ~ V4,
          str_detect(V3, "\\.\\d\\d") == FALSE &
            str_detect(V4, "\\.\\d\\d") == TRUE ~ V4,
          TRUE ~ V3
        )) %>%
      mutate(
        Points = case_when(Finals_Time == V3 ~ V4,
                           Finals_Time == V4 ~ "",
                           TRUE ~ ""),
        Points = case_when(is.numeric(Points) == FALSE ~ "",
                           TRUE ~ Points)) %>%
      na_if("") %>%
      mutate(
        V1 = NULL,
        V2 = NULL,
        V3 = NULL
      ) %>%
      select(
        Name,
        Place,
        Grade,
        School,
        Prelims_Time,
        Finals_Time,
        Row_Numb = V4
      ) %>%
      na_if("") %>%
      na_if("''") %>%
      # na_if("NT") %>%
      mutate(
        Finals_Time = replace(Finals_Time, str_length(Finals_Time) < 3, NA),
        Finals_Time = case_when(
          Place >= 1 &
            is.na(Finals_Time) == TRUE ~ coalesce(Finals_Time, Prelims_Time),
          Place >= 1 &
            is.na(Finals_Time) != TRUE ~ Finals_Time
        )) %>%
      mutate(
        School = case_when(
          is.na(School) == TRUE &
            str_detect(Grade, "[[:alpha:]]") == TRUE &
            str_length(Grade) > 1 ~ Grade,
          is.na(School) == FALSE &
            str_detect(Grade, "[[:alpha:]]") == FALSE ~ School,
          is.na(School) == FALSE ~ School
        ),
        School = case_when(
          str_length(School) < 1 | is.na(School) == TRUE ~ Name,
          str_length(School) >= 1 ~ School
        )) %>%
      mutate(
        Grade = replace(Grade, Grade == School, NA),
        Name = str_replace(Name, "^-", ""),
        Name = replace(Name, Name == School, NA)) %>%
      mutate(
        Prelims_Time = case_when(is.na(Name) == TRUE & str_detect(Grade, "\\d\\d\\.\\d\\d") == TRUE ~ Grade,
                                 TRUE ~ Prelims_Time)) %>%
      mutate(
        # Prelims_Time = replace(Prelims_Time, is.na(Name) == TRUE, Grade),
        Grade = replace(Grade, is.na(Name) == TRUE &
                          is.na(School) == FALSE, NA),
        Prelims_Time = str_replace(Prelims_Time, "x", ""),
        Prelims_Time = str_replace(Prelims_Time, "X", ""),
        Prelims_Time = str_extract(Prelims_Time, "[[:digit:][\\.\\d\\d\\:\\d\\d]]*")
      ) %>%
      mutate(
        Finals_Time = str_replace(Finals_Time, "x", ""),
        Finals_Time = str_replace(Finals_Time, "X", "")) %>%
      mutate(
        Finals_Time = str_extract(Finals_Time, "[[:digit:][\\.\\d\\d\\:\\d\\d]]*")
      ) %>%
      # mutate(Finals_Time = str_extract(Finals_Time, "[[:digit:][\\.\\d\\d\\:\\d\\d]]*")) %>%
      na_if("") %>%
      mutate(Prelims_Time = case_when(
        all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
        all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
      )) %>%
      mutate(
        Grade = replace(Grade, Grade == School, NA),
        School = case_when(
          # str_length(Grade) > 2 &
          is.na(Grade) == FALSE &
            str_detect(Grade, "\\'[[:alpha:]]\\'") == FALSE &
            str_detect(Grade, "\\.\\d") == FALSE &
            str_detect(Grade, "\\d") == FALSE &
            Grade %in% c("SR", "JR", "SO", "FR") == FALSE &
            Grade != School ~ paste(Grade, School, sep = " "),
          # str_length(Grade) <= 2 |
          #   is.na(Grade) == TRUE |
          #   str_detect(Grade, "\\'[[:alpha:]]\\'") == TRUE |
          #   str_detect(Grade, "\\.\\d") == TRUE ~ School
          TRUE ~ School
        ),
        School = case_when(
          str_detect(School, "\\'[[:upper:]]\\'") == TRUE &
            is.na(Name) == FALSE ~ Name,
          str_detect(School, "^[:upper:]$") == TRUE &
            is.na(Name) == FALSE ~ Name,
          str_detect(School, "^[:upper:]$") == FALSE |
            is.na(Name) == TRUE ~ School
        ),
        # Grade = replace(Grade, Grade == School, NA),
        Grade = replace(Grade, (
          str_detect(Grade, "\\d") == FALSE &
            Grade %in% c("SR", "JR", "SO", "FR") == FALSE
        ), NA),
        Grade = case_when(
          str_detect(Name, "\\.d{1,3}") == TRUE ~ str_extract(Name, "\\d{1,3}"),
          str_detect(Name, "\\.d{1,3}") == FALSE ~ Grade
        ),
        Name = str_replace(Name, "\\d{1,3}", ""),
        ### Specifically to deal with url45, Boys States 2001 where prelims times and school names got jammed up ###
        Prelims_Time = case_when(is.na(Prelims_Time) == TRUE & str_detect(School, "\\.") == TRUE ~ str_extract(School, "[:digit:]*[:punct:]?[:digit:]*[:punct:]?[:digit:]*"),
                                 is.na(Prelims_Time) == FALSE | str_detect(School, "\\.") == FALSE ~ Prelims_Time),
        School = str_replace(School, "-[:alpha:]*\\d.*$", ""),
        School = str_replace(School, "[:digit:]*[:punct:]?[:digit:]*[:punct:]?[:digit:]*", "")
      ) %>%
      mutate(Name = str_replace(Name, "^-", ""),
             Name = replace(Name, Name == School, NA))
  } else {
    df_4 <- data.frame(Name = character(),
                       Place = character(),
                       Grade = character(),
                       School = character(),
                       Prelims_Time = character(),
                       Finals_Time = character(),
                       Points = character(),
                       Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  if(length(data_length_3) > 0){
    df_3 <-
      as.data.frame(t(as.data.frame(data_length_3)),
                    row.names = FALSE,
                    stringsAsFactors = FALSE) %>%
      mutate(
        Place = str_split_fixed(V1, " ", n = 2)[, 1],
        Name = str_split_fixed(V1, " ", n = 2)[, 2],
        V1 = NULL,
        Grade = as.character(NA),
        School = as.character(NA),
        Finals_Time = V2,
        Prelims_Time = as.character(NA),
        V2 = NULL,
      ) %>%
      select(
        Name,
        Place,
        Grade,
        School,
        Prelims_Time,
        Finals_Time,
        Row_Numb = V3
      ) %>%
      na_if("") %>%
      na_if("''") %>%
      # na_if("NT") %>%
      mutate(Finals_Time = replace(Finals_Time, str_length(Finals_Time) < 3, NA),
             Finals_Time = case_when(Place >= 1 & is.na(Finals_Time) == TRUE ~ coalesce(Finals_Time, Prelims_Time),
                                     Place >= 1 & is.na(Finals_Time) != TRUE ~ Finals_Time),
             School = case_when(str_length(School) < 1 | is.na(School) == TRUE ~ Name,
                                str_length(School) >= 1 ~ School),
             Name = str_replace(Name, "^-", ""),
             Name = replace(Name, Name == School, NA),
             Prelims_Time = case_when(is.na(Name) == TRUE ~ Grade,
                                      is.na(Name) == FALSE ~ Prelims_Time),
             Grade = replace(Grade, is.na(Name) == TRUE & is.na(School) == FALSE, NA),
             Finals_Time = str_replace(Finals_Time, "x", ""),
             Finals_Time = str_replace(Finals_Time, "X", ""),
             Finals_Time = str_extract(Finals_Time, "[[:digit:][\\.\\:]]*"),
             Prelims_Time = str_replace(Prelims_Time, "x", ""),
             Prelims_Time = str_replace(Prelims_Time, "X", ""),
             Prelims_Time = str_extract(Prelims_Time, "[[:digit:][\\.\\:]]*")
      ) %>%
      na_if("") %>%
      mutate(Prelims_Time = case_when(all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
                                      all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time)
      )
  } else {
    df_3 <- data.frame(Name = character(),
                       Place = character(),
                       Grade = character(),
                       School = character(),
                       Prelims_Time = character(),
                       Finals_Time = character(),
                       Points = character(),
                       Row_Numb = character(),
                       stringsAsFactors = FALSE)
  }

  ### Joining Up New ####
  Min_Row_Numb <- min(events$Event_Row_Min)

  data <- full_join(df_7, df_6) %>%
    full_join(df_5) %>%
    full_join(df_4) %>%
    full_join(df_3) %>%
    filter(str_detect(Finals_Time, "\\.") == TRUE) %>%
    # left_join(data_8b, by = c("Finals_Time" = "Match_Time", "Place" = "Place")) %>%
    mutate(Row_Numb = as.numeric(Row_Numb)) %>%
    arrange(Row_Numb) %>%
    mutate(
      Place = as.numeric(Place),
      Place = case_when(
        is.na(lag(Place)) == TRUE ~ Place,
        lag(Place) == Place ~ Place + 0.1,
        lag(Place) != Place ~ Place
      ),
      Place = as.character(Place),
      Row_Numb = as.numeric(Row_Numb)
    ) %>%
    filter(Row_Numb >= Min_Row_Numb)

  data  <-
    transform(data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)])
  data$Row_Numb <- NULL

  data <- data %>%
    mutate(
      Prelims_Time = replace(Prelims_Time, str_detect(Prelims_Time, "\\.") == FALSE, NA),
      Prelims_Time = replace(
        Prelims_Time,
        (
          str_detect(Prelims_Time, "\\.") == TRUE &
            str_detect(Prelims_Time, "\\:") == FALSE &
            as.numeric(Prelims_Time) < 15
        ),
        NA
      ),
      Points = replace(Points, str_detect(Points, "[[:alpha:]]") == TRUE, NA),
      Points = replace(Points, str_detect(Points, "[^[:digit:]|\\.]"), NA),
      Place = round(as.numeric(Place))
    )


  return(data)
}
