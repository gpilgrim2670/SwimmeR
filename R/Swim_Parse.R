#' Formats swimming and diving data read with \code{read_results} into a dataframe
#'
#' Takes the output of \code{read_results} and cleans it, yielding a dataframe of swimming (and diving) results
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom dplyr coalesce
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr n_distinct
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_detect
#' @importFrom stringr str_length
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @param file output from \code{read_results}
#' @param avoid a list of strings.  Rows in \code{x} containing these strings will not be included. For example "Pool:", often used to label pool records, could be passed to \code{avoid}.  The default is \code{avoid_default}, which contains many strings similar to "Pool:", such as "STATE:" and "Qual:".  Users can supply their own lists to \code{avoid}.
#' @param typo a list of strings that are typos in the original results.  \code{swim_parse} is particularly sensitive to accidental double spaces, so "Central  High School", with two spaces between "Central" and "High" is a problem, which can be fixed.  Pass "Central High School" to \code{typo}.  Unexpected commas as also an issue, for example "Texas, University of" should be fixed using \code{typo} and \code{replacement}
#' @param replacement a list of fixes for the strings in \code{typo}.  Here one could pass "Central High School" (one space between "Central" and "High") and "Texas" to \code{replacement} fix the issues described in \code{typo}
#' @param splits either \code{TRUE} or the default, \code{FALSE} - should \code{swim_parse} attempt to include splits
#' @return returns a dataframe with columns \code{Name}, \code{Place}, \code{Grade}, \code{School}, \code{Prelims_Time}, \code{Finals_Time}, \code{Points}, \code{Event} & \code{DQ}.  Note all swims will have a \code{Finals_Time}, even if that time was actually swam in the prelims (i.e. a swimmer did not qualify for finals).  This is so that final results for an event can be generated from just one column.
#'
#' @examples \dontrun{
#' swim_parse(read_results("http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm", node = "pre"),
#'  typo = c("-1NORTH ROCKL"), replacement = c("1-NORTH ROCKL"))
#'  }
#' \dontrun{
#' swim_parse(read_results("inst/extdata/Texas-Florida-Indiana.pdf"),
#'  typo =  c("Indiana  University", ", University of"), replacement = c("Indiana University", ""))
#'  }
#' @seealso \code{swim_parse} must be run on the output of \code{\link{read_results}}
#'



Swim_Parse <-
  function(file,
           avoid = avoid_default,
           typo = typo_default,
           replacement = replacement_default,
           splits = FALSE) {

    # file <- read_results("http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm", node = "pre")
    # typo <- c("-1NORTH ROCKL")
    # replacement <- c("1-NORTH ROCKL")
    # avoid <- avoid_default

    # file <- read_results("~/SwimmeR/inst/extdata/s2-results.pdf")
    # avoid <- c("MR:")
    # typo <- c("Swim\\s{2,}Club", "Performance\\s{2,}Swim", "Swimming\\s{2,}Club", "Stamford\\s{2,}American\\s{2,}Internationa", "Uwcsea\\s{2,}Phoenix-ZZ", "AquaTech\\s{2,}Swimming", "Chinese\\s{2,}Swimming", "Aquatic\\s{2,}Performance", "SwimDolphia\\s{2}Aquatic School")
    # replacement <- c("Swim Club", "Performance Swim", "Swimming Club", "Stamford American International", "Uwcsea Phoenix-ZZ", "AquaTech Swimming", "Chinese Swimming", "Aquatic Performance", "SwimDolphia Aquatic School")

    # file <- read_results(system.file("extdata", "jets08082019_067546.pdf", package = "SwimmeR"))
    # avoid <- avoid_default
    # typo <- typo_default
    # replacement <- replacement_default

    # file <- read_results("http://www.section5swim.com/Results/GirlsHS/2000/Sec5/B/Single.htm", node = "pre")
    # avoid <- avoid_default
    # typo <- typo_default
    # replacement <- replacement_default

    # file <- SEC_Results_2
    # typo = c(
    #   "A&M",
    #   "FLOR",
    #   "Celaya-Hernande",
    #   # names which were cut off, and missing the last, first structure
    #   "Hernandez-Tome",
    #   "Garcia Varela,",
    #   "Von Biberstein,"
    # )
    # replacement = c(
    #   "AM",
    #   "Florida",
    #   "Celaya, Hernande",
    #   # replacement names that artificially impose last, first structure.  Names can be fixed after parsing
    #   "Hernandez, Tome",
    #   "Garcia, Varela",
    #   "Von, Biberstein")
    # avoid <- avoid_default

    #### strings that if a line begins with one of them the line is ignored ####
    avoid_default <-
      c(
        "Record",
        "RECORD",
        "^\\s*Public\\:",
        "^\\s*NYSPHSAA",
        "^\\s*NYSPHAA",
        "^\\s*State:",
        "^\\s*States:",
        "^\\s*STATE:",
        "^\\s*STATES:",
        "\\d\\:\\s",
        "^\\s*Finals",
        "^\\s*Prelims",
        "^\\s*Section\\:",
        "^\\s*SECTION\\:",
        "^\\s*Section [:alpha:]{1,2}\\:",
        "^\\s*SECTION [:alpha:]{1,2}\\:",
        "^\\s*Sectional\\:",
        "^\\s*Sectionals\\:",
        "^\\s*Hosted",
        "^\\s*Meet",
        "^\\s*MEET",
        "^\\s*Points",
        "^\\s*Rec\\.?\\:",
        "^\\s*REC\\.?\\:",
        "\\sRec\\.?\\:\\s",
        "^\\s*Rcd\\:",
        "^\\s*Cty\\:",
        "^\\s*League",
        "^\\s*League Rec\\.?\\:",
        "^\\s*LEAGUE",
        "^\\s*IAC\\:",
        "^\\s*AA\\:",
        "^\\s*AAA\\:",
        "^\\s*AAC\\:",
        "^\\s*Con\\.\\:",
        "^\\s*Auto\\:",
        "^\\s*Auto\\.\\:",
        "^\\s*Cons\\.\\:",
        "^\\s*Eastern\\:",
        "^\\s*Class [[:alpha:]]\\:",
        "^\\s*CLASS [[:alpha:]]\\:",
        "^\\s*Class '[[:alpha:]]'\\:",
        "^\\s*Pool\\:",
        "^\\s*POOL\\:",
        "^\\s*\\'A\\'\\:",
        "^\\s*Nassau\\:",
        "^\\s*CONS\\:",
        "^\\s*CONS\\.\\:",
        "^\\s*Qual\\.\\:",
        "^\\s*QUAL\\.\\:",
        "^\\s*Qual\\:",
        "^\\s*QUAL\\:",
        "^\\s*Qualify\\:",
        "^\\s*QUALIFY\\:",
        "^\\s*School\\:",
        "^\\s*SCHOOL\\:",
        "^\\s*School\\s*Prelims\\s*Finals",
        "^\\s*NFL\\:",
        "^\\s*NYS\\:",
        "^\\s*FED\\:",
        "^\\s*Sectio\\:",
        "^\\s*Sect\\:",
        "^\\s*SECT\\:",
        "^\\s*Sec\\:",
        "^\\s*SEC\\:",
        "^\\s*CHS\\:",
        "^\\s*Chs\\:",
        "^\\s*Large\\:",
        "^\\s*Small\\:",
        "^\\s*r\\:",
        "National\\:",
        "Meet\\:",
        "NCAA\\:"
      )

    #### define avoid_minimal ####
    avoid_minimal <- c("^\\s{1,}r\\:")

    #### default typo and replacement strings ####
    typo_default <- c("typo")

    replacement_default <- c("typo")

    ### assign row numbers ###
    as_lines_list_2 <- add_row_numbers(text = file)

    ### parsing html and pdf files ####
    if (stringr::str_detect(file[1], "^A107") == FALSE) {

    #### Pulls out event labels from text ####
    events <- event_parse(as_lines_list_2)

    #### set up strings ####
    Name_String <- "_?[:alpha:]+'?[:alpha:]+\\s?[:alpha:]*\\s?[:alpha:]*,\\s?[:alpha:]*\\s?[:alpha:]*,? [:alpha:]+\\s?[:alpha:\\-\\']*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
    Time_Score_String <- "\\d{0,2}\\:?\\d{1,3}\\.\\d{2}"
    Grade_String <- "^SR$|^JR$|^SO$|^FR$|^[:digit:]{1,3}$"
    Colon_String <- "\\:\\d\\d"

    #### clean input data ####
    suppressWarnings(
      data_1 <- as_lines_list_2 %>%
        # stringr::str_replace_all(stats::setNames(replacement, typo)) %>% # moved to top 8/26
        stringr::str_replace_all("\\*(\\d{1,})", replacement = "\\1") %>%  # removes * placed in front of place number in ties
        stringr::str_extract_all(
          "\n\\s*\\d*\\s* \\*?[:alpha:].*|\n\\s*\\d* \\d*\\-[:alpha:].*|\n\\s*-{2,5}\\s* [:alpha:].*|\n\\s*\\d* \\d*\\-[:alpha:].*"
        ) %>%
        .[purrr::map(., length) > 0] %>%
        .[purrr::map(., stringr::str_length) > 50] %>%
        .[purrr::map_lgl(., stringr::str_detect, paste0(Time_Score_String,"|DQ"))] %>% # must have \\.\\d\\d because all swimming and diving times do
        # .[purrr::map_lgl(., stringr::str_detect, "\\.\\d\\d")] %>% # must have \\.\\d\\d because all swimming and diving times do
        .[purrr::map_lgl(., stringr::str_detect, "[:alpha:]{2,}")] %>% # must have at least two letters in a row
        .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid)))] %>%
        stringr::str_remove_all("\n") %>%
        stringr::str_replace_all(stats::setNames(replacement, typo)) %>% # moved to top of pipeline 8/26
        stringr::str_replace_all("\\s*[&%]\\s*", "  ") %>% # added 8/21 for removing "&" and "%" as record designator
        # removed J etc. from next to swim, but does not remove X or x (for exhibition tracking)
        stringr::str_replace_all("[A-WYZa-wyz]+(\\d{1,2}\\:\\d{2}\\.\\d{2})", "\\1") %>%
        stringr::str_replace_all("(\\d{1,2}\\:\\d{2}\\.\\d{2})[A-WYZa-wyz]+", "\\1") %>%
        stringr::str_replace_all("[A-WYZa-wyz]+(\\d{2,3}\\.\\d{2})", "\\1") %>%
        stringr::str_replace_all("(d{2,3}\\.\\d{2})[A-WYZa-wyz]+", "\\1") %>%
        stringr::str_replace_all(" [:punct:]+(\\d{1,2}\\:\\d{2}\\.\\d{2})", " \\1") %>%
        stringr::str_replace_all("(\\d{1,2}\\:\\d{2}\\.\\d{2})[:punct:]+", " \\1") %>%
        stringr::str_replace_all(" [:punct:]+(\\d{2,3}\\.\\d{2})", " \\1") %>%
        stringr::str_replace_all("(\\d{2,3}\\.\\d{2})[:punct:]+", " \\1") %>%
        stringr::str_remove_all("\\s{2}J\\s{2}") %>%
        # remove q from next to time 10/21/2020
        stringr::str_remove_all(" q ") %>% # removes " q " sometimes used to designate a qualifying time
        stringr::str_replace_all("-{2,5}", "10000") %>% #8/26
        stringr::str_replace_all("(\\.\\d{2})\\d+", "\\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
        stringr::str_replace_all("\\d{1,2} (\\d{1,})$", "  \\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
        stringr::str_replace_all("\\*", "_") %>%
        trimws()
    )

    #### splits data into variables by splitting at multiple (>= 2) spaces ####
    data_1 <-
      unlist(purrr::map(data_1, stringr::str_split, "\\s{2,}"),
             recursive = FALSE)

    #### breaks data into subsets based on how many variables it has ####
    data_length_3 <- data_1[purrr::map(data_1, length) == 3]
    data_length_4 <- data_1[purrr::map(data_1, length) == 4]
    data_length_5 <- data_1[purrr::map(data_1, length) == 5]
    data_length_6 <- data_1[purrr::map(data_1, length) == 6]
    data_length_7 <- data_1[purrr::map(data_1, length) == 7]

    # treatment of DQs new 8/19
    suppressWarnings(DQ <-
                       data_1[stringr::str_detect(data_1, Time_Score_String, negate = TRUE) == TRUE])
    DQ_length_3 <- DQ[purrr::map(DQ, length) == 3]
    DQ_length_4 <- DQ[purrr::map(DQ, length) == 4]


    #### seven variables ####
    if (length(data_length_7) > 0) {
      suppressWarnings(
        df_7 <- data_length_7 %>%
          list_transform() %>%
          dplyr::mutate(
            Place = dplyr::case_when(
              stringr::str_detect(V1, "^[:digit:]*$") == TRUE ~ V1,
              TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 1]
            )
          ) %>%
          dplyr::mutate(
            Name = dplyr::case_when(
              stringr::str_detect(V1, ",") == TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2],
              stringr::str_detect(V1, "[:alpha:] [:alpha:]") == TRUE &
                stringr::str_detect(V2, "^\\'[:upper:]\\'$") == FALSE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2],
              stringr::str_detect(V1, ",") == FALSE &
                stringr::str_detect(V2, ",") == TRUE ~ V2
            )
          ) %>%
          dplyr::mutate(
            Grade = dplyr::case_when(
              stringr::str_detect(V2, Grade_String) == TRUE ~ V2,
              stringr::str_detect(V2, "^SR |^JR |^SO |^FR |^[:digit:]{1,3} ") == TRUE ~ stringr::str_extract(V2, "^SR |^JR |^SO |^FR |^10 |^11 |^[:digit:]{1,2} "),
              stringr::str_detect(V3, "^SR |^JR |^SO |^FR |^[:digit:]{1,3} ") == TRUE ~ stringr::str_extract(V3, "^SR |^JR |^SO |^FR |^10 |^11 |^[:digit:]{1,2} "),
              any(
                stringr::str_detect(V2, Grade_String)
              ) == FALSE &
                any(
                  stringr::str_detect(V3, Grade_String)
                ) == TRUE ~ V3
            ),
            Grade = trimws(Grade),
          ) %>%
          dplyr::mutate(
            School = dplyr::case_when(
              stringr::str_detect(V1, Name) &
                stringr::str_detect(V2, Grade) &
                stringr::str_detect(V3, Time_Score_String) == FALSE ~ V3,
              stringr::str_detect(V1, ",") == FALSE &
                stringr::str_detect(V1, "^[:digit:]*$") == FALSE &
                stringr::str_detect(V1, "[:alpha:] [:alpha:]") == FALSE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2],
              stringr::str_detect(V2, "^\\'[:upper:]\\'$") == TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2],
              stringr::str_detect(V2, "^\\'[:upper:]\\'$") == FALSE &
                stringr::str_detect(V1, "^[:digit:]*$") == FALSE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2],
              TRUE ~ V3
            )
          ) %>%
          mutate(
            School = str_remove(School, Grade),
            School = trimws(School)
          ) %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              any(stringr::str_detect(V3, Time_Score_String)) == TRUE &
                any(stringr::str_detect(V4, Time_Score_String)) == TRUE ~ V3,
              any(stringr::str_detect(V4, Time_Score_String)) == TRUE &
                any(stringr::str_detect(V5, Time_Score_String)) == TRUE ~ V4,
              any(stringr::str_detect(V4, "SEC")) == TRUE ~ V5
            )
          ) %>%
          dplyr::mutate(
            Finals_Time = dplyr::case_when(
              any(stringr::str_detect(V3, Time_Score_String)) == TRUE &
                any(stringr::str_detect(V4, Time_Score_String)) == TRUE ~ V4,
              any(stringr::str_detect(V4, Time_Score_String)) == TRUE &
                any(stringr::str_detect(V5, Time_Score_String)) == TRUE ~ V5,
              any(stringr::str_detect(V4, "SEC")) == TRUE ~ V6
            )
          ) %>%
          dplyr::mutate(
            Points = dplyr::case_when(Finals_Time != V6 ~ V6,
                                      Finals_Time == V6 ~ ""),
            Points = dplyr::case_when(is.numeric(Points) == FALSE ~ "",
                                      TRUE ~ Points)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::select(
            Place,
            Name,
            Grade,
            School,
            Prelims_Time,
            Finals_Time,
            Points,
            Row_Numb = V7
          ) %>%
          dplyr::na_if("") %>%
          dplyr::na_if("''") %>%
          dplyr::mutate(
            Finals_Time = replace(Finals_Time, stringr::str_length(Finals_Time) < 3, NA),
            Finals_Time = stringr::str_remove(Finals_Time, "J"),
            Finals_Time = dplyr::case_when(
              Place >= 1 &
                is.na(Finals_Time) == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
              Place >= 1 &
                is.na(Finals_Time) != TRUE ~ Finals_Time
            ),
            School = dplyr::case_when(
              is.na(School) == TRUE &
                stringr::str_detect(Grade, "[[:alpha:]]") == TRUE ~ Grade,
              is.na(School) == FALSE &
                stringr::str_detect(Grade, "[[:alpha:]]") == FALSE ~ School,
              is.na(School) == FALSE ~ School
            ),
            School = dplyr::case_when(
              stringr::str_length(School) < 1 | is.na(School) == TRUE ~ Name,
              stringr::str_length(School) >= 1 ~ School
            ),
            School = dplyr::case_when(
              stringr::str_detect(School, "\\'[[:upper:]]\\'|^\\-?[:upper:]$") == TRUE &
                is.na(Name) == FALSE ~ Name,
              TRUE ~ School
            ),
            Grade = replace(Grade, Grade == School, NA),
            School = dplyr::case_when(
              stringr::str_length(Grade) > 2 &
                is.na(Grade) == FALSE &
                stringr::str_detect(Grade, "\\'[[:alpha:]]\\'") == FALSE &
                stringr::str_detect(Grade, "\\.\\d") == FALSE ~ paste(Grade, School, sep = " "),
              stringr::str_length(Grade) <= 2 |
                is.na(Grade) == TRUE |
                stringr::str_detect(Grade, "\\'[[:alpha:]]\\'") == TRUE |
                stringr::str_detect(Grade, "\\.\\d") == TRUE ~ School
            ),
            Grade = replace(Grade, (
              stringr::str_length(Grade) > 2 & is.na(Grade) == FALSE
            ), NA),
            Grade = dplyr::case_when(
              stringr::str_detect(Grade, "^\\-?[:upper:]$") == TRUE ~ "",
              TRUE ~ Grade
            ),
            Name = replace(Name, Name == School, NA),
            ### dealing with exhibition times 8/18/2020
            Exhibition = dplyr::case_when(
              stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
              TRUE ~ 0
            ),
            ###
            Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String),
            Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
              all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
            )
          ) %>%
          dplyr::mutate(
            School = dplyr::case_when(
              stringr::str_detect(School, "\\'[[:alpha:]]\\'") == TRUE &
                is.na(Name) == FALSE ~ Name,
              stringr::str_detect(School, "\\'[[:alpha:]]\\'") == FALSE ~ School,
              stringr::str_detect(School, "^A$") == TRUE &
                is.na(Name) == FALSE ~ Name,
              stringr::str_detect(School, "^A$") == FALSE ~ School
            ),
            School = stringr::str_remove(School, "^SR |^JR |^SO |^FR |^10 |^11 |^12 |^9 |^8 |^7 "),
            Name = stringr::str_remove(Name, "^-"),
            Name = replace(Name, Name == School, NA)
          ) %>%
          dplyr::mutate(DQ = 0)
      )

    } else {
      df_7 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### six variables ####
    if (length(data_length_6) > 0) {
      suppressWarnings(
        df_6 <- data_length_6 %>%
          list_transform() %>%
          dplyr::mutate(Place = stringr::str_split_fixed(V1, " ", n = 2)[, 1]) %>%
          dplyr::mutate(
            Name = dplyr::case_when(
              stringr::str_detect(V1, ",") == TRUE ~ stringr::str_extract(V1, Name_String),
              stringr::str_detect(V2, ",") == TRUE ~ stringr::str_extract(V2, Name_String),
              V1 == Place &
                stringr::str_detect(V2, "[:digit:]") == FALSE ~ V2,
              TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2]
            ),
            V1 = stringr::str_remove(V1, Place),
            V1 = trimws(V1)
          ) %>%
          dplyr::mutate(
            Grade = dplyr::case_when(
              stringr::str_length(stringr::str_split_fixed(V2, " ", n = 2)[, 1]) <= 2 &
                stringr::str_detect(V2, "^SR |^JR |^SO |^FR |^[:digit:]{1,3} ") == TRUE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 1],
              stringr::str_detect(V3, "^SR |^JR |^SO |^FR |^[:digit:]{1,3} ") == TRUE ~ stringr::str_split_fixed(V3, " ", n = 2)[, 1],
              stringr::str_length(V2) <= 2 &
                stringr::str_detect(V2, "[[:alpha:]\\.]") == FALSE ~ V2,
              stringr::str_length(V2) <= 2 &
                stringr::str_detect(V2, Grade_String) == TRUE ~ V2,
              stringr::str_length(V3) <= 2 &
                stringr::str_detect(V3, Grade_String) == TRUE ~ V3,
              TRUE ~ ""
            )
          ) %>%
          na_if("") %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            School = dplyr::case_when(
              V3 == Grade ~ V4,
              V2 == Grade ~ V3,
              any(stringr::str_detect(V3, "SEC")) == TRUE ~ stringr::str_remove(V1, Name),
              is.na(Grade) &
                stringr::str_detect(V2, Time_Score_String) == FALSE &
                stringr::str_detect(V2, Name) == FALSE &
                stringr::str_detect(V2, "[:lower:]{1,}") == TRUE ~ V2,
              is.na(Grade) &
                stringr::str_detect(V2, Time_Score_String) == FALSE &
                stringr::str_detect(V2, Name) == FALSE &
                stringr::str_detect(V2, "[:alpha:]{2,}") == TRUE ~ V2,
              is.na(Grade) &
                stringr::str_detect(V2, Time_Score_String) == FALSE &
                stringr::str_detect(V2, Name) == FALSE &
                stringr::str_detect(V2, "[:lower:]{1,}") == FALSE ~ "",
              is.na(Grade) == TRUE &
                stringr::str_detect(V2, Name) == TRUE &
                stringr::str_detect(V3, "[:lower:]{2,}") == TRUE  ~ V3,
              is.na(Grade) == FALSE &
                stringr::str_detect(V2, Name) == TRUE &
                stringr::str_detect(V3, "[:lower:]{1,}") == TRUE ~ V3,
              is.na(Grade) == TRUE &
                stringr::str_detect(V2, Name) == TRUE &
                stringr::str_detect(V3, "[:lower:]{1,}") == FALSE ~ V2,
              is.na(Grade) == TRUE &
                stringr::str_detect(V2, Name) == TRUE ~ V3,
              TRUE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2]
            )
          ) %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              stringr::str_detect(V2, Time_Score_String) == TRUE & # new 8/18
                stringr::str_detect(V3, Time_Score_String) == TRUE ~ V2,
              stringr::str_detect(V4, Time_Score_String) == TRUE &
                stringr::str_detect(V5, Time_Score_String) == TRUE ~ V4,
              stringr::str_detect(V4, Time_Score_String) == FALSE &
                stringr::str_detect(V5, Time_Score_String) == TRUE ~ "",
              TRUE ~ V3
            )
          ) %>%
          dplyr::mutate(
            Finals_Time = dplyr::case_when(
              stringr::str_detect(V2, Time_Score_String) == TRUE & # new 8/18
                stringr::str_detect(V3, Time_Score_String) == TRUE ~ V3,
              stringr::str_detect(V4, Time_Score_String) == FALSE &
                stringr::str_detect(V5, Time_Score_String) == TRUE ~ V5,
              TRUE ~ V4
            )
          ) %>%
          dplyr::mutate(
            Points = dplyr::case_when(Finals_Time == V5 ~ "",
                                      TRUE ~ V5),
            Points = dplyr::case_when(is.numeric(Points) == FALSE ~ "",
                                      TRUE ~ Points)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::select(
            Name,
            Place,
            Grade,
            School,
            Prelims_Time,
            Finals_Time,
            Points,
            Row_Numb = V6
          ) %>%
          dplyr::na_if("") %>%
          dplyr::na_if("''") %>%
          dplyr::mutate(
            Finals_Time = replace(Finals_Time, stringr::str_length(Finals_Time) < 3, NA),
            Finals_Time = dplyr::case_when(
              Place >= 1 &
                is.na(Finals_Time) == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
              Place >= 1 &
                is.na(Finals_Time) != TRUE ~ Finals_Time
            ),
            School = dplyr::case_when(
              is.na(School) == TRUE &
                stringr::str_detect(Grade, "[[:alpha:]]") == TRUE ~ Grade,
              is.na(School) == FALSE &
                stringr::str_detect(Grade, "[[:alpha:]]") == FALSE ~ School,
              is.na(School) == FALSE ~ School
            ),
            School = dplyr::case_when(
              stringr::str_length(School) < 1 | is.na(School) == TRUE ~ Name,
              stringr::str_length(School) >= 1 ~ School
            ),
            Grade = replace(Grade, Grade == School, NA),
            School = dplyr::case_when(
              is.na(Grade) == FALSE &
                stringr::str_detect(Grade, "\\'[[:alpha:]]\\'") == FALSE &
                stringr::str_detect(Grade, Time_Score_String) == FALSE &
                stringr::str_detect(Grade, "\\d") == FALSE &
                Grade %in% c("SR", "JR", "SO", "FR") == FALSE &
                Grade != School ~ paste(Grade, School, sep = " "),
              TRUE ~ School
            ),
            Grade = replace(Grade, (
              stringr::str_length(Grade) > 2 & is.na(Grade) == FALSE
            ), NA),
            Name = replace(Name, Name == School, NA),
            ### dealing with exhibition times 8/18/2020
            Exhibition = dplyr::case_when(
              stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
              TRUE ~ 0
            ),
            ###
            Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String),
            Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String),
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
              all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
            )
          ) %>%
          dplyr::mutate(
            School = dplyr::case_when(
              stringr::str_detect(School, Time_Score_String) == TRUE ~ stringr::str_extract(School, "[:alpha:]*"),
              stringr::str_detect(School, "\\'[[:alpha:]]\\'") == TRUE &
                is.na(Name) == FALSE ~ Name,
              stringr::str_detect(School, "\\'[[:alpha:]]\\'") == FALSE ~ School,
              stringr::str_detect(School, "^\\-?[:upper:]$") == TRUE &
                is.na(Name) == FALSE ~ Name,
              # stringr::str_detect(School, "^-[:upper:]$") == TRUE &
              #   is.na(Name) == FALSE ~ Name,
              TRUE ~ School),
            School = stringr::str_remove(
              School,
              "^[:digit:]{1,2}\\s"
            )) %>%
            dplyr::mutate(
            Name = stringr::str_remove(Name, "^-"),
            Name = replace(Name, Name == School, NA)
          ) %>%
          dplyr::mutate(DQ = 0)
      )

    } else {
      df_6 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### five variables ####
    if (length(data_length_5) > 0) {
      suppressWarnings(
        df_5 <- data_length_5 %>%
          list_transform() %>%
          dplyr::mutate(Place = stringr::str_split_fixed(V1, " ", n = 2)[, 1]) %>%
          dplyr::mutate(
            Name = dplyr::case_when(
              stringr::str_detect(V1, ",") == TRUE ~ stringr::str_extract(V1, Name_String),
              stringr::str_detect(V2, ",") == TRUE ~ stringr::str_extract(V2, Name_String),
              stringr::str_detect(V1, ",") == FALSE &
                stringr::str_detect(V2, Time_Score_String) == TRUE ~ "",
              TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2]
            )
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            V1 = dplyr::case_when(
              is.na(Place) == FALSE ~ stringr::str_remove(V1, Place),
              TRUE ~ V1
            ),
            V1 = dplyr::case_when(
              is.na(Name) == FALSE ~ stringr::str_remove(V1, Name),
              TRUE ~ V1
            ),
            V1 = trimws(V1),
            Name = trimws(Name)
          ) %>%
          dplyr::mutate(
            Grade = dplyr::case_when(
              stringr::str_length(V2) <= 3 &
                stringr::str_detect(V2, "^SR|^JR|^SO|^FR|^\\d{1,3}$") == TRUE ~ V2,
              stringr::str_length(stringr::str_split_fixed(V2, " ", n = 2)[, 1]) <= 3 &
                stringr::str_detect(V2, "SR|JR|SO|FR|^\\d{1,3} ") ~ stringr::str_split_fixed(V2, " ", n = 2)[, 1],
              stringr::str_length(stringr::str_split_fixed(V3, " ", n = 2)[, 1]) <= 3 &
                stringr::str_detect(V3, "SR|JR|SO|FR|^\\d{1,3} ") ~ stringr::str_split_fixed(V3, " ", n = 2)[, 1],
              TRUE ~ ""
            )) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            Grade = trimws(Grade),
            V3 = case_when(is.na(Grade) == FALSE & stringr::str_detect(V3, Grade) == TRUE & stringr::str_detect(V3, Time_Score_String) == FALSE & Grade != "" ~ stringr::str_remove(V3, Grade),
                           TRUE ~ V3),
            V2 = case_when(is.na(Grade) == FALSE & stringr::str_detect(V2, Grade) == TRUE & stringr::str_detect(V2, Time_Score_String) == FALSE & Grade != "" ~ stringr::str_remove(V2, Grade),
                           TRUE ~ V2),
            V2 = trimws(V2),
            V3 = trimws(V3)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            School = dplyr::case_when(
              V2 == Name & str_detect(V4, Time_Score_String) == TRUE ~ V3,
              is.na(V2) & is.na(Name) == FALSE & str_detect(V4, Time_Score_String) == TRUE ~ V3,
              V2 == Grade &
                any(stringr::str_detect(V3, "^SEC \\d+$")) == FALSE ~ V3,
              V2 == Grade &
                any(stringr::str_detect(V3, "^SEC \\d+$")) == TRUE ~ V1,
              (V2 != Grade |
                 is.na(Grade)) &
                stringr::str_detect(V3, paste0(Time_Score_String, "|^NT$|^NP$")) &
                stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|^\\'\\'$") == FALSE &
                stringr::str_detect(V2, Time_Score_String) == FALSE ~ V2,
              (V2 != Grade |
                 is.na(Grade)) &
                stringr::str_detect(V3, paste0(Time_Score_String, "|^NT$|^NP$")) &
                stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|'[:upper:]'|^\\'\\'$") == TRUE &
                (V1 != Name | is.na(Name)) ~ V1,
              (V1 == Name |
                 is.na(Name)) &
                V2 != Grade ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2],
              stringr::str_detect(V2, paste0(Time_Score_String, "|^NT$|^NP$")) == TRUE ~ V1,
              TRUE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2]
            ),
            School = dplyr::case_when(
              stringr::str_detect(School, Grade) == TRUE &
                is.na(School) == FALSE ~ stringr::str_remove(School, Grade),
              TRUE ~ School
            ),
            School = trimws(School)
          ) %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              stringr::str_detect(V2, Time_Score_String) == TRUE &
                stringr::str_detect(V3, Time_Score_String) == TRUE ~ V2,
              stringr::str_detect(V3, Time_Score_String) == TRUE &
                stringr::str_detect(V4, Time_Score_String) == TRUE ~ V3,
              stringr::str_detect(V3, Time_Score_String) == FALSE &
                stringr::str_detect(V4, Time_Score_String) == TRUE ~ "",
              TRUE ~ V2
            )
          ) %>%
          dplyr::mutate(
            Finals_Time = dplyr::case_when(
              stringr::str_detect(V4, Time_Score_String) == TRUE ~ V4,
              TRUE ~ V3
            )
          ) %>%
          dplyr::mutate(
            Points = dplyr::case_when(Finals_Time == V3 ~ V4,
                                      Finals_Time == V4 ~ "",
                                      TRUE ~ ""),
            Points = dplyr::case_when(is.numeric(Points) == FALSE ~ "",
                                      TRUE ~ Points),
            Points = dplyr::case_when(is.numeric(Points) == FALSE ~ "",
                                      TRUE ~ Points)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::select(
            Name,
            Place,
            Grade,
            School,
            Prelims_Time,
            Finals_Time,
            Points,
            Row_Numb = V5
          ) %>%
          dplyr::na_if("") %>%
          dplyr::na_if("''") %>%
          dplyr::mutate(
            Finals_Time = replace(Finals_Time, dplyr::n_distinct(Finals_Time) <= 2, ""),
            Prelims_Time = replace(Prelims_Time, dplyr::n_distinct(Prelims_Time) <= 2, "")
          ) %>%
          dplyr::na_if("") %>%
          dplyr::na_if("''") %>%
          dplyr::mutate(
            Finals_Time = dplyr::case_when(
              sum(stringr::str_detect(Finals_Time, Time_Score_String)) >= 1 ~ stringr::str_remove(Finals_Time, "NT"),
              sum(stringr::str_detect(Finals_Time, Time_Score_String)) < 1 ~ Finals_Time,
              TRUE ~ Finals_Time
            )
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            School = dplyr::case_when(
              is.na(School) == TRUE &
                stringr::str_detect(Finals_Time, Time_Score_String) == FALSE ~ Finals_Time,
              is.na(School) == FALSE |
                stringr::str_detect(Finals_Time, Time_Score_String) == TRUE ~ School
            ),
            Finals_Time = replace(
              Finals_Time,
              stringr::str_detect(Finals_Time, "^NT$") == TRUE,
              NA
            ),
            Finals_Time = replace(Finals_Time, School == Finals_Time, NA),
            Finals_Time = replace(Finals_Time, stringr::str_length(Finals_Time) < 3, NA),
            Finals_Time = dplyr::case_when(
              Place >= 1 &
                is.na(Finals_Time) == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
              Place >= 1 &
                is.na(Finals_Time) != TRUE ~ Finals_Time
            ),
            ### dealing with exhibition times 8/18/2020
            Exhibition = dplyr::case_when(
              stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
              TRUE ~ 0
            ),
            ###
            Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String),
            Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String),
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
              all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
            )
          ) %>%
          dplyr::mutate(
            Points_2 = dplyr::case_when(
              stringr::str_detect(Points, Colon_String) == TRUE ~ "",
              as.numeric(Points) > 20 &
                stringr::str_detect(Points, Time_Score_String) == TRUE ~ "",
              stringr::str_detect(Points, Colon_String) == FALSE ~ Points
            ),
            Prelims_Time = dplyr::case_when(
              stringr::str_detect(Points, Colon_String) == TRUE ~ Finals_Time,
              as.numeric(Points) > 20 &
                stringr::str_detect(Points, Time_Score_String) == TRUE ~ Finals_Time,
              TRUE ~ Prelims_Time
            ),
            Finals_Time = dplyr::case_when(
              stringr::str_detect(Points, Colon_String) == TRUE ~ Points,
              as.numeric(Points) > 20 &
                stringr::str_detect(Points, Time_Score_String) == TRUE ~ Points,
              TRUE ~ Finals_Time
            )
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            Points = dplyr::case_when(
              is.na(Points_2) == FALSE ~ Points_2,
              Points == Finals_Time ~ "",
              TRUE ~ Points
            ),
            Points_2 = NULL,
            Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String),
            Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String),
            Prelims_Time = replace(
              Prelims_Time,
              stringr::str_detect(Prelims_Time, Time_Score_String) == FALSE,
              NA
            )
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            School = replace(School, stringr::str_detect(School, "\\.\\d") == TRUE, NA),
            School = dplyr::case_when(
              is.na(School) == TRUE &
                Grade %!in% c(
                  "FR",
                  "SO",
                  "JR",
                  "SR",
                  "7",
                  "8",
                  "05",
                  "06",
                  "07",
                  "08",
                  "09",
                  as.character(seq(10, 25, 1))
                ) &
                stringr::str_detect(Grade, "\\'[[:alpha:]]\\'") == FALSE &
                stringr::str_detect(Grade, "\\.\\d") == FALSE ~ Grade,
              is.na(School) == FALSE |
                Grade %in% c(
                  "FR",
                  "SO",
                  "JR",
                  "SR",
                  "7",
                  "8",
                  "05",
                  "06",
                  "07",
                  "08",
                  "09",
                  as.character(seq(10, 25, 1))
                ) |
                stringr::str_detect(Grade, "\\'[[:alpha:]]\\'") == TRUE |
                stringr::str_detect(Grade, "\\.\\d") == FALSE ~ School,
              is.na(School) == FALSE ~ School
            ),
            School = dplyr::case_when(
              stringr::str_length(School) < 1 | is.na(School) == TRUE ~ Name,
              stringr::str_length(School) >= 1 ~ School
            ),
            School = dplyr::case_when(
              is.na(Grade) == FALSE &
                stringr::str_detect(Grade, "\\'[[:alpha:]]\\'|\\.?\\d") == FALSE &
                Grade %in% c("SR", "JR", "SO", "FR") == FALSE &
                Grade != School ~ paste(Grade, School, sep = " "),
              TRUE ~ School
            ),
            School = dplyr::case_when(
              stringr::str_detect(School, "\\'[[:upper:]]\\'|^\\-?[:upper:]$") == TRUE &
                is.na(Name) == FALSE ~ Name,
              TRUE ~ School
            ),
            School = dplyr::case_when(
              stringr::str_detect(School, Time_Score_String) == TRUE ~ stringr::str_extract(School, "[:alpha:]*"),
              TRUE ~ School
            ),
            Grade = replace(Grade, Grade == School, NA),
            Grade = replace(Grade, stringr::str_detect(Grade, Time_Score_String) == TRUE, NA),
            Name = stringr::str_remove(Name, "^-"),
            Name = replace(Name, Name == School, NA),
            Grade = replace(Grade, is.na(Name) == TRUE &
                              is.na(School) == FALSE, NA),

            Grade = replace(Grade, Grade == School, NA),
            Grade = replace(Grade, (
              stringr::str_length(Grade) > 2 & is.na(Grade) == FALSE
            ), NA)

          ) %>%
          dplyr::mutate(DQ = 0)
      )
    } else {
      df_5 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### four variables ####
    if (length(data_length_4) > 0) {
      suppressWarnings(
        df_4 <- data_length_4 %>%
        list_transform() %>%
          dplyr::mutate(Place = stringr::str_split_fixed(V1, " ", n = 2)[, 1]) %>%
          dplyr::mutate(
            Name = dplyr::case_when(
              any(stringr::str_detect(V1, ",")) == TRUE &
                stringr::str_detect(V1, ",") == TRUE ~ stringr::str_extract(V1, Name_String),
              any(stringr::str_detect(V1, ",")) == TRUE &
                stringr::str_detect(V1, ",") == FALSE ~ "",
              stringr::str_detect(V2, ",") == TRUE ~ stringr::str_extract(V2, Name_String),
              stringr::str_detect(V1, ",") == FALSE &
                stringr::str_detect(V2, Time_Score_String) == TRUE ~ "",
              TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2]
            )
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            V1 = dplyr::case_when(
              is.na(Place) == FALSE ~ stringr::str_remove(V1, Place),
              TRUE ~ V1
            ),
            V1 = dplyr::case_when(
              is.na(Name) == FALSE ~ stringr::str_remove(V1, Name),
              TRUE ~ V1
            ),
            V1 = trimws(V1),
            Name = trimws(Name)
          ) %>%
          dplyr::mutate(
            Grade = dplyr::case_when(
              stringr::str_length(stringr::str_split_fixed(V2, " ", n = 2)[, 1]) <= 2 &
                stringr::str_detect(V2, "SR|JR|SO|FR|12|11|10|9|8|7|^\\d\\d ") ~ stringr::str_split_fixed(V2, " ", n = 2)[, 1],
              TRUE ~ ""
            ),
            Grade = trimws(Grade)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            School = dplyr::case_when(
              V2 == Grade &
                any(stringr::str_detect(V3, "^SEC \\d+$")) == FALSE ~ V3,
              V2 == Grade &
                any(stringr::str_detect(V3, "^SEC \\d+$")) == TRUE ~ V1,
              (V2 != Grade |
                 is.na(Grade)) &
                stringr::str_detect(V3, paste0(Time_Score_String, "|^NT$|^NP$")) &
                stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|^\\'\\'$") == FALSE &
                stringr::str_detect(V2, Time_Score_String) == FALSE ~ V2,
              (V2 != Grade |
                 is.na(Grade)) &
                stringr::str_detect(V3, paste0(Time_Score_String, "|^NT$|^NP$")) &
                stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|'[:upper:]'|^\\'\\'$") == TRUE &
                (V1 != Name | is.na(Name)) ~ V1,
              (V1 == Name |
                 is.na(Name)) &
                V2 != Grade ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2],
              stringr::str_detect(V2, paste0(Time_Score_String, "|^NT$|^NP$")) == TRUE ~ V1,
              TRUE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2]
            ),
            School = dplyr::case_when(
              stringr::str_detect(School, Grade) == TRUE &
                is.na(School) == FALSE ~ stringr::str_remove(School, Grade),
              TRUE ~ School
            ),
            School = trimws(School)
          ) %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              stringr::str_detect(V2, Time_Score_String) == TRUE &
                stringr::str_detect(V3, Time_Score_String) == TRUE ~ V2,
              stringr::str_detect(V3, Time_Score_String) == TRUE &
                stringr::str_detect(V4, Time_Score_String) == TRUE ~ V3,
              stringr::str_detect(V3, Time_Score_String) == FALSE &
                stringr::str_detect(V4, Time_Score_String) == TRUE ~ "",
              TRUE ~ ""
            )
          ) %>%
          na_if("") %>%
          dplyr::mutate(
            Finals_Time = dplyr::case_when(
              stringr::str_detect(V3, Time_Score_String) == TRUE &
                stringr::str_detect(V4, Time_Score_String) == TRUE ~ V4,
              stringr::str_detect(V3, Time_Score_String) == FALSE &
                stringr::str_detect(V4, Time_Score_String) == TRUE ~ V4,
              TRUE ~ V3
            )
          ) %>%
          dplyr::mutate(
            Points = dplyr::case_when(Finals_Time == V3 ~ V4,
                                      Finals_Time == V4 ~ "",
                                      TRUE ~ ""),
            Points = dplyr::case_when(is.numeric(Points) == FALSE ~ "",
                                      TRUE ~ Points)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::select(
            Name,
            Place,
            Grade,
            School,
            Prelims_Time,
            Finals_Time,
            Row_Numb = V4
          ) %>%
          dplyr::na_if("") %>%
          dplyr::na_if("''") %>%
          dplyr::mutate(
            Finals_Time = replace(Finals_Time, stringr::str_length(Finals_Time) < 3, NA),
            Finals_Time = dplyr::case_when(
              Place >= 1 &
                is.na(Finals_Time) == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
              Place >= 1 &
                is.na(Finals_Time) != TRUE ~ Finals_Time
            )
          ) %>%
          dplyr::mutate(
            School = dplyr::case_when(
              is.na(School) == TRUE &
                stringr::str_detect(Grade, "[[:alpha:]]") == TRUE &
                stringr::str_length(Grade) > 1 ~ Grade,
              is.na(School) == FALSE &
                stringr::str_detect(Grade, "[[:alpha:]]") == FALSE ~ School,
              is.na(School) == FALSE ~ School
            ),
            School = dplyr::case_when(
              stringr::str_length(School) < 1 | is.na(School) == TRUE ~ Name,
              stringr::str_length(School) >= 1 ~ School
            )
          ) %>%
          dplyr::mutate(
            Grade = replace(Grade, Grade == School, NA),
            Name = stringr::str_remove(Name, "^-"),
            Name = replace(Name, Name == School, NA)
          ) %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              is.na(Name) == TRUE &
                stringr::str_detect(Grade, Time_Score_String) == TRUE ~ Grade,
              TRUE ~ Prelims_Time
            )
          ) %>%
          dplyr::mutate(
            Grade = replace(Grade, is.na(Name) == TRUE &
                              is.na(School) == FALSE, NA),
            ### dealing with exhibition times 8/18/2020
            Exhibition = dplyr::case_when(
              stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
              TRUE ~ 0
            ),
            Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String)
          ) %>%
          dplyr::mutate(
            Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
              all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
            )
          ) %>%
          dplyr::mutate(
            Grade = replace(Grade, Grade == School, NA),
            School = dplyr::case_when(
              is.na(Grade) == FALSE &
                stringr::str_detect(Grade, "\\'[[:alpha:]]\\'") == FALSE &
                stringr::str_detect(Grade, Time_Score_String) == FALSE &
                stringr::str_detect(Grade, "\\d") == FALSE &
                Grade %in% c("SR", "JR", "SO", "FR") == FALSE &
                Grade != School ~ paste(Grade, School, sep = " "),
              TRUE ~ School
            ),
            School = dplyr::case_when(
              stringr::str_detect(School, "\\'[[:upper:]]\\'") == TRUE &
                is.na(Name) == FALSE ~ Name,
              stringr::str_detect(School, "^[:upper:]$") == TRUE &
                is.na(Name) == FALSE ~ Name,
              stringr::str_detect(School, "^[:upper:]$") == FALSE |
                is.na(Name) == TRUE ~ School
            ),
            Grade = replace(
              Grade,
              (
                stringr::str_detect(Grade, "\\d") == FALSE &
                  Grade %in% c("SR", "JR", "SO", "FR") == FALSE
              ),
              NA
            ),
            Grade = dplyr::case_when(
              stringr::str_detect(Name, "\\.d{1,3}") == TRUE ~ stringr::str_extract(Name, "\\d{1,3}"),
              stringr::str_detect(Name, "\\.d{1,3}") == FALSE ~ Grade
            ),
            Name = stringr::str_remove(Name, "\\d{1,3}"),
            Prelims_Time = dplyr::case_when(
              is.na(Prelims_Time) == TRUE &
                stringr::str_detect(School, "\\.") == TRUE ~ stringr::str_extract(
                  School,
                  "[:digit:]*[:punct:]?[:digit:]*[:punct:]?[:digit:]*"
                ),
              is.na(Prelims_Time) == FALSE |
                stringr::str_detect(School, "\\.") == FALSE ~ Prelims_Time
            ),
            School = stringr::str_remove(School, "-[:alpha:]*\\d.*$"),
            School = stringr::str_remove(
              School,
              "^[:digit:]{1,2}\\s")
          ) %>%
          na_if("") %>%
          dplyr::mutate(
            Name = stringr::str_remove(Name, "^-"),
            Name = replace(Name, Name == School, NA)
          ) %>%
          dplyr::mutate(DQ = 0)
      )
    } else {
      df_4 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### three variables ####
    if (length(data_length_3) > 0) {
      suppressWarnings(
        df_3 <- data_length_3 %>%
          list_transform() %>%
          dplyr::mutate(
            Place = stringr::str_split_fixed(V1, " ", n = 2)[, 1],
            Name = stringr::str_split_fixed(V1, " ", n = 2)[, 2],
            Grade = as.character(NA),
            School = as.character(NA),
            Finals_Time = V2,
            Prelims_Time = as.character(NA),
          ) %>%
          dplyr::select(
            Name,
            Place,
            Grade,
            School,
            Prelims_Time,
            Finals_Time,
            Row_Numb = V3
          ) %>%
          dplyr::na_if("") %>%
          dplyr::na_if("''") %>%
          dplyr::mutate(
            Finals_Time = replace(Finals_Time, stringr::str_length(Finals_Time) < 3, NA),
            Finals_Time = dplyr::case_when(
              Place >= 1 &
                is.na(Finals_Time) == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
              Place >= 1 &
                is.na(Finals_Time) != TRUE ~ Finals_Time
            ),
            School = dplyr::case_when(
              stringr::str_length(School) < 1 | is.na(School) == TRUE ~ Name,
              stringr::str_length(School) >= 1 ~ School
            ),
            Name = stringr::str_remove(Name, "^-"),
            Name = replace(Name, Name == School, NA),
            Prelims_Time = dplyr::case_when(
              is.na(Name) == TRUE ~ Grade,
              is.na(Name) == FALSE ~ Prelims_Time
            ),
            Grade = replace(Grade, is.na(Name) == TRUE &
                              is.na(School) == FALSE, NA),
            ### dealing with exhibition times 8/18/2020
            Exhibition = dplyr::case_when(
              stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
              TRUE ~ 0
            ),
            ###
            Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String),
            Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            Prelims_Time = dplyr::case_when(
              all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
              all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
            )
          ) %>%
          dplyr::mutate(DQ = 0)
      )
    } else {
      df_3 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### DQ data ####
    #### DQ 4 ####
    if (length(DQ_length_4) > 0) {
      suppressWarnings(
        df_DQ_4 <- DQ_length_4 %>%
          list_transform() %>%
          dplyr::mutate(Place = stringr::str_split_fixed(V1, " ", n = 2)[, 1]) %>%
          dplyr::mutate(
            Name = dplyr::case_when(
              any(stringr::str_detect(V1, ",")) == TRUE &
                stringr::str_detect(V1, ",") == TRUE ~ stringr::str_extract(V1, "_?[:alpha:]+'?[:alpha:]+, [:alpha:]+\\s?[:alpha:]?\\s?[:alpha:]?\\s?[:alpha:]?\\s?[:alpha:]?\\s?[:alpha:]?"),
              any(stringr::str_detect(V1, ",")) == TRUE &
                stringr::str_detect(V1, ",") == FALSE ~ "",
              stringr::str_detect(V2, ",") == TRUE ~ stringr::str_extract(V2, "_?[:alpha:]+'?[:alpha:]+, [:alpha:]+\\s?[:alpha:]?\\s?[:alpha:]?\\s?[:alpha:]?\\s?[:alpha:]?\\s?[:alpha:]?"),
              stringr::str_detect(V1, ",") == FALSE &
                stringr::str_detect(V2, Time_Score_String) == TRUE ~ "",
              TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2]
            )
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            V1 = dplyr::case_when(
              is.na(Place) == FALSE ~ stringr::str_remove(V1, Place),
              TRUE ~ V1
            ),
            V1 = dplyr::case_when(
              is.na(Name) == FALSE ~ stringr::str_remove(V1, Name),
              TRUE ~ V1
            ),
            V1 = trimws(V1),
            Name = trimws(Name)
          ) %>%
          dplyr::mutate(
            Grade = dplyr::case_when(
              stringr::str_length(stringr::str_split_fixed(V2, " ", n = 2)[, 1]) <= 3 &
                stringr::str_detect(V2, "SR|JR|SO|FR|12|11|10|9|8|7|^\\d{1,3} ") ~ stringr::str_split_fixed(V2, " ", n = 2)[, 1],
              TRUE ~ ""
            ),
            Grade = trimws(Grade)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::mutate(
            School = dplyr::case_when(
              V2 == Grade &
                any(stringr::str_detect(V3, "^SEC \\d+$")) == FALSE ~ V3,
              V2 == Grade &
                any(stringr::str_detect(V3, "^SEC \\d+$")) == TRUE ~ V1,
              (V2 != Grade |
                 is.na(Grade)) &
                stringr::str_detect(V3, paste0(Time_Score_String, "|^NT$|^NP$")) &
                stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|^\\'\\'$") == FALSE &
                stringr::str_detect(V2, Time_Score_String) == FALSE ~ V2,
              (V2 != Grade |
                 is.na(Grade)) &
                stringr::str_detect(V3, paste0(Time_Score_String, "|^NT$|^NP$")) &
                stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|'[:upper:]'|^\\'\\'$") == TRUE &
                (V1 != Name | is.na(Name)) ~ V1,
              (V1 == Name |
                 is.na(Name)) &
                V2 != Grade ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2],
              stringr::str_detect(V2, paste0(Time_Score_String, "|^NT$|^NP$")) == TRUE ~ V1,
              TRUE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2]
            ),
            School = dplyr::case_when(
              stringr::str_detect(School, Grade) == TRUE &
                is.na(School) == FALSE ~ stringr::str_remove(School, Grade),
              TRUE ~ School
            ),
            School = trimws(School)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::select(Name,
                        Place,
                        Grade,
                        School,
                        Row_Numb = V4) %>%
          dplyr::na_if("") %>%
          dplyr::na_if("''") %>%
          dplyr::mutate(DQ = 1)
      )

    } else {
      df_DQ_4 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### DQ 3 ####
    if (length(DQ_length_3) > 0) {
      suppressWarnings(
        df_DQ_3 <- DQ_length_3 %>%
          list_transform() %>%
          dplyr::mutate(Place = stringr::str_split_fixed(V1, " ", n = 2)[, 1]) %>%
          dplyr::mutate(
            School = stringr::str_split_fixed(V1, " ", n = 2)[, 2],
            School = trimws(School)
          ) %>%
          dplyr::na_if("") %>%
          dplyr::select(Place,
                        School,
                        Row_Numb = V3) %>%
          dplyr::na_if("") %>%
          dplyr::na_if("''") %>%
          dplyr::mutate(DQ = 1)
      )
    } else {
      df_DQ_3 <- data.frame(
        Row_Numb = character(),
        stringsAsFactors = FALSE
      )
    }

    #### Rejoin dataframes from each number of variables ####
    Min_Row_Numb <- min(events$Event_Row_Min)
    suppressWarnings(
      data <- dplyr::full_join(df_7, df_6) %>%
        dplyr::full_join(df_5) %>%
        dplyr::full_join(df_4) %>%
        dplyr::full_join(df_3) %>%
        dplyr::left_join(df_DQ_4) %>%
        dplyr::left_join(df_DQ_3) %>%
        dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
        dplyr::arrange(Row_Numb) %>%
        ### moved up from below for DQ work 8/20
        dplyr::mutate(DQ = dplyr::case_when(Place == 10000 & Exhibition == 0 ~ 1, # added exhibition condition 8/27
                                            TRUE ~ DQ)) %>%
        na_if(10000) %>%
        ####
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

    #### cleaning up final results ####

    suppressWarnings(
      data <- data %>%
        dplyr::mutate(
          Name = str_replace(Name, "_", "\\*"),
          Prelims_Time = replace(
            Prelims_Time,
            stringr::str_detect(Prelims_Time, "\\.") == FALSE,
            NA
          ),
          Prelims_Time = replace(
            Prelims_Time,
            (
              stringr::str_detect(Prelims_Time, "\\.") == TRUE &
                stringr::str_detect(Prelims_Time, "\\:") == FALSE &
                as.numeric(Prelims_Time) < 15
            ),
            NA
          ),
          Points = replace(
            Points,
            stringr::str_detect(Points, "[[:alpha:]]") == TRUE,
            NA
          ),
          Points = replace(
            Points,
            stringr::str_detect(Points, "[^[:digit:]|\\.]"),
            NA
          ),
          Place = round(as.numeric(Place)),
          Event = as.character(Event)
        ) %>%
        dplyr::mutate(
          Place = dplyr::case_when(is.na(Place) == TRUE &
                                     DQ == 0 ~ lag(Place) + 1,
                                   TRUE ~ Place)
        ) %>%
        ### added 8/20 as part of DQ, to remove records and such but not DQ results
        filter(
          DQ == 0 &
            stringr::str_detect(Finals_Time, Time_Score_String) == TRUE |
            DQ == 0 & stringr::str_detect(Prelims_Time, Time_Score_String) == TRUE | DQ == 1
        ) %>%
        dplyr::mutate(Exhibition = dplyr::case_when(is.na(Exhibition) == TRUE ~ 0,
                                                    TRUE ~ Exhibition)) %>%
        dplyr::mutate(Finals_Time = dplyr::case_when(DQ == 1 ~ "NA",
                                                     TRUE ~ Finals_Time)) %>%
        dplyr::na_if("NA")

    )
    if(splits == TRUE){
      splits_df <- splits_parse(as_lines_list_2)

      data <- data %>%
        dplyr::left_join(splits_df, by = 'Row_Numb')

      ### remove empty columns (all values are NA) ###
      data <- Filter(function(x)!all(is.na(x)), data)
    }

    data$Row_Numb <- NULL

      return(data)
    } else if (stringr::str_detect(file[1], "^A107") == TRUE) {
      # file <- add_row_numbers(text = file)
      data <- parse_hy3(file = file)
      return(data)
    }

  }
#' @rdname Swim_Parse
#' @export
swim_parse <- Swim_Parse
