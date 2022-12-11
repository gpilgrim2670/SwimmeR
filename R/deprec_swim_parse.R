#' Formats swimming and diving data read with \code{read_results} into a data
#' frame
#'
#' Takes the output of \code{read_results} and cleans it, yielding a data frame
#' of swimming (and diving) results.  Old version, retired in dev build on Dec
#' 21, 2020 and release version 0.7.0
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
#' @importFrom dplyr everything
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr pull
#' @importFrom dplyr between
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
#' @param avoid a list of strings.  Rows in \code{file} containing these strings
#'   will not be included. For example "Pool:", often used to label pool
#'   records, could be passed to \code{avoid}.  The default is
#'   \code{avoid_default}, which contains many strings similar to "Pool:", such
#'   as "STATE:" and "Qual:".  Users can supply their own lists to \code{avoid}.
#' @param typo a list of strings that are typos in the original results.
#'   \code{swim_parse_old} is particularly sensitive to accidental double
#'   spaces, so "Central  High School", with two spaces between "Central" and
#'   "High" is a problem, which can be fixed.  Pass "Central  High School" to
#'   \code{typo}.  Unexpected commas as also an issue, for example "Texas,
#'   University of" should be fixed using \code{typo} and \code{replacement}
#' @param replacement a list of fixes for the strings in \code{typo}.  Here one
#'   could pass "Central High School" (one space between "Central" and "High")
#'   and "Texas" to \code{replacement} fix the issues described in \code{typo}
#' @param splits either \code{TRUE} or the default, \code{FALSE} - should
#'   \code{swim_parse_old} attempt to include splits.
#' @param split_length either \code{25} or the default, \code{50}, the length of
#'   pool at which splits are recorded.  Not all results are internally
#'   consistent on this issue - some have races with splits by 50 and other
#'   races with splits by 25.
#' @param relay_swimmers either \code{TRUE} or the default, \code{FALSE} -
#'   should relay swimmers be reported.  Relay swimmers are reported in separate
#'   columns named \code{Relay_Swimmer_1} etc.
#' @return returns a data frame with columns \code{Name}, \code{Place},
#'   \code{Age}, \code{Team}, \code{Prelims_Time}, \code{Finals_Time},
#'   \code{Points}, \code{Event} & \code{DQ}.  Note all swims will have a
#'   \code{Finals_Time}, even if that time was actually swam in the prelims
#'   (i.e. a swimmer did not qualify for finals).  This is so that final results
#'   for an event can be generated from just one column.
#'
#' @examples \dontrun{
#' swim_parse_old(
#'  read_results("http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm", node = "pre"),
#'   typo = c("-1NORTH ROCKL"), replacement = c("1-NORTH ROCKL"),
#'   splits = TRUE,
#'   relay_swimmers = TRUE)
#'  }
#' \dontrun{
#' swim_parse_old(read_results("inst/extdata/Texas-Florida-Indiana.pdf"),
#'  typo =  c("Indiana  University", ", University of"), replacement = c("Indiana University", ""),
#'  splits = TRUE,
#'  relay_swimmers = TRUE)
#'  }
#' @seealso \code{swim_parse_old} must be run on the output of
#'   \code{\link{read_results}}
#'

swim_parse_old <-
  function(file,
           avoid = avoid_default,
           typo = typo_default,
           replacement = replacement_default,
           splits = FALSE,
           split_length = 50,
           relay_swimmers = FALSE) {

    #### default typo and replacement strings ####
    typo_default <- c("typo")

    replacement_default <- c("typo")

    if(length(typo) != length(replacement)) {
      stop("typo and replacement must have the same number of elements (be the same length)")
    }

    if(is.logical(splits) == FALSE) {
      stop("splits must be logical, either TRUE or FALSE")
    }

    if(is.numeric(split_length) == FALSE) {
      stop("split_length must be numeric, usually 50 or 25")
    }

    if(is.logical(relay_swimmers) == FALSE) {
      stop("relay_swimmers must be logical, either TRUE or FALSE")
    }

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
        "^\\s*Qual\\.\\:",
        "^\\s*Qualify\\:",
        "^\\s*QUALIFY\\:",
        "QUALIFY\\:",
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

    #### assign row numbers ####
    as_lines_list_2 <- add_row_numbers(text = file)
    # stringr::str_remove_all("\n") %>%
    # # trimws() %>%
    # stringr::str_replace_all(stats::setNames(replacement, typo))

    #### parsing html and pdf files ####
    if (stringr::str_detect(file[1], "^A107") == FALSE) {

      #### Pulls out event labels from text ####
      events <- event_parse(as_lines_list_2)

      #### set up strings ####
      # Name_String <- "_?[:alpha:]+'?[:alpha:]+\\s?[:alpha:]*\\s?[:alpha:]*,\\s?[:alpha:]*\\s?[:alpha:]*,? [:alpha:]+\\s?[:alpha:\\-\\']*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
      # Name_String <- "_?[:alpha:]+'?[:alpha:]+\\s?[:alpha:]*\\s?[:alpha:]*,\\s?[:alpha:]*\\s?[:alpha:]*,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
      # Name_String <- "_?[:alpha:]+'?[:alpha:\\-\\'\\.]+\\s?[:alpha:]*\\s?[:alpha:]*,\\s?[:alpha:]*\\s?[:alpha:]*,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
      # Name_String <- "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
      Name_String <-
        "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
      Time_Score_String <- "\\d{0,2}\\:?\\d{1,3}\\.\\d{2}"
      Time_Score_Specials_String <- paste0(Time_Score_String, "|^NT$|^NP$|^DQ$")
      Age_String <- "^SR$|^JR$|^SO$|^FR$|^[:digit:]{1,3}$"
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
          # trimws() %>%
          stringr::str_replace_all(stats::setNames(replacement, typo)) %>% # moved to top of pipeline 8/26
          stringr::str_replace_all("\\s*[&%]\\s*", " ") %>% # added 8/21 for removing "&" and "%" as record designator
          # removed J etc. from next to swim, but does not remove X or x (for exhibition tracking)
          stringr::str_replace_all("[A-WYZa-wyz]+(\\d{1,2}\\:\\d{2}\\.\\d{2})", "\\1") %>%
          stringr::str_replace_all("(\\d{1,2}\\:\\d{2}\\.\\d{2})[A-WYZa-wyz]+", "\\1") %>%
          stringr::str_replace_all("[A-WYZa-wyz]+(\\d{2,3}\\.\\d{2})", "\\1") %>%
          stringr::str_replace_all("(\\d{2,3}\\.\\d{2})[A-WYZa-wyz]+", "\\1") %>%
          stringr::str_replace_all(" [:punct:]+(\\d{1,2}\\:\\d{2}\\.\\d{2})", " \\1") %>%
          stringr::str_replace_all("(\\d{1,2}\\:\\d{2}\\.\\d{2})[:punct:]+", " \\1") %>%
          stringr::str_replace_all(" [:punct:]+(\\d{2,3}\\.\\d{2})", " \\1") %>%
          stringr::str_replace_all("(\\d{2,3}\\.\\d{2})[:punct:]+", " \\1") %>%
          # stringr::str_replace_all("(\\d{2,3}\\.\\d{2})[:punct:]+", "\\1 ") %>% should swap for line above 12/21 - need to test
          stringr::str_remove_all("\\s{2}J\\s{2}") %>%
          # remove 'A', 'B' etc. relay designators - should this go in typo instead?
          stringr::str_replace_all("  \\'[A-Z]\\'  ", "  ") %>%
          stringr::str_replace_all("  [A-Z]  ", "  ") %>%
          stringr::str_replace_all("\\'\\'", "  ") %>%
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
              Age = dplyr::case_when(
                stringr::str_detect(V2, Age_String) == TRUE ~ V2,
                stringr::str_detect(V2, "^SR |^JR |^SO |^FR |^[:digit:]{1,3} ") == TRUE ~ stringr::str_extract(V2, "^SR |^JR |^SO |^FR |^10 |^11 |^[:digit:]{1,2} "),
                stringr::str_detect(V3, "^SR |^JR |^SO |^FR |^[:digit:]{1,3}\\s?") == TRUE ~ stringr::str_extract(V3, "^SR |^JR |^SO |^FR |^10 |^11 |^[:digit:]{1,2}\\s?"),
                any(stringr::str_detect(V2, Age_String)) == FALSE &
                  any(stringr::str_detect(V3, Age_String)) == TRUE ~ V3
              ),
              Age = trimws(Age),
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V1, Name) &
                  stringr::str_detect(V2, Age) &
                  stringr::str_detect(V3, Time_Score_Specials_String) == FALSE ~ V3,
                stringr::str_detect(V1, paste0("^", Place, "$")) == TRUE &
                  stringr::str_detect(V2, paste0("^", Name, "$")) == TRUE &
                  stringr::str_detect(V3, paste0("^", Age, "$")) == TRUE ~ V4,
                stringr::str_detect(V1, ",") == FALSE &
                  stringr::str_detect(V1, "^[:digit:]*$") == FALSE &
                  stringr::str_detect(V1, "[:alpha:] [:alpha:]") == FALSE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2],
                stringr::str_detect(V2, "^\\'[:upper:]\\'$") == TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2],
                stringr::str_detect(V2, "^\\'[:upper:]\\'$") == FALSE &
                  stringr::str_detect(V1, "^[:digit:]*$") == FALSE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2],
                TRUE ~ V3
              )
            ) %>%
            mutate(Team = str_remove(Team, Age),
                   Team = trimws(Team)
            ) %>%
            # na_if_character("") %>%
            dplyr::mutate(
              Prelims_Time = dplyr::case_when(
                # any(stringr::str_detect(V3, Time_Score_Specials_String)) == TRUE &
                #   any(stringr::str_detect(V4, Time_Score_Specials_String)) == TRUE ~ V3,
                # any(stringr::str_detect(V4, Time_Score_Specials_String)) == TRUE &
                #   any(stringr::str_detect(V5, Time_Score_Specials_String)) == TRUE ~ V4,
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V4,
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V6, Time_Score_Specials_String) == TRUE ~ V5,
                any(stringr::str_detect(V4, "SEC")) == TRUE ~ V5
              )
            ) %>%
            dplyr::mutate(
              Finals_Time = dplyr::case_when(
                # any(stringr::str_detect(V3, Time_Score_Specials_String)) == TRUE &
                #   any(stringr::str_detect(V4, Time_Score_Specials_String)) == TRUE ~ V4,
                # any(stringr::str_detect(V4, Time_Score_Specials_String)) == TRUE &
                #   any(stringr::str_detect(V5, Time_Score_Specials_String)) == TRUE ~ V5,
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V4,
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V5,
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V6, Time_Score_Specials_String) == TRUE ~ V6,
                any(stringr::str_detect(V4, "SEC")) == TRUE ~ V6
              )
            ) %>%
            dplyr::mutate(
              Points = dplyr::case_when(Finals_Time != V6 ~ V6,
                                        Finals_Time == V6 ~ ""),
              Points = dplyr::case_when(stringr::str_detect(Points, "^\\d{1,}$") == FALSE ~ "",
                                        TRUE ~ Points)
            ) %>%
            na_if_character("") %>%
            dplyr::select(
              Place,
              Name,
              Age,
              Team,
              Prelims_Time,
              Finals_Time,
              Points,
              Row_Numb = V7
            ) %>%
            na_if_character("") %>%
            na_if_character("''") %>%
            dplyr::mutate(
              DQ = case_when(str_detect(Finals_Time, "^DQ$") ~ 1,
                             TRUE ~ 0),
              Finals_Time = replace(Finals_Time, stringr::str_length(Finals_Time) < 3, NA),
              Finals_Time = stringr::str_remove(Finals_Time, "J"),
              Finals_Time = dplyr::case_when(
                dplyr::between(Place, 1, 9999) &
                  is.na(Finals_Time) == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
                dplyr::between(Place, 1, 9999) &
                  is.na(Finals_Time) != TRUE ~ Finals_Time,
                TRUE ~ Finals_Time
              )) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                is.na(Team) == TRUE &
                  stringr::str_detect(Age, "[[:alpha:]]") == TRUE ~ Age,
                is.na(Team) == FALSE &
                  stringr::str_detect(Age, "[[:alpha:]]") == FALSE ~ Team,
                is.na(Team) == FALSE ~ Team
              ),
              Team = dplyr::case_when(
                stringr::str_length(Team) < 1 | is.na(Team) == TRUE ~ Name,
                stringr::str_length(Team) >= 1 ~ Team
              ),
              Team = dplyr::case_when(
                stringr::str_detect(Team, "\\'[[:upper:]]\\'|^\\-?[:upper:]$") == TRUE &
                  is.na(Name) == FALSE ~ Name,
                TRUE ~ Team
              ),
              Age = replace(Age, Age == Team, NA),
              Team = dplyr::case_when(
                stringr::str_length(Age) > 2 &
                  is.na(Age) == FALSE &
                  stringr::str_detect(Age, "\\'[[:alpha:]]\\'") == FALSE &
                  stringr::str_detect(Age, "\\.\\d") == FALSE ~ paste(Age, Team, sep = " "),
                stringr::str_length(Age) <= 2 |
                  is.na(Age) == TRUE |
                  stringr::str_detect(Age, "\\'[[:alpha:]]\\'") == TRUE |
                  stringr::str_detect(Age, "\\.\\d") == TRUE ~ Team
              ),
              Age = replace(Age, (
                stringr::str_length(Age) > 2 & is.na(Age) == FALSE
              ), NA),
              Age = dplyr::case_when(
                stringr::str_detect(Age, "^\\-?[:upper:]$") == TRUE ~ "",
                TRUE ~ Age
              ),
              Name = replace(Name, Name == Team, NA),
              ### dealing with exhibition times 8/18/2020
              Exhibition = dplyr::case_when(
                stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
                TRUE ~ 0
              ),
              ###
              Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String),
              Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String)
            ) %>%
            na_if_character("") %>%
            dplyr::mutate(
              Prelims_Time = dplyr::case_when(
                all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
                all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(Team, "\\'[[:alpha:]]\\'") == TRUE &
                  is.na(Name) == FALSE ~ Name,
                stringr::str_detect(Team, "\\'[[:alpha:]]\\'") == FALSE ~ Team,
                stringr::str_detect(Team, "^A$") == TRUE &
                  is.na(Name) == FALSE ~ Name,
                stringr::str_detect(Team, "^A$") == FALSE ~ Team
              ),
              Team = stringr::str_remove(Team, "^SR |^JR |^SO |^FR |^10 |^11 |^12 |^9 |^8 |^7 "),
              Name = stringr::str_remove(Name, "^-"),
              Name = replace(Name, Name == Team, NA)
            )
          # dplyr::mutate(DQ = 0)
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
              Age = dplyr::case_when(
                stringr::str_length(stringr::str_split_fixed(V2, " ", n = 2)[, 1]) <= 2 &
                  stringr::str_detect(V2, "^SR |^JR |^SO |^FR |^[:digit:]{1,3} ") == TRUE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 1],
                stringr::str_detect(V3, "^SR |^JR |^SO |^FR |^[:digit:]{1,3} ") == TRUE ~ stringr::str_split_fixed(V3, " ", n = 2)[, 1],
                stringr::str_length(V2) <= 2 &
                  stringr::str_detect(V2, "[[:alpha:]\\.]") == FALSE ~ V2,
                stringr::str_length(V2) <= 2 &
                  stringr::str_detect(V2, Age_String) == TRUE ~ V2,
                stringr::str_length(V3) <= 2 &
                  stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                TRUE ~ ""
              )
            ) %>%
            dplyr::mutate(V3 = case_when(is.na(Age) == FALSE & str_detect(V3, paste0("^", Age, " ")) ==TRUE ~ str_remove(V3, paste0(Age, " ")),
                                         TRUE ~ V3)) %>%
            na_if_character("") %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                V3 == Age ~ V4,
                V2 == Age ~ V3,
                any(stringr::str_detect(V3, "SEC")) == TRUE ~ stringr::str_remove(V1, Name),
                is.na(Age) &
                  stringr::str_detect(V2, Time_Score_String) == FALSE &
                  stringr::str_detect(V2, Name) == FALSE &
                  stringr::str_detect(V2, "[:lower:]{1,}") == TRUE ~ V2,
                is.na(Age) &
                  stringr::str_detect(V2, Time_Score_String) == FALSE &
                  stringr::str_detect(V2, Name) == FALSE &
                  stringr::str_detect(V2, "[:alpha:]{2,}") == TRUE ~ V2,
                is.na(Age) &
                  stringr::str_detect(V2, Time_Score_String) == FALSE &
                  stringr::str_detect(V2, Name) == FALSE &
                  stringr::str_detect(V2, "[:lower:]{1,}") == FALSE ~ "",
                is.na(Age) == TRUE &
                  stringr::str_detect(V2, Name) == TRUE &
                  stringr::str_detect(V3, "[:lower:]{2,}") == TRUE  ~ V3,
                is.na(Age) == FALSE &
                  stringr::str_detect(V2, Name) == TRUE &
                  stringr::str_detect(V3, "[:lower:]{1,}") == TRUE ~ V3,
                is.na(Age) == TRUE &
                  stringr::str_detect(V2, Name) == TRUE &
                  stringr::str_detect(V3, "[:lower:]{1,}") == FALSE ~ V2,
                is.na(Age) == TRUE &
                  stringr::str_detect(V2, Name) == TRUE ~ V3,
                is.na(Age) == FALSE &
                  stringr::str_detect(V2, Name) == TRUE ~ V3,
                TRUE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2]
              )
            ) %>%
            ### df_6 cannot have NT as time string since there are some schools called NT ###
            dplyr::mutate(
              Prelims_Time = dplyr::case_when(
                stringr::str_detect(V2, Time_Score_Specials_String) == TRUE & # new 8/18
                  stringr::str_detect(V3, Time_Score_Specials_String) == TRUE ~ V2,
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V4,
                stringr::str_detect(V4, Time_Score_Specials_String) == FALSE &
                  stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ "",
                TRUE ~ V3
              )
            ) %>%
            dplyr::mutate(
              Finals_Time = dplyr::case_when(
                stringr::str_detect(V2, Time_Score_Specials_String) == TRUE & # new 8/18
                  stringr::str_detect(V3, Time_Score_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V5,
                TRUE ~ V4
              )
            ) %>%
            dplyr::mutate(
              Points = dplyr::case_when(Finals_Time == V5 ~ "",
                                        # Finals_Time == V4 ~ V5
                                        TRUE ~ V5),
              Points = dplyr::case_when(stringr::str_detect(Points, "^\\d{1,}$") == FALSE ~ "",
                                        TRUE ~ Points)
            ) %>%
            na_if_character("") %>%
            dplyr::select(
              Name,
              Place,
              Age,
              Team,
              Prelims_Time,
              Finals_Time,
              Points,
              Row_Numb = V6
            ) %>%
            na_if_character("") %>%
            na_if_character("''") %>%
            dplyr::mutate(
              Finals_Time = replace(Finals_Time, stringr::str_length(Finals_Time) < 3, NA),
              Finals_Time = dplyr::case_when(
                Place >= 1 &
                  is.na(Finals_Time) == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
                Place >= 1 &
                  is.na(Finals_Time) != TRUE ~ Finals_Time
              ),
              Team = dplyr::case_when(
                is.na(Team) == TRUE &
                  stringr::str_detect(Age, "[[:alpha:]]") == TRUE ~ Age,
                is.na(Team) == FALSE &
                  stringr::str_detect(Age, "[[:alpha:]]") == FALSE ~ Team,
                is.na(Team) == FALSE ~ Team
              ),
              Team = dplyr::case_when(
                stringr::str_length(Team) < 1 | is.na(Team) == TRUE ~ Name,
                stringr::str_length(Team) >= 1 ~ Team
              ),
              Age = replace(Age, Age == Team, NA),
              Team = dplyr::case_when(
                is.na(Age) == FALSE &
                  stringr::str_detect(Age, "\\'[[:alpha:]]\\'") == FALSE &
                  stringr::str_detect(Age, Time_Score_String) == FALSE &
                  stringr::str_detect(Age, "\\d") == FALSE &
                  Age %in% c("SR", "JR", "SO", "FR") == FALSE &
                  Age != Team ~ paste(Age, Team, sep = " "),
                TRUE ~ Team
              ),
              Age = replace(Age, (
                stringr::str_length(Age) > 2 & is.na(Age) == FALSE
              ), NA),
              Name = replace(Name, Name == Team, NA),
              ### dealing with exhibition times 8/18/2020
              Exhibition = dplyr::case_when(
                stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
                TRUE ~ 0
              ),
              ###
              Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String),
              Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String),
            ) %>%
            na_if_character("") %>%
            dplyr::mutate(
              Prelims_Time = dplyr::case_when(
                all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
                all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(Team, Time_Score_String) == TRUE ~ stringr::str_extract(Team, "[:alpha:]*"),
                stringr::str_detect(Team, "\\'[[:alpha:]]\\'") == TRUE &
                  is.na(Name) == FALSE ~ Name,
                stringr::str_detect(Team, "\\'[[:alpha:]]\\'") == FALSE ~ Team,
                stringr::str_detect(Team, "^\\-?[:upper:]$") == TRUE &
                  is.na(Name) == FALSE ~ Name,
                # stringr::str_detect(Team, "^-[:upper:]$") == TRUE &
                #   is.na(Name) == FALSE ~ Name,
                TRUE ~ Team),
              Team = stringr::str_remove(
                Team,
                "^[:digit:]{1,2}\\s"
              )) %>%
            dplyr::mutate(
              Name = stringr::str_remove(Name, "^-"),
              Name = replace(Name, Name == Team, NA)
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
            dplyr::mutate(Place = dplyr::case_when(stringr::str_detect(V1, "^\\d{1,}$") ~ V1,
                                                   TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 1])) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(V1, ",") == TRUE ~ stringr::str_extract(V1, Name_String),
                stringr::str_detect(V2, ",") == TRUE ~ stringr::str_extract(V2, Name_String),
                stringr::str_detect(V2, ",") == FALSE & V1 == Place & stringr::str_detect(V3, Time_Score_Specials_String) == FALSE ~ stringr::str_extract(V2, Name_String),
                stringr::str_detect(V1, ",") == FALSE &
                  stringr::str_detect(V2, Time_Score_String) == TRUE ~ "",
                TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2]
              )
            ) %>%
            na_if_character("") %>%
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
              Age = dplyr::case_when(
                stringr::str_length(V2) <= 3 &
                  stringr::str_detect(V2, "^SR|^JR|^SO|^FR|^\\d{1,3}$") == TRUE ~ V2,
                stringr::str_length(stringr::str_split_fixed(V2, " ", n = 2)[, 1]) <= 3 &
                  stringr::str_detect(V2, "SR|JR|SO|FR|^\\d{1,3} ") ~ stringr::str_split_fixed(V2, " ", n = 2)[, 1],
                stringr::str_length(stringr::str_split_fixed(V3, " ", n = 2)[, 1]) <= 3 &
                  stringr::str_detect(V3, "SR|JR|SO|FR|^\\d{1,3} ") ~ stringr::str_split_fixed(V3, " ", n = 2)[, 1],
                TRUE ~ ""
              )) %>%
            na_if_character("") %>%
            dplyr::mutate(
              Age = trimws(Age),
              V3 = case_when(is.na(Age) == FALSE & stringr::str_detect(V3, Age) == TRUE & stringr::str_detect(V3, Time_Score_Specials_String) == FALSE & Age != "" ~ stringr::str_remove(V3, Age),
                             TRUE ~ V3),
              V2 = case_when(is.na(Age) == FALSE & stringr::str_detect(V2, Age) == TRUE & stringr::str_detect(V2, Time_Score_Specials_String) == FALSE & Age != "" ~ stringr::str_remove(V2, Age),
                             TRUE ~ V2),
              V2 = trimws(V2),
              V3 = trimws(V3)
            ) %>%
            na_if_character("") %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                V2 == Name & str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                is.na(V2) & is.na(Name) == FALSE & str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                V2 == Age &
                  any(stringr::str_detect(V3, "^SEC \\d+$")) == FALSE ~ V3,
                V2 == Age &
                  any(stringr::str_detect(V3, "^SEC \\d+$")) == TRUE ~ V1,
                (V2 != Age |
                   is.na(Age)) &
                  stringr::str_detect(V3, Time_Score_Specials_String) &
                  stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|^\\'\\'$") == FALSE &
                  stringr::str_detect(V2, Time_Score_Specials_String) == FALSE ~ V2,
                (V2 != Age |
                   is.na(Age)) &
                  stringr::str_detect(V3, Time_Score_Specials_String) &
                  stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|'[:upper:]'|^\\'\\'$") == TRUE &
                  (V1 != Name | is.na(Name)) ~ V1,
                (V1 == Name |
                   is.na(Name)) &
                  V2 != Age ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2],
                stringr::str_detect(V2, Time_Score_Specials_String) == TRUE ~ V1,
                TRUE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2]
              ),
              Team = dplyr::case_when(
                stringr::str_detect(Team, Age) == TRUE &
                  is.na(Team) == FALSE ~ stringr::str_remove(Team, Age),
                TRUE ~ Team
              ),
              Team = trimws(Team)
            ) %>%
            dplyr::mutate(
              Prelims_Time = dplyr::case_when(
                stringr::str_detect(V2, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V3, Time_Score_Specials_String) == TRUE ~ V2,
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V3, Time_Score_Specials_String) == FALSE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ "",
                TRUE ~ V2
              )
            ) %>%
            dplyr::mutate(
              Finals_Time = dplyr::case_when(
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V4,
                TRUE ~ V3
              )
            ) %>%
            dplyr::mutate(
              Points = dplyr::case_when(Finals_Time == V3 ~ V4,
                                        Finals_Time == V4 ~ "",
                                        TRUE ~ ""),
              Points = dplyr::case_when(stringr::str_detect(Points, "^\\d{1,}$") == FALSE ~ "",
                                        TRUE ~ Points)
            ) %>%
            na_if_character("") %>%
            dplyr::select(
              Name,
              Place,
              Age,
              Team,
              Prelims_Time,
              Finals_Time,
              Points,
              Row_Numb = V5
            ) %>%
            na_if_character("") %>%
            na_if_character("''") %>%
            dplyr::mutate(
              DQ = case_when(str_detect(Finals_Time, "^DQ$") ~ 1,
                             TRUE ~ 0)
            ) %>%
            ### dealing with exhibition times 8/18/2020
            dplyr::mutate(
              Exhibition = dplyr::case_when(
                stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
                TRUE ~ 0
              ),
              ###
              Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String),
              Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String), # added 11/6
            ) %>%
            na_if_character("") %>%
            dplyr::mutate(
              Finals_Time = replace(Finals_Time, dplyr::n_distinct(Finals_Time) <= 2, ""),
              Prelims_Time = replace(Prelims_Time, dplyr::n_distinct(Prelims_Time) <= 2, "")
            ) %>%
            na_if_character("") %>%
            na_if_character("''") %>%
            # dplyr::mutate(
            #   Finals_Time = dplyr::case_when(
            #     sum(stringr::str_detect(Finals_Time, Time_Score_String)) >= 1 ~ stringr::str_remove(Finals_Time, "NT"),
            #     sum(stringr::str_detect(Finals_Time, Time_Score_String)) < 1 ~ Finals_Time,
            #     TRUE ~ Finals_Time
            #   )
            # ) %>%
            na_if_character("") %>% # good 11/9
            dplyr::mutate(
              Team = dplyr::case_when(
                is.na(Team) == TRUE &
                  stringr::str_detect(Finals_Time, Time_Score_String) == FALSE ~ Finals_Time,
                is.na(Team) == FALSE |
                  stringr::str_detect(Finals_Time, Time_Score_String) == TRUE ~ Team
              ),
              Finals_Time = replace(
                Finals_Time,
                stringr::str_detect(Finals_Time, "^NT$") == TRUE,
                NA
              ), # okay 11/9
              Finals_Time = replace(Finals_Time, Team == Finals_Time, NA),
              Finals_Time = replace(Finals_Time, stringr::str_length(Finals_Time) < 3, NA), # okay 11/9
              Finals_Time = dplyr::case_when(
                dplyr::between(Place, 1, 9999) &
                  is.na(Finals_Time) == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
                dplyr::between(Place, 1, 9999) &
                  is.na(Finals_Time) != TRUE ~ Finals_Time,
                TRUE ~ Finals_Time
              ),
              #   ### dealing with exhibition times 8/18/2020
              #   Exhibition = dplyr::case_when(
              #     stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
              #     TRUE ~ 0
              #   ),
              #   ###
              #   Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String),
              #   Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String), # added 11/6
            ) %>%
            # na_if_character("") %>%
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
            na_if_character("") %>%
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
            na_if_character("") %>%
            dplyr::mutate(
              Team = replace(Team, stringr::str_detect(Team, "\\.\\d") == TRUE, NA),
              Team = dplyr::case_when(
                is.na(Team) == TRUE &
                  Age %!in% c(
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
                  stringr::str_detect(Age, "\\'[[:alpha:]]\\'") == FALSE &
                  stringr::str_detect(Age, "\\.\\d") == FALSE ~ Age,
                is.na(Team) == FALSE |
                  Age %in% c(
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
                  stringr::str_detect(Age, "\\'[[:alpha:]]\\'") == TRUE |
                  stringr::str_detect(Age, "\\.\\d") == FALSE ~ Team,
                is.na(Team) == FALSE ~ Team
              ),
              Team = dplyr::case_when(
                stringr::str_length(Team) < 1 | is.na(Team) == TRUE ~ Name,
                stringr::str_length(Team) >= 1 ~ Team
              ),
              Team = dplyr::case_when(
                is.na(Age) == FALSE &
                  stringr::str_detect(Age, "\\'[[:alpha:]]\\'|\\.?\\d") == FALSE &
                  Age %in% c("SR", "JR", "SO", "FR") == FALSE &
                  Age != Team ~ paste(Age, Team, sep = " "),
                TRUE ~ Team
              ),
              Team = dplyr::case_when(
                stringr::str_detect(Team, "\\'[[:upper:]]\\'|^\\-?[:upper:]$") == TRUE &
                  is.na(Name) == FALSE ~ Name,
                TRUE ~ Team
              ),
              Team = dplyr::case_when(
                stringr::str_detect(Team, Time_Score_String) == TRUE ~ stringr::str_extract(Team, "[:alpha:]*"),
                TRUE ~ Team
              ),
              Age = replace(Age, Age == Team, NA),
              Age = replace(Age, stringr::str_detect(Age, Time_Score_String) == TRUE, NA),
              Name = stringr::str_remove(Name, "^-"),
              Name = replace(Name, Name == Team, NA),
              Age = replace(Age, is.na(Name) == TRUE &
                              is.na(Team) == FALSE, NA),

              Age = replace(Age, Age == Team, NA),
              Age = replace(Age, (
                stringr::str_length(Age) > 2 & is.na(Age) == FALSE
              ), NA)

            )
          # dplyr::mutate(DQ = 0)
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
            na_if_character("") %>%
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
              Age = dplyr::case_when(
                stringr::str_length(stringr::str_split_fixed(V2, " ", n = 2)[, 1]) <= 2 &
                  stringr::str_detect(V2, "SR|JR|SO|FR|12|11|10|9|8|7|^\\d\\d ") ~ stringr::str_split_fixed(V2, " ", n = 2)[, 1],
                TRUE ~ ""
              ),
              Age = trimws(Age)
            ) %>%
            na_if_character("") %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                V2 == Age &
                  any(stringr::str_detect(V3, "^SEC \\d+$")) == FALSE ~ V3,
                V2 == Age &
                  any(stringr::str_detect(V3, "^SEC \\d+$")) == TRUE ~ V1,
                (V2 != Age |
                   is.na(Age)) &
                  stringr::str_detect(V3, Time_Score_Specials_String) & # added DQ to string 11/9
                  stringr::str_detect(V2, "^'[:upper:]'$|^[:upper:]$|^\\'\\'$") == FALSE &
                  stringr::str_detect(V2, Time_Score_Specials_String) == FALSE ~ V2,
                (V2 != Age |
                   is.na(Age)) &
                  stringr::str_detect(V3, Time_Score_Specials_String) & # added DQ to string 11/9
                  stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|'[:upper:]'|^\\'\\'$") == TRUE &
                  (V1 != Name | is.na(Name)) ~ V1,
                (V1 == Name |
                   is.na(Name)) &
                  V2 != Age ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2],
                stringr::str_detect(V2, Time_Score_Specials_String) == TRUE ~ V1, # added DQ to string 11/9
                TRUE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2]
              ),
              Team = dplyr::case_when(
                stringr::str_detect(Team, Age) == TRUE &
                  is.na(Team) == FALSE ~ stringr::str_remove(Team, Age),
                TRUE ~ Team
              ),
              Team = trimws(Team)
            ) %>%
            dplyr::mutate(
              Prelims_Time = dplyr::case_when(
                stringr::str_detect(V2, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V3, Time_Score_Specials_String) == TRUE ~ V2,
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V3, Time_Score_Specials_String) == FALSE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ "",
                TRUE ~ ""
              )
            ) %>%
            na_if_character("") %>%
            dplyr::mutate(
              Finals_Time = dplyr::case_when(
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V4,
                stringr::str_detect(V3, Time_Score_Specials_String) == FALSE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V4,
                TRUE ~ V3
              )
            ) %>%
            dplyr::mutate(
              Points = dplyr::case_when(Finals_Time == V3 ~ V4,
                                        Finals_Time == V4 ~ "",
                                        TRUE ~ ""),
              Points = dplyr::case_when(stringr::str_detect(Points, "^\\d{1,}$") == FALSE ~ "",
                                        TRUE ~ Points)
            ) %>%
            na_if_character("") %>%
            dplyr::select(
              Name,
              Place,
              Age,
              Team,
              Prelims_Time,
              Finals_Time,
              Row_Numb = V4
            ) %>%
            na_if_character("") %>%
            na_if_character("''") %>%
            dplyr::mutate(
              DQ = case_when(str_detect(Finals_Time, "^DQ$") ~ 1,
                             TRUE ~ 0),
              Finals_Time = replace(Finals_Time, stringr::str_length(Finals_Time) < 3, NA),
              Finals_Time = dplyr::case_when(
                Place >= 1 &
                  is.na(Finals_Time) == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
                Place >= 1 &
                  is.na(Finals_Time) != TRUE ~ Finals_Time
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                is.na(Team) == TRUE &
                  stringr::str_detect(Age, "[[:alpha:]]") == TRUE &
                  stringr::str_length(Age) > 1 ~ Age,
                is.na(Team) == FALSE &
                  stringr::str_detect(Age, "[[:alpha:]]") == FALSE ~ Team,
                is.na(Team) == FALSE ~ Team
              ),
              Team = dplyr::case_when(
                stringr::str_length(Team) < 1 | is.na(Team) == TRUE ~ Name,
                stringr::str_length(Team) >= 1 ~ Team
              )
            ) %>%
            dplyr::mutate(
              Age = replace(Age, Age == Team, NA),
              Name = stringr::str_remove(Name, "^-"),
              Name = replace(Name, Name == Team, NA)
            ) %>%
            dplyr::mutate(
              Prelims_Time = dplyr::case_when(
                is.na(Name) == TRUE &
                  stringr::str_detect(Age, Time_Score_String) == TRUE ~ Age,
                TRUE ~ Prelims_Time
              )
            ) %>%
            dplyr::mutate(
              Age = replace(Age, is.na(Name) == TRUE &
                              is.na(Team) == FALSE, NA),
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
            na_if_character("") %>%
            dplyr::mutate(
              Prelims_Time = dplyr::case_when(
                all(Prelims_Time == Finals_Time, na.rm = TRUE) ~ as.character(NA),
                all(Prelims_Time == Finals_Time, na.rm = TRUE) == FALSE ~ Prelims_Time
              )
            ) %>%
            dplyr::mutate(
              Age = replace(Age, Age == Team, NA),
              Team = dplyr::case_when(
                is.na(Age) == FALSE &
                  stringr::str_detect(Age, "\\'[[:alpha:]]\\'") == FALSE &
                  stringr::str_detect(Age, Time_Score_String) == FALSE &
                  stringr::str_detect(Age, "\\d") == FALSE &
                  Age %in% c("SR", "JR", "SO", "FR") == FALSE &
                  Age != Team ~ paste(Age, Team, sep = " "),
                TRUE ~ Team
              ),
              Team = dplyr::case_when(
                stringr::str_detect(Team, "\\'[[:upper:]]\\'") == TRUE &
                  is.na(Name) == FALSE ~ Name,
                stringr::str_detect(Team, "^[:upper:]$") == TRUE &
                  is.na(Name) == FALSE ~ Name,
                stringr::str_detect(Team, "^[:upper:]$") == FALSE |
                  is.na(Name) == TRUE ~ Team
              ),
              Age = replace(
                Age,
                (
                  stringr::str_detect(Age, "\\d") == FALSE &
                    Age %in% c("SR", "JR", "SO", "FR") == FALSE
                ),
                NA
              ),
              Age = dplyr::case_when(
                stringr::str_detect(Name, "\\.d{1,3}") == TRUE ~ stringr::str_extract(Name, "\\d{1,3}"),
                stringr::str_detect(Name, "\\.d{1,3}") == FALSE ~ Age
              ),
              Name = stringr::str_remove(Name, "\\d{1,3}"),
              Prelims_Time = dplyr::case_when(
                is.na(Prelims_Time) == TRUE &
                  stringr::str_detect(Team, "\\.") == TRUE ~ stringr::str_extract(
                    Team,
                    "[:digit:]*[:punct:]?[:digit:]*[:punct:]?[:digit:]*"
                  ),
                is.na(Prelims_Time) == FALSE |
                  stringr::str_detect(Team, "\\.") == FALSE ~ Prelims_Time
              ),
              Team = stringr::str_remove(Team, "-[:alpha:]*\\d.*$"),
              Team = stringr::str_remove(
                Team,
                "^[:digit:]{1,2}\\s")
            ) %>%
            na_if_character("") %>%
            dplyr::mutate(
              Name = stringr::str_remove(Name, "^-"),
              Name = replace(Name, Name == Team, NA)
            )
          # dplyr::mutate(DQ = 0)
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
              Age = as.character(NA),
              Team = as.character(NA),
              Finals_Time = V2,
              Prelims_Time = as.character(NA),
            ) %>%
            dplyr::select(
              Name,
              Place,
              Age,
              Team,
              Prelims_Time,
              Finals_Time,
              Row_Numb = V3
            ) %>%
            na_if_character("") %>%
            na_if_character("''") %>%
            dplyr::mutate(
              Finals_Time = replace(Finals_Time, stringr::str_length(Finals_Time) < 3, NA),
              Finals_Time = dplyr::case_when(
                Place >= 1 &
                  is.na(Finals_Time) == TRUE ~ dplyr::coalesce(Finals_Time, Prelims_Time),
                Place >= 1 &
                  is.na(Finals_Time) != TRUE ~ Finals_Time
              ),
              Team = dplyr::case_when(
                stringr::str_length(Team) < 1 | is.na(Team) == TRUE ~ Name,
                stringr::str_length(Team) >= 1 ~ Team
              ),
              Name = stringr::str_remove(Name, "^-"),
              Name = replace(Name, Name == Team, NA),
              Prelims_Time = dplyr::case_when(
                is.na(Name) == TRUE ~ Age,
                is.na(Name) == FALSE ~ Prelims_Time
              ),
              Age = replace(Age, is.na(Name) == TRUE &
                              is.na(Team) == FALSE, NA),
              ### dealing with exhibition times 8/18/2020
              Exhibition = dplyr::case_when(
                stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
                TRUE ~ 0
              ),
              ###
              Finals_Time = stringr::str_extract(Finals_Time, Time_Score_String),
              Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_String)
            ) %>%
            na_if_character("") %>%
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
                # stringr::str_detect(V2, Time_Score_Special_String) == TRUE ~ "",
                TRUE ~ stringr::str_split_fixed(V1, " ", n = 2)[, 2]
              )
            ) %>%
            na_if_character("") %>%
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
              Age = dplyr::case_when(
                stringr::str_length(stringr::str_split_fixed(V2, " ", n = 2)[, 1]) <= 3 &
                  stringr::str_detect(V2, "SR|JR|SO|FR|12|11|10|9|8|7|^\\d{1,3} ") ~ stringr::str_split_fixed(V2, " ", n = 2)[, 1],
                TRUE ~ ""
              ),
              Age = trimws(Age)
            ) %>%
            na_if_character("") %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                V2 == Age &
                  any(stringr::str_detect(V3, "^SEC \\d+$")) == FALSE ~ V3,
                V2 == Age &
                  any(stringr::str_detect(V3, "^SEC \\d+$")) == TRUE ~ V1,
                (V2 != Age |
                   is.na(Age)) &
                  stringr::str_detect(V3, Time_Score_Specials_String) &
                  stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|^\\'\\'$") == FALSE &
                  stringr::str_detect(V2, Time_Score_String) == FALSE ~ V2,
                (is.na(V1) == TRUE & stringr::str_detect(V3, Time_Score_Specials_String) == TRUE & stringr::str_detect(V2, "\\d") == FALSE) ~ V2,
                (V2 != Age |
                   is.na(Age)) &
                  stringr::str_detect(V3, Time_Score_Specials_String) &
                  stringr::str_detect(V2, "'[:upper:]'|^[:upper:]$|'[:upper:]'|^\\'\\'$") == TRUE &
                  (V1 != Name | is.na(Name)) ~ V1,
                (V1 == Name |
                   is.na(Name)) &
                  V2 != Age ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2],
                stringr::str_detect(V2, Time_Score_Specials_String) == TRUE ~ V1,
                TRUE ~ stringr::str_split_fixed(V2, " ", n = 2)[, 2]
              ),
              Team = dplyr::case_when(
                stringr::str_detect(Team, Age) == TRUE &
                  is.na(Team) == FALSE ~ stringr::str_remove(Team, Age),
                TRUE ~ Team
              ),
              Team = trimws(Team)
            ) %>%
            na_if_character("") %>%
            dplyr::select(Name,
                          Place,
                          Age,
                          Team,
                          Row_Numb = V4) %>%
            na_if_character("") %>%
            na_if_character("''") %>%
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
              Team = stringr::str_split_fixed(V1, " ", n = 2)[, 2],
              Team = trimws(Team)
            ) %>%
            na_if_character("") %>%
            dplyr::select(Place,
                          Team,
                          Row_Numb = V3) %>%
            na_if_character("") %>%
            na_if_character("''") %>%
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
        # data <- dplyr::full_join(df_7, df_6) %>%
        #   dplyr::full_join(df_5) %>%
        #   dplyr::full_join(df_4) %>%
        #   dplyr::full_join(df_3) %>%
        #   dplyr::left_join(df_DQ_4) %>%
        #   dplyr::left_join(df_DQ_3) %>%
        data <- dplyr::bind_rows(df_7, df_6) %>%
          dplyr::bind_rows(df_5) %>%
          dplyr::bind_rows(df_4) %>%
          dplyr::bind_rows(df_3) %>%
          dplyr::left_join(df_DQ_4) %>%
          dplyr::left_join(df_DQ_3) %>%
          dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
          dplyr::arrange(Row_Numb) %>%
          ### moved up from below for DQ work 8/20
          dplyr::mutate(DQ = dplyr::case_when(Place == 10000 & Exhibition == 0 ~ 1, # added exhibition condition 8/27
                                              TRUE ~ DQ)) %>%
          na_if_numeric(10000) %>%
          dplyr::mutate(dplyr::across(c(Name, Team), ~ stringr::str_replace_all(., "10000", "--"))) %>% # remove any "10000"s added in erroniuously
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

      if("Points" %in% names(data) == FALSE)
      {data$Points <- NA}

      #### add in events based on row number ranges ####
      data  <-
        transform(data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)])

      #### cleaning up final results ####

      suppressWarnings(
        data <- data %>%
          dplyr::mutate(
            Name = stringr::str_replace(Name, "_", "\\*"),
            Prelims_Time = replace(
              Prelims_Time,
              stringr::str_detect(Prelims_Time, "\\.") == FALSE,
              NA
            ),
            Prelims_Time = replace(
              Prelims_Time,
              (stringr::str_detect(Prelims_Time, "\\.") == TRUE &
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
          dplyr::filter(
            DQ == 0 &
              stringr::str_detect(Finals_Time, Time_Score_String) == TRUE |
              DQ == 0 & stringr::str_detect(Prelims_Time, Time_Score_String) == TRUE | DQ == 1
          ) %>%
          dplyr::mutate(Exhibition = dplyr::case_when(is.na(Exhibition) == TRUE ~ 0,
                                                      TRUE ~ Exhibition)) %>%
          dplyr::mutate(Finals_Time = dplyr::case_when(DQ == 1 ~ "NA",
                                                       TRUE ~ Finals_Time)) %>%
          na_if_character("NA")

      )


      #### adding relay swimmers in ####
      if (relay_swimmers == TRUE) {
        relay_swimmers_df <- collect_relay_swimmers_old(as_lines_list_2, typo_2 = typo, replacement_2 = replacement)
        data <- data %>%
          dplyr::left_join(relay_swimmers_df, by = 'Row_Numb')
      }

      #### adding splits back in ####
      if (splits == TRUE) {
        splits_df <- splits_parse(as_lines_list_2, split_len = split_length)

        if (any(str_detect(data$Event, "Relay")) == TRUE) {
          relay_row <- data %>%
            dplyr::filter(stringr::str_detect(Event, "Relay")) %>%
            head(1) %>%
            dplyr::select(Row_Numb) %>%
            dplyr::pull()

          relay_offset <-
            ifelse(stringr::str_detect(as_lines_list_2[relay_row + 2], "\\d\\d\\.\\d\\d") == TRUE,
                   1,
                   2)

          data <- data %>%
            dplyr::mutate(
              Row_Numb = dplyr::case_when(
                stringr::str_detect(Event, "Relay") == TRUE ~ Row_Numb + relay_offset,
                TRUE ~ Row_Numb
              )
            ) %>%
            dplyr::left_join(splits_df, by = 'Row_Numb')

        } else {
          data <- data %>%
            dplyr::left_join(splits_df, by = 'Row_Numb')
        }

        ### remove empty columns (all values are NA) ###
        data <- Filter(function(x)
          !all(is.na(x)), data)
      }

      #### if there is a Place column it should be first ####
      if("Place" %in% names(data)){
        data <- data %>%
          dplyr::select(Place, dplyr::everything())
      }

      data$Row_Numb <- NULL
      message("Beginning with version 0.6.0 the Grade and School output columns have been renamed Age and Team respectively.  Please adjust your work flows as needed.")


      return(data)

    } else if (stringr::str_detect(file[1], "^A107") == TRUE) {
      # file <- add_row_numbers(text = file)
      data <- hy3_parse(file = file)
      return(data)
    }


  }
