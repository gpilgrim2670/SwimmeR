#' Formats swimming and diving data read with \code{read_results} into a dataframe
#'
#' Takes the output of \code{read_results} and cleans it, yielding a dataframe of swimming (and diving) results
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr lag
#' @importFrom dplyr case_when
#' @importFrom dplyr na_if
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr pull
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_length
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @param file output from \code{read_results}
#' @param avoid a list of strings.  Rows in \code{file} containing these strings will not be included. For example "Pool:", often used to label pool records, could be passed to \code{avoid}.  The default is \code{avoid_default}, which contains many strings similar to "Pool:", such as "STATE:" and "Qual:".  Users can supply their own lists to \code{avoid}.  \code{avoid} is handled before \code{typo} and \code{replacement}.
#' @param typo a list of strings that are typos in the original results.  \code{swim_parse} is particularly sensitive to accidental double spaces, so "Central  High School", with two spaces between "Central" and "High" is a problem, which can be fixed.  Pass "Central  High School" to \code{typo}.  Unexpected commas as also an issue, for example "Texas, University of" should be fixed using \code{typo} and \code{replacement}
#' @param replacement a list of fixes for the strings in \code{typo}.  Here one could pass "Central High School" (one space between "Central" and "High") and "Texas" to \code{replacement} fix the issues described in \code{typo}
#' @param format_results should the results be formated for analysis (special strings like \code{"DQ"} replaced with \code{NA}, \code{Finals_Time} as definative column)?  Default is \code{TRUE}
#' @param splits either \code{TRUE} or the default, \code{FALSE} - should \code{swim_parse} attempt to include splits.
#' @param split_length either \code{25} or the default, \code{50}, the length of pool at which splits are recorded.  Not all results are internally consistent on this issue - some have races with splits by 50 and other races with splits by 25.
#' @param relay_swimmers either \code{TRUE} or the default, \code{FALSE} - should relay swimmers be reported.  Relay swimmers are reported in separate columns named \code{Relay_Swimmer_1} etc.
#' @return returns a data frame with columns \code{Name}, \code{Place}, \code{Age}, \code{Team}, \code{Prelims_Time}, \code{Finals_Time}, \code{Points}, \code{Event} & \code{DQ}.  Note all swims will have a \code{Finals_Time}, even if that time was actually swam in the prelims (i.e. a swimmer did not qualify for finals).  This is so that final results for an event can be generated from just one column.
#'
#' @examples \dontrun{
#' swim_parse(read_results("http://www.nyhsswim.com/Results/Boys/2008/NYS/Single.htm", node = "pre"),
#'  typo = c("-1NORTH ROCKL"), replacement = c("1-NORTH ROCKL"),
#'  splits = TRUE,
#'  relay_swimmers = TRUE)
#'  }
#' \dontrun{
#' swim_parse(read_results("inst/extdata/Texas-Florida-Indiana.pdf"),
#'  typo =  c("Indiana  University", ", University of"), replacement = c("Indiana University", ""),
#'  splits = TRUE,
#'  relay_swimmers = TRUE)
#'  }
#' @seealso \code{swim_parse} must be run on the output of \code{\link{read_results}}
#'

Swim_Parse <-
  function(file,
           avoid = avoid_default,
           typo = typo_default,
           replacement = replacement_default,
           format_results = TRUE,
           splits = FALSE,
           split_length = 50,
           relay_swimmers = FALSE) {

    #### default typo and replacement strings ####
    typo_default <- c("typo")

    replacement_default <- c("typo")

    if(length(typo) != length(replacement)) {
      stop("typo and replacement must have the same number of elements (be the same length)")
    }

    if(is.logical(format_results) == FALSE) {
      stop("format_results must be logical, either TRUE or FALSE")
    }

    if(is.logical(splits) == FALSE) {
      stop("plits must be logical, either TRUE or FALSE")
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
        # "[:upper:]\\:",
        "[A-S]\\:", # to allow EVENT:
        "[U-Z]\\:", # to allow EVENT:
        "[A-MO-Z]T\\:", # to allow EVENT:
        "[a-q]\\:", # want to make sure to include r: for reaciton times in splits lines
        "[s-z]\\:", # want to make sure to include r: for reaciton times in splits lines
        "[:alpha:]r\\:",
        "\\.\\:",
        "\\d\\:\\s",
        "\\'\\:"
        # "Record",
        # "RECORD",
        # "^\\s*NYSPHSAA",
        # "^\\s*NYSPHAA",
        # "^\\s*Finals",
        # "^\\s*Prelims",
        # "^\\s*Hosted",
        # "^\\s*Meet",
        # "^\\s*MEET",
        # "^\\s*Points",
        # "^\\s*League",
        # "^\\s*LEAGUE",
        # "^\\s*School\\s*Prelims\\s*Finals",
        # "^\\s*r\\:",
        # "NCAA",
      )

    #### define avoid_minimal ####
    avoid_minimal <- c("^\\s{1,}r\\:")

    #### testing ####
    # file <- read_results("https://cdn.swimswam.com/wp-content/uploads/2018/08/2004-Division-I-NCAA-Championships-Men-results1.pdf")
    # avoid <- avoid_default
    # typo <- typo_default
    # replacement <- replacement_default

    #### combine avoid and avoid_default
    avoid <- unique(c(avoid, avoid_default))

  if (stringr::str_detect(file[1], "^A107") == TRUE) { # for .hy3 files
    # file <- add_row_numbers(text = file)
    data <- parse_hy3(file = file)
    return(data)

  } else if (any(stringr::str_detect(file[1:5], "S\\.A\\.M\\.M\\.S\\.")) == TRUE) { # for S.A.M.M.S files
    data <- parse_samms(file_samms = file,
                        avoid_samms = avoid,
                        typo_samms = typo,
                        replacement_samms = replacement,
                        format_samms = format_results)
    return(data)

  } else { # hytek files

    #### assign row numbers ####
    as_lines_list_2 <- file %>%
      .[purrr::map_lgl(., stringr::str_detect, "Early take-off", negate = TRUE)] %>% # removes DQ rational used in some relay DQs that messes up line spacing between relay and swimmers/splits - must happen before adding in row numbers
      add_row_numbers() %>%
      .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid)))] %>%
      stringr::str_replace_all(stats::setNames(replacement, typo)) %>% # replace typos with replacements
      stringr::str_replace_all("DISQUAL", " DQ ") %>%
      stringr::str_replace_all("EVENT\\:", "Event")

    #### parsing html and pdf files ####
    # if (stringr::str_detect(file[1], "^A107") == FALSE) {

      #### Pulls out event labels from text ####
      events <- event_parse(as_lines_list_2)

      #### set up strings ####
      Name_String <-
        "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
      Time_Score_String <- "\\d{0,2}\\:?\\d{1,3}\\.\\d{2}"
      Time_Score_Specials_String <- paste0(Time_Score_String, "|^NT$|^NP$|^DQ$|^NS$|^SCR$")
      Age_String <- "^SR$|^JR$|^SO$|^FR$|^[:digit:]{1,3}$"
      # Colon_String <- "\\:\\d\\d"

      #### clean input data ####

      data_cleaned <- as_lines_list_2 %>%
        stringr::str_remove("^\n\\s{0,}") %>%
        # .[purrr::map(., length) > 0] %>%
        .[purrr::map(., stringr::str_length) > 50] %>% # slight speed boost from cutting down length of file
        .[purrr::map_lgl(., stringr::str_detect, paste0(Time_Score_String,"|DQ|SCR"))] %>% # must have \\.\\d\\d because all swimming and diving times do
        .[purrr::map_lgl(., stringr::str_detect, "[:alpha:]")] %>%
        .[purrr::map_lgl(., stringr::str_detect, "r\\:\\+?\\-?\\s?\\d", negate = TRUE)] %>% # remove reaction times
        # .[purrr::map_lgl(., stringr::str_detect, "[:alpha:]\\:", negate = TRUE)] %>% # remove records
        .[purrr::map_dbl(., stringr::str_count, "\\)") < 2] %>%  # remove inline splits from older style hy-tek results circa 2005
        # .[purrr::map_lgl(., stringr::str_detect, " \\:\\d", negate = TRUE)] %>% # remove other inline splits from older style hytek results circa 2005
        stringr::str_replace_all("\\s?[&%]\\s?", " ") %>% # added 8/21 for removing "&" and "%" as record designator
        # removed J etc. from next to swim, but does not remove X or x (for exhibition tracking)
        stringr::str_replace_all("[A-WYZa-wyz]+(\\d{1,2}\\:\\d{2}\\.\\d{2})", "\\1") %>%
        stringr::str_replace_all("(\\d{1,2}\\:\\d{2}\\.\\d{2})[A-WYZa-wyz]+", "\\1") %>%
        stringr::str_replace_all("[A-WYZa-wyz]+(\\d{2,3}\\.\\d{2})", "\\1") %>%
        stringr::str_replace_all("(\\d{2,3}\\.\\d{2})[A-WYZa-wyz]+", "\\1") %>%
        stringr::str_replace_all(" [:punct:]+(\\d{1,2}\\:\\d{2}\\.\\d{2})", " \\1") %>%
        stringr::str_replace_all("(\\d{1,2}\\:\\d{2}\\.\\d{2})[:punct:]+", " \\1") %>%
        stringr::str_replace_all(" [:punct:]+(\\d{2,3}\\.\\d{2})", " \\1") %>%
        stringr::str_replace_all("(\\d{2,3}\\.\\d{2})[:punct:]+", "\\1 ") %>%
        stringr::str_remove_all("\\s{2}J\\s{2}") %>%
        # remove 'A', 'B' etc. relay designators - should this go in typo instead?
        stringr::str_replace_all(" \\'[A-Z]\\' ", "  ") %>%
        stringr::str_replace_all("  [A-WYZ]  ", "  ") %>%
        stringr::str_replace_all("\\'\\'", "  ") %>%
        # remove meet ID from old result
        stringr::str_replace_all(" \\d{4} ", "   ") %>%
        stringr::str_replace_all(" \\d{1,3} (?=\\s*\\d{1,2}\\.?\\d?\\d?\\s+\\d{1,5}$)", "   ") %>% # remove column of powerpoint values in NYS Boys 2007.  It goes time, powerpoint, actual points, row numb
        stringr::str_replace_all("(?<=\\.\\d)(\\d)\\s+X(?=\\s+\\d{1,5}$)", "\\1X") %>% # brings Xs for exhibition that are spaced out in closer
        # remove q from next to time 10/21/2020
        stringr::str_remove_all(" q ") %>% # removes " q " sometimes used to designate a qualifying time
        stringr::str_replace_all("-{2,5}", "10000") %>% #8/26
        stringr::str_replace_all("(\\.\\d{2})\\d+", "\\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
        stringr::str_replace_all("\\d{1,2} (\\d{1,})$", "  \\1 ") %>% # added 8/21 for illinois to deal with points column merging with final times column
        # stringr::str_replace_all("\\*(?=[:digit:])", "_") %>% # for * prior to place for foreign athletes and sometimes ties
        stringr::str_replace_all("\\*(?=[:alpha:])", "_") %>% # for * prior to name for foreign athletes
        stringr::str_replace_all("\\*", "  ") %>%
        trimws()


      #### insert double spaces where needed ####
      data_cleaned <- data_cleaned %>%
        stringr::str_replace_all("(?<=\\d)\\s+[:upper:]?\\s+(?=\\d)", "  ") %>% # letters like P or M to denote pool or meet record
        stringr::str_replace_all("(?<=\\d) (?=[:alpha:])", "  ") %>% # mostly to split place and name
        stringr::str_replace_all("(?<=\\d) (?=_)", "  ") %>% # mostly to split place and name, if name is precended by "_" as a stand-in for "*"
        stringr::str_replace_all("(?<=\\d) (?=\\d)", "  ") %>% # mostly to split place team names that start with a number, like in NYS results (5-Fairport etc.)
        stringr::str_replace_all("(?<=[:alpha:]),(?=[:alpha:])", ", ") %>% # split names that don't have a space between last,first
        stringr::str_replace_all("(?<=[:alpha:])\\. (?=[:digit:])", "\\.  ") %>% # split abreviated team names like Southern Cal. and times
        stringr::str_replace_all("(?<=\\d) (?=_)", "  ") %>% # spacing between place and atheltes with */_ leading name
        stringr::str_replace_all("(?<=\\)) (?=[:alpha:])", "  ") %>% # spacing between place) and names
        stringr::str_replace_all(" FR ", "       FR      ") %>% # split age and team
        stringr::str_replace_all(" SO ", "       SO      ") %>% # split age and team
        stringr::str_replace_all(" JR ", "       JR      ") %>% # split age and team
        stringr::str_replace_all(" SR ", "       SR      ") %>% # split age and team
        stringr::str_replace_all("(?<=[:alpha:])\\s{2,3}(?=[:alpha:])", " ") %>% # testing 12/21/2020 would help with typos
        stringr::str_replace_all("(?<=[:alpha:]) (?=\\d)", "  ") %>% # split name and age
        stringr::str_replace_all("(?<=\\,) (?=\\d)", "  ") %>% # split name and age if name is so long that it ends with a ","
        stringr::str_replace_all("(?<=\\d) (?=\\d{1,}$)", "  ") %>%  # split off row_numb
        stringr::str_replace_all("(?<=\\.\\d\\d\\s{1,10}\\d?\\d?\\:?\\d?\\d\\.\\d\\d)\\s{1,10}[:alpha:]{1,5}\\d?\\s{1,10}(?=\\d{1,})", "  ") %>%  # removes "AAC" or "AAA" or "NYS" or "SEC1" etc. from after finals time
        # stringr::str_replace_all("NYS|AAA|AAC|SEC\\d{1,2}", "  ") %>%
        # stringr::str_replace_all("SEC\\d{1,2}", "  ") %>%  # for old NYS results
        stringr::str_replace_all("\\d{3}\\.\\d{2}\\s+(\\d{3}\\.\\d{2})\\s+\\d{3}\\.\\d{2}\\s+(\\d{3}\\.\\d{2})", "\\1  \\2") # for old NCAA results with diving scores and DDs

      #### splits data into variables by splitting at multiple (>= 2) spaces ####
      data_cleaned <-
        unlist(purrr::map(data_cleaned, stringr::str_split, "\\s{2,}"),
               recursive = FALSE)

      # unique(map(data_cleaned, length))

      #### breaks data into subsets based on how many variables it has ####
      data_length_3 <- data_cleaned[purrr::map(data_cleaned, length) == 3]
      data_length_4 <- data_cleaned[purrr::map(data_cleaned, length) == 4]
      data_length_5 <- data_cleaned[purrr::map(data_cleaned, length) == 5]
      data_length_6 <- data_cleaned[purrr::map(data_cleaned, length) == 6]
      data_length_7 <- data_cleaned[purrr::map(data_cleaned, length) == 7]
      data_length_8 <- data_cleaned[purrr::map(data_cleaned, length) == 8]
      # data_length_9 <- data_cleaned[purrr::map(data_cleaned, length) == 9]

      # treatment of DQs new 8/19
      suppressWarnings(DQ <-
                         data_cleaned[stringr::str_detect(data_cleaned, Time_Score_String, negate = TRUE) == TRUE])

      DQ_length_3 <- DQ[purrr::map(DQ, length) == 3]
      DQ_length_4 <- DQ[purrr::map(DQ, length) == 4]
      # DQ_length_5 <- DQ[purrr::map(DQ, length) == 5]

      # #### nine variables
      # if (length(data_length_9) > 0) {
      #   suppressWarnings(
      #     df_9 <- data_length_9 %>%
      #       list_transform()
      #   )
      # } else {
      #   df_9 <- data.frame(
      #     Row_Numb = character(),
      #     stringsAsFactors = FALSE
      #   )
      # }

      #### eight variables
      if (length(data_length_8) > 0) {
        suppressWarnings(
          df_8 <- data_length_8 %>%
            list_transform() %>%
            filter(stringr::str_detect(V1, "\\.") == FALSE) %>% # occasionally olde results with DQs in the splits will end up here - this removes them
            dplyr::na_if("") %>%
            dplyr::select(
              Place = V1,
              Name = V2,
              Age = V3,
              Team = V4,
              Prelims_Time = V5,
              Finals_Time= V6,
              Points = V7,
              Row_Numb = V8
            )
        )
      } else {
        df_8 <- data.frame(
          Row_Numb = character(),
          stringsAsFactors = FALSE
        )
      }


      #### seven variables ####
      if (length(data_length_7) > 0) {
        suppressWarnings(
          df_7 <- data_length_7 %>%
            list_transform() %>%
            dplyr::mutate(              Place = dplyr::case_when(stringr::str_detect(V1, "\\d") == TRUE ~ V1,
                                                                 TRUE ~ "10000"),
              Age = dplyr::case_when(stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                                                 TRUE ~ "NA"),
                          Team = dplyr::case_when(stringr::str_detect(V3, Age_String) == TRUE ~ V4,
                                                  stringr::str_detect(V3, Age_String) == FALSE & stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                                                  TRUE ~ "NA"),
                          Prelims_Time = dplyr::case_when(
                            stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                              stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V4,
                            stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE ~ V5,
                            TRUE ~ "NA"
                          ),
                          Finals_Time = dplyr::case_when(
                            stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                              stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V5,
                            stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                              stringr::str_detect(V6, Time_Score_Specials_String) == FALSE ~ V5,
                            stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                              stringr::str_detect(V6, Time_Score_Specials_String) == TRUE ~ V6,
                            TRUE ~ "NA"
                          ),
                          Points = dplyr::case_when(
                            # stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                              stringr::str_detect(V5, Finals_Time) == TRUE &
                                stringr::str_detect(V6, "^5\\d\\.|^4\\d\\.|^3\\d\\.|^2\\d\\.") == FALSE &
                              stringr::str_detect(V6, "^\\d{1,2}\\.?\\d?\\d?") ~ V6,
                            TRUE ~ "NA"
                          )
            ) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place,
              Name = V2,
              Age,
              Team,
              Prelims_Time,
              Finals_Time,
              Points,
              Row_Numb = V7
            )
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
            dplyr::mutate(
              Place = dplyr::case_when(stringr::str_detect(V1, "\\d") == TRUE ~ V1,
                                       TRUE ~ "10000"),
              Name = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Time_Score_Specials_String) == FALSE ~ V2,
                TRUE ~ "NA"
              ),
              Age = dplyr::case_when(stringr::str_detect(V3, Age_String) == TRUE ~ V3,
                                     TRUE ~ "NA"),
              Team = dplyr::case_when(
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE ~ V2,
                stringr::str_detect(V3, Time_Score_Specials_String) == FALSE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                stringr::str_detect(V3, Age_String) == TRUE ~ V4,
                TRUE ~ "NA"
              ),
              Prelims_Time = dplyr::case_when(
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                # stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V4,
                TRUE ~ "NA"
              ),
              Finals_Time = dplyr::case_when(
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V4,
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V5, Time_Score_Specials_String) == FALSE ~ V4,
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V5,
                TRUE ~ "NA"
              ),
              Points = dplyr::case_when(
                # stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                stringr::str_detect(V4, Finals_Time) == TRUE &
                  stringr::str_detect(V5, "^5\\d\\.|^4\\d\\.|^3\\d\\.|^2\\d\\.") == FALSE &
                  stringr::str_detect(V5, "^\\d{1,2}\\.?\\d?\\d?") ~ V5,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(Place,
                          Name,
                          Age,
                          Team,
                          Prelims_Time,
                          Finals_Time,
                          Points,
                          Row_Numb = V6)
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
            dplyr::mutate(
              Place = dplyr::case_when(stringr::str_detect(V1, "\\d") == TRUE ~ V1,
                                       TRUE ~ "NA"),
              Name = dplyr::case_when(
                stringr::str_detect(V1, Name_String) == TRUE &
                  stringr::str_detect(V2, Time_Score_Specials_String) == FALSE ~ V1,
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Time_Score_Specials_String) == FALSE ~ V2,
                TRUE ~ "NA"
              ),
              Age = dplyr::case_when(stringr::str_detect(V2, Age_String) == TRUE ~ V2,
                          TRUE ~ "NA"),
              Team = dplyr::case_when(
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE ~ V2,
                stringr::str_detect(V3, Time_Score_Specials_String) == FALSE ~ V3,
                TRUE ~ "NA"
              ),
              Prelims_Time = dplyr::case_when(
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                TRUE ~ "NA"
              ),
              Finals_Time = dplyr::case_when(
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == FALSE ~ V3,
                # stringr::str_detect(V3, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V4,
                TRUE ~ "NA"
              )
            ) %>%

            dplyr::na_if("NA") %>%
            dplyr::select(
              Place,
              Name,
              Age,
              Team,
              Prelims_Time,
              Finals_Time,
              Row_Numb = V5
            )
        )
      } else {
        df_5 <- data.frame(Row_Numb = character(),
                           stringsAsFactors = FALSE)
      }

      #### four variables ####
      if (length(data_length_4) > 0) {
        suppressWarnings(
          df_4 <- data_length_4 %>%
            list_transform() %>%
            filter(stringr::str_detect(V1, "\\.") == FALSE) %>% # occasionally olde results with DQs in the splits will end up here - this removes them
            dplyr::mutate(
              Place = dplyr::case_when(stringr::str_detect(V1, "\\d") == TRUE ~ V1,
                                       TRUE ~ "10000"),
              Team = dplyr::case_when(
                stringr::str_detect(V1, "\\d") == FALSE ~ V1,
                stringr::str_detect(V1, "\\d") == TRUE &
                  stringr::str_detect(V2, "[:alpha:]{2,}") == TRUE ~ V2,
                TRUE ~ "NA"
              ),
              Prelims_Time = dplyr::case_when(
                stringr::str_detect(V2, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V3, Time_Score_Specials_String) == TRUE ~ V2,
                TRUE ~ "NA"
              ),
              Finals_Time = dplyr::case_when(
                stringr::str_detect(V3, Time_Score_Specials_String) == TRUE ~ V3,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::na_if("") %>%
            dplyr::select(Place,
                          Team,
                          Prelims_Time,
                          Finals_Time,
                          Row_Numb = V4)
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
            dplyr::mutate(Place = "10000") %>%
            dplyr::select(
              Place,
              Team = V1,
              Finals_Time = V2,
              Row_Numb = V3
            )
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
            dplyr::mutate(Place = "10000") %>%
            dplyr::select(
              Place = V1,
              Team = V2,
              Finals_Time = V3,
              Row_Numb = V4
            ) %>%
            dplyr::mutate(DQ = 1)
        )

      } else {
        df_DQ_4 <- data.frame(
          Row_Numb = character(),
          DQ = numeric(),
          stringsAsFactors = FALSE
        )
      }

      #### DQ 3 ####
      if (length(DQ_length_3) > 0) {
        suppressWarnings(
          df_DQ_3 <- DQ_length_3 %>%
            list_transform() %>%
            dplyr::mutate(Place = "10000") %>%
            dplyr::select(
              Place,
              Team = V1,
              Finals_Time = V2,
              Row_Numb = V3
            ) %>%
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
        data <- dplyr::bind_rows(df_8, df_7) %>%
          dplyr::bind_rows(df_6) %>%
          dplyr::bind_rows(df_5) %>%
          dplyr::bind_rows(df_4) %>%
          dplyr::bind_rows(df_3) %>%
          dplyr::left_join(df_DQ_4) %>%
          dplyr::left_join(df_DQ_3) %>%
          dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
          dplyr::arrange(Row_Numb)
      )


      suppressWarnings(
        data <- data %>%
          dplyr::mutate(
            Exhibition = dplyr::case_when(stringr::str_detect(Finals_Time, "x|X") == TRUE ~ 1,
                                          TRUE ~ 0),
            ###
            Finals_Time = stringr::str_extract(Finals_Time, Time_Score_Specials_String),
            Prelims_Time = stringr::str_extract(Prelims_Time, Time_Score_Specials_String)
          ) %>%
          ### moved up from below for DQ work 8/20
          dplyr::mutate(DQ = dplyr::case_when(Place == 10000 &
                                                Exhibition == 0 ~ 1, # added exhibition condition 8/27
                                              stringr::str_detect(Finals_Time, "DQ") == TRUE ~ 1,
                                              is.na(DQ) ~ 0,
                                              TRUE ~ DQ)) %>%
          na_if(10000) %>%
          dplyr::mutate(dplyr::across(
            c(Name, Team), ~ stringr::str_replace_all(., "10000", "--")
          )) %>% # remove any "10000"s added in erroniuously
          ####
          dplyr::mutate(
            Place = str_remove(Place, "\\)"),
            Place = str_remove(Place, "_"),
            Place = as.numeric(Place),
            Place = dplyr::case_when(
              is.na(dplyr::lag(Place)) == TRUE ~ Place,
              dplyr::lag(Place) == Place ~ Place + 0.1,
              dplyr::lag(Place) != Place ~ Place
            ),
            Place = as.character(Place),
            # Place = dplyr::case_when(Exhibition > 0 ~ "NA",
            #                          TRUE ~ Place),
            # Place = dplyr::na_if(Place, "NA"),
            Row_Numb = as.numeric(Row_Numb)
          )
          # ) %>%
          # dplyr::filter(Row_Numb >= Min_Row_Numb)
      )

      if("Points" %in% names(data) == FALSE)
      {data$Points <- NA}

      #### add in events based on row number ranges ####
      if(min(data$Row_Numb) < min(events$Event_Row_Min)){
        unknown_event <- data.frame(Event = "Unknown",
                                    Event_Row_Min = min(data$Row_Numb),
                                    Event_Row_Max = min(events$Event_Row_Min) - 1)
        events <- dplyr::bind_rows(unknown_event, events)
      }

      data  <-
        transform(data, Event = events$Event[findInterval(Row_Numb, events$Event_Row_Min)]) %>%
        dplyr::na_if("Unknown")

      #### cleaning up final results ####

      suppressWarnings(
        data <- data %>%
          dplyr::mutate(
            Name = stringr::str_replace_all(Name, "_", "\\*"),
            Place = round(as.numeric(Place)),
            Event = as.character(Event)
          ) %>%
          dplyr::mutate(
            Place = dplyr::case_when(is.na(Place) == TRUE &
                                       DQ == 0 ~ dplyr::lag(Place) + 1,
                                     TRUE ~ Place)
          ) %>%
          dplyr::mutate(Points = stringr::str_remove_all(Points, " "),
                        Points = as.numeric(Points))
      )

      #### cleaning ####
      if(format_results == TRUE){
        data <- format_results(data)
      }

      #### adding relay swimmers in ####
      if (relay_swimmers == TRUE) {
        # relay_swimmers_df <- collect_relay_swimmers(as_lines_list_2, typo_2 = typo, replacement_2 = replacement)
        relay_swimmers_df <- collect_relay_swimmers_2(as_lines_list_2)
        # relay_swimmers_df <- relay_swimmers

        relay_swimmers_df <-
        transform(relay_swimmers_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
          dplyr::select(-Row_Numb)

        data <- data %>%
          dplyr::left_join(relay_swimmers_df, c("Row_Numb" = "Row_Numb_Adjusted"))
      }

      #### adding splits back in ####
      if (splits == TRUE) {
        # split_length <- 50
        splits_df <- splits_parse(as_lines_list_2, split_len = split_length)

        #### matches row numbers in splits_df to avaiable row numbers in data
        # helps a lot with relays, since their row numbers vary based on whether or not relay swimmers are included
        # and if those swimmers are listed on one line or two
        splits_df  <-
          transform(splits_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
          dplyr::select(-Row_Numb)

          data <- data %>%
            dplyr::left_join(splits_df, by = c("Row_Numb" = "Row_Numb_Adjusted"))

          ### remove empty columns (all values are NA) ###
          data <- Filter(function(x)
            !all(is.na(x)), data)

      }

      ##### remove duplicated results ####
      # data <- data %>%
      #   dplyr::arrange(Name, Team, is.na(Wind_Speed), is.na(Prelims_Result)) %>% # new 1/1/21 to deal with results presented by heat and as final on same page
      #   dplyr::distinct(Name, Team, Event, Prelims_Result, Finals_Result, .keep_all = TRUE) # new 1/1/21 to deal with results presented by heat and as final on same page
      #
      #### if there is a Place column it should be first ####
      if("Place" %in% names(data)){
        data <- data %>%
          dplyr::select(Place, dplyr::everything())
      }

      data$Row_Numb <- NULL

      message("Beginning with version 0.6.0 the Grade and School output columns have been renamed Age and Team respectively.  Please adjust your work flows as needed.")

      return(data)

  }
  }


#' @rdname Swim_Parse
#' @export
swim_parse <- Swim_Parse
