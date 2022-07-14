#' Formats Omega style swimming and diving data read with \code{read_results}
#' into a data frame
#'
#' Takes the output of \code{read_results} and cleans it, yielding a data frame
#' of swimming (and diving) results
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
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_length
#' @importFrom stringr str_sort
#' @importFrom purrr map_lgl
#' @importFrom purrr map
#' @importFrom stats setNames
#'
#' @param file_omega output from \code{read_results}
#' @param avoid_omega a list of strings.  Rows in \code{file_omega} containing
#'   these strings will not be included. For example "Pool:", often used to
#'   label pool records, could be passed to \code{avoid_omega}.  The default is
#'   \code{avoid_default}, which contains many strings similar to "Pool:", such
#'   as "STATE:" and "Qual:".  Users can supply their own lists to
#'   \code{avoid_omega}. \code{avoid_omega} is handled before \code{typo_omega}
#'   and \code{replacement_omega}.
#' @param typo_omega a list of strings that are typos in the original results.
#'   \code{swim_parse} is particularly sensitive to accidental double spaces, so
#'   "Central  High School", with two spaces between "Central" and "High" is a
#'   problem, which can be fixed.  Pass "Central  High School" to
#'   \code{typo_omega}. Unexpected commas as also an issue, for example "Texas,
#'   University of" should be fixed using \code{typo_omega} and
#'   \code{replacement_omega}
#' @param replacement_omega a list of fixes for the strings in
#'   \code{typo_omega}.  Here one could pass "Central High School" (one space
#'   between "Central" and "High") and "Texas" to \code{replacement_omega} fix
#'   the issues described in \code{typo_omega}
#' @param format_results should the results be formatted for analysis (special
#'   strings like \code{"DQ"} replaced with \code{NA}, \code{Finals} as
#'   definitive column)?  Default is \code{TRUE}
#' @param splits either \code{TRUE} or the default, \code{FALSE} - should
#'   \code{swim_parse} attempt to include splits.
#' @param split_length_omega either \code{25} or the default, \code{50}, the
#'   length of pool at which splits are recorded.  Not all results are
#'   internally consistent on this issue - some have races with splits by 50 and
#'   other races with splits by 25.
#' @param relay_swimmers_omega should names of relay swimmers be captured?
#'   Default is \code{FALSE}
#' @return returns a data frame with columns \code{Name}, \code{Place},
#'   \code{Age}, \code{Team}, \code{Prelims}, \code{Finals},
#'   \code{Points}, \code{Event} & \code{DQ}.  Note all swims will have a
#'   \code{Finals}, even if that time was actually swam in the prelims
#'   (i.e. a swimmer did not qualify for finals).  This is so that final results
#'   for an event can be generated from just one column.
#'
#' @seealso \code{swim_parse_omega} must be run on the output of
#'   \code{\link{read_results}}

swim_parse_omega <-
  function(file_omega,
           avoid_omega = avoid,
           typo_omega = typo,
           replacement_omega = replacement,
           format_results = TRUE,
           splits = FALSE,
           split_length_omega = split_length,
           relay_swimmers_omega = relay_swimmers) {


    #### testing ####

    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030105EF01FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030103EC02FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030101EE01FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030103EC06FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030201EF04FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030103EC04FFFFFFFFFFFF01.pdf") # men 100br final
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020102EE01FFFFFFFFFFFF01.pdf") # wave 1 200 back heats
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020201F104FFFFFFFFFFFF01.pdf") # wave 1 800 free
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020201EB01FFFFFFFFFFFF01.pdf") # wave 1 50 heats
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020101EB04FFFFFFFFFFFF01.pdf") # wave 1 50 final
    # file_omega <- read_results("https://www.omegatiming.com/File/0001150002FFFFFFFFFFFFFFFFFFFF22.pdf") # wave 1 whole book
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020201EC01FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020204EE04FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030103EC06FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results(system.file("extdata", "Omega_OT_100Br_Swimoff_2021.pdf", package = "SwimmeR"))
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020101EB01FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020102EE01FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results(system.file("extdata", "Omega_Wave1_1500_Finals_2021.pdf", package = "SwimmeR"))
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500030105EE02FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73A1_SWMM400MIM------------HEAT000100--.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73A1_SWMW100MBR------------HEAT000100--.pdf")
    # file_omega <- read_results(system.file("extdata", "Omega_OT_400IM_Prelims_2021.pdf", package = "SwimmeR"))
    # file_omega <- read_results("https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73A2_SWMM400MIM------------HEAT--------.pdf")
    # file_omega <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/OG2020-_SWM_C73A2_SWMM100MBF------------SFNL--------.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73B1_SWMM4X200MFR----------FNL-000100--.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73A1_SWMM100MFR------------FNL-000100--.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73A1_SWMM200MFR------------FNL-000100--.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73B1_SWMX4X100MMD----------FNL-000100--.pdf")
    # file_omega <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMW4X200MFR_HEAT.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73A1_SWMM400MIM------------FNL-000100--.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73A1_SWMM100MFR------------FNL-000100--.pdf")
    # file_omega <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMM50MFR_FNL.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020101EC01FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020205EF01FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500020204EE01FFFFFFFFFFFF01.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/0001150003FFFFFFFFFFFFFFFFFFFF22.pdf")
    # file_omega <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Omega/Omega_OT_Wave2_FullResults_2021.pdf")
    # file_omega <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMM100MBF_SFNL.pdf")
    # file_omega <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Omega/Omega_OT_Wave2_W400Fr_Heats_2021.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/paralympic-games/resPG2020-/pdf/PG2020-/SWM/PG2020-_SWM_C73A1_SWMM400MFR--09010-----HEAT000100--.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/paralympic-games/resPG2020-/pdf/PG2020-/SWM/PG2020-_SWM_C73A2_SWMW100MBF--13030-----HEAT--------.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/paralympic-games/resPG2020-/pdf/PG2020-/SWM/PG2020-_SWM_C73A1_SWMM200MIM--06022-----FNL-000100--.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/olympic-games/resOG2020-/pdf/OG2020-/SWM/OG2020-_SWM_C73B1_SWMW4X100MFR----------HEAT000100--.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/paralympic-games/resPG2020-/pdf/PG2020-/SWM/PG2020-_SWM_C73A2_SWMM50MFR---11010-----HEAT--------.pdf")
    # file_omega <- read_results("https://olympics.com/tokyo-2020/paralympic-games/resPG2020-/pdf/PG2020-/SWM/PG2020-_SWM_C73B1_SWMX4X100MFR13033-----FNL-000100--.pdf")
    # file_omega <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/SwimmeR%20Test%20Files/PG2020_SWMM200MIM_FNL.pdf")
    # file_omega <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Paralympics2020/raw_files/PG2020_SWMW150MIM_04042_FNL.pdf")
    # file_omega <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/Tokyo2020/SWMM4X200MFR_FNL.pdf")
    # file_omega <- read_results("https://www.omegatiming.com/File/00011500100201EF01FFFFFFFFFFFF01.pdf")
    file_omega <- read_results(system.file("extdata", "RESULTS_BOOK.pdf", package = "SwimmeR"))
    # file_omega <- read_results("https://raw.githubusercontent.com/gpilgrim2670/Pilgrim_Data/master/SwimmeR%20Test%20Files/PG2020_SWMM200MIM_FNL.pdf")
    # avoid_omega <- c("abcxyz")
    # typo_omega <- c("typo")
    # replacement_omega <- c("typo")
    # split_length_omega <- 50
    # relay_swimmers_omega <- TRUE
    # splits <- TRUE

    #### assign row numbers ####
    as_lines_list_2 <- file_omega %>%
      .[stringr::str_detect(., "Early take-off", negate = TRUE)] %>% # removes DQ rational used in some relay DQs that messes up line spacing between relay and swimmers/splits - must happen before adding in row numbers
      .[stringr::str_detect(., "  1?\\d50m ", negate = TRUE)] %>% # removes cumulative split rows for Omega results only but does not remove Women's 150m IM for para games
      add_row_numbers() %>%
      .[purrr::map_lgl(., ~ !any(stringr::str_detect(., avoid_omega)))] %>%
      stringr::str_replace_all(stats::setNames(replacement_omega, typo_omega)) %>% # replace typos with replacements
      # stringr::str_replace_all("DISQUAL", " DQ ") %>%
      stringr::str_replace_all("EVENT\\:", "Event")

    #### parsing omega files ####

      #### Pulls out event labels from text ####
      events <- event_parse(as_lines_list_2)

      #### Pulls out heat labels from text ####
      heats <- heat_parse_omega(as_lines_list_2)

      #### Pulls out event date from text ####
      Event_Date_String <- paste0("(?<=^\n)\\d{1,2}\\s(", paste(toupper(month.abb), collapse = "|"), ")\\s\\d{4}")
      Event_Date <- stringr::str_extract(as_lines_list_2, Event_Date_String)
      Event_Date <- Event_Date[!is.na(Event_Date)][1]
      Event_Date <- as.Date(Event_Date, format = "%d %b %Y")

      #### set up strings ####
      Name_String <-
        "_?[:alpha:]+\\s?\\'?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\'\\.]*,?\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:]*\\.?,? [:alpha:]+\\s?[:alpha:\\-\\'\\.]*\\s?[:alpha:\\-\\']*\\s?[:alpha:]*\\s?[:alpha:]*\\s?[:alpha:\\.]*"
      Time_Score_String <- "\\d{0,2}\\:?\\d{1,3}\\.\\d{2}"
      Time_Score_Specials_String <- paste0("^NT$|^NP$|^DQ$|^DSQ$|^D?NS$|^SCR$|^x?X?", Time_Score_String, "x?X?$")
      Time_Score_Specials_String_Extract <- paste0(Time_Score_String, "|^NT$|^NP$|^DQ$|^NS$|^SCR$")
      Para_String <- "^SB?M?\\d{1,2}$"
      Reaction_String <- "^\\+\\s?\\d\\.\\d{3}$|^\\-\\s?\\d\\.\\d{3}$|^[0-1]\\.00$|^[0-1]\\.\\d\\d$"
      Record_String <- "^\\=?W[:upper:]$|^\\=?AR$|^\\=?US$|^\\=?CR$|^\\=?OT$|^\\=?OR$|^\\=?[:upper:]R$"
      Header_string <- "\\sDisqualified\\s|\\sReaction\\sTime\\s"
      Heat_String <- "Heat\\s\\d{1,}\\sof\\s\\d{1,}|Semifinal\\s+\\d{1,}|Final|(Heats?)(?![:alpha:])"
      DOB_String_2 <- paste0("(?<=\\s)\\d{2}\\s(", paste(toupper(month.abb), collapse = "|"), ")\\s\\d{4}(?=\\s+\\d{1,2}\\.\\d{2})")
      DOB_String_1 <- paste0("(?<=\\s)\\d{1}\\s(", paste(toupper(month.abb), collapse = "|"), ")\\s\\d{4}(?=\\s+\\d{1,2}\\.\\d{2})")
      Age_String <- "^\\d{2}$"

      #### Collect DOBs ####
      DOBs_2 <- stringr::str_extract(as_lines_list_2, DOB_String_2)
      DOBs_2 <- unique(DOBs_2[!is.na(DOBs_2)])
      DOBs_2_Format <- as.Date(DOBs_2, format = "%d %b %Y")
      Ages_2 <-
        as.character(round(as.numeric(
          difftime(Event_Date, DOBs_2_Format, units = "days") / 365
        ), 0))

      DOBs_1 <- stringr::str_extract(as_lines_list_2, DOB_String_1)
      DOBs_1 <- unique(DOBs_1[!is.na(DOBs_1)])
      DOBs_1_Format <- as.Date(DOBs_1, format = "%d %b %Y")
      Ages_1 <-
        as.character(round(as.numeric(
          difftime(Event_Date, DOBs_1_Format, units = "days") / 365
        ), 0))

      #### clean input data ####

      data_cleaned <- as_lines_list_2 %>%
        stringr::str_remove("^\n\\s{0,}") %>%
        .[stringr::str_length(.) > 50] %>% # slight speed boost from cutting down length of file
        .[stringr::str_detect(., paste0(Time_Score_String,"|DS?Q|SCR|D?NS"))] %>% # must have \\.\\d\\d because all swimming and diving times do
        .[stringr::str_detect(., "[:alpha:]{2,}.*[:alpha:]")] %>% # need some letters, need them to not just be a single instance of DQ etc.
        .[stringr::str_detect(., "[:alpha:]{2,} Record", negate = TRUE)] %>%  # remove legend contents that will be included if their are DQs, DNS etc. Omega
        .[stringr::str_detect(., "[:alpha:] DSQ [:alpha:]", negate = TRUE)] %>% # omega Wave II removes notes from when a DSQ is overturned
        .[stringr::str_detect(., "\\d{2} [:upper:]{3} \\d{4} GOLD", negate = TRUE)] %>%  # omega remove medalist results from the end of Wave II 2021 OTs
        .[stringr::str_detect(., Heat_String, negate = TRUE)] %>% # omega removes weird formatting issue where results and heat labels are connected
        {if (length(Ages_2) > 0) purrr::reduce2(.init = ., DOBs_2, Ages_2, stringr::str_replace_all) else .} %>%
        {if (length(Ages_1) > 0) purrr::reduce2(.init = ., DOBs_1, Ages_1, stringr::str_replace_all) else .} %>%
        stringr::str_replace_all("([:alpha:])\\. ", "\\1 ") %>% # remove periods from athlete names Omega
        # .[!all(dplyr::between(purrr::map_int(., stringr::str_count, "\\.|\\("), 6.1, 7.9), purrr::map_int(., stringr::str_count, "\\.") != 6)] %>%  # keeps relay swimmers + splits from 4x200 races from getting in Omega
        .[stringr::str_detect(., "[:alpha:]\\s+\\-\\d\\.\\d{2}\\s+\\d{1,}$", negate = TRUE)] %>%
        .[stringr::str_detect(., Header_string, negate = TRUE)] %>%
        stringr::str_replace_all("( [:upper:]{2},\\s)*\\s?(?<![:upper:])\\=?[:upper:]{2}\\.?\\s(?=\\s{1,}\\d{1,}$)", "  ") %>% # remove record strings like OR, OC but miss DNS, DSQ etc.
        stringr::str_replace_all("\\s?[&%]\\s?", " ") %>% # added 8/21 for removing "&" and "%" as record designator

        stringr::str_remove_all("(?<=\\d\\.\\d{2}\\s?)[:punct:]") %>% # remove symbols attached to times as record designator
        stringr::str_remove_all("(?<=\\.\\d{2}\\s?)[A-WYZ|\\$|q](?=\\s)") %>% # remove letters attached to times as record designator
        stringr::str_replace_all(" [qQ](?=\\d{1,5} )", "   ") %>% # remove q|Q attached to points
        stringr::str_replace_all("[A-WYZa-wyz]+(\\d{1,2}\\:\\d{2}\\.\\d{2})", "\\1") %>% # removed J etc. from next to swim, but does not remove X or x (for exhibition tracking)
        stringr::str_replace_all("(\\d{1,2}\\:\\d{2}\\.\\d{2})[A-WYZa-wyz]+", "\\1") %>%
        stringr::str_replace_all("[A-WYZa-wyz]+(\\d{2,3}\\.\\d{2})", "\\1") %>%
        stringr::str_replace_all("(\\d{2,3}\\.\\d{2})[A-WYZa-wyz]+", "\\1") %>%
        stringr::str_replace_all(" [:punct:]+(\\d{1,2}\\:\\d{2}\\.\\d{2})", " \\1") %>%
        stringr::str_replace_all("(\\d{1,2}\\:\\d{2}\\.\\d{2})[:punct:]+", " \\1") %>%
        stringr::str_replace_all(" [:punct:]+(\\d{2,3}\\.\\d{2})", " \\1") %>%
        stringr::str_replace_all("(\\d{2,3}\\.\\d{2})[:punct:]+", "\\1 ") %>%
        stringr::str_replace_all("\\(\\=?\\d\\)", "   ") %>% # omega
        stringr::str_remove_all("\\s{2}J\\s{2}") %>%
        stringr::str_remove_all("\\=(?=\\d)") %>%
        stringr::str_replace_all("  [A-WYZ]\\??  ", "  ") %>% # omega
        stringr::str_replace_all(" R?\\? ", "  ") %>% # omega
        stringr::str_replace_all(" \\d{4} ", "   ") %>% # omega
        stringr::str_replace_all("(?<=\\.\\d)(\\d)\\s+X(?=\\s+\\d{1,5}$)", "\\1X") %>% # brings Xs for exhibition that are spaced out in closer
        # remove q from next to time 10/21/2020
        stringr::str_remove_all(" q ") %>% # removes " q " sometimes used to designate a qualifying time
        stringr::str_replace_all("-{2,5}", "10000") %>% #8/26
        stringr::str_replace_all("\\*(?=[:alpha:])", "_") %>% # for * prior to name for foreign athletes
        stringr::str_replace_all("\\*", "  ") %>%
        stringr::str_replace_all("(?<=\\s)\\+\\s(?=[:digit:])", "  +") %>% # for reaction times
        stringr::str_replace_all("(?<=\\s)\\-\\s(?=[:digit:])", "  -") %>% # for reaction times
        stringr::str_remove_all("\U2666") %>%         # remove black diamond
        trimws()


      #### insert double spaces where needed ####
      data_cleaned <- data_cleaned %>%
        stringr::str_replace_all("(?<=\\d)\\s+[:upper:]?\\s+(?=\\d)", "  ") %>% # letters like P or M to denote pool or meet record
        stringr::str_replace_all(" \\=[:upper:]R\\,?", "  ") %>% # omega remove record designators like =AR
        stringr::str_replace_all("(?<=\\d) (?=[:alpha:])", "  ") %>% # mostly to split place and name
        stringr::str_replace_all("(?<=\\d) (?=_)", "  ") %>% # mostly to split place and name, if name is preceded by "_" as a stand-in for "*"
        stringr::str_replace_all("(?<=\\d) (?=\\d)", "  ") %>% # mostly to split place team names that start with a number, like in NYS results (5-Fairport etc.)
        stringr::str_replace_all("(?<=[:alpha:]),(?=[:alpha:])", ", ") %>% # split names that don't have a space between last,first
        stringr::str_replace_all("(?<=[:alpha:])\\. (?=[:digit:])", "\\.  ") %>% # split abbreviated team names like Southern Cal. and times
        stringr::str_replace_all("(?<=\\d) (?=_)", "  ") %>% # spacing between place and athletes with */_ leading name
        stringr::str_replace_all("(?<=\\)) (?=[:alpha:])", "  ") %>% # spacing between place) and names
        stringr::str_replace_all("(SM?B?1\\d{1})(\\d{1,2})", "\\1   \\2") %>%  # split para classification and age
        stringr::str_replace_all(" NT ", "       NT      ") %>% # split prelim and final
        stringr::str_replace_all("(?<=[:alpha:])\\s{2,3}(?=[:alpha:])", " ") %>% # testing 12/21/2020 would help with typos
        stringr::str_replace_all("(?<=[:alpha:]) (?=\\d)", "  ") %>% # split name and age
        stringr::str_replace_all("(?<=[:alpha:])(\\d{1,3}\\-\\d{2})", "  \\1  ") %>% # split name and yyy-mm age
        stringr::str_replace_all("(?<=\\,) (?=\\d)", "  ") %>% # split name and age if name is so long that it ends with a ","
        stringr::str_replace_all("(?<=\\d) (?=\\d{1,}$)", "  ") %>%  # split off row_numb
        stringr::str_replace_all("(?<=\\.\\d\\d\\s{1,10}\\d?\\d?\\:?\\d?\\d\\.\\d\\d)\\s{1,10}[:alpha:]{1,5}\\d?\\s{1,10}(?=\\d{1,})", "  ") %>%  # removes "AAC" or "AAA" or "NYS" or "SEC1" etc. from after finals time
        stringr::str_replace_all("(?<=\\s)S(?=B?M?\\d{1,2})", "  S") %>%  # for para classifications
        stringr::str_replace_all("([:alpha:])(SB?M?\\d{1,2})", "\\1  \\2") %>%  # for splitting names and para classifications
        # {if(stringr::str_detect(events$Event[1], "elay") == FALSE)
        #   stringr::str_replace_all(., "(^[:upper:]{3,})", "NA  NA  \\1") else . # for GUTIERREZ BERMUDEZ Juan Jose in 2020 Para Games 200IM Finals
        # }
        stringr::str_replace_all("(^[:upper:]{3,}.*\\s{3,}[:upper:]{3})", "7  6  \\1") # for GUTIERREZ BERMUDEZ Juan Jose in 2020 Para Games 200IM Finals

      #### if data_cleaned is empty ####
      if(!length(data_cleaned) > 0){
        message("No results found in file")

      } else {

      #### splits data into variables by splitting at multiple (>= 2) spaces ####
      data_cleaned <-
        unlist(purrr::map(data_cleaned, stringr::str_split, "\\s{2,}"),
               recursive = FALSE)

      # unique(map(data_cleaned, length))

      #### breaks data into subsets based on how many variables it has ####
      data_length_5 <- data_cleaned[purrr::map(data_cleaned, length) == 5]
      data_length_6 <- data_cleaned[purrr::map(data_cleaned, length) == 6]
      data_length_7 <- data_cleaned[purrr::map(data_cleaned, length) == 7]
      data_length_8 <- data_cleaned[purrr::map(data_cleaned, length) == 8]
      data_length_9 <- data_cleaned[purrr::map(data_cleaned, length) == 9]
      data_length_10 <- data_cleaned[purrr::map(data_cleaned, length) == 10]
      data_length_11 <- data_cleaned[purrr::map(data_cleaned, length) == 11]
      data_length_12 <- data_cleaned[purrr::map(data_cleaned, length) == 12]
      data_length_13 <- data_cleaned[purrr::map(data_cleaned, length) == 13]
      # data_length_14 <- data_cleaned[purrr::map(data_cleaned, length) == 14]

      # treatment of DQs new 8/19
      suppressWarnings(DQ <-
                         data_cleaned[stringr::str_detect(data_cleaned, Time_Score_String, negate = TRUE) == TRUE])

      DQ_length_3 <- DQ[purrr::map(DQ, length) == 3]
      DQ_length_4 <- DQ[purrr::map(DQ, length) == 4]
      # DQ_length_5 <- DQ[purrr::map(DQ, length) == 5]

      #### thirteen variables ####
      if (length(data_length_13) > 0) {
        suppressWarnings(
          df_13 <- data_length_13 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat = V2,
              Lane = V3,
              Name = V4,
              Team = V5,
              Age = V6,
              Reaction_Time = V7,
              Finals = V11,
              Row_Numb = V13
            )
        ) %>%
          dplyr::filter(stringr::str_detect(Place, "^\\d+$"))
      } else {
        df_13 <- data.frame(
          Row_Numb = character(),
          stringsAsFactors = FALSE
        )
      }

      #### twelve variables ####
      if (length(data_length_12) > 0) {
        suppressWarnings(
          df_12 <- data_length_12 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::mutate(Heat = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Lane = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V3,
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == FALSE ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Name = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V3,
                                                  TRUE ~ V4)) %>%
            dplyr::mutate(Para = dplyr::case_when(stringr::str_detect(V4, Para_String) == TRUE ~ V4,
                                                  TRUE ~ "NA")) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V4,
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == TRUE  ~ V5,
                TRUE ~ V5
              )
            ) %>%
            dplyr::mutate(
              Reaction_Time = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V5,
                stringr::str_detect(V6, Reaction_String) == TRUE ~ V6,
                stringr::str_detect(V7, Reaction_String) == TRUE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(Age = case_when(stringr::str_detect(V6, Age_String) == TRUE ~ V6,
                                                              TRUE ~ "NA")) %>%
            dplyr::mutate(
              Finals = dplyr::case_when(
                stringr::str_detect(V11, Time_Score_Specials_String) == TRUE &
                  sec_format(V10) < sec_format(V11) ~ V11,
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V10,
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V10,
                stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V8, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V9, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V10, Time_Score_Specials_String) == TRUE ~ V10,
               stringr::str_detect(V10, Time_Score_Specials_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::na_if("") %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat,
              Lane,
              Name,
              Age,
              Para,
              Team,
              Reaction_Time,
              Finals,
              Row_Numb = V12
            ) %>%
            dplyr::filter(stringr::str_detect(Place, "^\\d+$"))
        )
      } else {
        df_12 <- data.frame(
          Row_Numb = character(),
          stringsAsFactors = FALSE
        )
      }

      #### eleven variables ####
      if (length(data_length_11) > 0) {
        suppressWarnings(
          df_11 <- data_length_11 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::filter(stringr::str_detect(V10, "Championship") == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::mutate(Heat = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Lane = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V3,
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == FALSE ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Name = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V3,
                                                  TRUE ~ V4)) %>%
            dplyr::mutate(Para = dplyr::case_when(stringr::str_detect(V4, Para_String) == TRUE ~ V4,
                                                  TRUE ~ "NA")) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V4,
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == TRUE  ~ V5,
                TRUE ~ V5
              )
            ) %>%
            dplyr::mutate(
              Reaction_Time = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V5,
                stringr::str_detect(V6, Reaction_String) == TRUE ~ V6,
                stringr::str_detect(V7, Reaction_String) == TRUE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(Age = case_when(stringr::str_detect(V6, Age_String) == TRUE ~ V6,
                                          TRUE ~ "NA")) %>%
            dplyr::mutate(
              Finals = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V9,
                stringr::str_detect(V5, Reaction_String) == TRUE &
                  stringr::str_detect(V9, Time_Score_Specials_String) == TRUE ~ V9,
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == TRUE &
                  stringr::str_detect(V9, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V10, "\\:") == FALSE ~ V9,
                stringr::str_detect(V10, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V9, Time_Score_Specials_String) == TRUE &
                  (sec_format(V9) > sec_format(V10)) ~ V9,
                stringr::str_detect(V10, Time_Score_Specials_String) == TRUE ~ V10,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat,
              Lane,
              Name,
              Age,
              Para,
              Team,
              Reaction_Time,
              Finals,
              Row_Numb = V11
            ) %>%
            dplyr::filter(stringr::str_detect(Place, "^\\d+$"))
        )
      } else {
        df_11 <- data.frame(
          Row_Numb = character(),
          stringsAsFactors = FALSE
        )
      }

      #### ten variables ####
      if (length(data_length_10) > 0) {
        suppressWarnings(
          df_10 <- data_length_10 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::filter(stringr::str_detect(V1, "en") == FALSE) %>% # remove Women's Men's etc.
            dplyr::filter(stringr::str_detect(V9, "Championship") == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::mutate(Heat = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Lane = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V3,
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == FALSE ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Name = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V3,
                                                  TRUE ~ V4)) %>%
            dplyr::mutate(Para = dplyr::case_when(stringr::str_detect(V4, Para_String) == TRUE ~ V4,
                                                  TRUE ~ "NA")) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V4,
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == TRUE  ~ V5,
                TRUE ~ V5
              )
            ) %>%
            dplyr::mutate(
              Reaction_Time = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V5,
                stringr::str_detect(V6, Reaction_String) == TRUE ~ V6,
                stringr::str_detect(V7, Reaction_String) == TRUE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(Age = case_when(stringr::str_detect(V6, Age_String) == TRUE ~ V6,
                                          TRUE ~ "NA")) %>%
            dplyr::mutate(
              Finals = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V9,
                stringr::str_detect(V5, Reaction_String) == TRUE & stringr::str_detect(V9, Time_Score_Specials_String) == TRUE ~ V9,
                stringr::str_detect(Heat, "NA") == TRUE & stringr::str_detect(V4, Para_String) == TRUE & stringr::str_detect(V9, Time_Score_Specials_String) == TRUE ~ V9,
                stringr::str_detect(V8, Time_Score_Specials_String) == TRUE & stringr::str_detect(V9, Time_Score_Specials_String) == TRUE &
                  (sec_format(V9) > sec_format(V8)) ~ V9,
                stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat,
              Lane,
              Name,
              Age,
              Para,
              Team,
              Reaction_Time,
              Finals,
              Row_Numb = V10
            ) %>%
            dplyr::filter(stringr::str_detect(Place, "^\\d+$"))
        )
      } else {
        df_10 <- data.frame(Row_Numb = character(),
                            stringsAsFactors = FALSE)
      }

      #### nine variables ####
      if (length(data_length_9) > 0) {
        suppressWarnings(
          df_9 <- data_length_9 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::filter(stringr::str_detect(V1, "(Women)|(Men)") == FALSE) %>% # remove Women's Men's etc.
            dplyr::filter(stringr::str_detect(V3, "(Women)|(Men)") == FALSE) %>% # remove Women's Men's etc.
            dplyr::filter(stringr::str_detect(V4, "(Women)|(Men)") == FALSE) %>% # remove Women's Men's etc.
            dplyr::na_if("") %>%
            dplyr::mutate(Heat = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Lane = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V3,
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == FALSE ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Name = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V3,
                                                  TRUE ~ V4)) %>%
            dplyr::mutate(Para = dplyr::case_when(stringr::str_detect(V4, Para_String) == TRUE ~ V4,
                                                  TRUE ~ "NA")) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE ~ V4,
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == TRUE  ~ V5,
                TRUE ~ V5
              )
            ) %>%
            dplyr::mutate(
              Reaction_Time = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V4, Para_String) == FALSE &
                  stringr::str_detect(V5, "^\\d\\.\\d{2}") == TRUE ~ V5,
                # stringr::str_detect(Heat, "NA") == TRUE &
                #   stringr::str_detect(V4, Para_String) == TRUE &
                  stringr::str_detect(V6, "^\\d\\.\\d{2}") == TRUE ~ V6,
                stringr::str_detect(V7, "^\\d\\.\\d{2}") == TRUE &
                  stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V7,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(Age = case_when(stringr::str_detect(V6, Age_String) == TRUE ~ V6,
                                          TRUE ~ "NA")) %>%
            dplyr::mutate(
              Finals = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE & stringr::str_detect(V4, Para_String) == FALSE ~ V7,
                stringr::str_detect(V7, "[1-9]\\:\\d\\d") == TRUE  ~ V7,
                stringr::str_detect(V6, Reaction_String) == TRUE & stringr::str_detect(V7, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V8, "\\d\\d\\.\\d\\d") == FALSE ~ V7,
                stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat,
              Lane,
              Name,
              Age,
              Para,
              Team,
              Reaction_Time,
              Finals,
              Row_Numb = V9
            ) %>%
            dplyr::filter(stringr::str_detect(Place, "^\\d+$"))
        )
      } else {
        df_9 <- data.frame(
          Row_Numb = character(),
          stringsAsFactors = FALSE
        )
      }

      #### eight variables ####
      if (length(data_length_8) > 0) {
        suppressWarnings(
          df_8 <- data_length_8 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::filter(stringr::str_detect(V1, "(Women)|(Men)") == FALSE) %>% # remove Women's Men's etc.
            dplyr::filter(stringr::str_detect(V4, "(Women)|(Men)") == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::mutate(Heat = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(
              Lane = case_when(
                str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V3,
                str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V3, "^\\d{1,2}$") == FALSE ~ V2,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(Name = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V3,
                                                  TRUE ~ V4)) %>%
            dplyr::mutate(Para = dplyr::case_when(stringr::str_detect(V4, Para_String) == TRUE ~ V4,
                                                  TRUE ~ "NA")) %>%
            dplyr::mutate(Team = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE &
                                                    stringr::str_detect(V4, Para_String) == FALSE ~ V4,
                                                  stringr::str_detect(Heat, "NA") == TRUE &
                                                    stringr::str_detect(V4, Para_String) == TRUE  ~ V5,
                                                  TRUE ~ V5)) %>%
            dplyr::mutate(
              Reaction_Time = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V5, "^\\d\\.\\d\\d") == TRUE ~ V5,
                stringr::str_detect(V6, "^\\d\\.\\d\\d") == TRUE &
                  str_detect(V5, "\\d\\d\\.\\d\\d") == FALSE ~ V6,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(Age = case_when(stringr::str_detect(V6, Age_String) == TRUE ~ V6,
                                          TRUE ~ "NA")) %>%
            dplyr::mutate(
              Finals = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V5, Reaction_String) == TRUE &
                  stringr::str_detect(V6, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V7, "^\\d\\d?\\.\\d\\d$") == TRUE &
                  (sec_format(V7) < sec_format(V6)) == TRUE ~ V6,
                stringr::str_detect(Heat, "NA") == FALSE &
                  stringr::str_detect(V6, "[1-9]\\:\\d\\d") == TRUE &
                  stringr::str_detect(V7, "^\\d?\\d\\.\\d\\d") == TRUE &
                  (sec_format(V7) < sec_format(V6)) == TRUE ~ V6,
                stringr::str_detect(Heat, "NA") == TRUE ~ V7,
                stringr::str_detect(V7, "[1-9]\\:\\d\\d") == TRUE  ~ V7,
                stringr::str_detect(V6, Reaction_String) == TRUE &
                  stringr::str_detect(V7, Time_Score_Specials_String) == TRUE ~ V7,
                stringr::str_detect(V8, Time_Score_Specials_String) == TRUE ~ V8,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat,
              Lane,
              Name,
              Age,
              Para,
              Team,
              Reaction_Time,
              Finals,
              Row_Numb = V8
            ) %>%
            dplyr::filter(stringr::str_detect(Place, "\\d")) # removes relay swimmer rows
        )
      } else {
        df_8 <- data.frame(Row_Numb = character(),
                           stringsAsFactors = FALSE)
      }

      #### seven variables ####
      if (length(data_length_7) > 0) {
        suppressWarnings(
          df_7 <- data_length_7 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::mutate(Heat = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(Lane = case_when(
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V3,
              str_detect(V2, "^\\d{1,2}$") == TRUE &
                stringr::str_detect(V3, "^\\d{1,2}$") == FALSE ~ V2,
              TRUE ~ "NA"
            )) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE ~ V3,
                stringr::str_detect(Heat, V2) == TRUE &
                  stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ "NA",
                TRUE ~ V4
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE ~ V4,
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(V4, "[:alpha:]{3,}") == TRUE ~ V4,
                TRUE ~ V5
              )
            ) %>%
            dplyr::mutate(
              Reaction_Time = dplyr::case_when(
                stringr::str_detect(Heat, "NA") == TRUE &
                  stringr::str_detect(V5, "^\\d\\.\\d\\d") == TRUE ~ V5,
                stringr::str_detect(V6, "^\\d\\.\\d\\d") == TRUE &
                  str_detect(V5, "\\d\\d\\.\\d\\d") == FALSE ~ V6,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Age = case_when(
                stringr::str_detect(V6, Age_String) == TRUE ~ V6,
                stringr::str_detect(V6, Age_String) == FALSE &
                  stringr::str_detect(V5, Age_String) == TRUE ~ V5,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Finals = dplyr::case_when(
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE &
                  stringr::str_detect(Reaction_Time, V5) == FALSE ~ V5,
                stringr::str_detect(V6, Time_Score_Specials_String) == TRUE ~ V6,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place = V1,
              Heat,
              Lane,
              Name,
              Team,
              Age,
              Reaction_Time,
              Finals,
              Row_Numb = V7
            ) %>%
            dplyr::filter(!is.na(Finals)) %>%
            dplyr::filter(stringr::str_detect(Place, "\\d")) # removes relay swimmer rows
        )
      } else {
        df_7 <- data.frame(Row_Numb = character(),
                           stringsAsFactors = FALSE)
      }

      #### six variables ####
      if (length(data_length_6) > 0) {
        suppressWarnings(
          df_6 <- data_length_6 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::mutate(
              Place = dplyr::case_when(stringr::str_detect(V5, "DSQ|DNS") == TRUE ~ "10000",
                                       TRUE ~ V1)
            ) %>%
            dplyr::mutate(
              Heat = dplyr::case_when(
                stringr::str_detect(V1, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V2,
                stringr::str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V3, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V5, "DSQ|DNS") == FALSE ~ V2,
                stringr::str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V1, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V5, "DSQ|DNS") == TRUE ~ V1,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Lane = case_when(
                str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V3,
                str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V3, "^\\d{1,2}$") == FALSE ~ V2,
                str_detect(V1, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(Place, "10000") == TRUE ~ V1,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(Place, "10000") == TRUE &
                  stringr::str_detect(V2, Name_String) == TRUE ~ V2,
                stringr::str_detect(V4, "\\d\\:\\d{2}\\.\\d{2}") == TRUE ~ "NA",
                stringr::str_detect(Heat, "NA") == TRUE ~ V3,
                stringr::str_detect(V5, "DSQ|DNS") == TRUE ~ V3,
                stringr::str_detect(V3, Lane) == TRUE &
                  str_detect(V4, "[:upper:][:lower:]") == TRUE &
                  str_detect(V5, Time_Score_String) == TRUE ~ "NA",
                TRUE ~ V4
              )
            ) %>%
            dplyr::mutate(Para = dplyr::case_when(stringr::str_detect(V3, Para_String) == TRUE ~ V3,
                                                  TRUE ~ "NA")) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V4, "\\d\\:\\d{2}\\.\\d{2}") == TRUE ~ V3,
                stringr::str_detect(Heat, "NA") == TRUE ~ V4,
                stringr::str_detect(V5, Time_Score_Specials_String) == TRUE ~ V4,
                TRUE ~ V5
              )
            ) %>%
            dplyr::mutate(
              Reaction_Time = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE &
                                                 stringr::str_detect(V4, Time_Score_Specials_String) == FALSE &
                                                 stringr::str_detect(V5, "^\\d\\.\\d{2}") == TRUE ~ V5,
                                               TRUE ~ "NA")
            ) %>%
            dplyr::mutate(
              Finals = dplyr::case_when(
                str_detect(V4, "\\d\\:\\d{2}\\.\\d{2}") == TRUE &
                  str_detect(V5, "\\d\\.\\d{2}") ~ V4,
                TRUE ~ V5
              )
            ) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place,
              Heat,
              Lane,
              Name,
              Para,
              Team,
              Reaction_Time,
              Finals,
              Row_Numb = V6
            ) %>%
            dplyr::filter(stringr::str_detect(Place, "\\d")) # removes relay swimmer rows
        )
      } else {
        df_6 <- data.frame(Row_Numb = character(),
                           stringsAsFactors = FALSE)
      }

      #### five variables ####
      if (length(data_length_5) > 0) {
        suppressWarnings(
          df_5 <- data_length_5 %>%
            list_transform() %>%
            dplyr::filter(stringr::str_detect(V1, Record_String) == FALSE) %>%
            dplyr::na_if("") %>%
            dplyr::mutate(
              Place = dplyr::case_when(stringr::str_detect(V4, "DSQ|DNS") == TRUE ~ "10000",
                                       TRUE ~ V1)
            ) %>%
            dplyr::mutate(
              Heat = dplyr::case_when(
                stringr::str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V3, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V5, "DSQ|DNS") == FALSE ~ V2,
                stringr::str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V1, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V4, "DSQ") == TRUE ~ V1,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Lane = case_when(
                str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V3, "^\\d{1,2}$") == TRUE ~ V3,
                str_detect(V2, "^\\d{1,2}$") == TRUE &
                  stringr::str_detect(V3, "^\\d{1,2}$") == FALSE ~ V2,
                TRUE ~ "NA"
              )
            ) %>%
            dplyr::mutate(
              Name = dplyr::case_when(
                stringr::str_detect(V2, Name_String) == TRUE &
                  stringr::str_detect(V3, Time_Score_Specials_String) == FALSE &
                  stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V2,
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ "NA",
                stringr::str_detect(Heat, "NA") == TRUE ~ V3,
                stringr::str_detect(V5, "DSQ|DNS") == TRUE ~ V3,
                TRUE ~ V4
              )
            ) %>%
            dplyr::mutate(
              Team = dplyr::case_when(
                stringr::str_detect(V4, Time_Score_Specials_String) == TRUE ~ V3,
                stringr::str_detect(Heat, "NA") == TRUE ~ V4,
                stringr::str_detect(V5, "DSQ|DNS") == TRUE ~ V4,
                TRUE ~ V5
              )
            ) %>%
            # dplyr::mutate(Reaction_Time = dplyr::case_when(stringr::str_detect(Heat, "NA") == TRUE ~ V5,
            #                                                TRUE ~ "NA")) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              Place,
              Heat,
              Lane,
              Name,
              Team,
              # Reaction_Time,
              Finals = V4,
              Row_Numb = V5
            ) %>%
            dplyr::filter(stringr::str_detect(Place, "\\d")) # removes relay swimmer rows
        )
      } else {
        df_5 <- data.frame(Row_Numb = character(),
                           stringsAsFactors = FALSE)
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
              Finals = V3,
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
              Finals = V2,
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

      #### Rejoin data frames from each number of variables ####
      Min_Row_Numb <- min(events$Event_Row_Min)
      suppressWarnings(
        data <- dplyr::bind_rows(df_13, df_12) %>%
          dplyr::bind_rows(df_11) %>%
          dplyr::bind_rows(df_10) %>%
          dplyr::bind_rows(df_9) %>%
          dplyr::bind_rows(df_8) %>%
          dplyr::bind_rows(df_7) %>%
          dplyr::bind_rows(df_6) %>%
          dplyr::bind_rows(df_5) %>%
          dplyr::full_join(df_DQ_4) %>%
          dplyr::full_join(df_DQ_3) %>%
          dplyr::mutate(Row_Numb = as.numeric(Row_Numb)) %>%
          dplyr::arrange(Row_Numb)
      )


      # if("Prelims" %in% names(data) == FALSE){
      #   data$Prelims <- NA
      # }
      # if("Age" %in% names(data) == FALSE){
      #   data$Age <- "NA"
      # }

      suppressWarnings(
        data <- data %>%
          dplyr::mutate(
            Exhibition = dplyr::case_when(stringr::str_detect(Finals, "x|X") == TRUE ~ 1,
                                          TRUE ~ 0),
          ) %>%
          dplyr::mutate(
            DQ = dplyr::case_when(
              as.numeric(Place) == 10000 &
                Exhibition == 0 ~ 1,
              # added exhibition condition 8/27
              stringr::str_detect(Finals, "DQ|DSQ|DNS|DFS") == TRUE ~ 1,
              is.na(DQ) ~ 0,
              TRUE ~ DQ
            )
          ) %>%
          na_if(as.numeric(10000)) %>%
          dplyr::mutate(dplyr::across(
            c(Name, Team), ~ stringr::str_replace_all(., "10000", "--")
          )) %>% # remove any "10000"s added in erroneously
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
            Row_Numb = as.numeric(Row_Numb)
          ) %>%
          dplyr::filter(Team %!in% c("Gold", "Silver", "Bronze")) %>%
          dplyr::filter(Name %!in% c("GOLD", "SILVER", "BRONZE"))
      )


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

      #### adding heats based on row number ranges ####
      if ("Heat" %in% names(data) == TRUE) {
        if (all(is.na(data$Heat))) {
          data <- data %>%
            select(-Heat)
        }
      }
      if ("Heat" %in% names(data) == FALSE) {
        if (min(data$Row_Numb) < min(heats$Heat_Row_Min)) {
          unknown_heat <- data.frame(
            Heat = "NA",
            Heat_Row_Min = min(data$Row_Numb),
            Heat_Row_Max = min(heats$Heat_Row_Min) - 1
          )
          heats <- dplyr::bind_rows(unknown_heat, heats)
        }

        data  <-
          transform(data, Heat = heats$Heat[findInterval(Row_Numb, heats$Heat_Row_Min)]) %>%
          dplyr::na_if("NA")
      }

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
          dplyr::mutate(Reaction_Time = case_when(stringr::str_detect(Event, "(R|r)elay|\\dx\\d{3}") == TRUE ~ "NA",
                                                  TRUE ~ Reaction_Time)) %>%
          dplyr::na_if("NA")
      )

      #### cleaning ####
      if(format_results == TRUE){
        data <- format_results(data)
      }

      #### adding relay swimmers in ####
      if (relay_swimmers_omega == TRUE) {
        relay_swimmers_df <- collect_relay_swimmers_omega(as_lines_list_2)

        relay_swimmers_df <-
        transform(relay_swimmers_df, Row_Numb_Adjusted = data$Row_Numb[findInterval(Row_Numb, data$Row_Numb)]) %>%
          dplyr::select(-Row_Numb)

        data <- data %>%
          dplyr::left_join(relay_swimmers_df, c("Row_Numb" = "Row_Numb_Adjusted"))
      }

      #### adding splits back in ####
      if (splits == TRUE) {

        data <- data %>%
          dplyr::mutate(dplyr::across(where(is.numeric), as.character))

        data_ind <- data %>%
          dplyr::filter(stringr::str_detect(Event, "(R|r)elay|\\dx\\d{3}") == FALSE)

        data_relay <- data %>%
          dplyr::filter(stringr::str_detect(Event, "(R|r)elay|\\dx\\d{3}") == TRUE)

        if(nrow(data_ind) > 0) {

        # split_length <- 50
        splits_df <-
          splits_parse(as_lines_list_2, split_len = split_length_omega)

        #### matches row numbers in splits_df to available row numbers in data
        # helps a lot with relays, since their row numbers vary based on whether or not relay swimmers are included
        # and if those swimmers are listed on one line or two
        splits_df  <-
          transform(splits_df, Row_Numb_Adjusted = data_ind$Row_Numb[findInterval(Row_Numb, as.numeric(data_ind$Row_Numb))]) %>%
          dplyr::mutate(Row_Numb_Adjusted = as.character(Row_Numb_Adjusted)) %>%
          dplyr::select(-Row_Numb)

        # new names for omega splits, because first 50 isn't captured
        splits_df <- splits_df %>%
          dplyr::rename_with(splits_rename_omega, dplyr::starts_with("Split"), split_len = split_length_omega) %>%
          dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), sec_format))

        suppressWarnings(
          data_ind <- data_ind %>%
            dplyr::left_join(splits_df, by = c("Row_Numb" = "Row_Numb_Adjusted")) %>%
            dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), as.numeric)) %>%
            dplyr::mutate(
              Split_Total = rowSums(dplyr::across(dplyr::starts_with("Split")), na.rm = TRUE),
              Finals_Sec = sec_format(Finals),
              Split_50 = Finals_Sec - Split_Total,
              Split_50 = dplyr::case_when(Split_50 < 0 ~ 10000,
                                          TRUE ~ Split_50)
            ) %>%
            dplyr::na_if(10000) %>%
            dplyr::select(-Split_Total, -Finals_Sec) %>%
            dplyr::mutate(dplyr::across(
              dplyr::starts_with("Split"), format, nsmall = 2
            )) %>%
            dplyr::mutate(dplyr::across(where(is.numeric), as.character)) %>%
            dplyr::mutate(dplyr::across(where(is.character), trimws)) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(
              !dplyr::starts_with("Split"),
              stringr::str_sort(names(.), numeric = TRUE)
            ) # keep splits columns in order
        )
        }

        if(nrow(data_relay) > 0){
          splits_df <-
            splits_parse_omega_relays(as_lines_list_2, split_len = split_length_omega)

          #### matches row numbers in splits_df to available row numbers in data
          # helps a lot with relays, since their row numbers vary based on whether or not relay swimmers are included
          # and if those swimmers are listed on one line or two
          splits_df  <-
            transform(splits_df, Row_Numb_Adjusted = data_relay$Row_Numb[findInterval(Row_Numb, as.numeric(data_relay$Row_Numb))]) %>%
            dplyr::mutate(Row_Numb_Adjusted = as.character(Row_Numb_Adjusted)) %>%
            dplyr::select(-Row_Numb)

          data_relay <- data_relay %>%
            # dplyr::filter(Finals != "DNS") %>%
            # dplyr::filter(Finals != "DSQ") %>%
            dplyr::left_join(splits_df, by = c("Row_Numb" = "Row_Numb_Adjusted")) %>%
            dplyr::na_if(10000) %>%
            dplyr::mutate(dplyr::across(dplyr::starts_with("Split"), format, nsmall = 2)) %>%
            dplyr::mutate(dplyr::across(where(is.numeric), as.character)) %>%
            dplyr::mutate(dplyr::across(where(is.character), trimws)) %>%
            dplyr::na_if("NA") %>%
            dplyr::select(!dplyr::starts_with("Split"),
                          stringr::str_sort(names(.), numeric = TRUE)) # keep splits columns in order
        }


          data <- data_ind %>%
            dplyr::bind_rows(data_relay) %>%
            dplyr::arrange(as.numeric(Row_Numb))

      }

      # remove reaction time from relays
      if("Reaction_Time" %in% names(data)){
        data <- data %>%
          dplyr::mutate(Reaction_Time = dplyr::case_when(stringr::str_detect(Event, "Relay") == TRUE ~ "NA",
                                                         TRUE ~ Reaction_Time)) %>%
          dplyr::na_if("NA")
      }

      ### remove empty columns (all values are NA) ###
      data <- Filter(function(x)
        !all(is.na(x)), data)

      #### if there is a Place column it should be first ####
      if("Place" %in% names(data)){
        data <- data %>%
          dplyr::select(Place, dplyr::everything())
      }

      if(all("Place" %in% names(data), "Heat" %in% names(data))){
        data <- data %>%
          dplyr::select(Place, Heat, dplyr::everything())
      }

      data$Row_Numb <- NULL

      # message("Beginning with SwimmeR v0.6.0 the Grade and School output columns are renamed Age and Team respectively.  Please adjust your work flows as needed.")

      return(data)

      }
  }


#' @export

