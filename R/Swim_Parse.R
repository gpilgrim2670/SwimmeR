#' Formats swimming and diving data read with \code{read_results} into a data
#' frame
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
#' @param file output from \code{read_results}
#' @param avoid a list of strings.  Rows in \code{file} containing these strings
#'   will not be included. For example "Pool:", often used to label pool
#'   records, could be passed to \code{avoid}.  The default is
#'   \code{avoid_default}, which contains many strings similar to "Pool:", such
#'   as "STATE:" and "Qual:".  Users can supply their own lists to \code{avoid}.
#'   \code{avoid} is handled before \code{typo} and \code{replacement}.
#' @param typo a list of strings that are typos in the original results.
#'   \code{swim_parse} is particularly sensitive to accidental double spaces, so
#'   "Central  High School", with two spaces between "Central" and "High" is a
#'   problem, which can be fixed.  Pass "Central  High School" to \code{typo}.
#'   Unexpected commas as also an issue, for example "Texas, University of"
#'   should be fixed using \code{typo} and \code{replacement}
#' @param replacement a list of fixes for the strings in \code{typo}.  Here one
#'   could pass "Central High School" (one space between "Central" and "High")
#'   and "Texas" to \code{replacement} fix the issues described in \code{typo}
#' @param format_results should the results be formatted for analysis (special
#'   strings like \code{"DQ"} replaced with \code{NA}, \code{Finals_Time} as
#'   definitive column)?  Default is \code{TRUE}
#' @param splits either \code{TRUE} or the default, \code{FALSE} - should
#'   \code{swim_parse} attempt to include splits.
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
#' @seealso \code{swim_parse} must be run on the output of
#'   \code{\link{read_results}}

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

    if(any(!is.logical(format_results) & is.na(format_results)) == TRUE) {
      stop("format_results must be logical, either TRUE or FALSE")
    }

    if(any(!is.logical(splits) & is.na(splits)) == TRUE) {
      stop("splits must be logical, either TRUE or FALSE")
    }

    if(is.numeric(split_length) == FALSE) {
      stop("split_length must be numeric, usually 50 or 25")
    }

    if(any(!is.logical(relay_swimmers) & is.na(relay_swimmers)) == TRUE) {
      stop("relay_swimmers must be logical, either TRUE or FALSE")
    }

    #### strings that if a line begins with one of them the line is ignored ####
    avoid_default <-
      c(
        # "[:upper:]\\:",
        "[A-S]\\:", # to allow EVENT:
        "[U-Z]\\:", # to allow EVENT:
        "[A-MO-Z]T\\:", # to allow EVENT:
        "[a-q]\\:", # want to make sure to include r: for reaction times in splits lines
        "[s-z]\\:", # want to make sure to include r: for reaction times in splits lines
        "[:alpha:]r\\:",
        "\\.\\:",
        "\\d\\:\\s",
        "\\'\\:",
        "QUALIFYING "
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

    #### combine avoid and avoid_default
    avoid <- unique(c(avoid, avoid_default))

    #### message only posts once per session ####
    ## removed in v0.11.0 7/14/21 ##
    # if(getOption("age_team_warning_0.6.0", TRUE)) {
    #   message("Beginning with SwimmeR v0.6.0 the Grade and School output columns are renamed Age and Team respectively.  Please adjust your work flows as needed.")
    #
    #   options("age_team_warning_0.6.0" = FALSE)
    # }

    if(stringr::str_detect(file[1], "^read_results_flag$") == TRUE){

      # remove read_results flag

      file <- file[-1]

    } else{

      # if read_results flag isn't present post an error

      stop("Please run read_results on file prior to running swim_parse.")
    }

    if (stringr::str_detect(file[1], "^A107") == TRUE) { # for .hy3 files
      # file <- add_row_numbers(text = file)
      data <- hy3_parse(file = file)
      return(data)

    } else if (any(stringr::str_detect(file[1:6], "S\\.A\\.M\\.M\\.S\\.|MEET SANCTION NUMBER")) == TRUE) { # for S.A.M.M.S files
      data <- swim_parse_samms(file_samms = file,
                          avoid_samms = avoid,
                          typo_samms = typo,
                          replacement_samms = replacement,
                          format_samms = format_results)
      return(data)

    } else if (any(stringr::str_detect(file, "(Official Timekeeping by Omega)|(Report created )")) == TRUE) {

      data <- swim_parse_omega(
        file_omega = file,
        avoid_omega = avoid,
        typo_omega = typo,
        replacement_omega = replacement,
        splits = splits,
        split_length_omega = split_length,
        relay_swimmers_omega = relay_swimmers
      )

      return(data)

    } else if ((any(stringr::str_detect(file, "Splash Meet Manager")))) {
      data <- swim_parse_splash(
        file_splash = file,
        avoid_splash = avoid,
        typo_splash = typo,
        replacement_splash = replacement,
        splits = splits,
        split_length_splash = split_length,
        relay_swimmers_splash = relay_swimmers
      )

      return(data)

    } else { # hytek files

      data <- swim_parse_hytek(
        file_hytek = file,
        avoid_hytek = avoid,
        typo_hytek = typo,
        replacement_hytek = replacement,
        splits = splits,
        split_length_hytek = split_length,
        relay_swimmers_hytek = relay_swimmers
      )

      return(data)

    }
  }



#' @rdname Swim_Parse
#' @export
swim_parse <- Swim_Parse
