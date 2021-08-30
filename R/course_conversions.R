#' Swimming Course Convertor
#'
#' Used to convert times between Long Course Meters, Short Course Meters and
#' Short Course Yards
#'
#' @importFrom purrr map_chr
#'
#' @param time A time, or vector of times to convert.  Can be in either seconds
#'   (numeric, \code{95.97}) format or swim (character, \code{"1:35.97"}) format
#' @param event The event swum as \code{"100 Fly"}, \code{"200 IM"}, \code{"400
#'   Free"}, \code{"50 Back"}, \code{"200 Breast"} etc.
#' @param course The course in which the time was swum as \code{"LCM"},
#'   \code{"SCM"} or \code{"SCY"}
#' @param course_to The course to convert the time to as \code{"LCM"},
#'   \code{"SCM"} or \code{"SCY"}
#' @param verbose If \code{TRUE} will return a data frame containing columns \itemize{ \item
#'   Time \item Course \item Course_To \item Event \item Time_Converted_sec
#'   \item Time_Converted_mmss }.  If \code{FALSE} (the default) will return only a converted time.
#'
#' @return returns the \code{time} for a specified \code{event} and
#'   \code{course} converted to a time for the specified \code{course_to} in
#'   swimming format OR a data frame containing columns \itemize{ \item
#'   Time \item Course \item Course_To \item Event \item Time_Converted_sec
#'   \item Time_Converted_mmss } depending on the value of \code{verbose}
#'
#' @examples course_convert(time = "1:35.93", event = "200 Free", course = "SCY", course_to = "LCM")
#' course_convert(time = 95.93, event = "200 Free", course = "scy", course_to = "lcm")
#' course_convert(time = 53.89, event = "100 Fly", course = "scm", course_to = "scy")
#'
#' @note Relays are not presently supported.
#' @references Uses the USA swimming age group method described here:
#'   \url{https://support.teamunify.com/en/articles/260}
#' @export


course_convert <- function(time, event, course, course_to, verbose = FALSE) {
  x <- purrr::map(time, course_convert_helper, event = event, course = course, course_to = course_to, verbose = verbose)
  if(verbose == FALSE){
    x <- unlist(x)
  }

  #### for verbose ####
  # should return one data frame, but map will bury it as the first element of a list length 1
  # don't want a data frame in a list
  if(length(x) == 1){
    x <- x[[1]]
  }

  return(x)
}

#' Swimming Course Convertor Helper
#'
#' Used to convert times between Long Course Meters, Short Course Meters and
#' Short Course Yards
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_to_title
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_detect
#' @importFrom purrr map_dbl
#'
#' @param time A time, or vector of times to convert.  Can be in either seconds
#'   (numeric, \code{95.97}) format or swim (character, \code{"1:35.97"}) format
#' @param event The event swum as \code{"100 Fly"}, \code{"200 IM"}, \code{"400
#'   Free"}, \code{"50 Back"}, \code{"200 Breast"} etc.
#' @param course The course in which the time was swum as \code{"LCM"},
#'   \code{"SCM"} or \code{"SCY"}
#' @param course_to The course to convert the time to as \code{"LCM"},
#'   \code{"SCM"} or \code{"SCY"}
#' @param verbose If \code{TRUE} will return a data frame containing columns \itemize{ \item
#'   Time \item Course \item Course_To \item Event \item Time_Converted_sec
#'   \item Time_Converted_mmss }.  If \code{FALSE} (the default) will return only a converted time.
#'
#' @return returns the \code{time} for a specified \code{event} and
#'   \code{course} converted to a time for the specified \code{course_to} in
#'   swimming format OR a data frame containing columns \itemize{ \item
#'   Time \item Course \item Course_To \item Event \item Time_Converted_sec
#'   \item Time_Converted_mmss } depending on the value of \code{verbose}
#'
#' @seealso \code{course_convert_helper} is a helper function inside \code{\link{course_convert}}

course_convert_helper <-
  function(time, event, course, course_to, verbose = FALSE) {

    #### testing ####
    # time = "4:45.00"
    # event = "500 Free"
    # course = "SCY"
    # course_to = "LCM"
    #
    # time = 53.89
    # event = "100 Fly"
    # course = "SCY"
    # course_to = "LCM"

    #### verbose must be TRUE or FALSE ####
    if (any(is.logical(verbose) == FALSE, is.na(verbose) == TRUE)) {
      stop("verbose must be either TRUE or FALSE")
    }

    if(is.na(time) == TRUE) return(NA)

    #### event strings ####
    stroke_strings <-
      "(Freestyle)|(Free)|(Butterfly)|(Fly)|(Breaststroke)|(Breast)|(Backstroke)|(Back)|(Individual Medley)|(IM)"
    non_IM_strings <-
      "(Freestyle)|(Free)|(Butterfly)|(Fly)|(Breaststroke)|(Breast)|(Backstroke)|(Back)"
    IM_strings <- "(Individual Medley)|(IM)"

    #### Actual Function ####


    #### Regularize
    time <-
      ifelse(is.character(time) == TRUE,
             purrr::map_dbl(time, sec_format),
             time)

    course <- stringr::str_to_upper(course, locale = "en")
    if (any(course %notin% c("LCM", "SCY", "SCM")) == TRUE)
      stop("Enter a correctly formatted course")

    course_to <-
      stringr::str_to_upper(course_to, locale = "en")
    if (any(course_to %notin% c("LCM", "SCY", "SCM")) == TRUE)
      stop("Enter a correctly formatted course_to")


    event_distance <-
      as.numeric(stringr::str_extract(event, "\\d{2,4}"))


    event_stroke <-
      stringr::str_extract(event, stroke_strings)


    #### fFactor ####
    fFactor <- 1.11

    if (all((course == "LCM"),
            (course_to == "SCM")

    )) {
      fFactor <- 1.0
    }

    if (all((course == "LCM"),
            (course_to == "SCY"),
            stringr::str_detect(event, "(400.? Free)|(800.? Free)")
    )) {
      fFactor <- 0.8925
    }

    if (all((course == "LCM"),
            (course_to == "SCY"),
            stringr::str_detect(event, "1500.? Free"))) {
      fFactor <- 1.02
    }

    if (all((course == "SCY"),
            (course_to == "LCM"),
            stringr::str_detect(event, "(500.? Free)|(1000.? Free)")
    )) {
      fFactor <- 0.8925
    }

    if (all((course == "SCY"),
            (course_to == "LCM"),
            stringr::str_detect(event, "1650.? Free"))) {
      fFactor <- 1.02
    }


    #### Incre ####

    Incre <- 1

    if (all(stringr::str_detect(event_stroke, "(Butterf)?F?ly"))) {
      Incre <- 0.7
    }

    if (all(stringr::str_detect(event_stroke, "Free(style)?"))) {
      Incre <- 0.8
    }

    if (all(stringr::str_detect(Incre, "Back(stroke)?"))) {
      Incre <- 0.6
    }

    if (all(stringr::str_detect(event_stroke, "Breast(stroke)?"))) {
      # not strictly needed, default Incre is 1
      Incre <- 1.0
    }

    if (all(stringr::str_detect(event_stroke, IM_strings))) {
      #
      Incre <- 0.8
    }


    #### fIncre ####
    fIncre <- Incre

    if (all(event_distance == 50)) {
      fIncre <- Incre
    }

    if (all(event_distance == 100)) {
      fIncre <- 2 * Incre
    }

    if (all(event_distance == 200)) {
      fIncre <- 4 * Incre
    }

    if (all((event_distance == 400),
            (course == "LCM"),
            (course_to == "SCY"),
            stringr::str_detect(event_stroke, IM_strings)
    )) {
      fIncre <- 6.4
    }

    if (all((event_distance == 400),
            (course == "SCY"),
            (course_to == "LCM"),
            stringr::str_detect(event_stroke, IM_strings)
    )) {
      fIncre <- 6.4
    }

    if (all((event_distance > 200),
            (course == "LCM"),
            (course_to == "SCY"),
            stringr::str_detect(event_stroke, non_IM_strings)
    )) {
      fIncre <- 0
    }

    if (all((event_distance > 200),
            (course == "SCY"),
            (course_to == "LCM"),
            stringr::str_detect(event_stroke, non_IM_strings)
    )) {
      fIncre <- 0
    }

    if (all((event_distance %in% c(400, 500)),
            (course == "SCM"),
            (course_to == "LCM"),
            stringr::str_detect(event_stroke, non_IM_strings)
    )) {
      fIncre <- 6.4
    }

    if (all((event_distance %in% c(800, 1000)),
            (course == "SCM"),
            (course_to == "LCM"))) {
      fIncre <- 12.8
    }

    if (all((event_distance == 1500),
            (course == "SCM"),
            (course_to == "LCM"))) {
      fIncre <- 24.0
    }

    if (all((event_distance  == 400), (course == "LCM"), (course_to == "SCM"))) {
      fIncre <- 6.4
    }

    if (all((event_distance  == 800), (course == "LCM"), (course_to == "SCM"))) {
      fIncre <- 12.8
    }

    if (all((event_distance  == 1500),
            (course == "LCM"),
            (course_to == "SCM"))) {
      fIncre <- 24.0
    }

    if (all((event_distance  == 400),
            (course == "LCM"),
            (course_to == "SCY"),
            stringr::str_detect(event_stroke, IM_strings)
    )) {
      fIncre <- 6.4
    }

    fIncre <- ifelse(is.na(fIncre) == TRUE, 0, fIncre)

    #### Time_Converted_sec ####

    if (all((course == "SCY"), (course_to %in% c("LCM", "SCM")))) {
      Time_Converted_sec <- time * fFactor + fIncre
    }

    if (all((course == "LCM"), (course_to %in% c("SCY", "SCM")))) {
      Time_Converted_sec <- (time - fIncre) / fFactor
    }

    if (all((course == "SCM"), (course_to == "SCY"))) {
      Time_Converted_sec <- time / fFactor
    }

    if (all((course == "SCM"), (course_to == "LCM"))) {
      Time_Converted_sec <- time + fIncre
    }

    if (all(course == course_to)) {
      Time_Converted_sec <- time
    }

    Time_Converted_mmss <- mmss_format(Time_Converted_sec)

    Time_Converted_sec <-
      round(as.numeric(sprintf("%05.2f", Time_Converted_sec)), 2)

    time <- mmss_format(time)

    if (verbose == TRUE) {
      df <- data.frame(
        "Time" = time,
        "Course" = course,
        "Course_To" = course_to,
        "Event" = event,
        Time_Converted_sec,
        Time_Converted_mmss
      )
      return(df)
    } else {
      return(Time_Converted_mmss)
    }
  }


#' Course converter, returns data frame
#'
#' Used to convert times between Long Course Meters, Short Course Meters and
#' Short Course Yards, returns data frame
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_to_title
#' @importFrom stringr str_split_fixed
#' @importFrom purrr map_dbl
#'
#' @param time A time, or vector of times to convert.  Can be in either seconds
#'   (numeric, \code{95.97}) format or swim (character, \code{"1:35.97"}) format
#' @param event The event swum as \code{"100 Fly"}, \code{"200 IM"}, \code{"400
#'   Free"}, \code{"50 Back"}, \code{"200 Breast"} etc.
#' @param course The course in which the time was swum as \code{"LCM"},
#'   \code{"SCM"} or \code{"SCY"}
#' @param course_to The course to convert the time to as \code{"LCM"},
#'   \code{"SCM"} or \code{"SCY"}
#'
#' @return This function returns a data frame including columns: \itemize{ \item
#'   Time \item Course \item Course_To \item Event \item Time_Converted_sec
#'   \item Time_Converted_mmss }
#'
#' @examples course_convert_df(time = "1:35.93", event = "200 Free", course = "SCY", course_to = "LCM")
#' course_convert_df(time = 95.93, event = "200 Free", course = "scy", course_to = "lcm")
#' course_convert_df(time = 53.89, event = "100 Fly", course = "scm", course_to = "scy")
#'
#' @note Relays are not presently supported.
#' @references Uses the USA swimming age group method described here
#'   \url{https://support.teamunify.com/en/articles/260}
#'
#' @export

course_convert_DF <- function(time, event, course, course_to) {

  .Defunct(new = "course_convert(verbose = TRUE)", msg = "course_convert_df is defunct.  Please use course_convert(verbose = TRUE) instead.", package = "SwimmeR")

}

#' @rdname course_convert_DF
#' @export
course_convert_df <- course_convert_DF
