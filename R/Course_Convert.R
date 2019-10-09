utils::globalVariables(c("fFactor", "fIncre", "Time_Converted_sec"))
#' Swimming Course Convertor
#'
#' Used to convert times between Long Course Meters, Short Course Meters and Short Course Yards
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @import tibble
#' @import dplyr
#' @import stringr
#' @import purrr
#'
#' @param time A time, or vector of times to convert.  Can be in either seconds (numeric, \code{95.97}) format or swim (character, \code{"1:35.97"}) format
#' @param event The event swum as \code{"100 Fly"}, \code{"200 IM"}, \code{"400 Free"}, \code{"50 Back"}, \code{"200 Breast"} etc.
#' @param course The course in which the time was swum as \code{"LCM"}, \code{"SCM"} or \code{"SCY"}
#' @param course_to The course to convert the time to as \code{"LCM"}, \code{"SCM"} or \code{"SCY"}
#' @return returns the \code{time} for a specified \code{event} and \code{course} converted to a time for the specified \code{course_to} in swimming format
#'
#' @examples course_convert(time = "1:35.93", event = "200 Free", course = "SCY", course_to = "LCM")
#' course_convert(time = 95.93, event = "200 Free", course = "scy", course_to = "lcm")
#' course_convert(time = 53.89, event = "100 Fly", course = "scm", course_to = "scy")
#'
#' @note Relays are not presently supported.
#' @references Uses the USA swimming age group method described here: \url{https://support.teamunify.com/en/articles/260}
#'
#' @export

course_convert <- function(time, event, course, course_to) {
  `%notin%` <- Negate(`%in%`)
  Swim <- tibble(time, course, course_to, event)
  Swim$time <- ifelse(is.character(Swim$time) == TRUE, map_dbl(Swim$time, sec_format), Swim$time)
  Swim$course <- str_to_upper(Swim$course, locale = "en")
  if(any(Swim$course %notin% c("LCM", "SCY", "SCM")) == TRUE) stop("Enter a correctly formatted course")
  Swim$course_to <- str_to_upper(Swim$course_to, locale = "en")
  if(any(Swim$course_to %notin% c("LCM", "SCY", "SCM")) == TRUE) stop("Enter a correctly formatted course_to")
  Swim$event_distance <- as.numeric(str_split_fixed(Swim$event, " ", n = 2)[,1])
  Swim$event_stroke <- str_split_fixed(Swim$event, " ", n = 2)[,2]
  Swim$event_stroke <- str_to_title(Swim$event_stroke, locale = "en")
  if(Swim$event_stroke %notin% c("Free", "Fly", "Back", "Breast", "Im") == TRUE) stop("Enter a correct swimming stroke")
  Swim <- Swim %>%
    mutate(
      fFactor = 1,
      fFactor = case_when(
        course == "LCM" & course_to == "SCM" ~ 1,
        course == "LCM" &
          course_to == "SCY" & event %in% c("400 Free", "800 Free") ~ 0.8925,
        course == "LCM" &
          course_to == "SCY" & event == "1500 Free" ~ 1.02,
        course == "SCY" &
          course_to == "LCM" &
          event %in% c("500 Free", "1000 Free") ~ 0.8925,
        course == "SCY" &
          course_to == "LCM" & event == "1650 Free" ~ 1.02),
      fFactor = ifelse(is.na(fFactor) == TRUE, 1.11, fFactor),
      Incre = case_when(event_stroke == "Fly" ~ .7,
                        event_stroke == "Back" ~ .6,
                        event_stroke == "Breast" ~ 1.0,
                        event_stroke == "Free" ~ .8,
                        event_stroke == "IM" ~ .8),
      fIncre = case_when(event_distance == 50 ~ Incre,
                         event_distance == 100 ~ 2*Incre,
                         event_distance == 200 ~ 4*Incre,
                         # event_distance == 400 ~ 8*Incre,
                         event_distance == 400 & event_stroke == "IM" & course == "LCM" & course_to == "SCY" ~ 6.4,
                         event_distance == 400 & event_stroke == "IM" & course == "SCY" & course_to == "LCM" ~ 6.4,
                         event_distance > 200 & event_stroke %in% c("Free", "Fly", "Back", "Breast") & course == "LCM" & course_to == "SCY" ~ 0,
                         event_distance > 200 & event_stroke %in% c("Free", "Fly", "Back", "Breast") & course == "SCY" & course_to == "LCM" ~ 0,
                         event_distance %in% c(400, 500) & course == "SCM" & course_to == "LCM" ~ 6.4,
                         event_distance %in% c(800, 1000) & course == "SCM" & course_to == "LCM" ~ 12.8,
                         event_distance %in% c(1500, 1650) & course == "SCM" & course_to == "LCM" ~ 24.0,
                         event_distance %in% c(400, 500) & course == "LCM" & course_to == "SCM" ~ 6.4,
                         event_distance %in% c(800, 1000) & course == "LCM" & course_to == "SCM" ~ 12.8,
                         event_distance %in% c(1500, 1650) & course == "LCM" & course_to == "SCM" ~ 24.0,
                         event_stroke == "IM" & event_distance == 400 & course == "LCM" & course_to == "SCY" ~ 6.4),
      fIncre = ifelse(is.na(fIncre) == TRUE, 0, fIncre),
      Time_Converted_sec = case_when(course == "SCY" & course_to %in% c("LCM", "SCM") ~ time * fFactor + fIncre,
                                 course == "LCM" & course_to %in% c("SCY", "SCM") ~ (time-fIncre)/fFactor,
                                 course == "SCM" & course_to == "SCY" ~ time/fFactor,
                                 course == "SCM" & course_to == "LCM" ~ time + fIncre,
                                 course == course_to ~ time),
      Time_Converted_mmss = mmss_format(Time_Converted_sec),
      Time_Converted_sec = round(as.numeric(sprintf("%05.2f", Time_Converted_sec)), 2),
      time = mmss_format(time)
    )


  return(Swim$Time_Converted_mmss)
}
