#' Course converter
#'
#' Used to convert times between Long Course Meters, Short Course Meters and Short Course Yards
#'
#' @author Greg Pilgrim
#'
#' @import dplyr
#' @import stringr
#'
#' @param time A time, or vector of times to convert
#' @param course The course in which the time was swum
#' @param course_to The course to convert the time to
#' @param event The event swum as "100 Fly", "200 IM", "400 Free", "50 Back", "200 Breast" etc.
#'
#' @export

# https://support.teamunify.com/en/articles/260

# Returns only converted time
Course_Convert <- function(time, course, course_to, event) {
  Swim <- tibble(time, course, course_to, event, stringsAsFactors = FALSE)
  Swim$course <- str_to_upper(course, locale = "en")
  Swim$course_to <- str_to_upper(course_to, locale = "en")
  Swim$event_distance <- as.numeric(str_split_fixed(Swim$event, " ", n = 2)[,1])
  Swim$event_stroke <- str_split_fixed(Swim$event, " ", n = 2)[,2]
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
                         event_distance == 400 ~ 8*Incre,
                         (event_distance == 500 & course == "SCM" & course_to == "LCM") ~ 6.4,
                         event_distance == 1000 & course == "SCM" & course_to == "LCM" ~ 12.8,
                         event_distance == 1650 & course == "SCM" & course_to == "LCM" ~ 24.0,
                         event_distance == 400 & course == "SCM" & course_to == "LCM" ~ 6.4,
                         event_distance == 800 & course == "SCM" & course_to == "LCM" ~ 12.8,
                         event_distance == 1500 & course == "SCM" & course_to == "LCM" ~ 24.0,
                         event_stroke == "IM" & event_distance == 400 & course == "LCM" & course_to == "SCY" ~ 6.4),
      fIncre = ifelse(is.na(fIncre) == TRUE, 0, fIncre),
      Time_Converted = case_when(course == "SCY" & course_to %in% c("LCM", "SCM") ~ time * fFactor + fIncre,
                                 course == "LCM" & course_to %in% c("SCY", "SCM") ~ (time-fIncre)/fFactor,
                                 course == "SCM" & course_to == "SCY" ~ time/fFactor,
                                 course == "SCM" & course_to == "LCM" ~ time + fIncre,
                                 course == course_to ~ time)

    )

  return(Swim %>%
           select(Time_Converted))
}

# Returns dataframe of converted time, event, old time
Course_Convert_DF <- function(time, course, course_to, event) {
  Swim <- data.frame(time, course, course_to, event, stringsAsFactors = FALSE)
  Swim$event_distance <- as.numeric(str_split_fixed(event, "-", n = 2)[,1])
  Swim$event_stroke <- str_split_fixed(event, "-", n = 2)[,2]
}

LK_Swim <- data.frame(Lilly_King$Time, Lilly_King$Event, "SCY", "LCM", stringsAsFactors = FALSE)
LK_Swim$event_distance <- as.numeric(str_split_fixed(LK_Swim$Lilly_King.Event, " ", n = 2)[,1])
LK_Swim$event_stroke <- as.numeric(str_split_fixed(LK_Swim$Lilly_King.Event, " ", n = 2)[,2])


Swim <- tibble(time = 59.65, course = "LCM", course_to = "SCY", event = "100 Fly", stringsAsFactors = FALSE)
Swim$event_distance <- as.numeric(str_split_fixed(Swim$event, " ", n = 2)[,1])
Swim$event_stroke <- str_split_fixed(Swim$event, " ", n = 2)[,2]
Swim %>%
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
                       event_distance == 400 ~ 8*Incre,
                       (event_distance == 500 & course == "SCM" & course_to == "LCM") ~ 6.4,
                       event_distance == 1000 & course == "SCM" & course_to == "LCM" ~ 12.8,
                       event_distance == 1650 & course == "SCM" & course_to == "LCM" ~ 24.0,
                       event_distance == 400 & course == "SCM" & course_to == "LCM" ~ 6.4,
                       event_distance == 800 & course == "SCM" & course_to == "LCM" ~ 12.8,
                       event_distance == 1500 & course == "SCM" & course_to == "LCM" ~ 24.0,
                       event_stroke == "IM" & event_distance == 400 & course == "LCM" & course_to == "SCY" ~ 6.4),
    fIncre = ifelse(is.na(fIncre) == TRUE, 0, fIncre),
    Time_Converted = case_when(course == "SCY" & course_to %in% c("LCM", "SCM") ~ time * fFactor + fIncre,
                               course == "LCM" & course_to %in% c("SCY", "SCM") ~ (time-fIncre)/fFactor,
                               course == "SCM" & course_to == "SCY" ~ time/fFactor,
                               course == "SCM" & course_to == "LCM" ~ time + fIncre,
                               course == course_to ~ time)

  )
