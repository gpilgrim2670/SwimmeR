#' Equation for computing swimming times from DPS, turn and start times
#'
#' Takes a numeric item or list of numeric items representing seconds (eg 95.37) and converts to a character string or list of strings in swimming format ("1:35.37").
#'
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param Swim_Time total time for a swim
#' @param Reaction_Time time leaving the blocks
#' @param Underwater_Time time spent underwater (dolphin kicking, pullouts, etc)
#' @param Turn_Time time spent completing turns
#' @param Cycle_Count total cycles
#' @param Stroke_Rate time per cycle
#' @return whichever value is missing, solved for with respect to those provided
#'
#' @examples
#'
#' @export
#'
#' @references from \url{https://www.usaswimming.org/docs/default-source/clinics/regional-coach-clinics/pittsburgh-rcc/race-stats---every-coach-can-use-everyday-for-every-athlete-rcc.pdf}
#'
Swim_Equation <- function(Swim_Time, Reaction_Time, Underwater_Time, Turn_Time, Cycle_Count, Stroke_Rate) {
  if(sum(missing(Swim_Time) + missing(Reaction_Time) + missing(Underwater_Time) + missing(Turn_Time) + missing(Cycle_Count) + missing(Stroke_Rate)) > 1) stop("Please enter 5 variables")
  if(missing(Swim_Time)) {
  ST <- (Reaction_Time + Underwater_Time + Turn_Time) + (Cycle_Count * Stroke_Rate)
  return(ST)
  } else if (missing(Reaction_Time)) {
    RT <- (Swim_Time - Turn_Time - Underwater_Time - (Cycle_Count * Stroke_Rate))
    return(RT)
  } else if (missing(Underwater_Time)) {
    UT <- (-Reaction_Time + Swim_Time - Turn_Time - (Cycle_Count * Stroke_Rate))
    return(UT)
  } else if (missing(Turn_Time)) {
    TT <- (-Reaction_Time + Swim_Time - Underwater_Time - (Cycle_Count * Stroke_Rate))
    return(TT)
  } else if (missing(Cycle_Count)) {
    CC <- (-(Reaction_Time - Swim_Time + Turn_Time + Underwater_Time)/Stroke_Rate)
    return(CC)
  } else if (missing(Stroke_Rate)){
    SR <- (-(Reaction_Time - Swim_Time + Turn_Time + Underwater_Time)/Cycle_Count)
    return(SR)
  }
}
# https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions
