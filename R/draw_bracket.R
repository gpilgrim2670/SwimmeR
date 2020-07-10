#' Creates a bracket for tournaments involving 5 to 64 teams, single elimination
#'
#' Will draw a single elimination bracket for the appropriate number of teams, inserting first round byes for higher seeds as needed
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @export
#'
#' @importFrom stringr str_pad
#' @importFrom stringr str_length
#'
#' @param teams a list of teams, ordered by desired seed, to place in bracket.  Must be between 5 and 64 inclusive.  Teams must have unique names
#' @param title bracket title
#' @param text_size number passed to \code{cex} in plotting
#' @param round_two a list of teams advancing to the second round (need not be in order)
#' @param round_three a list of teams advancing to the third round (need not be in order)
#' @param round_four a list of teams advancing to the forth round (need not be in order)
#' @param round_five a list of teams advancing to the fifth round (need not be in order)
#' @param round_six a list of teams advancing to the fifth round (need not be in order)
#' @param champion the name of the overall champion team (there can be only one)
#' @return a plot of a bracket for the teams, with results and titles as specified
#'
#'
#' @examples \dontrun{
#' teams <- c("red", "orange", "yellow", "green", "blue", "indigo", "violet")
#' round_two <- c("red", "yellow", "blue", "indigo")
#' round_three <- c("red", "blue")
#' champion <- "red"
#' draw_bracket(teams = teams,
#'              round_two = round_two,
#'              round_three = round_three,
#'              champion = champion)
#'}
#'
#'@references based on \code{draw.bracket} from the seemingly now defunct \code{mRchmadness} package by Eli Shayer and Saber Powers and used per the terms of that package's GPL-2 license

draw_bracket <- function(teams,
                        title = "Championship Bracket",
                        text_size = 0.7,
                        round_two = NULL,
                        round_three = NULL,
                        round_four = NULL,
                        round_five = NULL,
                        round_six = NULL,
                        champion = NULL) {

  # Sanitize inputs

  # text_size as numeric
  if(is.numeric(text_size) == FALSE){
    stop("text_size must be numeric")
  }

  # too many or too few teams
  if(length(teams) < 5) {
    stop("length(teams) must be greater than or equal to 5")
  }
  if(length(teams) > 64) {
    stop("length(teams) must be less than or equal to 64")
  }
  # teams must have unique names
  if(length(teams) != length(unique(teams))){
    stop("Each element of teams must be unique")
  }
  if(!is.null(champion) & length(champion) != 1){
    stop("There can only be one champion")
  }


  teams = as.character(teams)

  # determine length of teams, number of seeds
  viable_numbers <- c(8, 16, 32, 64)

  seed <- seq(1:min(viable_numbers[viable_numbers >= length(teams)]))


  # seed <- if (length(teams) %in% viable_numbers) {
  #   seq(1:length(teams))
  # } else {
  #   rep(1:viable_numbers[base::findInterval(length(teams), viable_numbers) + 1])
  # }

  teams <- c(teams, rep("bye", length(seed) - length(teams)))

  # Errors in number of teams advancing
  if(!is.null(round_two) & length(round_two) != length(teams)/2) {
    stop("length(round_two) must be half of length(teams)")
  }
  if(!is.null(round_three) & length(round_three) != length(teams)/4) {
    stop("length(round_three) must be one quarter of length(teams)")
  }
  if(!is.null(round_four) & length(round_four) != length(teams)/8) {
    stop("length(round_four) must be one eighth of length(teams)")
  }
  if(!is.null(round_five) & length(round_five) != length(teams)/16) {
    stop("length(round_five) must be one sixteenth of length(teams)")
  }
  if(!is.null(round_six) & length(round_six) != length(teams)/32) {
    stop("length(round_six) must be one thirty-second of length(teams)")
  }

  names(seed) <- teams
  names(teams) <- teams
  seed_folded <- seed %>% fold(1) %>% fold(2) %>% fold(4)


  # convert initial team placement from seed order to matchup order and add seed to text string
  teams <- teams %>% fold(1) %>% fold(2) %>%  fold(4)
  teams[1:(length(teams)/2)] <- paste(seed_folded[1:(length(teams)/2)],  teams[1:(length(teams)/2)], sep = " ")
  teams[(1 + length(teams)/2):length(teams)] <- paste(teams[(1 + length(teams)/2):length(teams)], seed_folded[(1 + length(teams)/2):length(teams)],  sep = " ")
  teams[(1 + length(teams)/2):length(teams)] <-
    str_pad(teams[(1 + length(teams)/2):length(teams)],
            max(str_length(teams[(1 + length(teams)/2):length(teams)])),
            side = c("left"),
            pad = " ")

  # x and y coordinates for centers of all horizontal lines in bracket plot
  if (length(teams) == 2) {
    x <- c(-1, 1, 0)
    y <- c(3 / 5, 2 / 5, 1 / 2)
  } else if (length(teams) == 4) {
    x <- c(c(-2, -2), 2, 2, -1, 1, 0)
    y <- c(rep(c(3 / 4, 1 / 4), 2), 3 / 5, 2 / 5, 1 / 2)
  } else if (length(teams) == 8) {
    x <- c(rep(-3, 4), rep(3, 4), c(-2, -2), 2, 2, -1, 1, 0)
    y <- c(rep(seq(7 / 8, 1 / 8,-1 / 4), 2),
           rep(c(3 / 4, 1 / 4), 2), 3 / 5, 2 / 5, 1 / 2)
  } else if (length(teams) == 16) {
    x = c(
      rep(-4, 8),
      rep(4, 8),
      rep(-3, 4),
      rep(3, 4),
      c(-2,-2),
      2,
      2,
      -1,
      1,
      0
    )
    y = c(
          rep(seq(15 / 16, 1 / 16,-1 / 8), 2),
          rep(seq(7 / 8, 1 / 8,-1 / 4), 2),
          rep(c(3 / 4, 1 / 4), 2),
          3 / 5,
          2 / 5,
          1 / 2)
  } else if (length(teams) == 32) {
    x = c(
      rep(-5, 16),
      rep(5, 16),
      rep(-4, 8),
      rep(4, 8),
      rep(-3, 4),
      rep(3, 4),
      c(-2,-2),
      2,
      2,
      -1,
      1,
      0
    )
    y = c(
          rep(seq(31 / 32, 1 / 32,-1 / 16), 2),
          rep(seq(15 / 16, 1 / 16,-1 / 8), 2),
          rep(seq(7 / 8, 1 / 8,-1 / 4), 2),
          rep(c(3 / 4, 1 / 4), 2),
          3 / 5,
          2 / 5,
          1 / 2)

  } else if (length(teams) == 64) {
    x = c(
      rep(-6, 32),
      rep(6, 32),
      rep(-5, 16),
      rep(5, 16),
      rep(-4, 8),
      rep(4, 8),
      rep(-3, 4),
      rep(3, 4),
      c(-2,-2),
      2,
      2,
      -1,
      1,
      0
    )
    y = c(rep(seq(63 / 64, 1 / 64,-1 / 32), 2),
          rep(seq(31 / 32, 1 / 32,-1 / 16), 2),
          rep(seq(15 / 16, 1 / 16,-1 / 8), 2),
          rep(seq(7 / 8, 1 / 8,-1 / 4), 2),
          rep(c(3 / 4, 1 / 4), 2),
          3 / 5,
          2 / 5,
          1 / 2)
  }

  if(length(teams) == 2){
    y_pad <- 0.06
  } else if (length(teams) == 4){
    y_pad <- 0.04
  } else if (length(teams) == 8){
    y_pad <- 0.03
  } else if (length(teams) == 16){
    y_pad <- 0.025
  } else if (length(teams) == 32){
    y_pad <- 0.02
  } else if (length(teams) == 64){
    y_pad <- 0.015
  }

  graphics::plot(NA, xlim = c(-7, 7), ylim = 0:1, xlab = '', ylab = '',
                 axes = FALSE)

  # horizontal line segments
  graphics::segments(x - 1 / 2, y, x + 1 / 2, y)

  # vertical line segments
  graphics::segments((x + (x < 0) - 1 / 2)[seq(1, length(x) - 3, 2)],
                     y[seq(1, length(y) - 3, 2)],
                     (x + (x < 0) - 1 / 2)[seq(2, length(x) - 3, 2)],
                     y[seq(2, length(y) - 3, 2)])

  # fill in initial seeding
  graphics::text(x[1:(length(teams)/2)] - 0.46, y[1:(length(teams)/2)] + y_pad, teams[1:(length(teams)/2)], cex = text_size,
                 adj = 0)
  graphics::text(x[(1 + length(teams)/2):length(teams)] - 0.15, y[(1 + length(teams)/2):length(teams)] + y_pad, teams[(1 + length(teams)/2):length(teams)], cex = text_size,
                 adj = 0)

  if (!is.null(round_two)) {
    # fill in round two results
    graphics::text(x[(length(teams) + 1):(length(teams) + length(teams)/4)] - 0.3,
                   y[(length(teams) + 1):(length(teams) + length(teams)/4)] + y_pad,
                   teams[names(teams) %in% round_two][1:(length(round_two)/2)],
                   cex = text_size,
                   adj = 0)

    graphics::text(x[(length(teams) + length(teams)/4 + 1):(length(teams) + length(teams)/2)] - 0.35,
                   y[(length(teams) + length(teams)/4 + 1):(length(teams) + length(teams)/2)] + y_pad,
                   teams[names(teams) %in% round_two][(1 + length(round_two)/2):length(round_two)],
                   cex = text_size,
                   adj = 0)
  }

  if (!is.null(round_three) & length(teams) %in% c(8, 16, 32, 64)) {
    # fill in round three results
    graphics::text(x[(length(teams) + length(teams)/2 + 1):(length(teams) + (5/8)*length(teams))] - 0.2,
                   y[(length(teams) + length(teams)/2 + 1):(length(teams) + (5/8)*length(teams))] + y_pad,
                   teams[names(teams) %in% round_three][1:(length(round_three)/2)],
                   cex = text_size,
                   adj = 0)

    graphics::text(x[(1 + length(teams) +  (5/8)*length(teams)):(length(teams) + (3/4)*length(teams))] - 0.3,
                   y[(1 + length(teams) +  (5/8)*length(teams)):(length(teams) + (3/4)*length(teams))] + y_pad,
                   teams[names(teams) %in% round_three][(1 + length(round_three)/2):length(round_three)],
                   cex = text_size,
                   adj = 0)
  }

  if (!is.null(round_four) & length(teams) %in% c(16, 32, 64)) {
    # fill in round four results
    graphics::text(x[(1 + length(teams) + (3/4)*length(teams)):(length(teams) + (13/16)*length(teams))] - 0.2,
                   y[(1 + length(teams) + (3/4)*length(teams)):(length(teams) + (13/16)*length(teams))] + y_pad,
                   teams[names(teams) %in% round_four][1:(length(round_four)/2)],
                   cex = text_size,
                   adj = 0)

    graphics::text(x[(1 + length(teams) + (13/16)*length(teams)):(length(teams) + (7/8)*length(teams))] - 0.3,
                   y[(1 + length(teams) + (13/16)*length(teams)):(length(teams) + (7/8)*length(teams))] + y_pad,
                   teams[names(teams) %in% round_four][(1 + length(round_four)/2):length(round_four)],
                   cex = text_size,
                   adj = 0)
  }

  if (!is.null(round_five) & length(teams) %in% c(32, 64)) {
    # fill in round five results
    graphics::text(x[(1 + length(teams) + (7/8)*length(teams)):(length(teams) + (29/32)*length(teams))] - 0.2,
                   y[(1 + length(teams) + (7/8)*length(teams)):(length(teams) + (29/32)*length(teams))] + y_pad,
                   teams[names(teams) %in% round_five][1:(length(round_five)/2)],
                   cex = text_size,
                   adj = 0)

    graphics::text(x[(1+ length(teams) + (29/32)*length(teams)):(length(teams) + (15/16)*length(teams))] - 0.3,
                   y[(1+ length(teams) + (29/32)*length(teams)):(length(teams) + (15/16)*length(teams))] + y_pad,
                   teams[names(teams) %in% round_five][(1 + length(round_five)/2):length(round_five)],
                   cex = text_size,
                   adj = 0)
  }

  if (!is.null(round_six) & length(teams) == 64) {
    # fill in round six results
    graphics::text(x[(2 * length(teams) - 3)] - 0.2,
                   y[(2 * length(teams) - 3)] + y_pad,
                   teams[names(teams) %in% round_six][1:(length(round_six)/2)],
                   cex = text_size,
                   adj = 0)

    graphics::text(x[2 * length(teams) - 2] - 0.3,
                   y[2 * length(teams) - 2] + y_pad,
                   teams[names(teams) %in% round_six][(1 + length(round_six)/2):length(round_six)],
                   cex = text_size,
                   adj = 0)
  }

  if (!is.null(champion)) {
    # fill in champion
    graphics::text(x[2 * length(teams) - 1] - 0.25,
                   y[2 * length(teams) - 1] + y_pad,
                   teams[names(teams) %in% champion][1],
                   cex = text_size,
                   adj = 0)
  }
  graphics::title(as.character(title[1]))
}
