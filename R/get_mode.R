#' Find the mode (most commonly occurring) element of a list
#'
#' Determines which element of list appears most frequently.  Based on \code{base::which.max}, so if multiple values appear with the same frequency will return the first one.  Ignores \code{NA} values.
#' In the context of swimming data is often used to clean team names, as in the Lilly King example below.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param x A list
#' @return the element of \code{x} which appears most frequently.  Ties go to the lowest index, so the element which appears first.
#'
#' @examples a <- c("a", "a", "b", "c")
#' get_mode(a)
#' ab <- c("a", "a", "b", "b", "c") # returns "a", not "b"
#' get_mode(ab)
#' a_na <- c("a", "a", NA, NA, "c")
#' get_mode(a_na)
#' numbs <- c(1, 1, 2, 2, 2, 3, NA)
#' get_mode(numbs)
#'
#' Name <- c(rep("Lilly King", 5))
#' Team <- c(rep("IU", 2), "Indiana", "IUWSD", "Indiana University")
#' df <- data.frame(Name, Team, stringsAsFactors = FALSE)
#' df$Team <- get_mode(df$Team)
#'
#' @export

get_mode <- function(x) {
  unique_x <- unique(x[!is.na(x)])
  unique_x[which.max(tabulate(match(x, unique_x)))]
}



