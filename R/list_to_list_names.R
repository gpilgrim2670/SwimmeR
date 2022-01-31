#' Initialize a named list of lists
#'
#' Convert a single list to a list of lists, with the names of the lists taken
#' from the original list, \code{list_of_names}.  The new lists will all have a
#' single value, initialized as \code{value}.
#'
#' @param list_of_names a list of values, likely strings, to be the names of
#'   sub-lists in a new list of lists
#' @param value a value to initialize the first element of all sub-lists to.
#'   Defaults to \code{0}
#' @return returns a list of lists with sub-list names from \code{list_of_names}
#'   and first elements from \code{value}.  Used inside \code{determine_entries}

list_to_list_names <- function(list_of_names, value = 0) {
  eval(substitute(
    list_names <-
      list_value,
    list(
      list_names = as.name(list_of_names),
      list_value = value
    )
  ))
}
