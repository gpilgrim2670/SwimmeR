#' Orders all names as "Firstname Lastname"
#'
#' Names are sometimes listed as Firstname Lastname, and sometimes as Lastname, Firstname.  The \code{names_reorder} function converts all names to Firstname Lastname based on comma position.
#' The reverse, going to Lastname, Firstname is not possible because some athletes have multiple first names or multiple last names and without the comma to differentiate between the two a distinction cannot be made.
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr na_if
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom stringr str_split_fixed
#'
#' @export
#'
#' @examples
#' name_reorder(
#' data.frame(Name = c("King, Lilly", "Lilly King", NA, "Richards Ross, Sanya", "Phelps, Michael F"))
#' )
#' name_reorder(
#' data.frame(Name = c("King, Lilly", "Lilly King", NA, "Richards Ross, Sanya")), verbose = TRUE)
#'
#' @param df a data frame output from \code{swim_parse} containing a column called \code{Name} with some names as Lastname, Firstname
#' @param verbose defaults to \code{FALSE}.  If set to \code{TRUE} then returned data frame will include columns \code{First_Name} and \code{Last_Name} extracted based on comma position in Lastname, Firstname
#' @return a data frame with a column \code{Name_Reorder} containing values from \code{Name} reordered as Firstname Lastname in addition to all other columns in input \code{df}.  Can also contain columns \code{First_Name} and \code{Last_Name} based on value of \code{verbose} argument

name_reorder <- function(df, verbose = FALSE) {

  df <- df %>%
    dplyr::mutate(Name = stringr::str_remove(Name, "^\\*")) %>%
    # dplyr::mutate(Name_2 = stringr::str_replace(Name, " [:upper:]$", "")) %>% # removes middle initials
    dplyr::mutate(Last_Name = dplyr::case_when(# pulls out last name when it's followed by a comma
      stringr::str_detect(Name, ",") ~ stringr::str_split_fixed(Name, ",", n = 2)[, 1],
      TRUE ~ ""
    )) %>%
    dplyr::na_if("") %>%
    dplyr::mutate(First_Name = dplyr::case_when(
      is.na(Last_Name) == FALSE ~ stringr::str_remove(Name, paste0(Last_Name, ",", " ")), # gets first name if last name was identified above
      TRUE ~ ""
    )) %>%
    dplyr::na_if("") %>%
    dplyr::mutate(Name_3 = dplyr::case_when( # recombines first names and last names identified above into Firstname Lastname order
      is.na(First_Name) == FALSE &
        is.na(Last_Name) == FALSE ~ paste(First_Name, Last_Name, sep = " "),
      TRUE ~ Name # names not identified above are already in Firstname Lastname order
    ))

  if (verbose == TRUE) {
    df <- df %>%
    dplyr::select("Name_Reorder" = Name_3,
                  First_Name,
                  Last_Name,
                  dplyr::everything()
    )
  } else {
    df <- df %>%
    dplyr::select("Name_Reorder" = Name_3,
                  dplyr::everything(),
                  -First_Name,
                  -Last_Name)
  }

  return(df)

}
