% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/name_reorder.R
\name{name_reorder}
\alias{name_reorder}
\title{Orders all names as Firstname Lastname}
\usage{
name_reorder(df, verbose = FALSE)
}
\arguments{
\item{df}{a data frame output from \code{swim_parse} containing a column called \code{Name} with some names as Lastname, Firstname}

\item{verbose}{defaults to \code{FALSE}.  If set to \code{TRUE} then returned data frame will include columns \code{First_Name} and \code{Last_Name} extracted based on comma position in Lastname, Firstname}
}
\value{
a data frame with a column \code{Name_Reorder} containing values from \code{Name} reordered as Firstname Lastname in addition to all other columns in input \code{df}.  Can also contain columns \code{First_Name} and \code{Last_Name} based on value of \code{verbose} argument
}
\description{
Names are sometimes listed as Firstname Lastname, and sometimes as Lastname, Firstname.  The \code{names_reorder} function converts all names to Firstname Lastname based on comma position.
The reverse, going to Lastname, Firstname is not possible because some people have multiple first names or multiple last names and without the comma to differentiate between the two a distition cannot be made.
}
\examples{
name_reorder(
data.frame(Name = c("King, Lilly", "Lilly King", NA, "Richards Ross, Sonia"))
)
name_reorder(
data.frame(Name = c("King, Lilly", "Lilly King", NA, "Richards Ross, Sonia")), verbose = TRUE)

}
\author{
Greg Pilgrim \email{gpilgrim2670@gmail.com}
}
