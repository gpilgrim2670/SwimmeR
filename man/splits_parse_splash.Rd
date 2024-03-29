% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splash_splits_parse.R
\name{splits_parse_splash}
\alias{splits_parse_splash}
\title{Collects splits within \code{swim_parse_splash} for Splash results}
\usage{
splits_parse_splash(raw_results)
}
\arguments{
\item{raw_results}{output of \code{read_results} with row numbers appended by
\code{add_row_numbers}}
}
\value{
returns a data frame with split times and row numbers
}
\description{
Takes the output of \code{read_results} and, inside of
\code{swim_parse_splash}, extracts split times and associated row numbers
}
\seealso{
\code{splits_parse} runs inside \code{\link{swim_parse_splash}} on
  the output of \code{\link{read_results}} with row numbers from
  \code{\link{add_row_numbers}}
}
