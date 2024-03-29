% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/omega_swim_parse.R
\name{swim_parse_omega}
\alias{swim_parse_omega}
\title{Formats Omega style swimming and diving data read with \code{read_results}
into a data frame}
\usage{
swim_parse_omega(
  file_omega,
  avoid_omega = avoid,
  typo_omega = typo,
  replacement_omega = replacement,
  format_results = TRUE,
  splits = FALSE,
  split_length_omega = split_length,
  relay_swimmers_omega = relay_swimmers
)
}
\arguments{
\item{file_omega}{output from \code{read_results}}

\item{avoid_omega}{a list of strings.  Rows in \code{file_omega} containing
these strings will not be included. For example "Pool:", often used to
label pool records, could be passed to \code{avoid_omega}.  The default is
\code{avoid_default}, which contains many strings similar to "Pool:", such
as "STATE:" and "Qual:".  Users can supply their own lists to
\code{avoid_omega}. \code{avoid_omega} is handled before \code{typo_omega}
and \code{replacement_omega}.}

\item{typo_omega}{a list of strings that are typos in the original results.
\code{swim_parse} is particularly sensitive to accidental double spaces, so
"Central  High School", with two spaces between "Central" and "High" is a
problem, which can be fixed.  Pass "Central  High School" to
\code{typo_omega}. Unexpected commas as also an issue, for example "Texas,
University of" should be fixed using \code{typo_omega} and
\code{replacement_omega}}

\item{replacement_omega}{a list of fixes for the strings in
\code{typo_omega}.  Here one could pass "Central High School" (one space
between "Central" and "High") and "Texas" to \code{replacement_omega} fix
the issues described in \code{typo_omega}}

\item{format_results}{should the results be formatted for analysis (special
strings like \code{"DQ"} replaced with \code{NA}, \code{Finals} as
definitive column)?  Default is \code{TRUE}}

\item{splits}{either \code{TRUE} or the default, \code{FALSE} - should
\code{swim_parse} attempt to include splits.}

\item{split_length_omega}{either \code{25} or the default, \code{50}, the
length of pool at which splits are recorded.  Not all results are
internally consistent on this issue - some have races with splits by 50 and
other races with splits by 25.}

\item{relay_swimmers_omega}{should names of relay swimmers be captured?
Default is \code{FALSE}}
}
\value{
returns a data frame with columns \code{Name}, \code{Place},
  \code{Age}, \code{Team}, \code{Prelims}, \code{Finals},
  \code{Points}, \code{Event} & \code{DQ}.  Note all swims will have a
  \code{Finals}, even if that time was actually swam in the prelims
  (i.e. a swimmer did not qualify for finals).  This is so that final results
  for an event can be generated from just one column.
}
\description{
Takes the output of \code{read_results} and cleans it, yielding a data frame
of swimming (and diving) results
}
\seealso{
\code{swim_parse_omega} must be run on the output of
  \code{\link{read_results}}
}
