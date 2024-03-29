% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splits_to_lap.R
\name{splits_to_lap_helper_recalc}
\alias{splits_to_lap_helper_recalc}
\title{Helper function for converting cumulative splits to lap splits}
\usage{
splits_to_lap_helper_recalc(
  df,
  i,
  split_cols = split_cols,
  threshold = threshold
)
}
\arguments{
\item{df}{a data frame containing splits in cumulative format}

\item{i}{list of values to iterate along}

\item{split_cols}{list of columns containing splits}

\item{threshold}{a numeric value above which a split is taken to be
cumulative}
}
\value{
a list of data frames with all splits in lap format for a particular
  event, each with a single split column converted to lap format
}
\description{
Helper function for converting cumulative splits to lap splits
}
