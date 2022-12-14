% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_nuclear_pulses.R
\name{.get_time}
\alias{.get_time}
\title{Get word boundary time}
\usage{
.get_time(nuclear_regions, files, .which = "word_start")
}
\arguments{
\item{nuclear_regions}{Output of \code{get_nuclear_tg}}

\item{files}{Files, from within the mutate call on a grouped data frame,
will grab the first file name}

\item{.which}{Either \code{word_start} or \code{word_end}}
}
\value{
Timestamp specified by \code{.which}
}
\description{
Helper for \code{code_nuclear_pulses} to lookup nuclear word start and end
timestamps. This is called from within a \code{mutate} call.
}
