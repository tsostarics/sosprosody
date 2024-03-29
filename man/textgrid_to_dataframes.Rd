% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textgrid_to_dataframes.R
\name{textgrid_to_dataframes}
\alias{textgrid_to_dataframes}
\title{Convert textgrid object to list of dataframes}
\usage{
textgrid_to_dataframes(textgrid, .remove_filename = TRUE)
}
\arguments{
\item{textgrid}{TextGrid object from \code{tg.read}}

\item{.remove_filename}{Logical, whether to remove the \code{.TextGrid} part from
the file name}
}
\value{
List of dataframes for each tier
}
\description{
Given a textgrid object, return a list of dataframes.
Each entry in the list is named according to the tier name.
}
