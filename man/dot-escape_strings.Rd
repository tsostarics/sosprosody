% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_praat_arguments.R
\name{.escape_strings}
\alias{.escape_strings}
\title{Add quotes to strings for command line call}
\usage{
.escape_strings(strings)
}
\arguments{
\item{strings}{Character vector}
}
\value{
String vector, any elements containing a space have \verb{"} appended to
both sides
}
\description{
Add quotes to strings for command line call
}
