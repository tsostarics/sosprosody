% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_praatscript_arguments.R
\name{get_praatscript_arguments}
\alias{get_praatscript_arguments}
\title{Extract arguments from praat script form}
\usage{
get_praatscript_arguments(script_path)
}
\arguments{
\item{script_path}{Path to the script}
}
\value{
Dataframe with \code{datatype}, \code{varname}, and \code{default_value} columns
}
\description{
Arguments in a praat script are specified in the form. This is the top-most
portion of a script that starts with \code{form} and ends with \code{endform}.
Between these, the argument names and types are declared, along with
an optional default value. This function will read the form section of the
script and return a dataframe containing the form arguments, their datatype,
and their default value. Lines with comments will be removed.
All values will be coerced to the \code{character} R datatype.
If a default value is not found, \code{NA} will be returned.
The order of the variables is shown in the order they appear in the script.
}
\details{
See https://www.fon.hum.uva.nl/praat/manual/Scripting_6_1__Arguments_to_the_script.html
for more information on Praat data types.
}
