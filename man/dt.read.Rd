% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dt.R
\name{dt.read}
\alias{dt.read}
\title{dt.read}
\usage{
dt.read(fileNameDurationTier, encoding = "UTF-8")
}
\arguments{
\item{fileNameDurationTier}{file name of DurationTier}

\item{encoding}{File encoding (default: \code{"UTF-8"}), \code{"auto"} for auto-detect of Unicode encoding}
}
\value{
DurationTier object
}
\description{
Reads DurationTier from Praat. Supported formats: text file, short text file,
spreadsheet, headerless spreadsheet (headerless not recommended,
it does not contain tmin and tmax info).
}
\details{
Note: All credit for these functions go to Bořil, T., & Skarnitzl, R. (2016)
I needed to read in duration tiers, which have the same exact format as
pitch tiers, so all I really needed was to find-and-replace "PitchTier" with
"DurationTier" from here:
https://github.com/bbTomas/rPraat/blob/master/R/pt.R
}
\examples{
\dontrun{
dt <- dt.read("demo/H.DurationTier")
}
}
