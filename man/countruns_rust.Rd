% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extendr-wrappers.R
\name{countruns_rust}
\alias{countruns_rust}
\title{Get a summary of runs analysis using rust code}
\usage{
countruns_rust(rr, annotations)
}
\arguments{
\item{rr}{Vector of RR intervals}

\item{annotations}{Vector of annotations (0 for normal beats, non-zero for abnormal)}
}
\value{
A vector of data with rows and columns as attributes
}
\description{
Get a summary of runs analysis using rust code
}
