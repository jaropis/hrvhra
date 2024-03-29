% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampen.R
\name{ncm_correlation_sums}
\alias{ncm_correlation_sums}
\title{Correlation Sums Calculation Using the NCM Algorithm}
\usage{
ncm_correlation_sums(signal, m, r)
}
\arguments{
\item{signal}{numeric vector, the data for which the Sample Entropy is to be calculated.}

\item{m}{integer, the embedding dimension.}

\item{r}{numeric, the radius of comparison.}
}
\value{
numeric vector, correlation sums for different embedding dimensions.
}
\description{
This function calculates correlation sums for a given signal,
embedding dimension, and radius of comparison using the NCM algorithm.
It is used internally by `ncm_samp_en`.
}
