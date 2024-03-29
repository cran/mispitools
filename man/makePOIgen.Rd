% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makePOIgen.R
\name{makePOIgen}
\alias{makePOIgen}
\title{Make POIs gen: a function for obtaining a database with genetic information from simulated POIs or UHRs.}
\usage{
makePOIgen(numsims = 100, reference, seed = 123)
}
\arguments{
\item{numsims}{Number of simulations performed (numer of POIs or UHRs).}

\item{reference}{Indicate the reference STRs/SNPs frequency database used for simulations.}

\item{seed}{Select a seed for simulations. If it is defined, results will be reproducible. Suggested, seed = 123}
}
\value{
An object of class data.frame with genetic information from POIs (randomly sampled from the frequency database).
}
\description{
Make POIs gen: a function for obtaining a database with genetic information from simulated POIs or UHRs.
}
\examples{
library(forrel) 
freqdata <- getfreqs(Argentina)
makePOIgen(numsims = 100, reference = freqdata, seed = 123)
}
