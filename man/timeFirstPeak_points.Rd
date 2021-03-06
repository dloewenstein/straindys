% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeFirstPeak_points.R
\name{timeFirstPeak_points}
\alias{timeFirstPeak_points}
\title{Find time to true strain peaks using percentagepoints criteria for circumferential/longitudinal strain}
\usage{
timeFirstPeak_points(x, strain, thresh, incr, position.firstStrainColumn)
}
\arguments{
\item{x}{dataframe containing the strain columns in a long format.}

\item{strain}{the strain variable you want to mutate, written inside "".}

\item{thresh}{number of negative percentagepoints that a valley has to reach to be
considered a true valley.}

\item{incr}{number of percentagepoints by which the strain increase between valley and the
following peak has to reach.}

\item{position.firstStrainColumn}{the index position for the first strain segment in the dataframe}
}
\value{
A dataframe with id and a straincolumn containing the time for the first peak
fulfilling the criteria
}
\description{
Find time to true strain peaks using percentagepoints criteria for circumferential/longitudinal strain
}
\examples{

}
