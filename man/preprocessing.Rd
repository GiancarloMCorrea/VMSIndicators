% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{preprocessing}
\alias{preprocessing}
\title{Preprocessing VMS data}
\usage{
preprocessing(
  vmsdata,
  catchdata,
  vesseldata,
  cutoff_dc = 5,
  cutoff_time = 8,
  min_trip_time = 5
)
}
\arguments{
\item{vmsdata}{VMS dataset.}

\item{catchdata}{Dataset with landings.}

\item{vesseldata}{Vessel information.}

\item{cutoff_dc}{Minimum distance to the shoreline (in nautical miles)
to identify a fishing trip.}

\item{cutoff_time}{Maximum time interval (in hours) allowed between
arrival time and landing time.}

\item{min_trip_time}{Minimum trip time allowed (in hours).}
}
\value{
The VMS database with quantities calculated.
}
\description{
Calculates required variables to calculate the efficiency indicators and make plots.
}
