% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trackloss.R
\name{rm_trackloss_events}
\alias{rm_trackloss_events}
\title{Removes events with excessive trackloss}
\usage{
rm_trackloss_events(data = data, RequiredData = NULL)
}
\arguments{
\item{data}{A data table object output by \code{\link{mark_trackloss}}.}

\item{RequiredData}{A number indicating the percentage of data required to 
be included (i.e., removes events with less than this amount of data).}
}
\value{
An object of type data table as described in \link[tibble]{tibble}.
}
\description{
\code{rm_trackloss_events} removes events with less data than the specified
amount.
}
\examples{
\dontrun{
library(VWPre)
# Remove events...
df <- rm_trackloss_events(data = dat, RequiredData = 50)
}
}
