% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interactive.R
\name{plot_var_app}
\alias{plot_var_app}
\title{Plots diagnostic plots of subject/item variance.}
\usage{
plot_var_app(data)
}
\arguments{
\item{data}{A data table object output by either \code{\link{bin_prop}}. 
\code{\link{transform_to_elogit}}, or \code{\link{create_binomial}}.}
}
\description{
\code{plot_var_app} calculates and plots within-subject/item standard deviation,
along with standardized by-subject/item means for a given interest area, within
a given time window.
}
\examples{
\dontrun{
library(VWPre)
# For plotting variability in the data
plot_var_app(data = dat) 
}
}
