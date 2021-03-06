% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_plot.R
\name{summary_plot}
\alias{summary_plot}
\title{Summary by plots}
\usage{
summary_plot(x, ...)
}
\arguments{
\item{x}{\code{"grlyr"} object from \code{calc_biomass()}.}

\item{...}{further arguments (currently ignored).}
}
\value{
A data.frame with estimates for each functional group 'fg',
     including 'mass', 'masssd', 'c', 'csd', 'n', 'nsd' (all in kg
     per ha), as well as 'vol' and 'volsd' (cubic meters per ha),
     and 'cover' and 'coversd' (percentage points per plot).
}
\description{
Summary of ground layer observations at the plot-level.
}
\details{
Gives basic descriptive summaries, disaggregated by functional
     groups.
}
\examples{
data(est)
x <- calc_biomass(est)
s <- summary_plot(x)

}
