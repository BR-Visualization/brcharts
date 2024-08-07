% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci_calculation.R
\name{calculate_diff_rates}
\alias{calculate_diff_rates}
\title{CI for treatment difference in exposure-adjusted rates}
\usage{
calculate_diff_rates(rate1, rate2, py1, py2, cl = 0.95)
}
\arguments{
\item{rate1}{(\code{numeric})\cr Event or incidence rate (per 100 PYs) in active treatment}

\item{rate2}{(\code{numeric})\cr Event or incidence rate (per 100 PYs) in comparator treatment}

\item{py1}{(\code{numeric})\cr 100PEY or 100PYAR in active treatment}

\item{py2}{(\code{numeric})\cr 100PEY or 100PYAR in comparator treatment}

\item{cl}{(\code{numeric})\cr confidence level}
}
\description{
Derive mean difference and associated confidence intervals
for exposure-adjusted rates (per 100 PYs)
}
\examples{
calculate_diff_rates(
  rate1 = 152.17, rate2 = 65.21, py1 = 230, py2 = 230,
  cl = 0.95
)
}
