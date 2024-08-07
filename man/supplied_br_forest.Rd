% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest_dot_plot_utils.R
\name{supplied_br_forest}
\alias{supplied_br_forest}
\title{Function to trigger analysis based on type}
\usage{
supplied_br_forest(do, x, y, z)
}
\arguments{
\item{do}{Type of analysis}

\item{x}{(\code{data.frame}) Effects table}

\item{y}{Change reference group}

\item{z}{Confidence interval calculation}
}
\value{
data frame for specified type of analysis
}
\description{
Function to trigger analysis based on type
}
\examples{

forest_plot_src <- subset(effects_table, !is.na(Prop1))

forest_plot_data <- supplied_br_forest(
  "Absolute risk",
  forest_plot_src,
  "Y",
  "Calculated"
)
head(forest_plot_data)
}
