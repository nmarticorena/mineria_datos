% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/liftPlot.R
\name{liftPlot}
\alias{liftPlot}
\title{Lift Plot}
\usage{
liftPlot(score, quantiles = 100, cumulative = T,  quantileCuts = NULL, title = "Lift", legendTitle = NULL, xlab = NULL, ylab = NULL, groupCol = NULL)
}
\arguments{
\item{score}{List of scores as dataframes or a single dataframe with al scores binded.}

\item{quantiles}{integer, number of buckets to make the lift curve.}

\item{cumulative}{boolean, Is cumulative lift?}

\item{quantileCuts}{integer vector, quantiles form 1:\code{quantiles} to cut the curves and mark in the plot.}

\item{title, legendTitle, xlab, ylab}{characters, plot tittle, legend title, x-axis label and y-axis label.}

\item{groupCol}{character indicating the name of the column to group-by the differents 
score groups if \code{score} is a single dataframe.}
}
\value{
It returns a ggplot object so it can be post modificated.
}
\description{
Plot the  Lift curve(s) from a(some) score(s) dataframe(s)
}
\author{
Martin Vicencio
}
