% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAuc.R
\name{getAuc}
\alias{getAuc}
\title{Get AUC}
\usage{
getAuc(score,aucDigits = 4, groupCol = NULL)
}
\arguments{
\item{score}{List of scores as dataframes or a single dataframe with al scores binded.}

\item{aucDigits}{integer indicating the number of decimal places to be used for auc indicators.}

\item{groupCol}{character indicating the name of the column to group-by the differents 
score groups if \code{score} is a single dataframe.}
}
\value{
It returns a two column dataframe, one \code{id} with the group of scores and \code{auc} with the calculated auc.
}
\description{
Get the Auc indicator from a(some) score(s) dataframe(s)
}
\author{
Martin Vicencio
}
