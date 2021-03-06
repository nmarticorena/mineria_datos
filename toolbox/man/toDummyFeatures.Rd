% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/featuresDummy.R
\name{toDummyFeatures}
\alias{toDummyFeatures}
\title{To Dummy Features}
\usage{
toDummyFeatures(dataset,features= attr(dataset,"features"))
}
\arguments{
\item{dataset}{dataset with the variables to pass to dummies.}

\item{features}{features list, takes the atribute from \code{dataset} by default.}
}
\value{
Return the input dataset without the original columns that were passed to dummies, and the new columns placed at the end.
}
\description{
It pass to dummy variables those that are in \code{features$as_dummy}.
}
\details{
It deletes the names of variables passed to dummies from \code{features$allSelected} and \code{features$predictor_var} 
in the process. This function calls \code{toFullDummy()}.
}
\author{
Martin Vicencio
}
