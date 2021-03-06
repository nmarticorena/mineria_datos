% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isBetween.R
\name{isBetween}
\alias{isBetween}
\title{Is Between}
\usage{
isBetween(x,a,b,...)
}
\arguments{
\item{x}{array with elements to check. Don't use List nor Dataframes.}

\item{a}{value or array with elements to use as lower bound. Don't use List nor Dataframes.}

\item{b}{value or array with elements to use as upper bound. Don't use List nor Dataframes.}

\item{...}{array of "nulls". It takes NA, Inf, -Inf, NULL and NaN as "nulls" by default.}
}
\value{
Returns a logical array with TRUE for "i" positions where a<=x[i]<=b (or a[i]<=x[i]<=b[i],
 if a and b are arrays).
}
\description{
Recieve one array and two numbers and find which elements of the array are between the two numbers.
Can also recieve three arrays of same length and compares each element of one
with other two's same posistion elements to check if the first one is between these two last.
}
\details{
All "nulls" in a or b would be replaced by -Inf or Inf respectevely to be compared.
}
\author{
Daniel Fischer
}
