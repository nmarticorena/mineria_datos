% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eco.R
\name{eco}
\alias{eco}
\title{Eco}
\usage{
eco(x,n)
}
\arguments{
\item{x}{an array}

\item{n}{an integer >0}
}
\value{
An array with same length as x.
}
\description{
takes an array and sums n displaced versions of it. Each k-displaced version of the array got zeros on the first k positions.
k takes values from 1 to n
}
\author{
Daniel Fischer
}
