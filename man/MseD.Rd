% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Prediction.R
\name{MseD}
\alias{MseD}
\title{MseD}
\usage{
MseD(coef, data)
}
\arguments{
\item{coef}{\link{vector} containing the estimated coefficients \mjseqn{\hat{\beta}}.}

\item{data}{\link{list} containing the data, elements must be named \code{X} and \code{Y}, where \code{X} is a
\code{n x k} matrix and \code{Y} is a vector of length \code{n}. Here, \code{n} represents the number of
observations and \code{k} is the number of  \mjseqn{\beta} coefficients.}
}
\value{
the Mean Square Prediction error.
}
\description{
\loadmathjax Compute Mean Square Prediction error for a linear model.
}
