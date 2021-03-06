% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CrossValidation.R
\name{CrossVD}
\alias{CrossVD}
\title{CrossVD}
\usage{
CrossVD(data, K = NULL, get_mean = T, verb = F, OPT, ...)
}
\arguments{
\item{data}{\link{list} containing the data, elements must be named \code{X} and \code{Y}, where \code{X} is a
\code{n x k} matrix and \code{Y} is a vector of length \code{n}. Here, \code{n} represents the number of
observations and \code{k} is the number of  \mjseqn{\beta} coefficients.}

\item{K}{\link{integer} the number of folds. Set equal to \code{NULL} to run a Leave-one-out cross validation.
If \code{K} is larger than the number of observations, the Leave-one-out cross validation is run by default.}

\item{get_mean}{\link{boolean} if \code{TRUE}, the CV-MSE is returned, otherwise the function returns the MSE computed for each fold.}

\item{verb}{\link{bool} if \code{TRUE}, it prints more information about the status of the algorithm (default is \code{FALSE}).}

\item{OPT}{\link{function} the optimization function whose prediction power has to be tested. If can only be equal to
\code{\link{GradD}} or \code{\link{SteepD}}.}

\item{...}{optional arguments to OPT}
}
\value{
if \code{get_mean} is \code{TRUE}, the CV-MSE is returned.
Otherwise the function returns a \link{vector} containing all MSE computed for each fold.
}
\description{
This function calculates the estimated K-fold or Leave-one-out cross-validation mean squared prediction error.
}
