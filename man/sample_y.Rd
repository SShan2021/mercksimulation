% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_y.R
\name{sample_y}
\alias{sample_y}
\title{Generate the response variable from the dataset}
\usage{
sample_y(df, parameter_list, beta_zero = 0)
}
\arguments{
\item{df}{The dataframe of predictors.}

\item{parameter_list}{The vector of parameters specifying beta values.}

\item{beta_zero}{The intercept parameter (default is 0).}
}
\value{
A vector of generated outcome variables
}
\description{
Use the predictors to generate an outcome based on the logistic model.
}
\examples{
sample_y(data_x, grid[10,])



}
