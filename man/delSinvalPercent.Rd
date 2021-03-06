% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoremodel.R
\name{delSinvalPercent}
\alias{delSinvalPercent}
\title{Delete Variables Based on Single-Value Percent}
\usage{
delSinvalPercent(df, percent = 0.9, exclude = NULL)
}
\arguments{
\item{df}{A dataframe.}

\item{percent}{The given threshold, default \code{0.9}.}

\item{exclude}{Vector of column names or numbers to exclude, default \code{NULL}.}
}
\value{
A dataframe after deleted
}
\description{
\code{delSinvalPercent} will delete variables whose single-value percent is more than or equal to the given threshold, then return a new dataframe.
}
\details{
generally, in score model, the threshold of single value percent is often set to 0.9.
}
\seealso{
Other data preprocessing functions: \code{\link{delFewValues}},
  \code{\link{delNArate}}, \code{\link{excludeCol}},
  \code{\link{maxSinvalPercent}}
}
\concept{data preprocessing functions}
