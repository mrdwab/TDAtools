\name{TDAFreq}
\alias{TDAFreq}
\title{Calculate frequency tables from a vector}
\usage{
  TDAFreq(inVec, breaks, histogram = FALSE)
}
\arguments{
  \item{inVec}{The input vector.}

  \item{breaks}{How many breaks are required or where the
  breaks should be?}

  \item{histogram}{Logical. Should a histogram be plotted?
  Defaults to \code{FALSE}.}
}
\description{
  Takes an input vector and calculates a frequency table
  using \code{\link{cut}} that includes cumulative
  frequency and relative frequency values.
}
\examples{
set.seed(1)
x <- sample(300, 100, replace = TRUE)
TDAFreq(x, 10)
TDAFreq(x, breaks = seq(0, 300, 60), histogram = TRUE)

\dontshow{rm(x)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{cut}}
}

