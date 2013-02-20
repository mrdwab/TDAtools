\docType{package}
\name{TDAtools-package}
\alias{TDAtools}
\alias{TDAtools-package}
\title{A collection of functions written for students at the Tata-Dhan Academy}
\description{
  This package collects some functions that were written to
  be used by students at the Tata-Dhan Academy,
  particularly for their courses in quantitative techniques
  and research.
}
\details{
  \tabular{ll}{ Package: \tab TDAtools\cr Type: \tab
  Package\cr Version: \tab 1.0\cr Date: \tab 2013-02-12\cr
  License: \tab GPL-2\cr }
}
\examples{
## TDASample
TDASample("Tata-Dhan Academy", 100, 10)

MyGroupedDF <- data.frame(
ClassLow = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450),
ClassHigh = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
Freq = c(78, 123, 187, 82, 51, 47, 13, 9, 6, 4))

## TDAGroupMean
TDAGroupMean(MyGroupedDF)

## TDAGroupMedian
TDAGroupMedian(MyGroupedDF)
}
\author{
  Ananda Mahto

  Maintainer: Ananda Mahto <anandamahto@dhan.org>
}
\keyword{package}

