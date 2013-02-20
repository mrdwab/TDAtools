\name{TDAGroupMedian}
\alias{TDAGroupMedian}
\title{Estimate the sample median of grouped data}
\usage{
  TDAGroupMedian(inDF)
}
\arguments{
  \item{inDF}{The input \code{data.frame}. This
  \code{data.frame} \emph{must} include the following as
  the first three columns (see \emph{Examples}): \itemize{
  \item The lower boundaries of the class \item The upper
  boundaries of the class \item The frequency for that
  class }}
}
\description{
  Often, we are provided with summary frequency tables from
  which we need to perform some calculations. This function
  estimates the \code{\link{median}} of grouped data,
  entered as a \code{data.frame}.
}
\note{
  It is also possible to call this function directly on the
  stored output of the \code{\link{TDAFreq}} function,
  though it would make more sense to calculate the
  \code{\link{median}} from the raw data if that is
  available
}
\examples{
## Your input data.frame should look like this

MyGroupedDF <- data.frame(
ClassLow = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450),
ClassHigh = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
Freq = c(78, 123, 187, 82, 51, 47, 13, 9, 6, 4))
MyGroupedDF
TDAGroupMedian(MyGroupedDF)

set.seed(1)
x <- sample(300, 100, replace = TRUE)
median(x)
temp <- TDAFreq(x, 10)

## Now, imagine that we have deleted x, but still want the median
##   Not exactly the same, but close...
TDAGroupMedian(temp)

\dontshow{rm(x, temp, MyGroupedDF)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{median}}, \code{\link{TDAGroupMean}}
}

