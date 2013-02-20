#'Calculated the arithmetic mean of grouped data
#'
#'This is essentially \code{\link{weighted.mean}} for a \code{data.frame} representing grouped data.
#'
#'@param inDF The input \code{data.frame}. This \code{data.frame} \emph{must} include the following as the first three columns (see \emph{Examples}):
#'\itemize{
#'\item The lower boundaries of the class
#'\item The upper boundaries of the class
#'\item The frequency for that class
#'}
#'@note It is also possible to call this function directly on the stored output of the \code{\link{TDAFreq}} function, though it would make more sense to calculate the simple arithmetic \code{\link{mean}} from the raw data if that is available
#'@author Ananda Mahto
#'@seealso \code{\link{weighted.mean}}, \code{\link{mean}}, \code{\link{TDAFreq}} 
#'
#'@examples
#'
#'## Your input data.frame should look like this
#'
#'MyGroupedDF <- data.frame(
#'  ClassLow = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450),
#'  ClassHigh = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
#'  Freq = c(78, 122, 187, 82, 51, 47, 13, 9, 6, 4))
#'MyGroupedDF
#'TDAGroupMean(MyGroupedDF)
#'
#'set.seed(1)
#'x <- sample(300, 100, replace = TRUE)
#'mean(x)
#'temp <- TDAFreq(x, 10)
#'
#'## Now, imagine that we have deleted x, but still want the mean
#'##   Not exactly the same, but very close...
#'TDAGroupMean(temp)
#'
#'\dontshow{rm(x, temp, MyGroupedDF)}
#'
TDAGroupMean <- function(inDF) {
  if (isTRUE("TDAFreq" %in% class(inDF))) {
    T1 <- gsub("\\[|\\]|\\(|\\)", "", inDF$Class)
    Midpoints <- colMeans(sapply(strsplit(T1, ","), as.numeric))
  }
  else {
    Midpoints <- rowMeans(inDF[1:2])
  }
  weighted.mean(Midpoints, inDF$Freq)
}
