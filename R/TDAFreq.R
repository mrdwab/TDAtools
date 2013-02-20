#'Calculate frequency tables from a vector
#'
#'Takes an input vector and calculates a frequency table using \code{\link{cut}} that includes cumulative frequency and relative frequency values.
#'
#'@param inVec The input vector.
#'@param breaks How many breaks are required or where the breaks should be?
#'@param histogram Logical. Should a histogram be plotted? Defaults to \code{FALSE}.
#'@author Ananda Mahto
#'@seealso \code{\link{cut}}
#'@examples
#'
#'set.seed(1)
#'x <- sample(300, 100, replace = TRUE)
#'TDAFreq(x, 10)
#'TDAFreq(x, breaks = seq(0, 300, 60), histogram = TRUE)
#'
#'\dontshow{rm(x)}
#'
TDAFreq <- function(inVec, breaks, histogram = FALSE) {
  temp <- cut(inVec, breaks = breaks, include.lowest = TRUE)
  if(isTRUE(histogram)) hist(inVec, breaks)
  temp <- data.frame(table(temp))
  temp$RelFreq <- temp$Freq/sum(temp$Freq)
  temp$CumFreq <- cumsum(temp$Freq)
  temp$CumRelFreq <- cumsum(temp$RelFreq)
  class(temp) <- c("TDAFreq", class(temp))
  temp
}
