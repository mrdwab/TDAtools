SeedMe <- function(inString = inString) {
  if (is.factor(inString)) inString <- as.character(inString)    
  if (nchar(inString) <= 3) stop("inString must be > 3 characters")
  string1 <- "jnt3g127rbfeqixkos 586d90pyal4chzmvwu"
  string2 <- "2dyn0uxq ovalrpksieb3fhjw584cm9t7z16g"
  instring <- chartr(string1, string2, tolower(inString))
  t1 <- sd(c(suppressWarnings(sapply(strsplit(instring, ""),
                                     as.numeric))), na.rm = TRUE)
  t2 <- c(sapply(strsplit(instring, " "), nchar))
  t3 <- c(na.omit(sapply(strsplit(instring, ""), match, letters)))
  floor(sum(t1, sd(t2), mean(t2), prod(fivenum(t3)),
            mean(t3), sd(t3), na.rm=TRUE))
}