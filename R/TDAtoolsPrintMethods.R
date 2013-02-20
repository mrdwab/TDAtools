print.TDASample <- function(x, digits = 15, prefix = "", ...) {
  cat("",
      "+---------------------------------------------------+\n",
      "|           INPUT PARAMETERS AND METADATA           |\n",
      "+---------------------------------------------------+\n\n",
          "  Sample drawn on          : ", x$Metadata[1], sep = prefix)
  cat("\n", "  Seed input               : ", dQuote(x$Metadata[2]), 
      sep = prefix)
  cat("\n", "  Population size          : ", x$Metadata[3], sep = prefix)
  cat("\n", "  Number of samples needed : ", x$Metadata[4], sep = prefix)
  cat("\n\n", "  Seed used                : ", x$SeedUsed, sep = prefix)  
  cat("\n\n",
      "+---------------------------------------------------+\n",
      "|                      SAMPLES                      |\n",
      "+---------------------------------------------------+", sep = prefix)
  cat("\n\n", "FINAL SAMPLE\n\n", sep = prefix)
  print(x$FinalSample)
  cat("\n", "FINAL SAMPLE (sorted)\n\n", sep = prefix)
  print(x$FinalSample_sorted)
  invisible(x)
}

print.TDAFreq <- function(x, ...) {
  cat("", "NOTE:\n")
  cat("", "  ", dQuote('['), "represents inclusive of that number;\n")
  cat("", "  ", dQuote('('), "represents excluding that number.\n\n")
  cat("", "Thus,", dQuote("(3,4]"), "should be read as\n")
  cat("   ", dQuote("Greater than 3 but less than or equal to 4"), "\n\n")
  print.data.frame(x)
  invisible(x)
}