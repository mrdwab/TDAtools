\name{TDASample}
\alias{TDASample}
\title{Use an alphanumeric string as the seed to generate a random sample}
\usage{
  TDASample(inString, N, n)
}
\arguments{
  \item{inString}{The alphanumeric string that you want to
  use for generating your seed}

  \item{N}{The total population you are sampling from}

  \item{n}{The number of samples you need}
}
\value{
  Returns a \code{\link{list}} with the class
  \code{TDASample} to handle the formatting of the output.
  The list items include: \itemize{ \item \code{Metadata} A
  character vector of length = 4 containing the date the
  sample was drawn, the input string that was used, the
  population size, and the desired sample size. \item
  \code{SeedUsed} The seed that was automatically generated
  by the function. \item \code{FinalSample} The final
  result of running \code{set.seed(SeedUsed);
  sample(--input parameters--)} \item
  \code{FinalSample_sorted} As above, but sorted }
}
\description{
  Use this function to generate a reproducible random
  sample. Random samples are usually made reproducible by
  setting a numeric seed with \code{\link{set.seed}}. This
  function does some transformations to a given
  alphanumeric string before compting a seed and drawing a
  sample based on that seed.
}
\note{
  The \code{inString} argument is \emph{not}
  case-sensitive, but it is "space-sensitive". That is to
  say, \code{inString = "Ananda Mahto"} will result in the
  same seed as \code{inString = "ananda mahto"}, but a
  different seed from \code{inString = "anandamahto"} or
  \code{inString = " Ananda Mahto "} \emph{(note the extra
  spaces before and after the names)}.

  For a more robust conversion from alphanumeric strings to
  seeds before sampling, the
  \code{\link[mrdwabmisc:stringseed.sampling]{stringseed.sampling}}
  function from the "mrdwabmisc" package is recommended.
}
\examples{
TDASample(inString = "Ananda Mahto", N = 100, n = 20)

## Should have given a seed of 142190
## Lets verify the sample manually
set.seed(142190)
sample(100, 20)

TDASample("Some crazy! Text string with 123 numbers () in it$", 100, 20)
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{sample}}, \code{\link{set.seed}},
  \code{\link[mrdwabmisc:stringseed.sampling]{stringseed.sampling}}
}

