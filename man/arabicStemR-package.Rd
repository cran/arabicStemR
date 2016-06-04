\name{arabicStemR-package}
\alias{arabicStemR-package}
\alias{arabicStemR}
\docType{package}
\title{
A package for stemming Arabic for text analysis.
}
\description{
This package is a stemmer for texts in Arabic, as part of the txtorg utility for text analysis workflow.  The stemmer is loosely based on the light 10 stemmer, but with a number of modifications.
}
\details{ 
Use the \code{stem} function.
}
\author{
Maintainer: Rich Nielsen <rnielsen@mit.edu>
}

\keyword{ package }
\seealso{ \code{\link{stem}}}
\examples{
# Load data

data(aljazeera)

## stem and transliterate the results
stem(aljazeera)

## stem and return the stemlist
out <- stem(aljazeera,returnStemList=TRUE)
out$text
out$stemlist
}
