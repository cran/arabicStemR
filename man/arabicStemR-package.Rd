\name{arabicStemR-package}
\alias{arabicStemR-package}
\alias{arabicStemR}
\docType{package}
\title{
A package for stemming Arabic for text analysis.
}
\description{
This package is a stemmer for texts in Arabic (Modern Standard).  The stemmer is loosely based on the light 10 stemmer, but with a number of modifications.
}
\details{ 
Use the \code{stemArabic} function.
}
\author{
Maintainer: Rich Nielsen <rnielsen@mit.edu>
}

\keyword{ package }
\seealso{ \code{\link{stemArabic}}}
\examples{
## generate some text in Arabic
x <- "\u628\u633\u645 \u0627\u0644\u0644\u0647
     \u0627\u0644\u0631\u062D\u0645\u0646 
     \u0627\u0644\u0631\u062D\u064A\u0645"

## stem and transliterate
stemArabic(x)

## stem while not stemming certain words
stem(x, dontStemTheseWords = c("alr7mn"))

## stem and return the stemlist
out <- stemArabic(x,returnStemList=TRUE)
out$text
out$stemlist
}
