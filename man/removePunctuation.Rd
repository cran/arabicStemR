\name{removePunctuation}
\alias{removePunctuation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove punctuation.
}
\description{
Removes punctuation from a string, including some specialized Arabic characters.
}
\usage{
removePunctuation(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which punctuation should be removed.}
}
\value{
Returns a string with punctuation removed.
}
\author{
Rich Nielsen
}

\examples{
## Create string with Arabic characters and punctuation

x <- '\u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627!!!?'

## Remove punctuation

removePunctuation(x)  

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
