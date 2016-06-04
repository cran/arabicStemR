\name{cleanLatinChars}
\alias{cleanLatinChars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean Latin characters
}
\description{
Cleans Latin characters from a string
}
\usage{
cleanLatinChars(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{
A string from which Latin characters should be removed.
}
}
\value{
\code{cleanLatinChars} returns a string with Latin characters removed.
}
\author{
Rich Nielsen
}

\examples{
## Create string with Arabic and latin characters 

x <- '\u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627 Hello'

## Rewmove latin characters from string

cleanLatinChars(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
