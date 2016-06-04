\name{removeEnglishNumbers}
\alias{removeEnglishNumbers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove English numbers
}
\description{
Removes Arabic numerals from a string.
}
\usage{
removeEnglishNumbers(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which English numerals should be removed.}
}
\value{
\code{removeEnglishNumbers} returns a string with English numerals removed.
}
\author{
Rich Nielsen
}

\examples{

## Create string with Arabic characters and English number

x <- '\u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627 123'

## Remove English Numbers

removeNumbers(x) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
