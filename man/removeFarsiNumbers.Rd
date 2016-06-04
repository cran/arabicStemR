\name{removeFarsiNumbers}
\alias{removeFarsiNumbers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Farsi numbers
}
\description{
Removes Farsi numerals from a string.
}
\usage{
removeFarsiNumbers(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which Farsi numerals should be removed.}
}
\value{
\code{removeFarsiNumbers} returns a string with Arabic numerals removed.
}
\author{
Rich Nielsen
}

\examples{
## Create string with Arabic characters and numbers 

x <- '\u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627 \u06f1\u06f2\u06f3\u06f4\u06f5'

## Remove Farsi numbers
removeFarsiNumbers(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
