\name{removeNewlineChars}
\alias{removeNewlineChars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove new line characters
}
\description{
Removes new line characters from a string.
}
\usage{
removeNewlineChars(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which new line characters should be removed.}
}
\value{
\code{removeNewlineChars} returns a string with new line characters removed.
}
\author{
Rich Nielsen
}
\examples{

## Create string with Arabic characters 

x <- '\u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627
      \u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627'

## Remove newline characters (gets rid of \n\r\t\f\v)

removeNewlineChars(x)  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
