\name{removeDiacritics}
\alias{removeDiacritics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Arabic diacritics
}
\description{
Removes diacritics from Arabic unicode text.
}
\usage{
removeDiacritics(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which Arabic diacritics should be removed.}
}
\value{
\code{removeDiacritics} returns a string with Arabic diacritics removed.
}
\author{
Rich Nielsen
}

\examples{
## Create string with Arabic characters and diacritics 

x<- '\u0627\u0647\u0644\u0627\u064b \u0648\u0633\u0647\u0644\u0627\u064b'

## Remove diacritics
removeDiacritics(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
