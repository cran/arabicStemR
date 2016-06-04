\name{removeArabicNumbers}
\alias{removeArabicNumbers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Arabic numbers
}
\description{
Removes Arabic numerals from a string.
}
\usage{
removeArabicNumbers(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which Arabic numerals should be removed.}
}
\value{
\code{removeArabicNumbers} returns a string with Arabic numerals removed.
}
\author{
Rich Nielsen
}
\examples{
## Create string with Arabic characters and numbers 

x <- '\u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627 \u0661\u0662\u0663'

## Remove Arabic numbers
removeArabicNumbers(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
