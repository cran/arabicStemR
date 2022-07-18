\name{cleanChars}
\alias{cleanChars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean all characters that are not Latin or Arabic
}
\description{
Cleans any characters in string that are not in either the Latin unicode range
or in the Arabic alphabet
}
\usage{
cleanChars(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{
A string from which characters which are not Latin or Arabic should be removed.
}
}
\value{
\code{cleanChars} returns a string with only Latin and Arabic characters.
}
\author{
Rich Nielsen
}

\examples{

## Create string with Arabic, latin, and Hebrew characters 

x <- '\u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627 Hello \u05d0'

## Remove characters from string that are not Arabic or latin

cleanChars(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
