\name{fixAlifs}
\alias{fixAlifs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardize different hamzas on alif seats
}
\description{
Standardize different hamzas on alif seats in a string.
}
\usage{
fixAlifs(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{
A string from which different alifs are standardized.
}
}
\value{
\code{fixAlifs} returns a string with standardized alifs.}

\author{
Rich Nielsen
}

\examples{
## Create string with Arabic characters 
x <- '\u0622 \u0623 \u0675'

## Standardize Alifs
fixAlifs(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

