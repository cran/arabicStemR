\name{removePrefixes}
\alias{removePrefixes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Arabic prefixes
}
\description{
Removes some Arabic prefixes from a unicode string.  The prefixes are: "waw", "alif-lam", "waw-alif-lam", "ba-alif-lam", "kaf-alif-lam", "fa-alif-lam", and "lam-lam."  Prefixes are removed from a word (as defined by spaces) only if the remaining stem would not be too short.
}
\usage{
removePrefixes(texts, x1 = 4, x2 = 4, x3 = 5, x4 = 5, x5 = 5, x6 = 5, x7 = 4, 
dontstem = c('\u0627\u0644\u0644\u0647','u0644\u0644\u0647'))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{An Arabic-language string in unicode}
  \item{x1}{The number of letters that must remain in a word for the function to remove the prefix "waw".}
  \item{x2}{The number of letters that must remain in a word for the function to remove the prefix "alif-lam".}
  \item{x3}{The number of letters that must remain in a word for the function to remove the prefix "waw-alif-lam".}
  \item{x4}{The number of letters that must remain in a word for the function to remove the prefix "ba-alif-lam".}
  \item{x5}{The number of letters that must remain in a word for the function to remove the prefix "kaf-alif-lam".}
  \item{x6}{The number of letters that must remain in a word for the function to remove the prefix "fa-alif-lam".}
  \item{x7}{The number of letters that must remain in a word for the function to remove the prefix "lam-lam".}
  \item{dontstem}{Words that should not be stemmed (entered in unicode).}
}
\value{
  Returns a string with Arabic prefixes removed.
}
\author{
Rich Nielsen
}
\examples{
## Create string with Arabic characters

x <- '\u0627\u0644\u0644\u063a\u0629 \u0627\u0644\u0639\u0631\u0628\u064a\u0629
 \u062c\u0645\u064a\u0644\u0629 \u062c\u062f\u0627'

# Remove Prefixes

removePrefixes(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
