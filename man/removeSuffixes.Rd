\name{removeSuffixes}
\alias{removeSuffixes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Arabic suffixes
}
\description{
Removes some Arabic suffixes from a unicode string.  The suffixes (in order of removal) are: "ha-alif", "alif-nun", "alif-ta", "waw-nun", "yah-nun", "yah-heh", "yah-ta marbutta", "heh", "ta marbutta", and "yah."  Suffixes are removed from a word (as defined by spaces) only if the remaining stem would not be too short. Only one suffix is removed from each word.
}
\usage{
removeSuffixes(texts, x1 = 4, x2 = 4, x3 = 4, x4 = 4, 
x5 = 4, x6 = 4, x7 = 4, x8 = 3, x9 = 3, x10 = 3, 
dontstem = c('\u0627\u0644\u0644\u0647','u0644\u0644\u0647'))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{An Arabic-language string in unicode.}
  \item{x1}{The number of letters that must be in a word for the function to remove the suffix "ha-alif".}
  \item{x2}{The number of letters that must be in a word for the function to remove the suffix "alif-nun".}
  \item{x3}{The number of letters that must be in a word for the function to remove the suffix "alif-ta".}
  \item{x4}{The number of letters that must be in a word for the function to remove the suffix "waw-nun".}
  \item{x5}{The number of letters that must be in a word for the function to remove the suffix "yah-nun".}
  \item{x6}{The number of letters that must be in a word for the function to remove the suffix "yah-heh".}
  \item{x7}{The number of letters that must be in a word for the function to remove the suffix "yah-ta marbutta".}
  \item{x8}{The number of letters that must be in a word for the function to remove the suffix "heh".}
  \item{x9}{The number of letters that must be in a word for the function to remove the suffix "ta marbutta".}
  \item{x10}{The number of letters that must be in a word for the function to remove the suffix "yah".}
  \item{dontstem}{Words that should not be stemmed (entered in unicode).}
}

\value{
  Returns a string with Arabic suffixes removed.
}
\author{
Rich Nielsen
}
\examples{
## Create string with Arabic characters

x <- '\u0627\u0644\u0644\u063a\u0629 \u0627\u0644\u0639\u0631\u0628\u064a\u0629
 \u062c\u0645\u064a\u0644\u0629 \u062c\u062f\u0627'

# Remove Suffixes

removeSuffixes(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
