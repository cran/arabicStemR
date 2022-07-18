\name{removeNumbers}
\alias{removeNumbers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove English, Arabic, and Farsi numerals.
}
\description{
Removes English, Arabic, and Farsi numerals from a string.
}
\usage{
removeNumbers(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{texts}{A string from which English, Arabic, and Farsi numerals should be removed.}
}
\value{
\code{removeNumbers} returns a string with English, Arabic, and Farsi numerals removed.

}
\author{
Rich Nielsen
}
\examples{

## Create string with Arabic characters and number

x <- '\u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627 123 \u0661\u0662\u0663'

## Remove Numbers

removeNumbers(x) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
