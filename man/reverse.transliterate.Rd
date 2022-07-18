\name{reverse.transliterate}
\alias{reverse.transliterate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transliterate latin characters into Arabic unicode characters
}
\description{
Transliterates latin characters into Arabic unicode characters using a transliteration system developed by Rich Nielsen.
}
\usage{
reverse.transliterate(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string in latin characters to be transliterated into Arabic characters.}
}

\value{
  Returns a string in Arabic characters.
}
\author{
Rich Nielsen
}
\examples{
## Create latin string following the arabicStemR package transliteration scheme.

x <- 'al3rby'

## Convert latin characters into Arabic unicode characters

reverse.transliterate(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

