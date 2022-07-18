\name{transliterate}
\alias{transliterate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transliterate Arabic unicode characters into latin characters
}
\description{
Transliterates Arabic unicode characters into latin characters using a transliteration system developed by Rich Nielsen.
}
\usage{
transliterate(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string in Arabic characters to be transliterated into latin characters.
}
}

\value{
Returns a string in latin characters.
}
\author{
Rich Nielsen
}
\examples{
## Create Arabic string 

x <- '\u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627'

## Performs transliteration of Arabic into latin characters.
transliterate(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
