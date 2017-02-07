\name{removeStopWords}
\alias{removeStopWords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Arabic stopwords.
}
\description{
Defines a list of Arabic-language stopwords and removes them from a string.
}
\usage{
removeStopWords(texts, defaultStopwordList=TRUE, customStopwordList=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which Arabic stopwords should be removed.}
  \item{defaultStopwordList}{
If TRUE, use the default stopword list of words to be removed.  If FALSE, do not use the default stopword list.  Default is TRUE.
}
  \item{customStopwordList}{
Optional user-specified stopword list of words to be removed, supplied as a vector of strings in either Arabic UTF-8 or Latin characters following the stemmer's transliteration scheme (words without Arabic UTF-8 characters are processed with reverse.transliterate()). Default is NULL.
}
}
\value{
Returns a string with Arabic stopwords removed.
}
\author{
Rich Nielsen
}

\examples{
## Create string with Arabic characters

x <- '\u0627\u0647\u0644\u0627 \u0648\u0633\u0647\u0644\u0627
 \u064a\u0627  \u0635\u062f\u064a\u0642\u064a'

## Remove stop words
removeStopWords(x)$text

## Not run
## To see the full list of stop words 
## removeStopWords(x)$arabicStopwordList

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
