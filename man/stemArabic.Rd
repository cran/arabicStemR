\name{stemArabic}
\alias{stemArabic}
\title{
Arabic Stemmer for Text Analysis
}
\description{
Allows users to stem Arabic texts for text analysis.
}
\usage{
stemArabic(dat, cleanChars = TRUE, cleanLatinChars = TRUE, 
    transliteration = TRUE, returnStemList = FALSE,
	defaultStopwordList=TRUE, customStopwordList=NULL,
	dontStemTheseWords = c("allh", "llh"))
}

\arguments{
  \item{dat}{
The original data, as a vector of texts.
}
  \item{cleanChars}{
Removes all unicode characters except Latin characters and Arabic alphabet
 }
  \item{cleanLatinChars}{
Removes Latin characters
}
  \item{transliteration}{
Transliterates the text
}
  \item{returnStemList}{
Performs stemming by removing prefixes and suffixes
}
  \item{defaultStopwordList}{
If TRUE, use the default stopword list of words to be removed.  If FALSE, do not use the default stopword list.  Default is TRUE.
}
  \item{customStopwordList}{
Optional user-specified stopword list of words to be removed, supplied as a vector of strings in either Arabic UTF-8 or Latin characters following the stemmer's transliteration scheme (words without Arabic UTF-8 characters are processed with reverse.transliterate()). Default is NULL.
}
  \item{dontStemTheseWords}{
Optional vector of strings that should not be stemmed.  These words can be supplied as transliterated Arabic (according to the transliteration scheme of transliterate() and reverse.transliterate()) or in unicode Arabic.  If a term matches an element of this argument at any intermediate point in stemming, that term will not be stemmed further.  The default is c("allh","llh") because in most applications, stemming these common words for "God" creates some confusion by resulting in the string "lh".
}
}
\details{
\code{stemArabic} prepares texts in Arabic for text analysis by stemming.
}

\value{
\code{stemArabic} returns a named list with the following elements:
  \item{text}{The stemmed text}
  \item{stemlist}{A list of the stemmed words.}
}
\author{
Rich Nielsen
}
\examples{
## generate some text in Arabic
x <- "\u628\u633\u645 \u0627\u0644\u0644\u0647
     \u0627\u0644\u0631\u062D\u0645\u0646 
     \u0627\u0644\u0631\u062D\u064A\u0645"

## inspect
print(x)

## stem and transliterate
stemArabic(x)

## stem while not stemming certain words
stem(x, dontStemTheseWords = c("alr7mn"))

## stem and return the stemlist
out <- stemArabic(x,returnStemList=TRUE)
out$text
out$stemlist
}
