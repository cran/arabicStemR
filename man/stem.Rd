\name{stem}
\alias{stem}
\title{
Arabic Stemmer for Text Analysis
}
\description{
Allows users to stem Arabic texts for text analysis.
}
\usage{
stem(dat, cleanChars = TRUE, cleanLatinChars = TRUE, 
    transliteration = TRUE, returnStemList = FALSE)
}

\arguments{
  \item{dat}{
The original data.
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
}
\details{
\code{stem} prepares texts in Arabic for text analysis by stemming.
}

\value{
\code{stem} returns a named list with the following elements:
  \item{text}{The stemmed text}
  \item{stemlist}{A list of the stemmed words.}
}
\author{
Rich Nielsen
}
\examples{
# Load data

data(aljazeera)

## stem and transliterate the results
stem(aljazeera)

## stem and return the stemlist
out <- stem(aljazeera,returnStemList=TRUE)
out$text
out$stemlist


## This allows you to see which words are being combined
## Interpret this as follows:
i <- 1 
## This is the i'th stem in quotes (with the original word as the label)
out$stemlist[i]  
## These are all the words that resolve to the same stem.
names(out$stemlist)[out$stemlist==out$stemlist[i]] 
## And this will provide a count.  
mytab <- table(names(out$stemlist)[out$stemlist==out$stemlist[i]])
for(i in 1:length(mytab)){print(mytab[i])}
## Note that if you just look at "mytab", it will appear incorrect because
## R displays the Arabic labels from right to left but the numbers from left
## to right (thanks R!).

## This can be done for all of the stems
result <- sapply(out$stemlist, function(x){table(names(out$stemlist)[out$stemlist==x])})
for(i in 1:length(result)){
  cat(paste("stemmed:",out$stemlist[i],"\n"))
  cat("unstemmed:")
  print(result[[i]])
  cat("\n")
}
## display the results correctly for the i'th stem
i <- 1
for(j in 1:length(result[[i]])){print(result[[i]][j])}

}
