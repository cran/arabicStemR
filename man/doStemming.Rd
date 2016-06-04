\name{doStemming}
\alias{doStemming}
\title{
Removes Arabic prefixes and suffixes
}
\description{
Removes prefixes and suffixes, and can return a list matching the words to stemmed words. Does not stem different forms of Allah.
}
\usage{
doStemming(texts, dontstem =  c('\u0627\u0644\u0644\u0647','\u0644\u0644\u0647'))
}

\arguments{
  \item{texts}{
The original texts.
}
  \item{dontstem}{
By default, does not stem different forms of Allah
 }
  
}

\value{
\code{doStemming} returns a named list with the following elements:
  \item{text}{The stemmed text}
  \item{stemmedWords}{A list matching the words and the stemmed words.}}

\author{
Rich Nielsen}

\examples{
## Create string with Arabic characters

x <- '\u0627\u0644\u0644\u063a\u0629 \u0627\u0644\u0639\u0631\u0628\u064a\u0629
 \u062c\u0645\u064a\u0644\u0629 \u062c\u062f\u0627'


## Remove prefixes and suffixes

y<-doStemming(x)

y$text
y$stemmedWords

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
