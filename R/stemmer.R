## An arabic stemmer, modeled after after the light10 stemmer, but with substantial modifications
## Richard Nielsen
## This version: 10/15/2015

###########################################################
## A list of all chars in the Arabic unicode range
## I use this below to demonstrate which characters are being cleaned
## by the cleanChars() function.
triplet <- c(paste("0",c(60:69),sep=""),paste("06",c("A","B","C","D","E","F"),sep=""))
ArabicUnicodeChars <- as.vector(sapply(triplet,function(x){paste(x,c(0:9,c("A","B","C","D","E","F")),sep="")}))
x1 <- paste0("\\u",ArabicUnicodeChars)
ArabicUnicodeChars <- sapply(x1,function(x){parse(text = paste0("'", x, "'"))[[1]]})
rm(x1,triplet)


###########################################################
## Trim funtion
## This is used throughout to trim whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


###########################################################
## package all the stemmer functions together
stem <- function(dat, cleanChars=TRUE, cleanLatinChars=TRUE, 
                 transliteration=TRUE, returnStemList=FALSE){
  dat <- removeNewlineChars(dat)  ## gets rid of \n\r\t\f\v
  dat <- removePunctuation(dat)  ## gets rid of punctuation
  dat <- removeDiacritics(dat)  ## gets rid of Arabic diacritics
  dat <- removeEnglishNumbers(dat)  ## gets rid of English numbers
  dat <- removeArabicNumbers(dat)  ## gets rid of Arabic numbers
  dat <- removeFarsiNumbers(dat)  ## gets rid of Farsi numbers
  dat <- fixAlifs(dat)  ## standardizes different hamzas on alif seats
  if(cleanChars){dat <- cleanChars(dat)}  ## removes all unicode chars except Latin chars and Arabic alphabet
  if(cleanLatinChars){dat <- cleanLatinChars(dat)}  ## removes all Latin chars
  dat <- removeStopWords(dat)$text  ## removes the stopwords
  if(returnStemList==TRUE){
    tmp <- doStemming(dat) ## removes prefixes and suffixes, and can return a list matching words to stemmed words
    dat <- tmp$text
    stemlist <- tmp$stemmedWords
    if(transliteration){dat <- transliterate(dat)}  ## performs transliteration
    return(list(text=dat,stemlist=stemlist))
  } else {
    dat <- removePrefixes(dat)  ## removes prefixes
    dat <- removeSuffixes(dat)  ## removes suffixes
    if(transliteration){dat <- transliterate(dat)}  ## performs transliteration
    return(dat)
  }
}

###########################################################
## remove numbers

## Removes Latin character numbers
removeEnglishNumbers <- function(texts){
  texts <- gsub('[0-9]',' ',texts)
  # remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

## Removes Arabic character numbers
removeArabicNumbers <- function(texts){
  texts <- gsub('[\u0660-\u0669]',' ',texts)
  # remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

## Removes Farsi character numbers
removeFarsiNumbers <- function(texts){
  texts <- gsub('[\u06f0-\u06f9]',' ',texts)
  # remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

## bundles the three functions for removing numbers
removeNumbers <- function(texts){
  texts <- removeEnglishNumbers(texts)
  texts <- removeArabicNumbers(texts)
  texts <- removeFarsiNumbers(texts)
  return(texts)
}

###########################################################
## clean out junk characters

## removes punctuation
removePunctuation <- function(texts){
  ## replace arabic specific punctuation
  texts <- gsub('\u060c|\u061b|\u061f|\u066c|\u066d|\u06d4|\u06dd|\u06de|\u06e9',' ',texts)
  ## replace other junk characters that sometimes show up
  texts <- gsub('[\u200C-\u200F]|&nbsp|~|\u2018|\u2022|\u2013|\u2026|\u201c|\u201d|\u2019|\ufd3e|\ufd3f', ' ', texts)
  texts <- gsub('\xbb|\xab|\xf7|\xb7', ' ', texts)
  ## remove general punctuation
  texts <- gsub(pattern="[[:punct:]]", texts, replacement=" ")
  # remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

## removes diacritic marks indicating short Arabic vowels
removeDiacritics <- function(texts){
  ## diacritics that I replace without spaces (appear over or under chars)
  texts <- gsub('[\u0610-\u061a]|\u0640|[\u064b-\u065f]|\u0670|[\u06d6-\u06dc]|[\u06df-\u06e4]|[\u06e7-\u06e8]|[\u06ea-\u06ed]',
                '',texts)
  # remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}
## see which diacritics are removed
#grep('[\u0610-\u061a]|\u0640|[\u064b-\u065f]|\u0670|[\u06d6-\u06dc]|[\u06df-\u06e4]|[\u06e7-\u06e8]|[\u06ea-\u06ed]',  ArabicUnicodeChars, value=T)


## remove \n \r \t \f \v
removeNewlineChars <- function(texts){
  texts <- gsub('\n|\r|\t|\f|\v',' ',texts)
  # remove extra spaces
  return(trim(gsub(" {2,}"," ", texts)))
}


## bundle the functins for punctuation, diacritics, and newlines together.   Mostly to preserve possible reverse compatibility with older versions
clean <- function(texts){
  texts <- removePunctuation(texts)
  texts <- removeDiacritics(texts)
  texts <- removeNewlineChars(texts)
  return(texts)
}


#######################################################
## Standardize the alifs

fixAlifs <- function(texts){
  texts <- gsub('\u0622|\u0623|\u0625|[\u0671-\u0673]|\u0675','\u0627', texts)
  return(texts)
}


#######################################################
## clean up the characters

## This function removes any characters in the text that are not in either the Latin unicode range
## or in the Arabic alphabet + "p".  To see which characters are retained, uncomment and run this
## line of code: ArabicUnicodeChars[!sapply(ArabicUnicodeChars,cleanChars)==""]
cleanChars <- function(texts){
  # http://jrgraphix.net/research/unicode_blocks.php
  ## ones I'm dropping
  texts <- gsub('[\u00A0-\u0600]|[\u0600-\u0621]|[\u063b-\u0640]|[\u064b-\u065f]|[\u066a-\u067d]|[\u067f-\u06ff]|[\u0700-\uFB4F]|[\uFB50-\uFDFF]|[\uFE00-\uFFFF]','',texts)
  ## I could sort through these ones too: http://jrgraphix.net/r/Unicode/FB50-FDFF, but I'm not right now
  ## clean up spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

## This function removes all latin characters using unicode ranges
cleanLatinChars <- function(texts){
  # http://jrgraphix.net/research/unicode_blocks.php
  ## ones I'm aiming to keep
  # texts <- gsub('[[:alpha:]]','',texts)
  # Romney fix
  texts <- gsub("\\p{Latin}", "", texts, perl = TRUE)
  ## I could sort through these ones too: http://jrgraphix.net/r/Unicode/FB50-FDFF, but I'm not right now
  ## clean up spaces
  return(trim(gsub(" {2,}"," ", texts)))
}

#######################################################
## Remove stopwords

## Removes stopwords from a list that I've put together

removeStopWords <- function(texts){
  
  # Split up the words...
  textsSplit = strsplit(texts," ")[[1]]
  
  preps  <- c('\u0641\u064a',  #fy
              '\u0641\u064a\u0647',  #fyh
              '\u0641\u064a\u0647\u0627',  #fyha
              '\u0641\u064a\u0647\u0645',  #fyhm
              '\u0639\u0644\u0649',  #3lA
              '\u0639\u0644\u064a\u0643',  #3lyk
              '\u0639\u0644\u064a\u0643\u0645',  #3lykm
              '\u0639\u0644\u064a\u0646\u0627',  #3lyna
              '\u0639\u0644\u064a\u0647',  #3lyh
              '\u0639\u0644\u064a\u0647\u0627',  #3lyha
              '\u0639\u0644\u064a\u0647\u0645',  #3lyhm
              '\u0639\u0644\u064a',  #3ly
              '\u0628\u0647',  #bh
              '\u0628\u0647\u0627',  #bha
              '\u0628\u0647\u0645',  #bhm
              '\u0628\u0647\u0630\u0627',  #bhia
              '\u0628\u0630\u0644\u0643',  #bilk
              '\u0628\u0643',  #bk
              '\u0628\u0643\u0645',  #bkm
              '\u0628\u0643\u0644',  #bkl
              '\u0628\u0645\u0627',  #bma
              '\u0628\u0645\u0646',  #bmn
              '\u0628\u0646\u0627',  #bna
              '\u0644\u0647',  #lh
              '\u0644\u0647\u0627',  #lha
              '\u0644\u0647\u0645',  #lhm
              '\u0645\u0639',  #m3
              '\u0645\u0639\u0647',  #m3h
              '\u0645\u0639\u0647\u0627',  #m3ha
              '\u0645\u0639\u0647\u0645',  #m3hm
              '\u0639\u0646',  #3n
              '\u0639\u0646\u0627',  #3na
              '\u0639\u0646\u0647',  #3nh
              '\u0639\u0646\u0647\u0627',  #3nha
              '\u0639\u0646\u0647\u0645',  #3nhm
              '\u062a\u062d\u062a',  #t7t
              '\u062d\u062a\u0649',  #7tA
              '\u0641\u0648\u0642',  #fwQ
              '\u0641\u0648\u0642\u064e',  #fwQ?
              '\u0628\u062c\u0627\u0646\u0628',  #bjanb
              '\u0623\u0645\u0627\u0645',  #amam
              '\u0623\u0645\u0627\u0645\u064e',  #amam?
              '\u0627\u0645\u0627\u0645',  #amam
              '\u062e\u0627\u0631\u062c',  #Karj
              '\u0628\u0627\u0644\u062e\u0627\u0631\u062c',  #balKarj
              '\u062d\u0648\u0644\u064e',  #7wl?
              '\u062d\u0648\u0644',  #7wl
              '\u0631\u063a\u0645',  #rGm
              '\u0628\u0627\u0644\u0631\u063a\u0645',  #balrGm
              '\u0631\u063a\u0645\u064e',  #rGm?
              '\u0645\u0646\u0630',  #mni
              '\u0645\u0646\u0630\u064f',  #mni?
              '\u0645\u0646',  #mn
              '\u062e\u0644\u0627\u0644',  #Klal
              '\u062e\u0644\u0627\u0644\u064e',  #Klal?
              '\u062d\u0648\u0644',  #7wl
              '\u062d\u0648\u0644\u064e',  #7wl?
              '\u0642\u0628\u0644',  #Qbl
              '\u0642\u0628\u0644\u064e',  #Qbl?
              '\u0648\u0641\u0642\u0627',  #wfQa
              '\u0625\u0644\u0649',  #alA
              '\u0627\u0644\u0649\u0648\u0631\u0627\u0621\u064e',  #alAwraq?
              '\u0648\u0631\u0627\u0621',  #wraq
              '\u0628\u064a\u0646\u064e',  #byn?
              '\u0628\u064a\u0646',  #byn
              '\u0628\u064a\u0646\u0647\u0645',  #bynhm
              '\u0628\u064a\u0646\u0647\u0645\u0627',  #bynhma
              '\u0628\u064a\u0646\u0643\u0645',  #bynkm
              '\u0628\u064a\u0646\u0645\u0627',  #bynma
              '\u0628\u062f\u0648\u0646',  #bdwn
              '\u0644\u0643\u0646',  #lkn
              '\u0628\u0627\u062a\u062c\u0627\u0647',  #batjah
              '\u0623\u0642\u0644',  #aQl
              '\u0627\u0642\u0644',  #aQl
              '\u0627\u0643\u062b\u0631'  #akUr
  )
  
  
  # Demonstrative, subject and relative pronouns
  pronouns <- c('\u0647\u0630\u0627',  #hia
                '\u0647\u0630\u0647',  #hih
                '\u0630\u0644\u0643',  #ilk
                '\u062a\u0644\u0643',  #tlk
                '\u0647\u0624\u0644\u064e\u0627\u0621',  #hol?aq
                '\u0647\u0624\u0644\u0627\u0621',  #holaq
                '\u0627\u0648\u0644\u0627\u0626\u0643',  #awla5k
                '\u0647\u0630\u0627\u0646',  #hian
                '\u0647\u0630\u064a\u0646\u0647\u062a\u0627\u0646',  #hiynhtan
                '\u0647\u062a\u064a\u0646\u0623\u0646\u0627',  #htynana
                '\u0627\u0646\u0627',  #ana
                '\u0623\u0646\u062a',  #ant
                '\u0647\u0645\u0627',  #hma
                '\u0623\u0646\u062a\u064e',  #ant?
                '\u0627\u0646\u062a',  #ant
                '\u0623\u0646\u062a',  #ant
                '\u0623\u0646\u062a\u0650',  #ant?
                '\u0627\u0646\u062a\u0647\u0648',  #anthw
                '\u0647\u0648\u064e',  #hw?
                '\u0647\u0648',  #hw
                '\u0647\u064a',  #hy
                '\u0647\u064a\u064e',  #hy?
                '\u0646\u062d\u0646',  #n7n
                '\u0623\u0646\u062a\u0645',  #antm
                '\u0627\u0646\u062a\u0645',  #antm
                '\u0623\u0646\u062a\u0645',  #antm
                '\u0627\u0646\u062a\u0645',  #antm
                '\u0647\u064f\u0645',  #h?m
                '\u0647\u0645',  #hm
                '\u0644\u0647\u0645',  #lhm
                '\u0645\u0646\u0647\u0645',  #mnhm
                '\u0648\u0647\u0645',  #whm
                '\u0627\u0644\u062a\u064a',  #alty
                '\u0627\u0644\u0630\u064a',  #aliy
                '\u0627\u0644\u0644\u0630\u0627\u0646',  #allian
                '\u0627\u0644\u0644\u0630\u064a\u0646',  #alliyn
                '\u0627\u0644\u0644\u062a\u0627\u0646',  #alltan
                '\u0627\u0644\u0644\u062a\u064a\u0646')  #alltyn
  
  # Particles
  particles <- c('\u0627\u0646',  #an
                 '\u0648\u0627\u0646',  #wan
                 '\u0625\u0646',  #an
                 '\u0625\u0646\u0647',  #anh
                 '\u0625\u0646\u0647\u0627',  #anha
                 '\u0625\u0646\u0647\u0645',  #anhm
                 '\u0625\u0646\u0647\u0645\u0627',  #anhma
                 '\u0625\u0646\u064a',  #any
                 '\u0648\u0625\u0646',  #wan
                 '\u0648\u0623\u0646',  #wan
                 '\u0627\u0646',  #an
                 '\u0627\u0646\u0647',  #anh
                 '\u0627\u0646\u0647\u0627',  #anha
                 '\u0627\u0646\u0647\u0645',  #anhm
                 '\u0627\u0646\u0647\u0645\u0627',  #anhma
                 '\u0627\u0646\u064a',  #any
                 '\u0623\u0646\u0643',  #ank
                 '\u0625\u0646\u0643',  #ank
                 '\u0627\u0646\u0643',  #ank
                 '\u0623\u0646\u0643\u0645',  #ankm
                 '\u0625\u0646\u0643\u0645',  #ankm
                 '\u0627\u0646\u0643\u0645',  #ankm
                 '\u0627\u0646\u0646\u0627',  #anna
                 '\u0648\u0627\u0646',  #wan
                 '\u0648\u0627\u0646',  #wan
                 '\u0623\u0646',  #an
                 '\u0627\u0646',  #an
                 '\u0623\u0644\u0627',  #ala
                 '\u0628\u0623\u0646',  #ban
                 '\u0627\u0646',  #an
                 '\u0627\u0644\u0627',  #ala
                 '\u0628\u0627\u0646',  #ban
                 '\u0628\u0627\u0646\u0647\u0645',  #banhm
                 '\u0623\u0646\u0647',  #anh
                 '\u0623\u0646\u0647\u0627',  #anha
                 '\u0623\u0646\u0647\u0645',  #anhm
                 '\u0623\u0646\u0647\u0645\u0627',  #anhma
                 '\u0627\u0646\u0647',  #anh
                 '\u0627\u0646\u0647\u0627',  #anha
                 '\u0627\u0646\u0647\u0645',  #anhm
                 '\u0627\u0646\u0647\u0645\u0627',  #anhma
                 '\u0623\u0630',  #ai
                 '\u0627\u0630',  #ai
                 '\u0627\u0630\u0627',  #aia
                 '\u0625\u0630',  #ai
                 '\u0625\u0630\u0627',  #aia
                 '\u0648\u0625\u0630',  #wai
                 '\u0648\u0625\u0630\u0627',  #waia
                 '\u0627\u0630',  #ai
                 '\u0627\u0630',  #ai
                 '\u0627\u0630\u0627',  #aia
                 '\u0627\u0630',  #ai
                 '\u0627\u0630\u0627',  #aia
                 '\u0641\u0627\u0630\u0627',  #faia
                 '\u0645\u0627\u0630\u0627',  #maia
                 '\u0648\u0627\u0630',  #wai
                 '\u0648\u0627\u0630\u0627',  #waia
                 '\u0644\u0648\u0644\u0627',  #lwla
                 '\u0644\u0648',  #lw
                 '\u0648\u0644\u0648\u0633\u0648\u0641',  #wlwswf
                 '\u0644\u0646',  #ln
                 '\u0645\u0627',  #ma
                 '\u0644\u0645',  #lm
                 '\u0648\u0644\u0645',  #wlm
                 '\u0623\u0645\u0627',  #ama
                 '\u0627\u0645\u0627',  #ama
                 '\u0644\u0627',  #la
                 '\u0648\u0644\u0627',  #wla
                 '\u0625\u0644\u0627',  #ala
                 '\u0627\u0644\u0627',  #ala
                 '\u0623\u0645',  #am
                 '\u0623\u0648',  #aw
                 '\u0627\u0645',  #am
                 '\u0627\u0648',  #aw
                 '\u0628\u0644',  #bl
                 '\u0642\u062f',  #Qd
                 '\u0648\u0642\u062f',  #wQd
                 '\u0644\u0642\u062f',  #lQd
                 '\u0623\u0646\u0645\u0627',  #anma
                 '\u0625\u0646\u0645\u0627',  #anma
                 '\u0628\u0644',  #bl
                 '\u0627\u0646\u0645\u0627',  #anma
                 '\u0627\u0646\u0645\u0627',  #anma
                 '\u0648')  #w
  
  # Connectors
  connectors <- c('\u0628\u0645\u0627',  #bma
                  '\u0643\u0645\u0627',  #kma
                  '\u0644\u0645\u0627',  #lma
                  '\u0644\u0623\u0646',  #lan
                  '\u0644\u0627\u0646',  #lan
                  '\u0644\u064a', #ly
                  '\u0644\u0649', #ly
                  '\u0644\u0647\u0630\u0623', #lhia
                  '\u0644\u0630\u0623', #lia
                  '\u0644\u0623\u0646\u0647',  #lanh
                  '\u0644\u0623\u0646\u0647\u0627',  #lanha
                  '\u0644\u0623\u0646\u0647\u0645',  #lanhm
                  '\u0644\u0627\u0646',  #lan
                  '\u0644\u0627\u0646\u0647',  #lanh
                  '\u0644\u0627\u0646\u0647\u0627',  #lanha
                  '\u0644\u0627\u0646\u0647\u0645',  #lanhm
                  '\u062b\u0645',  #Um
                  '\u0623\u064a\u0636\u0627',  #ayDa
                  '\u0627\u064a\u0636\u0627',  #ayDa
                  '\u0643\u0630\u0644\u0643',  #kilk
                  '\u0642\u0628\u0644',  #Qbl
                  '\u0628\u0639\u062f',  #b3d
                  '\u0644\u0643\u0646',  #lkn
                  '\u0648\u0644\u0643\u0646',  #wlkn
                  '\u0644\u0643\u0646\u0647',  #lknh
                  '\u0644\u0643\u0646\u0647\u0627',  #lknha
                  '\u0644\u0643\u0646\u0647\u0645',  #lknhm
                  '\u0641\u0642\u0637',  #fQT
                  '\u0631\u063a\u0645',  #rGm
                  '\u0628\u0627\u0644\u0631\u063a\u0645',  #balrGm
                  '\u0628\u0641\u0636\u0644',  #bfDl
                  '\u062d\u064a\u062b',  #7yU
                  '\u0628\u062d\u064a\u062b',  #b7yU
                  '\u0644\u0643\u064a',  #lky
                  '\u0647\u0646\u0627',  #hna
                  '\u0647\u0646\u0627\u0643',  #hnak
                  '\u0628\u0633\u0628\u0628',  #bsbb
                  '\u0630\u0627\u062a',  #iat
                  '\u0630\u0648',  #iw
                  '\u0630\u064a',  #iy
                  '\u0630\u0649',  #iy
                  '\u0648\u0647', #wh
                  '\u064a\u0627',  #ya
                  '\u0627\u0646\u0645\u0627',  #anma
                  '\u0641\u0647\u0630\u0627',  #fhia
                  '\u0641\u0647\u0648',  #fhw
                  '\u0641\u0645\u0627',  #fma
                  '\u0641\u0645\u0646',  #fmn
                  '\u0641\u064a\u0645\u0627', #fyma
                  '\u0641\u0647\u0644',  #fhl
                  '\u0648\u0647\u0644',  #whl
                  '\u0641\u0647\u0624\u0644\u0627\u0621',  #fholaq
                  '\u0643\u0630\u0627', #kia
                  '\u0644\u0630\u0644\u0643', #lilk
                  '\u0644\u0645\u0627\u0630\u0627', #lmaia
                  '\u0644\u0645\u0646', #lmn
                  '\u0644\u0646\u0627',  #lna
                  '\u0645\u0646\u0627',  #mna
                  '\u0645\u0646\u0643',  #mnk
                  '\u0645\u0646\u0643\u0645',  #mnkm
                  '\u0645\u0646\u0647\u0645\u0627',  #mnhm
                  '\u0645\u0646\u0647\u0645\u0627',  #mnhma
                  '\u0644\u0643', #lk
                  '\u0648\u0644\u0648', #wlw
                  '\u0645\u0645\u0627', #mma
                  '\u0648\u0645\u0627', #wma
                  '\u0648\u0645\u0646', #wmn
                  '\u0639\u0646\u062f',  #3nd
                  '\u0639\u0646\u062f\u0647\u0645',  #3ndhm
                  '\u0639\u0646\u062f\u0645\u0627',  #3ndma
                  '\u0639\u0646\u062f\u0646\u0627',  #3ndna
                  '\u0639\u0646\u0647\u0645\u0627',  #3nhma
                  '\u0639\u0646\u0643',  #3nk
                  '\u0627\u0630\u0646',  #ain
                  '\u0627\u0644\u0630\u064a',  #aliy
                  '\u0641\u0627\u0646\u0627',  #fana
                  '\u0641\u0627\u0646\u0647\u0645',  #fanhm
                  '\u0641\u0647\u0645',  #fhm
                  '\u0641\u0647',  #fh
                  '\u0641\u0643\u0644',  #fkl
                  '\u0644\u0643\u0644',  #lkl
                  '\u0644\u0643\u0645',  #lkm
                  '\u0641\u0644\u0645',  #flm
                  '\u0641\u0644\u0645\u0627',  #flma
                  '\u0641\u064a\u0643',  #fyk
                  '\u0641\u064a\u0643\u0645',  #fykm
                  '\u0644\u0647\u0630\u0627')    # lhia
  
  all <- c(preps,pronouns,particles,connectors)
  all <- unique(c(all,fixAlifs(all)))
  
  if(length(textsSplit) > 0){
    for(i in 1:length(textsSplit)){
      if(textsSplit[i] %in% all){textsSplit[i] <- ""}
    } 
  }
  texts <- paste(textsSplit, collapse=" ")
  texts <- trim(gsub(" {2,}"," ", texts))
  return(list(text=texts,arabicStopwordList=all))
}

#######################################################
## Transliterate from arabic to latin characters for use with text analysis software.
## All transliteration is 1-to-1, except for some special characters which I provide 
## transliteration for but generally recommend cleaning out instead.

transliterate <- function(texts){
  # The alphabet 
  texts <- gsub('\u0627', 'a', texts)
  texts <- gsub('\u0649', 'A', texts)
  texts <- gsub('\u0628', 'b', texts)
  texts <- gsub('\u062a', 't', texts)
  texts <- gsub('\u062b', 'U', texts)
  texts <- gsub('\u062c', 'j', texts)
  texts <- gsub('\u062d', '7', texts)
  texts <- gsub('\u062e', 'K', texts)
  texts <- gsub('\u062f', 'd', texts)
  texts <- gsub('\u0630', 'i', texts)
  texts <- gsub('\u0631', 'r', texts)
  texts <- gsub('\u0632', 'z', texts)
  texts <- gsub('\u0633', 's', texts)
  texts <- gsub('\u0634', 'W', texts)
  texts <- gsub('\u0635', 'S', texts)
  texts <- gsub('\u0636', 'D', texts)
  texts <- gsub('\u0637', 'T', texts)
  texts <- gsub('\u0638', 'Z', texts)
  texts <- gsub('\u0639', '3', texts)
  texts <- gsub('\u063a', 'G', texts)
  texts <- gsub('\u0641', 'f', texts)
  texts <- gsub('\u0642', 'Q', texts)
  texts <- gsub('\u0643', 'k', texts)
  texts <- gsub('\u0644', 'l', texts)
  texts <- gsub('\u0645', 'm', texts)
  texts <- gsub('\u0646', 'n', texts)
  texts <- gsub('\u0647', 'h', texts)
  texts <- gsub('\u0648', 'w', texts)
  texts <- gsub('\u064a', 'y', texts)
  # Hamzas
  texts <- gsub('\u0623', 'a', texts)
  texts <- gsub('\u0625', 'a', texts)
  texts <- gsub('\u0624', 'o', texts)
  texts <- gsub('\u0626', '5', texts)
  texts <- gsub('\u0621', 'q', texts)
  texts <- gsub('\u0622', 'a', texts)
  # taa-marbuta and other special letters
  texts <- gsub('\u0629', '0', texts)
  
  # Rare Characters
  texts <- gsub('\u067E', 'p', texts)  # Arabic "peh" -- ba with three dots
  texts <- gsub('\u06C1', 'h', texts)  # another version of heh
  texts <- gsub('\u06A9', 'k', texts)  # another version of kaf, called keheh
  texts <- gsub('\u0679', 't', texts)  # a taa with a Taa over it?
  texts <- gsub("\u06BA", 'n', texts)  # a noon without a dot?
  texts <- gsub("\u06D2", 'y', texts)  # a weird yeh
  texts <- gsub('\u06cc', 'y', texts)  # ARABIC LETTER DOTLESS YA
  texts <- gsub("\u0671", 'a', texts)  # ARABIC LETTER DOTLESS YA
  texts <- gsub("\ufedf", 'l', texts)  # http://www.webtoolhub.com/tn561380-xhtml-characters-list.aspx?type=script&category=arabic-form-b
  texts <- gsub("\uFEEB", 'h', texts)  # http://www.webtoolhub.com/tn561380-xhtml-characters-list.aspx?type=script&category=arabic-form-b
  texts <- gsub("\u063f", 'y', texts)  # (special three dot yeh) http://www.marathon-studios.com/unicode/U063F/Arabic_Letter_Farsi_Yeh_With_Three_Dots_Above
  texts <- gsub("\u063d", 'y', texts)  # special yah: http://www.marathon-studios.com/unicode/U063D/Arabic_Letter_Farsi_Yeh_With_Inverted_V
  texts <- gsub("\u063e", 'y', texts)  # special yah
  texts <- gsub("\u063b", 'k', texts)  # keheh with dots
  texts <- gsub("\u063c", 'k', texts)  # keheh with dots
  
  return(texts)
}



#######################################################
## Transliterate from Latin to Arabic

## this reverses the transliteration, except that it cannot reproduce hamzas on alifs (they 
## all come out as plain alifs).

reverse.transliterate <- function(texts){
  
  txts <- unlist(strsplit(texts, ""))
  
  # The alphabet 
  txts <- sapply(txts, gsub, pattern='a', replacement='\u0627')
  txts <- sapply(txts, gsub, pattern='A', replacement='\u0649')
  txts <- sapply(txts, gsub, pattern='b', replacement='\u0628')
  txts <- sapply(txts, gsub, pattern='t', replacement='\u062a')
  txts <- sapply(txts, gsub, pattern='U', replacement='\u062b')
  txts <- sapply(txts, gsub, pattern='j', replacement='\u062c')
  txts <- sapply(txts, gsub, pattern='7', replacement='\u062d')
  txts <- sapply(txts, gsub, pattern='K', replacement='\u062e')
  txts <- sapply(txts, gsub, pattern='d', replacement='\u062f')
  txts <- sapply(txts, gsub, pattern='i', replacement='\u0630')
  txts <- sapply(txts, gsub, pattern='r', replacement='\u0631')
  txts <- sapply(txts, gsub, pattern='z', replacement='\u0632')
  txts <- sapply(txts, gsub, pattern='s', replacement='\u0633')
  txts <- sapply(txts, gsub, pattern='W', replacement='\u0634')
  txts <- sapply(txts, gsub, pattern='S', replacement='\u0635')
  txts <- sapply(txts, gsub, pattern='D', replacement='\u0636')
  txts <- sapply(txts, gsub, pattern='T', replacement='\u0637')
  txts <- sapply(txts, gsub, pattern='Z', replacement='\u0638')
  txts <- sapply(txts, gsub, pattern='3', replacement='\u0639')
  txts <- sapply(txts, gsub, pattern='G', replacement='\u063a')
  txts <- sapply(txts, gsub, pattern='f', replacement='\u0641')
  txts <- sapply(txts, gsub, pattern='Q', replacement='\u0642')
  txts <- sapply(txts, gsub, pattern='k', replacement='\u0643')
  txts <- sapply(txts, gsub, pattern='l', replacement='\u0644')
  txts <- sapply(txts, gsub, pattern='m', replacement='\u0645')
  txts <- sapply(txts, gsub, pattern='n', replacement='\u0646')
  txts <- sapply(txts, gsub, pattern='h', replacement='\u0647')
  txts <- sapply(txts, gsub, pattern='w', replacement='\u0648')
  txts <- sapply(txts, gsub, pattern='y', replacement='\u064a')
  # Hamzas
  txts <- sapply(txts, gsub, pattern='o', replacement='\u0624')
  txts <- sapply(txts, gsub, pattern='5', replacement='\u0626')
  txts <- sapply(txts, gsub, pattern='q', replacement='\u0621')
  # taa-marbuta and other special letters
  txts <- sapply(txts, gsub, pattern='0', replacement='\u0629') 
  txts <- sapply(txts, gsub, pattern='a', replacement='\u064b') # this is the tanwiin over alif at the end of accusative nouns
  
  # Rare Characters
  txts <- sapply(txts, gsub, pattern='p', replacement='\u067E')  # Arabic "peh" -- ba with three dots
  
  texts <- paste(txts, collapse="")
  Encoding(texts) <- "UTF-8"
  
  return(texts)
}


############################################################
## removes prefixes and suffixes, roughly in the same way as the light10 stemmer

############################################################
## Slower, more useful stemming.
## The first stemmer ("doStemming") is a bit slower on benchmarks
## but does some useful things:  1) it's modular, and 2) it returns
## a list of the stemmed and unstemmed terms so you can back out
## what words are associated with what stems.

## The arguments specify, for each prefix, the length that the word must
## be in order to have a prefix removed.
## Note that I only allow one prefix to be taken off each word.
## I also have a list of words not to stem (variants of allah).

## These are the component functions for removing each prefix
## These functions expect a word split into letters
## alif-lam
rAlifLam <- function(word, minwordlen=4){if(length(word) >= minwordlen){if(paste(word[1:2],collapse="") == '\u0627\u0644'){word[1:2] <- ""}}; return(paste(word,collapse=""))}
## waw-alif-lam
rWawAlifLam <- function(word, minwordlen=5){if(length(word) >= minwordlen){if(paste(word[1:3],collapse="") == '\u0648\u0627\u0644'){word[1:3] <- ""}}; return(paste(word,collapse=""))}
## ba-alif-lam
rBaAlifLam <- function(word, minwordlen=5){if(length(word) >= minwordlen){if(paste(word[1:3],collapse="") == '\u0628\u0627\u0644'){word[1:3] <- ""}}; return(paste(word,collapse=""))}
## kaf-alif-lam
rKafAlifLam <- function(word, minwordlen=5){if(length(word) >= minwordlen){if(paste(word[1:3],collapse="") == '\u0643\u0627\u0644'){word[1:3] <- ""}}; return(paste(word,collapse=""))}
## fa-alif-lam
rFaAlifLam <- function(word, minwordlen=5){if(length(word) >= minwordlen){if(paste(word[1:3],collapse="") == '\u0641\u0627\u0644'){word[1:3] <- ""}}; return(paste(word,collapse=""))}
## lam-lam
rLamLam <- function(word, minwordlen=4){if(length(word) >= minwordlen){if(paste(word[1:2],collapse="") == '\u0644\u0644'){word[1:2] <- ""}}; return(paste(word,collapse=""))}
## waw
rWaw <- function(word, minwordlen=4){if(length(word) >= minwordlen){if(paste(word[1],collapse="") == '\u0648'){word[1] <- ""}}; return(paste(word,collapse=""))}

## suffixes
## ha-alif
rHaAlif <- function(word, minwordlen=4){if(length(word) >= minwordlen){if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u0647\u0627'){word[ (length(word)-1):length(word) ] <- ""}}; return(paste(word,collapse=""))}
## alif-nun
rAlifNun <- function(word, minwordlen=4){if(length(word) >= minwordlen){if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u0627\u0646'){word[ (length(word)-1):length(word) ] <- ""}}; return(paste(word,collapse=""))}
## alif-ta
rAlifTa <- function(word, minwordlen=4){if(length(word) >= minwordlen){if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u0627\u062a'){word[ (length(word)-1):length(word) ] <- ""}}; return(paste(word,collapse=""))}
## waw-nun
rWawNun <- function(word, minwordlen=4){if(length(word) >= minwordlen){if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u0648\u0646'){word[ (length(word)-1):length(word) ] <- ""}}; return(paste(word,collapse=""))}
## yah-nun
rYahNun <- function(word, minwordlen=4){if(length(word) >= minwordlen){if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u064a\u0646'){word[ (length(word)-1):length(word) ] <- ""}}; return(paste(word,collapse=""))}
## yah-heh
rYahHeh <- function(word, minwordlen=4){if(length(word) >= minwordlen){if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u064a\u0647'){word[ (length(word)-1):length(word) ] <- ""}}; return(paste(word,collapse=""))}
#yah-ta marbutta
rYahTamarbutta <- function(word, minwordlen=4){if(length(word) >= minwordlen){if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u064a\u0629'){word[ (length(word)-1):length(word) ] <- ""}}; return(paste(word,collapse=""))}
# heh
rHeh <- function(word, minwordlen=3){if(length(word) >= minwordlen){if(paste(word[ length(word) ],collapse="") == '\u0647'){word[ length(word) ] <- ""}}; return(paste(word,collapse=""))}
# ta marbutta
rTamarbutta <- function(word, minwordlen=3){if(length(word) >= minwordlen){if(paste(word[ length(word) ],collapse="") == '\u0629'){word[ length(word) ] <- ""}}; return(paste(word,collapse=""))}
# yah
rYah <- function(word, minwordlen=3){if(length(word) >= minwordlen){if(paste(word[ length(word) ],collapse="") == '\u064a'){word[ length(word) ] <- ""}}; return(paste(word,collapse=""))}


## bundle them all together
doStemming <- function(texts, dontstem =  c('\u0627\u0644\u0644\u0647','\u0644\u0644\u0647')){
  # Split up the words...
  textsSplit <- strsplit(texts," ")[[1]]
  ## if there are actually words to stem
  if(length(textsSplit) > 0){
    ts0 <- textsSplit
    # run the prefix functions
    for(i in 1:length(textsSplit)){
      word0 <- textsSplit[i]
      if(!(word0 %in% dontstem)){
        word1 <- strsplit(word0,"")[[1]]
        word <- rAlifLam(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rWawAlifLam(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rBaAlifLam(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rKafAlifLam(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rFaAlifLam(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rLamLam(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rWaw(word1);if(word != word0){textsSplit[i] <- word; next}    
      }
    }
    # suffixes
    for(i in 1:length(textsSplit)){
      word0 <- textsSplit[i]
      if(!(word0 %in% dontstem)){
        word1 <- strsplit(word0,"")[[1]]
        word <- rHaAlif(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rAlifNun(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rAlifTa(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rWawNun(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rYahNun(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rYahHeh(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rYahTamarbutta(word1);if(word != word0){textsSplit[i] <- word; next}    
        word <- rHeh(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rTamarbutta(word1);if(word != word0){textsSplit[i] <- word; next}
        word <- rYah(word1);if(word != word0){textsSplit[i] <- word; next}    
      }
    }
    ## return the texts pasted back together
    names(textsSplit) <- ts0
  } # end "if length textsSplit > 0"
  texts <- paste(textsSplit,collapse=" ")
  return(list(text=texts, stemmedWords=textsSplit))
}


############################################################
## another stemmer that is even faster but doesn't allow
## different word lengths for different prefixes and suffixes

doStemming2 <- function(texts, dontstem =  c('\u0627\u0644\u0644\u0647','\u0644\u0644\u0647')){
  # Split up the words...
  textsSplit <- strsplit(texts," ")[[1]]
  ts0 <- textsSplit
  
  # define prefixes  -- move these down below the other block and uncomment perl=T to do look-aheads
  AlifLam <- '\u0627\u0644(?=.{2,})'
  WawAlifLam <- '\u0648\u0627\u0644(?=..)'
  BaAlifLam <- '\u0628\u0627\u0644(?=..)'
  KafAlifLam <- '\u0643\u0627\u0644(?=..)'
  FaAlifLam <- '\u0641\u0627\u0644(?=..)'
  LamLam <- '\u0644\u0644(?=..)'
  Waw <- '\u0648(?=...)'
  # define suffixes
  HaAlif <- '(?<=..)\u0647\u0627'
  AlifNun <- '(?<=..)\u0627\u0646'
  AlifTa <- '(?<=..)\u0627\u062a'
  WawNun <- '(?<=..)\u0648\u0646'
  YahNun <- '(?<=..)\u064a\u0646'
  YahHeh <- '(?<=..)\u064a\u0647'
  YahTamarbutta <- '(?<=..)\u064a\u0629'
  Heh <- '(?<=..)\u0647'
  Tamarbutta <- '(?<=..)\u0629'
  Yah <- '(?<=..)\u064a'
  
  # define prefixes
  AlifLam <- '\u0627\u0644'
  WawAlifLam <- '\u0648\u0627\u0644'
  BaAlifLam <- '\u0628\u0627\u0644'
  KafAlifLam <- '\u0643\u0627\u0644'
  FaAlifLam <- '\u0641\u0627\u0644'
  LamLam <- '\u0644\u0644'
  Waw <- '\u0648'
  # define suffixes
  HaAlif <- '\u0647\u0627'
  AlifNun <- '\u0627\u0646'
  AlifTa <- '\u0627\u062a'
  WawNun <- '\u0648\u0646'
  YahNun <- '\u064a\u0646'
  YahHeh <- '\u064a\u0647'
  YahTamarbutta <- '\u064a\u0629'
  Heh <- '\u0647'
  Tamarbutta <- '\u0629'
  Yah <- '\u064a'
  
  ## prefixes
  for(i in 1:length(textsSplit)){
    word0 <- textsSplit[i]
    if(!(word0 %in% dontstem)){
      textsSplit[i] <- gsub(paste0("^",paste(AlifLam,WawAlifLam,BaAlifLam,KafAlifLam,FaAlifLam,LamLam,Waw,sep="|^")), '', word0)#, perl=T)
    }
  }
  # suffixes
  for(i in 1:length(textsSplit)){
    word0 <- textsSplit[i]
    if(!(word0 %in% dontstem)){
      textsSplit[i] <- gsub(paste0(paste(HaAlif,AlifNun,AlifTa,WawNun,YahNun,YahHeh,YahTamarbutta,Heh,Tamarbutta,Yah,sep="$|"),"$"), '', word0)#, perl=T)
    }
  }
  ## return the texts pasted back together
  names(textsSplit) <- ts0
  texts <- paste(textsSplit,collapse=" ")
  return(list(text=texts, stemmedWords=textsSplit))
}


############################################################
## faster, less flexible stemming

## The next two functions, "removePrefixes" and "removeSuffixes" are a bit faster
## on benchmarks but not modular and don't return a list matching words to stemmed words.

## The arguments specify, for each suffix, the length that the word must
## be in order to have a suffix removed.
## Note that I only allow one suffix to be taken off each word.
## I also have a list of words not to stem (variants of allah).

removePrefixes <- function(texts, x1=4, x2=4, x3=5, x4=5, x5=5,
                           x6=5, x7=4, dontstem =  c('\u0627\u0644\u0644\u0647','u0644\u0644\u0647') ){
  
  # Split up the words...
  textsSplit = strsplit(texts," ")[[1]]
  ## if there are actually words to stem
  if(length(textsSplit) > 0){
    
    for(i in 1:length(textsSplit)){
      word = textsSplit[i]
      
      ## a list of words to not stem
      if(!(word %in% dontstem)){
        
        word = strsplit(word,"")[[1]]  
        
        ## alif-lam
        if(length(word) >= x2){                 
          if(paste(word[1:2],collapse="") == '\u0627\u0644'){
            word[1:2] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## waw-alif-lam
        if(length(word) >= x3){                 
          if(paste(word[1:3],collapse="") == '\u0648\u0627\u0644'){
            word[1:3] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## ba-alif-lam
        if(length(word) >= x4){
          if(paste(word[1:3],collapse="") == '\u0628\u0627\u0644'){
            word[1:3] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## kaf-alif-lam
        if(length(word) >= x5){
          if(paste(word[1:3],collapse="") == '\u0643\u0627\u0644'){
            word[1:3] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## fa-alif-lam
        if(length(word) >= x6){
          if(paste(word[1:3],collapse="") == '\u0641\u0627\u0644'){
            word[1:3] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## lam-lam
        if(length(word) >= x7){
          if(paste(word[1:2],collapse="") == '\u0644\u0644'){
            word[1:2] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## waw
        if(length(word) >= x1){
          if(word[1] == '\u0648'){
            word[1] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
      } # close the "if word isn't in this list"
    } # close loop over words
  } # close "if length textsSplit > 0"
  texts <- trim(paste(textsSplit,collapse=" "))
  texts <- trim(gsub(" {2,}"," ", texts))
  return(texts)
}


############################################################
## remove suffixes, roughly in the same way as the light10 stemmer

## The arguments specify, for each suffix, the length that the word must
## be in order to have a suffix removed.
## Note that I only allow one suffix to be taken off each word.
## I also have a list of words not to stem (variants of allah).

removeSuffixes <- function(texts, x1=4, x2=4, x3=4, x4=4, x5=4, x6=4, x7=4, x8=3, x9=3, x10=3,
                           dontstem =  c('\u0627\u0644\u0644\u0647','u0644\u0644\u0647')){
  
  # Split up the words...
  textsSplit = strsplit(texts," ")[[1]]
  
  ## if there are actually words to stem
  if(length(textsSplit) > 0){
    
    for(i in 1:length(textsSplit)){
      word = textsSplit[i]
      
      ## a list of words to not stem
      if(!(word %in% dontstem)){
        
        word = strsplit(word,"")[[1]]  
        
        ## ha-alif
        if(length(word) >= x1){                 
          if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u0647\u0627'){
            word[ (length(word)-1):length(word) ] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## alif-nun
        if(length(word) >= x2){                 
          if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u0627\u0646'){
            word[ (length(word)-1):length(word) ] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }                  
        
        ## alif-ta
        if(length(word) >= x3){                 
          if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u0627\u062a'){
            word[ (length(word)-1):length(word) ] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }   
        
        ## waw-nun
        if(length(word) >= x4){                 
          if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u0648\u0646'){
            word[ (length(word)-1):length(word) ] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## yah-nun
        if(length(word) >= x5){                 
          if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u064a\u0646'){
            word[ (length(word)-1):length(word) ] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## yah-heh
        if(length(word) >= x6){                 
          if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u064a\u0647'){
            word[ (length(word)-1):length(word) ] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## yah-ta marbutta
        if(length(word) >= x7){                 
          if(paste(word[ (length(word)-1):length(word) ],collapse="") == '\u064a\u0629'){
            word[ (length(word)-1):length(word) ] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## heh
        if(length(word) >= x8){                 
          if(paste(word[ length(word) ],collapse="") == '\u0647'){
            word[ length(word) ] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## ta marbutta
        if(length(word) >= x9){                 
          if(paste(word[ length(word) ],collapse="") == '\u0629'){
            word[ length(word) ] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        }
        
        ## yah
        if(length(word) >= x9){                 
          if(paste(word[ length(word) ],collapse="") == '\u064a'){
            word[ length(word) ] <- ""
            textsSplit[i] <- trim(paste(word,collapse=""))
            next
          }
        } 
      } # close the "if word isn't in this list"
    } # close loop over words  
  } # close "if length textsSplit > 0"
  texts <- trim(paste(textsSplit,collapse=" "))
  texts <- trim(gsub(" {2,}"," ", texts))
  return(texts)
}

############################################################


