##########################################################
###                       Thesis                       ###
##########################################################

##########################################################
### --- Author: Manuel von Krosigk
### --- Date: 2017-07-14
### --- Description: structuring unstructured text files
##########################################################

# Manipulate for TA -------------------------------------------------------

setwd("C:/Users/Admin/Google Drive/Masterthesis")

library(tm)

## OMO before 2007:
#content <- Corpus(DirSource("data/articles_2001_2007/2001_12_11")) # does not read neat
pathname <- "/data/articles_2001_2007/2015-12-16/"

# load files into corpus
# get listing of .txt files in directory
filenames <- list.files(paste0(getwd(), pathname),pattern='*.txt')

library(readr)
# read files into a character vector
files <- lapply(paste0(paste0(getwd(), pathname),filenames),readLines)
for(i in seq(files)){files[i]  <- paste(files[i])}
#files <- lapply(paste0(paste0(getwd(), pathname),filenames),read_file)

# create corpus from vector
content <- Corpus(VectorSource(files))
##

## OMO after 2007:
load("OMO_20151217_short.RData") # articles +/- 5 days around OMO 20151217
min(OMO_20151217[,3]) # first article
max(OMO_20151217[,3]) # last article 

# maybe smoother with "DirSource(directory = "texts/",encoding ="latin1" )"
#title   <- Corpus(VectorSource(OMO_20151217[,2])) # discard?
content <- Corpus(VectorSource(OMO_20151217[,1]))


#summary(content)[1:10,]
# add title to content column -> slow
#transform(OMO_20151217, newcol=paste(OMO_20151217[,2], OMO_20151217[,1], sep=" "))

# inspect(title)
#writeLines(as.character(title[[23]]))
writeLines(as.character(content[[23]])) # example

# detect text language
library(textcat)
all(textcat(content)=="english") # everything in english?
content <- Corpus(VectorSource(OMO_20151217[which(
  textcat(content)=="english"),1])) 

# POS tagging -------------------------------------------------------------
# part of speech tagging, see Schweinberger(2016)
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type ="source")
library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(stringr)
library(gsubfn)
library(plyr)

# prepare corpus for POS tagging, convert to character
corpus.tmp    <- laply(content, function(t){as.character(t)})
# paste all elements of the corpus together
corpus.tmp    <- lapply(corpus.tmp, function(x){x <- paste(x, collapse = " ")})
# clean corpus
corpus.tmp    <- lapply(corpus.tmp, function(x){x <- enc2utf8(x)})
corpus.tmp    <- gsub(" {2,}", " ", corpus.tmp)
# remove spaces at beginning and end of strings
corpus.tmp    <- str_trim(corpus.tmp, side = "both")

# convert corpus files into strings
corpus.tmp    <- lapply(corpus.tmp, function(x){x <- as.String(x)})

# apply annotators to Corpus
Corpus.tagged <- lapply(corpus.tmp, function(x){
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator    <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator,word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  # y3 <- annotate (x, Maxent_POS_Tag_Annotator(probs = TRUE), y1)
  y2w  <- subset(y2, type == "word")
  tags <- sapply(y2w$features , '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)}
)

# alternative: http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/
# output format not optimal for further analyses

Corpus.tagged_backup <- Corpus.tagged

# Pre-process data --------------------------------------------------------

# list possible tags; 'nested' acronyms last!
Tags_unique <- c("''", "-LRB-", "-RRB-", "$", ",", ":",  "``", "CC",
                 "CD", "DT", "EX", "FW", "IN", "JJR", "JJS", "JJ", "MD", 
                 "NNPS", "NNP", "NNS", "NN", "PDT", "POS", "PRP$", "PRP",
                 "RBR", "RBS", "RB", "RP", "SYM", "TO", "UH", "VBD",
                 "VBG", "VBN", "VBP", "VBZ", "VB","WDT", "WP$","WP", "WRB")
Tags_keep    <- c(#"-LRB-", "-RRB-",
                "CC", "FW", "JJR", "JJS", "JJ", 
                "MD", "NNPS", "NNP", "NNS", "NN", "PDT", "RBR",
                "RBS", "RB", "VBD", "VBG", "VBN", "VBP", "VBZ", "VB")
Tags_discard <- setdiff(Tags_unique,Tags_keep)

#Corpus.tagged <- Corpus.tagged_backup

# remove non-cruicial terms
for(i in seq(Corpus.tagged)){
  for(j in seq(Tags_discard)){
    Corpus.tagged[[i]] <- gsub(
      paste(paste("\\b\\S+", Tags_discard[j], sep = ""),"\\b",sep=""), "", Corpus.tagged[[i]])
  }
  #  Corpus.tagged[[i]] <- gsub("\\b\\S+CD\\b", "", Corpus.tagged[[i]]) #rm /CD manually
  #  Corpus.tagged[[i]] <- gsub(",/,", "", Corpus.tagged[[i]]) # rm ,/,
  #  Corpus.tagged[[i]] <- gsub("./.", "", Corpus.tagged[[i]]) # rm ./.
  #  Corpus.tagged[[i]] <- gsub("$", "", Corpus.tagged[[i]])   # rm $ -> doesn't work?
  #  Corpus.tagged[[i]] <- gsub("'", "", Corpus.tagged[[i]])   # rm '    
} # alt. use tm_map to remove numbers and punctuation

# remove tags from cruicial words
for (i in seq(Corpus.tagged)){
  for(j in seq(Tags_keep)){  
    Corpus.tagged[[i]] <- gsub(
      paste("/", Tags_keep[j], sep = ""), "", Corpus.tagged[[i]])
  }
}

# inspect
#Corpus.tagged[[1]]
#Corpus.tagged_backup[[1]]

# convert list to VCorpus
Corpus.untagged <- Corpus(VectorSource(Corpus.tagged))
# Corpus.untagged <- as.VCorpus(Corpus.tagged) # bad idea

# remove punctuation
Corpus.untagged <- tm_map(Corpus.untagged, removePunctuation)
# remove digits (DO WE NEED THEM)
Corpus.untagged <- tm_map(Corpus.untagged, removeNumbers)

# transform all words to lowercase (careful with meta data)
#Corpus.untagged <- tm_map(Corpus.untagged, tolower) # content_transformer(tolower)?
Corpus.untagged <- tm_map(Corpus.untagged, content_transformer(tolower))

# stopwords or tf-idf, c.f. http://tidytextmining.com/tfidf.html (important words in all docs!)

# how to best discard stopwords so no ambiguous words get deleted?
stpw1  <- stopwords('english') # any words with ambiguous meaning in context?
stpw2  <- scan(file='data/MyStopwords.txt', what='character',
               quiet=T) # own stop words
comn    <- unique(c(stpw1,stpw2)) # select unique stopwords
mystopwords <- unique(c(gsub("'","",comn),comn)) # final stop word list
Corpus.untagged <- tm_map(Corpus.untagged, removeWords, mystopwords)

# remove unnecessary whitespace TO LIST OR TO CORPUS?
Corpus.untagged <- tm_map(Corpus.untagged, stripWhitespace)

# alternative a):
#for (i in seq(Corpus.untagged)){
#  Corpus.untagged[[i]] <- gsub(" {2 ,}", "", Corpus.untagged[[i]])
#} # concatenates words in a weird way... FIX!

# alternative b):
#content <- gsub (" {2 ,}", " ", content)

#Corpus.untagged[[1]]
#writeLines(as.character(Corpus.untagged[[23]]))
#writeLines(as.character(content[[23]]))


# put in earlier?
# combine words that should stay together
comb.phrases <- scan(file='data/CombPhrases.txt', what='character',quiet=T,sep=",")
for(j in seq(Corpus.untagged)){
  # n-grams
  for(i in seq(comb.phrases)){# substitute phrase without space
  Corpus.untagged[[j]]$content <- gsub(comb.phrases[i], 
                               gsub(" ","",comb.phrases[i]), Corpus.untagged[[j]]$content)
  }
  # stemming problems by hand
  Corpus.untagged[[j]]$content <- gsub("rates", "rate", Corpus.untagged[[j]]$content)
  Corpus.untagged[[j]]$content <- gsub("economic", "econom", Corpus.untagged[[j]]$content)
  Corpus.untagged[[j]]$content <- gsub("economy", "econom", Corpus.untagged[[j]]$content)
  Corpus.untagged[[j]]$content <- gsub("schools", "school", Corpus.untagged[[j]]$content) 
  Corpus.untagged[[j]]$content <- gsub("funds", "fund", Corpus.untagged[[j]]$content) 
} # $content so the meta data does not change, important for character transformation

# stemming -> deterministic or statistical
# library(Rstem) # needs C/C++/Fortran
# lemmatisation: https://cran.r-project.org/web/packages/openNLP/openNLP.pdf
Corpus.untagged <- tm_map(Corpus.untagged,stemDocument)

writeLines(as.character(Corpus.untagged[[1]])) # inspect

# for further analysis try
articles <- laply(Corpus.untagged, function(t){as.character(t)})

# export text body
setwd("C:/Users/Admin/Google Drive/Masterthesis")
#save(Corpus.untagged, file="data/texts/20151217_Corpus_Clean.RData")

# Pre-process data alternative --------------------------------------------
# create the toSpace content transformer
toSpace <- content_transformer(
  function(x, pattern){
    return (gsub(pattern, " ", x))
  })

# transform to lower case (need to wrap in content_transformer)
content <- tm_map(content, content_transformer(tolower))

# eliminate non-text elements
notext  <- c(" ") # c/p from .txt file
for(i in 1:length(notext)){
  content <- tm_map(content, toSpace, notext[i]) 
}
content <- tm_map(content, removePunctuation)
# strip digits (std transformation, so no need for content_transformer)
content <- tm_map(content, removeNumbers) # numbers needed?

# combine words that should stay together ! to be extended
for (j in seq(content)){
  content[[j]] <- gsub("percentage point", "percentagepoint", content[[j]])
  content[[j]] <- gsub("economic recovery", "economicrecovery", content[[j]])
  content[[j]] <- gsub("janet yellen", "janetyellen", content[[j]])
  content[[j]] <- gsub("federal reserve bank", "fed", content[[j]])
  content[[j]] <- gsub("federal reserve", "fed", content[[j]])
  content[[j]] <- gsub("federal funds rate target", "federalfundsratetarget", content[[j]])
  content[[j]] <- gsub("monetary policy", "monetarypolicy", content[[j]])
  content[[j]] <- gsub("unemployment rate", "unemploymentrate", content[[j]])
  content[[j]] <- gsub("central bank", "centralbank", content[[j]])
  content[[j]] <- gsub("rates", "rate", content[[j]])
  content[[j]] <- gsub("economic", "econom", content[[j]])
  content[[j]] <- gsub("economy", "econom", content[[j]])
  content[[j]] <- gsub("last week", "lastweek", content[[j]])
  content[[j]] <- gsub("next week", "nextweek", content[[j]]) 
  content[[j]] <- gsub("part time", "parttime", content[[j]]) 
  content[[j]] <- gsub("schools", "school", content[[j]]) 
  content[[j]] <- gsub("funds", "fund", content[[j]]) 
} # add phrases that indicate endog and exog

# remove stopwords using the standard list in tm package
# remove them after part of speech tagging?

stpw1  <- stopwords('english')
stpw2  <- scan(file='data/MyStopwords.txt', what='character',
               quiet=T) # own stop words
comn    <- unique(c(stpw1,stpw2)) # select unique stopwords
mystopwords <- unique(c(gsub("'","",comn),comn)) # final stop word list
content <- tm_map(content, removeWords, mystopwords)

# some weird concatenated words like "accommodativeyellen"

# strip whitespace only cosmetic, rather not
# content <- tm_map(content, stripWhitespace)
content <- gsub (" {2 ,}", " ", content)

# stemming -> deterministic or statistical
# library(Rstem) # needs C/C++/Fortran
# lemmatisation: https://cran.r-project.org/web/packages/openNLP/openNLP.pdf
content <- tm_map(content,stemDocument)

writeLines(as.character(content[[23]])) # inspect

# fixing issues by hand -> find better way for that
#content <- tm_map(content, content_transformer(gsub), pattern = "organiz", replacement = "organ")

# export text body
setwd("C:/Users/Admin/Google Drive/Masterthesis")
#save(content,file="data/texts/20151217_ContentClean.RData")

# End ---------------------------------------------------------------------
