####################################################
##                    Thesis                      ##
####################################################

# Connect to database and pre-process data

# rm(list=ls())
library("sqldf")
library("XLConnect")

setwd("E:/20170421_MA_Data/reuters")

# Connect to database -----------------------------------------------------

#db <- dbConnect(drv="SQLite", dbname="reuters.db")
db <- dbConnect(SQLite(), dbname="reuters.db")

# get a list of all tables
alltables = dbListTables(db)

# *: id, article_id, content, pub_datetime, date_scraped, title, authors, raw_data, 
# date stuff: https://sqlite.org/lang_datefunc.html

# get the Articles as a data.frame
#dbGetQuery(db,'SELECT * FROM Articles LIMIT 1' )$content # read 1st article
#dbGetQuery(db, "SELECT * FROM Articles WHERE pub_datetime = '2015-01-03 23:51:00' ")
#dbGetQuery(db, "SELECT content, title FROM Articles WHERE pub_datetime = BETWEEN date(
#             '2015-01-03') AND date('2015-01-04') # doesn't work yet

#test1 <- dbGetQuery(db, "SELECT content, title FROM Articles WHERE 
#                    pub_datetime < datetime('2015-01-03 23:51:00','-1 day')")

# alternative for later: determine ids with elastic and reference accordingly

# 2008 and before no data \dots

# restrict to a few articles to make analysis feasible
OMO_20151217 <- dbGetQuery(db, "SELECT content, title, pub_datetime FROM Articles WHERE 
                    pub_datetime BETWEEN datetime('2015-12-17 14:00:00','-5 day') 
                    AND datetime('2015-12-17 14:00:00','+5 day')")
setwd("C:/Users/Admin/Google Drive/Masterthesis")
save(OMO_20151217,file="OMO_20151217.RData")

# Manipulate for TA -------------------------------------------------------

setwd("C:/Users/Admin/Google Drive/Masterthesis")
load("OMO_20151217_short.RData") # articles +/- 5 days around OMO 20151217
min(OMO_20151217[,3]) # first article
max(OMO_20151217[,3]) # last article 

library(tm)
# maybe smoother with "DirSource(directory = "texts/",encoding ="latin1" )"
title   <- Corpus(VectorSource(OMO_20151217[,2])) 
content <- Corpus(VectorSource(OMO_20151217[,1]))

#summary(content)[1:10,]
# add title to content column -> slow
#transform(OMO_20151217, newcol=paste(OMO_20151217[,2], OMO_20151217[,1], sep=" "))

# inspect(title)
writeLines(as.character(title[[23]])) # example
writeLines(as.character(content[[23]])) # example

# detect text language
library(textcat)
all(textcat(content)=="english") # everything in english?
content <- Corpus(VectorSource(OMO_20151217[which(
                  textcat(content)=="english"),1])) 

# part of speech tagging, see Schweinberger(2016)

# create the toSpace content transformer
toSpace <- content_transformer(
            function(x, pattern){
              return (gsub(pattern, " ", x))
            })

# transform to lower case (need to wrap in content_transformer)
content <- tm_map(content, content_transformer(tolower))

# eliminate non-text elements
notext  <- c("-",":","'",",","'",""",""","/", "-", ">", "<", "â???T", "â???o", "â???"", "â???\u009d")
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
