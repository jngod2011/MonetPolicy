####################################################
##                    Thesis                      ##
####################################################

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
load("OMO_20151217.RData") # articles +/- 5 days around OMO 20151217
min(OMO_20151217[,3]) # first article
max(OMO_20151217[,3]) # last article 

library(tm)
# maybe smoother with "DirSource(directory = "texts/",encoding ="latin1" )"
title   <- Corpus(VectorSource(OMO_20151217[,2])) 
content <- Corpus(VectorSource(OMO_20151217[,1])) 
summary(content)[1:10,]

# inspect(title)
writeLines(as.character(title[[23]])) # example
writeLines(as.character(content[[23]])) # example

# detect text language
library(textcat)
all(textcat(content)=="english") # everything in english?

# part of speech tagging, see Schweinberger(2016)

# create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {
              return (gsub(pattern, " ", x))
              })

# eliminate non-text elements
content <- tm_map(content, toSpace, "-") 
content <- tm_map(content, toSpace, ":")
content <- tm_map(content, toSpace, "'")
content <- tm_map(content, toSpace, ",")
content <- tm_map(content, toSpace, "'")
content <- tm_map(content, toSpace, """)
content <- tm_map(content, toSpace, """)
content <- tm_map(content, toSpace, "/")
content <- tm_map(content, removePunctuation)
# strip digits (std transformation, so no need for content_transformer)
content <- tm_map(content, removeNumbers) # numbers needed?
# transform to lower case (need to wrap in content_transformer)
content <- tm_map(content, content_transformer(tolower))

# remove stopwords using the standard list in tm package
# remove them after part of speech tagging?

stpw1  <- stopwords('english')
stpw2  <- c("ths","th","st","rd","s","will","whose","wants","going") # own stop words
comn  <- unique(c(stpw1,stpw2)) # select unique stopwords
stopwords <- unique(c(gsub("'","",comn),comn)) # final stop word list
content <- tm_map(content, removeWords, stopwords("english"))

# some weird concatenated words like "accommodativeyellen"

# strip whitespace only cosmetic, rather not
# content <- tm_map(content, stripWhitespace)

# combine words that should stay together ! to be extended
for (j in seq(content)){
  content[[j]] <- gsub("percentage point", "percentagepoint",content[[j]])
  content[[j]] <- gsub("economic recovery", "economicrecovery", content[[j]])
  content[[j]] <- gsub("janet yellen", "janetyellen", content[[j]])
  content[[j]] <- gsub("federal reserve bank", "fed", content[[j]])
  content[[j]] <- gsub("federal reserve", "fed", content[[j]])
  content[[j]] <- gsub("monetary policy", "monetarypolicy", content[[j]])
  content[[j]] <- gsub("unemployment rate", "unemploymentrate", content[[j]])
  content[[j]] <- gsub("central bank", "centralbank", content[[j]])
  content[[j]] <- gsub("rates", "rate", content[[j]])
  content[[j]] <- gsub("economic", "econom", content[[j]])
  content[[j]] <- gsub("economy", "econom", content[[j]])
}

# stemming -> deterministic or statistical
# library(Rstem) # needs C/C++/Fortran
# lemmatisation: https://cran.r-project.org/web/packages/openNLP/openNLP.pdf
content <- tm_map(content,stemDocument)

writeLines(as.character(content[[23]])) # inspect

# fixing issues by hand -> find better way for that
#content <- tm_map(content, content_transformer(gsub), pattern = "organiz", replacement = "organ")

# DTM ---------------------------------------------------------------------

content   <- tm_map(content, PlainTextDocument) # to make DTM command work
dtm       <- DocumentTermMatrix(content)
# inspect(dtm) # find errors in manipulation
# dim(as.matrix(dtm))

# export to excel:
#m <- as.matrix(dtm)   
#write.csv(m, file="dtm.csv") 

# create similar analysis with exog/endog sentiment: 
# https://cran.r-project.org/web/packages/tidytext/vignettes/tidying_casting.html

# Mining Corpus -----------------------------------------------------------

freq    <- colSums(as.matrix(dtm)) # frequency of occurrence o each word
length(freq) # check: total number of words
ord     <- order(freq,decreasing=TRUE) # descending order of word frequency
freq[head(ord)] # most frequent words
freq[tail(ord)] # least frequent words

# include words that occur in 3 to 27 documents & min and max length of word
dtmr    <- DocumentTermMatrix(content, control=list(wordLengths=c(4, 20), 
                            bounds = list(global = c(3,27)))) 
# ! arbitrary amount here, change once only relevan articles are considered

# alternative: 
# dtms <- removeSparseTerms(dtm, 0.1) # matrix that is max 10% empty space   
# inspect(dtms) 

# dtmr
freqr     <- colSums(as.matrix(dtmr)) # frequency of occurrence o each word
length(freqr) # check: total number of words
ordr      <- order(freqr,decreasing=TRUE) # descending order of word frequency
freqr[head(ordr)] # most frequent words
freqr[tail(ordr)] # least frequent words

findFreqTerms(dtmr,lowfreq=50) # all terms that appear 50 times

# check for: correlation (co-occurrence of words in multiple documents) 
# -> indicator for reaction to event?
# measure sentiment of correlated words!
findAssocs(dtmr,"rate", 0.8) # specify DTM and word
findAssocs(dtmr,"funds", 0.8)
findAssocs(dtmr,"econom", 0.6)
findAssocs(dtmr,"inflation", 0.8)
findAssocs(dtmr,"employment", 0.8)
# the presence of a term in these list is not indicative of its frequency
# measure of the frequency with which the two (search and result term)  co-occur
# not an indicator of nearness or contiguity
# further insights in potential classifications

# add cluster analysis and tokenization (e.g. bigrams)


# Graphics ----------------------------------------------------------------

library(ggplot2)
library(wordcloud)

wf  <- data.frame(term=names(freqr),occurrences=freqr) # term and occurence as col name
p   <- ggplot(subset(wf, freqr>100), aes(term, occurrences)) # plot terms with freq >100
p   <- p + geom_bar(stat="identity") # height of each bar is proportional to data value mapped to y-axis 
p   <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) # x-axis labels 45°
p

# Wordcloud
# setting the same seed each time ensures consistent look across clouds
set.seed(42)
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=brewer.pal(6,"Dark2"))   


# Clustering by Term Similarity



# General Queries ---------------------------------------------------------

# connect to the sqlite file
con = dbConnect(drv="SQLite", dbname="country.sqlite")
p1 = dbGetQuery( con,'select * from populationtable' )
# count the Articles in the SQLite table
p2 = dbGetQuery( con,'select count(*) from areastable' )
# find entries of the DB from the last week
p3 = dbGetQuery(con, "SELECT population WHERE DATE(timeStamp) < DATE('now', 'weekday 0', '-7 days')")
# Clear the results of the last query
dbClearResult(p3)
# Select population with managerial type of job
p4 = dbGetQuery(con, "select * from populationtable where jobdescription like '%manager%'")


# End ---------------------------------------------------------------------


