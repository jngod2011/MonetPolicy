##########################################################
###                       Thesis                       ###
##########################################################

##########################################################
### --- Author: Manuel von Krosigk
### --- Date: 2017-07-14
### --- Description: mine structured text files
##########################################################

# rm(list=ls())

# read cleaned body of text
setwd("C:/Users/Admin/Google Drive/Masterthesis")
#load("data/texts/20151217_ContentClean.RData") # articles +/- 5 days around OMO 20151217
# error?
content <- Corpus.untagged # decide on terminoligy later

# DTM ---------------------------------------------------------------------

content   <- tm_map(content, PlainTextDocument) # to make DTM command work
dtm       <- DocumentTermMatrix(content)
# inspect(dtm) # find errors in manipulation
# dim(as.matrix(dtm))
#save(dtm,file="dtm_20151217.RData")
#load("dtm_20151217.RData") 

# export to excel:
#m <- as.matrix(dtm) 
#rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
#                     substring(rownames(m),
#                               nchar(rownames(m))-12,nchar(rownames(m))-4))
#write.csv(m, file="dtm.csv") 

# create similar analysis with exog/endog sentiment: 
# https://cran.r-project.org/web/packages/tidytext/vignettes/tidying_casting.html

# Mining Corpus -----------------------------------------------------------

freq    <- colSums(as.matrix(dtm)) # frequency of occurrence of each word
length(freq) # check: total number of words
ord     <- order(freq,decreasing=TRUE) # descending order of word frequency
freq[head(ord,20)] # most frequent words
freq[tail(ord)] # least frequent words

# include words that occur in 10 to 300 documents & min and max length of word
dtmr    <- DocumentTermMatrix(content, control=list(wordLengths=c(5, 35), 
                                                    bounds = list(global = c(20,1900)))) 
# ! arbitrary amount here, change once only relevant articles are considered

# alternative: 
# dtms <- removeSparseTerms(dtm, 0.1) # matrix that is max 10% empty space   
# inspect(dtms) 

# dtmr
freqr     <- colSums(as.matrix(dtmr)) # frequency of occurrence o each word
length(freqr) # check: total number of words
ordr      <- order(freqr,decreasing=TRUE) # descending order of word frequency
freqr[head(ordr)] # most frequent words
freqr[tail(ordr)] # least frequent words

findFreqTerms(dtmr,lowfreq=20) # all terms that appear 200 times

# check for: correlation (co-occurrence of words in multiple documents) 
# -> indicator for reaction to event?
# measure sentiment of correlated words!
findAssocs(dtmr,"rate", 0.8) # specify DTM and word
findAssocs(dtmr,"fund", 0.8)
findAssocs(dtmr,"econom", 0.8)
findAssocs(dtmr,"inflation", 0.8)
findAssocs(dtmr,"employment", 0.8)
# the presence of a term in these list is not indicative of its frequency
# measure of the frequency with which the two (search and result term)  co-occur
# not an indicator of nearness or contiguity
# further insights in potential classifications

# add cluster analysis and tokenization (e.g. bigrams)

# from https://stats.stackexchange.com/questions/78321/term-frequency-inverse-document-frequency-tf-idf-weighting
#creating term matrix with TF-IDF weighting
terms <-DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

#or compute cosine distance among documents
dissimilarity(tdm, method = "cosine")

# Graphics ----------------------------------------------------------------

library(ggplot2)
library(wordcloud)

wf  <- data.frame(term=names(freqr), occurrences=freqr) # term and occurence as col name
p   <- ggplot(subset(wf, freqr>20), aes(term, occurrences)) # plot terms with freq >20
p   <- p + geom_bar(stat="identity") # height of each bar is proportional to data value mapped to y-axis 
p   <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) # x-axis labels 45°
p

# wordcloud
# setting the same seed each time ensures consistent look across clouds
set.seed(42)
wordcloud(names(freq), freq, max.words=100, rot.per=0.2,
          random.color = T, colors=brewer.pal(8,"Dark2"))   

# clustering by Term Similarity
dtmss   <- removeSparseTerms(dtm, 0.75) # max 75% empty space   
#inspect(dtmss)   
d       <- dist(t(dtmss), method="euclidian") # calculate distance between words
fit     <- hclust(d=d, method="ward.D2") # cluster them according to similarity

plot(fit, hang=-1)
groups  <- cutree(fit, k=5)   # "k" clusters   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around clusters   

# K-means clustering (into specified number of groups)
d       <- dist(t(dtmss), method="euclidian")   
kfit    <- kmeans(d, 2)
#print(kfit) # indicate ... 
library(fpc)
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 
# determine optimal number of clusters
# look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
d       <- dist(as.matrix(dtmss))
wss     <- 2:29
for (i in 2:29){
  wss[i]  <- sum(kmeans(d,centers=i,nstart=25)$withins)
}
plot(2:29, wss[2:29], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


# Topic Modelling ---------------------------------------------------------

library(topicmodels)

# Latent Dirichlet Allocation (LDA)

# Set parameters for Gibbs sampling
burnin  <- 4000
iter    <- 2000
thin    <- 500
seed    <- list(2003,5,63,100001,765)
nstart  <- 5
best    <- TRUE

# Number of topics
k       <- 4 # try different ones

# Run LDA using Gibbs sampling
ldaOut  <- LDA(dtm,k, method='Gibbs', control=list(nstart=nstart, 
                    seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

# docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
#write.csv(ldaOut.topics,file=paste('LDAGibbs',k,'DocsToTopics.csv'))

# top 6 terms in each topic
ldaOut.terms  <- as.matrix(terms(ldaOut,6))
#write.csv(ldaOut.terms,file=paste('LDAGibbs',k,'TopicsToTerms.csv'))

# probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
#write.csv(topicProbabilities,file=paste('LDAGibbs',k,'TopicProbabilities.csv'))

# Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

# Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

# write to file
#write.csv(topic1ToTopic2,file=paste('LDAGibbs',k,'Topic1ToTopic2.csv'))
#write.csv(topic2ToTopic3,file=paste('LDAGibbs',k,'Topic2ToTopic3.csv'))

# Classification Function -------------------------------------------------

# Corpus in character format for classification function
library(plyr)
articles <- laply(Corpus.untagged, function(t){as.character(t)})
head(articles, 3)

# mon. policy responds to econ developments
endog.words <- scan(file='data/EndogenousWords.txt', what='character',quiet=T)
# mon. policy responds to change in policy preferences
exog.words  <- scan(file='data/ExogenousWords.txt', what='character',quiet=T)


# write score sentiment function
score.sentiment <- function(input, endog.words, exog.words, .progress='none'){
  # returns for every item in the input file the number of endogenous - exogenous terms
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(input, function(sentence, endog.words, exog.words){
    
    # pre-processing done before -> include later in function    
    # clean up sentences with R's regex-driven global substitute, gsub():
    #    sentence = gsub('[[:punct:]]', '', sentence)
    #    sentence = gsub('[[:cntrl:]]', '', sentence)
    #    sentence = gsub('\\d+', '', sentence)
    #    # and convert to lower case:
    #    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    endog.matches = match(words, endog.words)
    exog.matches = match(words, exog.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    endog.matches = !is.na(endog.matches)
    exog.matches = !is.na(exog.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(endog.matches) - sum(exog.matches)
    
    return(score)
  }, 
  endog.words, exog.words, .progress=.progress)
  
  scores.df = data.frame(score=scores, text=input)
  return(scores.df)
}

# algorithm sanity check 
sample <- c("for inflation to remain on the same level","a new objective function",
            "We do not care about what what is happening at all")

results <- score.sentiment(input=sample, endog.words, exog.words)
class(results)
results$score # >0 means endog, <0 means exog, 0 means unclassified

# End ---------------------------------------------------------------------
