##########################################################
###                       Thesis                       ###
##########################################################

##########################################################
### --- Author: Manuel von Krosigk
### --- Date: 2017-07-14
### --- Description: analyse time series data
##########################################################

# Misc --------------------------------------------------------------------

# rm(list=ls())
setwd("C:/Users/Admin/Google Drive/Masterthesis")
# install.packages(c("httr", "XML"), repos = "http://cran.us.r-project.org")
#library(stargazer)
library(xtable)
#library(httr)
#library(XML)
library(xlsx)

# Read Yield Curve Data ----------------------------------------------------

# c.f. class MP Ellingsen, Söderström, p. 25
# announcements: https://www.federalreserve.gov/feeds/h15.html

# from Thompson Reuters Data Stream
YieldCurves           <- read.csv(file="data/DataStreamYieldCurves2002today.csv",
                                  header = T, sep = ";", na.strings = ".",
                                  stringsAsFactors = F)
YieldCurves[,1]       <- as.Date(YieldCurves[,1],"%Y-%m-%d")
colnames(YieldCurves) <- c("Date","1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y","30Y")

# Read Target Interest Rate -----------------------------------------------

# Source: https://www.federalreserve.gov/monetarypolicy/openmarket.htm
# Source: https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm
# Source: https://www.investing.com/economic-calendar/interest-rate-decision-168
# Source: http://www.tradingeconomics.com/united-states/interest-rate

OMO                       <- read.csv(file="data/FED_OpenMarket_Operations.csv", 
                                    header=TRUE, sep=";", na.strings =".", 
                                    stringsAsFactors=FALSE)
colnames(OMO)             <- c("Date","Scheduled","$Tgt_{low}$", "$Tgt_{high}$", "$\\Delta_{low}$", 
                               "$\\Delta_{high}$",
                               "1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y",
                               "30Y", "Classification")
OMO$Date                   <- as.Date(OMO$Date,"%Y-%m-%d") 

# fill Yield Curve data into OMO dates
for(i in 1:nrow(OMO)){
  if(!length(which(OMO[i,1]==YieldCurves[,1]))==T){# NA when date is not given
    OMO[i,7:17]                 <- NA
  }
  else{
    OMO[i,7:17]               <- YieldCurves[# find YC date that corresponds to OMO
                                          which(OMO[i,1]==YieldCurves[,1]), 
                                          2:ncol(YieldCurves)]
  }
} # export OMO table to latex after classification w/o 1st row

# set up data frame interest rates and targets
TargetRates               <- data.frame(YieldCurves[,1])
for(j in 2:3){# fill min and max target in columns 2 & 3
  for(i in 1:nrow(TargetRates)){
    if(!length(which(TargetRates[i,1]==OMO[,1]))==T){# NA when date is not given
      TargetRates[i,j]                 <- NA
    }
    else{
      TargetRates[i,j]                 <- OMO[which(TargetRates[i,1]==OMO[,1]),1+j] # cols 3 & 4 
    }
  }
}
colnames(TargetRates)             <- c('Date','Target_min','Taget_max')

# replace NAs with the last value that is not NA in columns 3 & 4 of data frame
for(j in 2:ncol(TargetRates)){
  # if first value is NA, take the one from previous year
  if(is.na(TargetRates[1,j])==T){
    TargetRates[1,j]              <- OMO[1,1+j] # first value is from 2002, cols 5 & 6 
  }
  for(i in 1:nrow(TargetRates)){
    if(is.na(TargetRates[i,j])==T){
      TargetRates[i,j]               <- TargetRates[i-1,j]
    }
  } 
}

#plot(TargetRates[,1],TargetRates[,2],type='l',xlab="Date",
#     ylab="Interest Rates",col='cornflowerblue')              # lower bound
#lines(TargetRates[,1],TargetRates[,3], col='cornflowerblue')  # upper bound

# add decisions where nothing was changed for analysis?

# Table for results
Tab_Class <- data.frame("Date"=OMO$Date, "Deterministic"=NA, "Deterministic_end"=NA, 
                        "Deterministic_ex"=NA, "Bayes"=NA, "Bayes_end"=NA, 
                        "Bayes_ex"=NA,"KNN"=NA, "KNN_end"=NA, "KNN_ex"=NA, 
                        "SVM"=NA, "SVM_end"=NA, "SVM_ex"=NA)

# Get classification ------------------------------------------------------

setwd("C:/Users/Admin/Google Drive/Masterthesis")

# Get functions
MyProcFct   <- dget("functions/MyProcFct.R")
MyClassFct  <- dget("functions/MyClassFct.R")

# Get input to functions
mystopwords <- scan(file='data/MyStopwords.txt', what='character',
                    quiet=T) # own stop words
myngrams    <- scan(file='data/CombPhrases.txt', what='character',quiet=T,sep=",")

# derive corpi from articles per OMO
OMOdates <- OMO$Date 
path         <- "/data/articles_2001_2007"
GetCorpus <- function(OMOdate, path){
  cl.dir <- paste0(sprintf("%s/%s",path, OMOdate),"/")
  cl.cor <- MyProcFct(path=cl.dir,mystopwords=mystopwords,myngrams=myngrams)
  result <- list(name = OMOdate, corp = cl.cor)
}
#Corpi   <- lapply(OMOdates, GetCorpus, path=path) # takes a while
#save(Corpi,file="Corpi_Factive.RData")
#load("Corpi_Factiva.RData")

# determine sentiment of every OMO

# List of phrases  to determine classification of corpus
# Mon. policy responses to econ developments
endog.words <- stemDocument(scan(file='data/EndogenousWords.txt', 
                                 what='character',quiet=T))
# Mon. policy responses to change in policy preferences
exog.words  <- stemDocument(scan(file='data/ExogenousWords.txt', 
                                 what='character',quiet=T))
# set confidence level
conf.level  <- 0.05

# Classify deterministically with own function and write in Tab_Class

for(i in seq(nrow(OMO))){
  Corpus <- Corpi[[i]]$corp
  # determine classification and store in table
  Det_Classification <- MyClassFct(Corpus.untagged=Corpus, endog.words=endog.words, 
                                   exog.words=exog.words, conf.level=conf.level)
  Tab_Class$Deterministic[i]      <- Det_Classification$Classification
  Tab_Class$Deterministic_end[i]  <- sum(Det_Classification$TabScore$Endog, na.rm = T)
  Tab_Class$Deterministic_ex[i]   <- sum(Det_Classification$Results$TabScore$Exog, na.rm = T)
}

# ML classification -------------------------------------------------------

setwd("C:/Users/Admin/Google Drive/Masterthesis")
MyProcFct   <- dget("functions/MyProcFct.R")
mystopwords <- scan(file='data/MyStopwords.txt', what='character',
                    quiet=T) # own stop words
myngrams    <- scan(file='data/CombPhrases.txt', what='character',quiet=T,sep=",")

## TRAINING SET
# build TDM
path        <- "/data"
sentiments  <- c("Endog","Exog")
generateTDM <- function(sentiment, path){
  cl.dir <- paste0(sprintf("%s/Training_%s",path, sentiment),"/")
  cl.cor <- MyProcFct(path=cl.dir,mystopwords=mystopwords,myngrams=myngrams)
  cl.tdm <- TermDocumentMatrix(cl.cor)
  #cl.tdm <- removeSparseTerms(cl.tdm, 0.7)
  result <- list(name = sentiment, tdm = cl.tdm)
}

#tdm_test <- lapply(sentiments, generateTDM, path=path)
#save(tdm_test, file="tdm_test.RData")
load("tdm_test.RData")

## ACTION SET
generateTDM2 <- function(Corpus){
  cl.cor <- Corpus$corp
  OMOdate <- Corpus$name
  cl.tdm <- TermDocumentMatrix(cl.cor)
  #cl.tdm <- removeSparseTerms(cl.tdm, 0.7)
  result <- list(name = OMOdate, tdm = cl.tdm)
}

#load("Corpi_Factiva.RData")
tdm_act <- lapply(Corpi, generateTDM2)

## COMBINE both
tdm_comb <- append(tdm_test,tdm_act)

bindClassToTDM <- function(tdm){
  cl.mat <- t(data.matrix(tdm[["tdm"]]))
  cl.df <- as.data.frame(cl.mat, stringsAsFactors = F)
  
  cl.df <- cbind(cl.df, rep(tdm[["name"]], nrow(cl.df)))
  colnames(cl.df)[ncol(cl.df)] <- "targetclass"
  return(cl.df)
}

classTDM <- lapply(tdm_comb, bindClassToTDM)

# stack
tdm.stack <- do.call(rbind.fill, classTDM)
tdm.stack[is.na(tdm.stack)] <- 0 
nrow(tdm.stack) # total number of docs
ncol(tdm.stack) # total terms

# prediction
tdm.class <- tdm.stack[,"targetclass"]
tdm.class <- tdm.class[tdm.class=="Endog" | tdm.class=="Exog"]
tdm.stack.nl <- tdm.stack[,!colnames(tdm.stack)%in%"targetclass"]

# KNN
library(class)
knn.pred <- knn(tdm.stack.nl[seq(tdm.class),], # train set w/o classification
                tdm.stack.nl[seq(nrow(tdm.stack.nl))[-seq(tdm.class)],], # action set
                tdm.class) # classification for training set

# get date of article and attach KNN prediction
ArticleDates <- tdm.stack[,"targetclass"]
ArticleDates <- ArticleDates[ArticleDates!="Endog" & ArticleDates!="Exog"]

# Enter results in Tab_Class table

tmp_KNN.pred.tab  <- data.frame("Date"=ArticleDates, "KNN.class"= knn.pred)

for(i in seq(length(OMO$Date))){
  Tab_Class$KNN_end[i] <- nrow(tmp_KNN.pred.tab[which(
                            tmp_KNN.pred.tab$Date==as.character(OMO$Date[i])&
                            tmp_KNN.pred.tab$KNN.class=="Endog"),])
  Tab_Class$KNN_ex[i]  <- nrow(tmp_KNN.pred.tab[which(
                            tmp_KNN.pred.tab$Date==as.character(OMO$Date[i])&
                            tmp_KNN.pred.tab$KNN.class=="Exog"),])
  if(Tab_Class$KNN_end[i]>Tab_Class$KNN_ex[i]){
        Tab_Class$KNN[i] <- "Endog"}
  else{
    if(Tab_Class$KNN_end[i]<Tab_Class$KNN_ex[i]){
          Tab_Class$KNN[i] <-"Exog"}
    else{Tab_Class$KNN[i] <-"Ambiguous"}
  }
}

rm(tmp_KNN.pred.tab)

# NAIVE BAYES
#library(RTextTools)
library(e1071)
NaiBayclassifier <- naiveBayes(as.matrix(tdm.stack.nl[seq(tdm.class),]), # test texts dtm as matrix
                         as.factor(tdm.class)) # test classifications

NaiBay.pred <- predict(NaiBayclassifier, # class element
                       tdm.stack.nl[seq(nrow(tdm.stack.nl))[-seq(tdm.class)],]) # action set 

# Enter results in Tab_Class table
tmp_NaiBay.pred.tab  <- data.frame("Date"=ArticleDates, "NaiBay.class"= NaiBay.pred)

for(i in seq(length(OMO$Date))){
  Tab_Class$Bayes_end[i] <- nrow(tmp_NaiBay.pred.tab[which(
                                 tmp_NaiBay.pred.tab$Date==as.character(OMO$Date[i])&
                                 tmp_NaiBay.pred.tab$NaiBay.class=="Endog"),])
  Tab_Class$Bayes_ex[i]  <- nrow(tmp_NaiBay.pred.tab[which(
                                 tmp_NaiBay.pred.tab$Date==as.character(OMO$Date[i])&
                                 tmp_NaiBay.pred.tab$NaiBay.class=="Exog"),])
  if(Tab_Class$Bayes_end[i]>Tab_Class$Bayes_ex[i]){
              Tab_Class$Bayes[i] <- "Endog"}
  else{
    if(Tab_Class$Bayes_end[i]<Tab_Class$Bayes_ex[i]){
              Tab_Class$Bayes[i] <-"Exog"}
    else{Tab_Class$Bayes[i] <-"Ambiguous"}
  }
}
rm(tmp_NaiBay.pred.tab)

# SVM
# see https://www.svm-tutorial.com/2014/11/svm-classify-text-r/
library(RTextTools)

# Configure the training data
Test_container <- create_container(tdm.stack.nl[seq(tdm.class),], # train set dtm
                              as.numeric(as.factor(tdm.class)),# train set classification
                              trainSize=seq(tdm.class), # train index
                              virgin=FALSE)
# train a SVM Model
SVMclassifier <- train_model(Test_container, "SVM", kernel="linear", cost=1)

# create the corresponding prediction container
Act_container <- create_container(tdm.stack.nl[seq(nrow(tdm.stack.nl))[-seq(tdm.class)],], # action set dtm
                                        labels=rep(0,length(seq(nrow(tdm.stack.nl))[-seq(tdm.class)])), # empty class
                                        testSize=1:length(seq(nrow(tdm.stack.nl))[-seq(tdm.class)]),  # test index
                                        virgin=FALSE)

svm.pred <- classify_model(Act_container, SVMclassifier) # 1: endog, 2: exog

# Enter results in Tab_Class table
tmp_svm.pred.tab  <- data.frame("Date"=ArticleDates, "SVM.class"= svm.pred$SVM_LABEL)

for(i in seq(length(OMO$Date))){
  Tab_Class$SVM_end[i] <- nrow(tmp_svm.pred.tab[which(
                               tmp_svm.pred.tab$Date==as.character(OMO$Date[i])&
                               tmp_svm.pred.tab$SVM.class=="1"),])
  Tab_Class$SVM_ex[i]  <- nrow(tmp_svm.pred.tab[which(
                               tmp_svm.pred.tab$Date==as.character(OMO$Date[i])&
                               tmp_svm.pred.tab$SVM.class=="2"),])
  if(Tab_Class$SVM_end[i]>Tab_Class$SVM_ex[i]){
                    Tab_Class$SVM[i] <- "Endog"}
  else{
    if(Tab_Class$Bayes_end[i]<Tab_Class$Bayes_ex[i]){
                    Tab_Class$SVM[i] <-"Exog"}
    else{Tab_Class$Bayes[i] <-"Ambiguous"}
  }
}
rm(tmp_svm.pred.tab)


####################################################
##                      END                       ##
####################################################
