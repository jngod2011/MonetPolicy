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
colnames(OMO)             <- c("Date","Sched","$Tgt_{low}$", "$Tgt_{high}$", "$\\Delta_{low}$", 
                               "$\\Delta_{high}$",
                               "$\\Delta$1M","$\\Delta$3M","$\\Delta$6M","$\\Delta$1Y",
                               "$\\Delta$2Y","$\\Delta$3Y","$\\Delta$5Y","$\\Delta$7Y",
                               "$\\Delta$10Y","$\\Delta$20Y","$\\Delta$30Y", "Class")
OMO$Date                   <- as.Date(OMO$Date,"%Y-%m-%d") 

# fill Yield Curve Change data into OMO dates
for(i in 1:nrow(OMO)){
  # NA when date is not given
  if(!length(which(OMO[i,1]==YieldCurves[,1]))==T){
    OMO[i,7:17]                 <- NA
  }
  # find YC date that corresponds to OMO
  else{
    OMO[i,7:17]               <- round(YieldCurves[
                                          which(OMO[i,1]==YieldCurves[,1]), 
                                          2:ncol(YieldCurves)]/
                                       YieldCurves[
                                          which(OMO[i,1]==YieldCurves[,1])-1, 
                                          2:ncol(YieldCurves)]-1,2)
  }
} # export OMO table to latex after classification w/o 1st row
# get rid of /0 issue
OMO <- do.call(data.frame,lapply(OMO, function(x) replace(x, is.infinite(x),NA)))
colnames(OMO)             <- c("Date","Sched","$Tgt_{low}$", "$Tgt_{high}$", "$\\Delta_{low}$", 
                               "$\\Delta_{high}$",
                               "1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y",
                               "30Y", "Class")

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

# Table for results
Tab_Class <- data.frame("Date"=OMO$Date, "Deterministic"=NA, "Deterministic_end"=NA, 
                        "Deterministic_ex"=NA, 
                        "Bayes"=NA, "Bayes_end"=NA, "Bayes_ex"=NA,
                        "ME"=NA, "ME_end"=NA, "ME_ex"=NA, 
                        "KNN"=NA, "KNN_end"=NA, "KNN_ex"=NA, 
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
# short: without training set articles; all including them
path         <- "/data/articles_2001_2007_short"
GetCorpus <- function(OMOdate, path){
  cl.dir <- paste0(sprintf("%s/%s",path, OMOdate),"/")
  cl.cor <- MyProcFct(path=cl.dir,mystopwords=mystopwords,myngrams=myngrams)
  result <- list(name = OMOdate, corp = cl.cor)
}
#Corpi   <- lapply(OMOdates, GetCorpus, path=path) # takes a while
#save(Corpi,file="Corpi_Factiva_short.RData")
#load("Corpi_Factiva.RData")

# count number of articles:
library(stringr)
#str_count(string = Corpi[[2]], pattern = "\\n")[2]
sum(sapply(Corpi, FUN = function(x) str_count(string = x, pattern = "\\n")[2] + 1))

# determine sentiment of every OMO

# List of phrases  to determine classification of corpus
library(tm)
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
  Tab_Class$Deterministic_ex[i]   <- sum(Det_Classification$TabScore$Exog, na.rm = T)
}


# ML setup ----------------------------------------------------------------

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

#tdm_train <- lapply(sentiments, generateTDM, path=path)
#save(tdm_train, file="tdm_train.RData")
load("tdm_train.RData")

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
tdm_comb <- append(tdm_train,tdm_act)

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

# get date of article
ArticleDates <- tdm.stack[,"targetclass"]
ArticleDates <- ArticleDates[ArticleDates!="Endog" & ArticleDates!="Exog"]

# Naive Bayes -------------------------------------------------------------

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
    else{Tab_Class$Bayes[i] <-"Ambig"}
  }
}
rm(tmp_NaiBay.pred.tab)



# Maximum Entropy ---------------------------------------------------------

# see https://www.r-bloggers.com/sentiment-analysis-with-machine-learning-in-r/

library(RTextTools)

# Configure the training data
Test_container <- create_container(tdm.stack.nl[seq(tdm.class),], # train set dtm
                                   as.numeric(as.factor(tdm.class)),# train set classification
                                   trainSize=seq(tdm.class), # train index
                                   virgin=FALSE)
# train a SVM Model
MEclassifier <- train_model(Test_container, "MAXENT", kernel="linear", cost=1)

# create the corresponding prediction container
Act_container <- create_container(tdm.stack.nl[seq(nrow(tdm.stack.nl))[-seq(tdm.class)],], # action set dtm
                                  labels=rep(0,length(seq(nrow(tdm.stack.nl))[-seq(tdm.class)])), # empty class
                                  testSize=1:length(seq(nrow(tdm.stack.nl))[-seq(tdm.class)]),  # test index
                                  virgin=FALSE)

me.pred <- classify_model(Act_container, MEclassifier) # 1: endog, 2: exog

# Enter results in Tab_Class table
tmp_me.pred.tab  <- data.frame("Date"=ArticleDates, "ME.class"= me.pred$MAXENTROPY_LABEL)

for(i in seq(length(OMO$Date))){
  Tab_Class$ME_end[i] <- nrow(tmp_me.pred.tab[which(
                              tmp_me.pred.tab$Date==as.character(OMO$Date[i])&
                                tmp_me.pred.tab$ME.class=="1"),])
  Tab_Class$ME_ex[i]  <- nrow(tmp_me.pred.tab[which(
                              tmp_me.pred.tab$Date==as.character(OMO$Date[i])&
                                tmp_me.pred.tab$ME.class=="2"),])
  if(Tab_Class$ME_end[i]>Tab_Class$ME_ex[i]){
    Tab_Class$ME[i] <- "Endog"}
  else{
    if(Tab_Class$ME_end[i]<Tab_Class$ME_ex[i]){
      Tab_Class$ME[i] <-"Exog"}
    else{Tab_Class$ME[i] <-"Ambig"}
  }
}
rm(tmp_me.pred.tab)


# KNN ---------------------------------------------------------------------

library(class)
knn.pred <- knn(tdm.stack.nl[seq(tdm.class),], # train set w/o classification
                tdm.stack.nl[seq(nrow(tdm.stack.nl))[-seq(tdm.class)],], # action set
                tdm.class) # classification for training set

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
    else{Tab_Class$KNN[i] <-"Ambig"}
  }
}
rm(tmp_KNN.pred.tab)



# SVM ---------------------------------------------------------------------

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
    if(Tab_Class$SVM_end[i]<Tab_Class$SVM_ex[i]){
                    Tab_Class$SVM[i] <-"Exog"}
    else{Tab_Class$SVM[i] <-"Ambig"}
  }
}
rm(tmp_svm.pred.tab)


# Export classification table ---------------------------------------------

colnames(Tab_Class)             <- c("Date",
                                     "$CB$", "$CB_{end}$", "$CB_{ex}$",
                                     "$NB$", "$NB_{end}$", "$NB_{ex}$",
                                     "$ME$", "$ME_{end}$", "$ME_{ex}$",
                                     "$KNN$", "$KNN_{end}$", "$KNN_{ex}$",
                                     "$SVM$", "$SVM_{end}$", "$SVM_{ex}$")

# export table to latex format
library(xtable)
#Tab_Class$Date <-as.character(OMO$Date) # fix date for latex export
#print(xtable(Tab_Class, align="llrrrrrrrrrrrrrrr", type="latex", 
#             digits=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
#             caption = "Target adjustment dates with corresponding number of as endogenous and exogenous classified articles through different text mining procedures.",
#             label = "tab:Tab_Class"),
#      sanitize.text.function = function(x){x}, include.rownames=F,
#      booktabs=TRUE, caption.placement="top", floating.environment='sidewaystable',
#      size="\\fontsize{8pt}{9pt}\\selectfont",
#      file="Text/chapters/tables_graphs/Tab_Class.tex")

#Tab_Class$Date                   <- as.Date(OMO$Date,"%Y-%m-%d") # right date format

# Performance evaluation -------------------------------------------------------

tab_Perf_Eval <- data.frame("Naive Bayes"=NA, "Maximum entropy"=NA, "knn"=NA, "SVM"=NA)
set.seed(40888) # loop around
test.size <- .9 # 90/10 training/test set
nloop     <- 500 # number of repetitions
for(k in seq(nloop)){
  # randomly draw train and test sample
  train.idx <- sample(length(tdm.class), ceiling(length(tdm.class) *test.size))
  test.idx <- (1:length(tdm.class))[- train.idx]

  ## Naive Bayes
  NB_class_PERF <- naiveBayes(as.matrix(tdm.stack.nl[train.idx,]), # test texts dtm as matrix
                              as.factor(tdm.class[train.idx])) # test classifications
  
  NB_pred_PERF <- predict(NB_class_PERF, # class element
                          tdm.stack.nl[test.idx,]) # action set 
  
  NB_conf.mat <- table("Predictions" = NB_pred_PERF, "Actual" = tdm.class[test.idx])
  #NB_conf.mat
  #(accuracy <- sum(diag(NB_conf.mat))/length(test.idx))
  tab_Perf_Eval[k,1] <- sum(diag(NB_conf.mat))/length(test.idx)
  
  rm(NB_class_PERF,NB_pred_PERF,NB_conf.mat)
  
  ## Maximum entropy
  
  # Configure the training data
  Test_container_PERF <- create_container(tdm.stack.nl[train.idx,], # train set dtm
                                          as.numeric(as.factor(tdm.class[train.idx])),# train set classification
                                          trainSize=seq(train.idx), # train index
                                          virgin=FALSE)
  # train a SVM Model
  MEclassifier_PERF <- train_model(Test_container_PERF, "MAXENT", kernel="linear", cost=1)
  
  # create the corresponding prediction container
  Act_container_PERF <- create_container(tdm.stack.nl[test.idx,], # action set dtm
                                         labels=rep(0,length(test.idx)), # empty class
                                         testSize=seq(test.idx),  # test index
                                         virgin=FALSE)
  
  tmp_me_pred_PERF <- classify_model(Act_container_PERF, MEclassifier_PERF) # 1: endog, 2: exog
  me_pred_PERF     <- matrix(nrow=nrow(tmp_me_pred_PERF),ncol=1)
  for(i in 1:nrow(tmp_me_pred_PERF)){
    if(tmp_me_pred_PERF$MAXENTROPY_LABEL[i]==1){
      me_pred_PERF[i]    <- "Endog"
    }
    if(tmp_me_pred_PERF$MAXENTROPY_LABEL[i]==2){
      me_pred_PERF[i]    <- "Exog"
    }
    #else{me_pred_PERF[i]    <- NA}
  }
  
  me_conf.mat <- table("Predictions" = as.character(me_pred_PERF), "Actual" = tdm.class[test.idx])
  #me_conf.mat
  #(accuracy <- sum(diag(me_conf.mat))/length(test.idx))
  tab_Perf_Eval[k,2] <- sum(diag(me_conf.mat))/length(test.idx)
  
  rm(Test_container_PERF,MEclassifier_PERF,Act_container_PERF,tmp_me_pred_PERF,me_pred_PERF,me_conf.mat)
  
  
  ## knn
  knn_pred_PERF <- knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.class[train.idx])
  
  knn_conf.mat <- table("Predictions" = knn_pred_PERF, "Actual" = tdm.class[test.idx])
  #knn_conf.mat
  #(accuracy <- sum(diag(knn_conf.mat))/length(test.idx))
  tab_Perf_Eval[k,3] <- sum(diag(knn_conf.mat))/length(test.idx)
  
  rm(knn_pred_PERF,knn_conf.mat)
  
  ## svm
  
  # Configure the training data
  Test_container_PERF <- create_container(tdm.stack.nl[train.idx,], # train set dtm
                                     as.numeric(as.factor(tdm.class[train.idx])),# train set classification
                                     trainSize=seq(train.idx), # train index
                                     virgin=FALSE)
  # train a SVM Model
  SVMclassifier_PERF <- train_model(Test_container_PERF, "SVM", kernel="linear", cost=1)
  
  # create the corresponding prediction container
  Act_container_PERF <- create_container(tdm.stack.nl[test.idx,], # action set dtm
                                    labels=rep(0,length(test.idx)), # empty class
                                    testSize=seq(test.idx),  # test index
                                    virgin=FALSE)
  
  tmp_svm_pred_PERF <- classify_model(Act_container_PERF, SVMclassifier_PERF) # 1: endog, 2: exog
  svm_pred_PERF     <- matrix(nrow=nrow(tmp_svm_pred_PERF),ncol=1)
  for(i in 1:nrow(tmp_svm_pred_PERF)){
    if(tmp_svm_pred_PERF$SVM_LABEL[i]==1){
      svm_pred_PERF[i]    <- "Endog"
    }
    if(tmp_svm_pred_PERF$SVM_LABEL[i]==2){
      svm_pred_PERF[i]    <- "Exog"
    }
    else{svm_pred_PERF[i]    <- NA}
  }
  
  svm_conf.mat <- table("Predictions" = as.character(svm_pred_PERF), "Actual" = tdm.class[test.idx])
  #svm_conf.mat
  #(accuracy <- sum(diag(svm_conf.mat))/length(test.idx))
  tab_Perf_Eval[k,4] <- sum(diag(svm_conf.mat))/length(test.idx)
  
  rm(Test_container_PERF,SVMclassifier_PERF,Act_container_PERF,tmp_svm_pred_PERF,svm_pred_PERF,svm_conf.mat)
  
  # delete rest
  rm(train.idx,test.idx)
}

#save(tab_Perf_Eval,file="tab_Perf_Eval.RData")
#load("tab_Perf_Eval.RData")

# export to latex
tab_PerfEval <- data.frame(rbind(
                  apply(tab_Perf_Eval, MARGIN=2, FUN=mean),
                  apply(tab_Perf_Eval, MARGIN=2, FUN=median),
                  apply(tab_Perf_Eval, MARGIN=2, FUN=sd),
                  apply(tab_Perf_Eval, MARGIN=2, FUN=min),
                  apply(tab_Perf_Eval, MARGIN=2, FUN=max))
)
colnames(tab_PerfEval)  <- c("Naive Bayes", "Maximum entropy", "knn", "SVM")
row.names(tab_PerfEval) <- c("Mean","Median","Std","Min","Max")

# export table to latex format
library(xtable)
#print(xtable(tab_PerfEval, align="lcccc", digits=c(0,4,4,4,4), 
#             type="latex", caption = "Simulated accuracy of different ML algorithms.",
#             label = "tab:Tab_PerfEval"),
#      sanitize.text.function = function(x){x}, include.rownames=T,
#      booktabs=TRUE, caption.placement="top", #floating.environment='sidewaystable',
#      #size="\\fontsize{8pt}{9pt}\\selectfont",
#      file="Text/chapters/tables_graphs/Tab_PerfEval.tex")



####################################################
##                      END                       ##
####################################################
