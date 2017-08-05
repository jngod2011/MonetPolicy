##########################################################
###                       Thesis                       ###
##########################################################

##########################################################
### --- Author: Manuel von Krosigk
### --- Date: 2017-08-04
### --- Description: 
###       Function which determines the sentiment of a 
###       corpus of newspaper articles around an OMO
### --- Input: "VCorpus" "Corpus" object
### --- Output: character string 'Endog', 'Exog' or 'Ambig'
##########################################################

function(Corpus.untagged,endog.words,exog.words,conf.level=0.05){

  # Load packages
  libs <- c("plyr", "stringr")
  lapply(libs, require, character.only=T)
  
  # Convert corpus format to character
  articles <- laply(Corpus.untagged, function(t){as.character(t)})
  
  # Set up score sentiment function
  score.sentiment <- function(input, endog.words, exog.words, 
                              .progress='none'){
    # returns for every item in the input file the number of 
    # endogenous and exogenous terms as well as their difference
  
    # require(plyr)
    # require(stringr)
    
    # Input: vector of sentences; 'plyr' for list or vector as an 'l'
    # to get simple array ("a") of scores back, use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(input, function(sentence, endog.words, exog.words){
      
      # split into words; str_split is in 'stringr'
      word.list = str_split(sentence, '\\s+')
      # list() is one level of hierarchy too much in some cases
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      endog.matches = match(words, endog.words)
      exog.matches = match(words, exog.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      endog.matches = !is.na(endog.matches)
      exog.matches = !is.na(exog.matches)
      
      # TRUE/FALSE is treated as 1/0 by sum():
      #   score = sum(endog.matches) - sum(exog.matches)
      score = c(sum(endog.matches), sum(exog.matches), 
                sum(endog.matches) - sum(exog.matches))
      
      return(score)
      #return(sum(endog.matches))
      #return(sum(exog.matches))    
    }, 
    endog.words, exog.words, .progress=.progress)
    
    #  scores.df = data.frame(score=scores, text=input)
    scores.df = data.frame(total.score=scores[,3], endog.score=scores[,1], 
                           exog.score=scores[,2], text=input)  
    return(scores.df)
  }
  
  results <- score.sentiment(input=articles, endog.words, exog.words)
  
  # decision rule whether the OMO is classified as endog or exog: 
  # stat comp of means
  TabScore <- as.data.frame(matrix(nrow=nrow(results),ncol=3, data=NA))
  colnames(TabScore) <- c("Class", "Endog", "Exog")
  
  for(i in 1:nrow(results)){
    if(results[i,1] > 0){
      TabScore[i,1] <- "Endog"
      TabScore[i,2] <- 1
      TabScore[i,3] <- 0    
    }
    if(results[i,1] < 0){
      TabScore[i,1] <- "Exog"
      TabScore[i,2] <- 0
      TabScore[i,3] <- 1
    }
    if(results[i,1] == 0){
      TabScore[i,1] <- "Ambiguous"
    }
  }
  
  conf.level <- conf.level
  
  if(mean(TabScore$Endog,na.rm = T)==1){print("Endogenous") # all are endog
    }else{if(mean(TabScore$Exog,na.rm = T)==1){print("Exogenous") # all are exog
      }else{ # t-test if not clear
  if(
    t.test(TabScore$Endog,TabScore$Exog, alternative="greater", 
           mu = 0, paired= FALSE, var.equal= FALSE, conf.level = 0.95)$p.value <
    conf.level){
    print("Endogenous")
  } else{
    if(
      t.test(TabScore$Endog,TabScore$Exog, alternative="less", 
             mu = 0, paired= FALSE, var.equal= FALSE, conf.level = 0.95)$p.value <
      conf.level){
      print("Exogenous")
    } else{
      print("Ambiguous")
    }
  }
  }
  }
}
