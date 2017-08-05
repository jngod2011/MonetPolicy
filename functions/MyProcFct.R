##########################################################
###                       Thesis                       ###
##########################################################

##########################################################
### --- Author: Manuel von Krosigk
### --- Date: 2017-08-04
### --- Description: 
###       Function that creates a corpus from single 
###       articles and pre-processes them
### --- Input: path to .txt files
### --- Output: "VCorpus" "Corpus" object
##########################################################

function(pathname,mystopwords,myngrams){
  
  # Load packages
  libs <- c("tm", "readr", "textcat", "NLP", "openNLP", 
            "openNLPmodels.en", "stringr", "gsubfn", "plyr")
  lapply(libs, require, character.only=T)

  # load files into corpus
  # get listing of .txt files in directory
  filenames <- list.files(paste0(getwd(), pathname),pattern='*.txt')
  
  # read files into a character vector
  files <- lapply(paste0(paste0(getwd(), pathname),filenames),readLines)
  for(i in seq(files)){files[i]  <- paste(files[i])}
  #files <- lapply(paste0(paste0(getwd(), pathname),filenames),read_file)
  
  # create corpus from vector
  content <- Corpus(VectorSource(files))
  
  # restrict to english articles
  #content <- Corpus(VectorSource(content[which(
  #  textcat(content)=="english"),1])) 
  
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
  
  # remove non-cruicial terms
  for(i in seq(Corpus.tagged)){
    for(j in seq(Tags_discard)){
      Corpus.tagged[[i]] <- gsub(
        paste(paste("\\b\\S+", Tags_discard[j], sep = ""),"\\b",sep=""), 
        "", Corpus.tagged[[i]])
    }
  } 
  
  # remove tags from cruicial words
  for (i in seq(Corpus.tagged)){
    for(j in seq(Tags_keep)){  
      Corpus.tagged[[i]] <- gsub(
        paste("/", Tags_keep[j], sep = ""), "", Corpus.tagged[[i]])
    }
  }
  
  # convert list to VCorpus
  Corpus.untagged <- Corpus(VectorSource(Corpus.tagged))

  # remove punctuation
  Corpus.untagged <- tm_map(Corpus.untagged, removePunctuation)
  # remove digits
  Corpus.untagged <- tm_map(Corpus.untagged, removeNumbers)
  
  # transform all words to lowercase (careful with meta data)
  Corpus.untagged <- tm_map(Corpus.untagged, content_transformer(tolower))
  
  # discard stopwords
  stpw1  <- stopwords('english')
  stpw2  <- mystopwords
  comn    <- unique(c(stpw1,stpw2)) # select unique stopwords
  mystopwords <- unique(c(gsub("'","",comn),comn)) # final stop word list
  Corpus.untagged <- tm_map(Corpus.untagged, removeWords, mystopwords)
  
  # remove unnecessary whitespace
  Corpus.untagged <- tm_map(Corpus.untagged, stripWhitespace)
  
  # combine words that should stay together
  comb.phrases <- myngrams
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
  
  # stemming
  Corpus.untagged <- tm_map(Corpus.untagged,stemDocument)

    return(Corpus.untagged)
}
