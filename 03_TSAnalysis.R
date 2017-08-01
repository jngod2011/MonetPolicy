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
# setwd("C:/Users/Admin/Google Drive/Masterthesis")
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
OMO[,1]                   <- as.Date(OMO[,1],"%Y-%m-%d") 
colnames(OMO)             <- c("Date","Scheduled","Target_l", "Target_h", "Change_l", 
                               "Change_h",
                               "1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y",
                               "30Y", "Classification")
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

plot(TargetRates[,1],TargetRates[,2],type='l',xlab="Date",
     ylab="Interest Rates",col='cornflowerblue')              # lower bound
lines(TargetRates[,1],TargetRates[,3], col='cornflowerblue')  # upper bound

# add decisions where nothing was changed for analysis?


# Policy Days vs Normal Days ----------------------------------------------

DeltaYieldCurves <- data.frame(YieldCurves[2:nrow(YieldCurves),1], 
                               diff(as.matrix(YieldCurves[,2:ncol(YieldCurves)])),NA,NA)
colnames(DeltaYieldCurves) <- c("Date","d1M","d3M","d6M","d1Y","d2Y","d3Y","d5Y","d7Y",
                                "d10Y","d20Y","d30Y","NP","P")

for(i in 1:nrow(DeltaYieldCurves)){
  if(length(which(OMO[,1]==DeltaYieldCurves[i,1])) == 0)
    DeltaYieldCurves[i,ncol(DeltaYieldCurves)]  <- 0
  else
    DeltaYieldCurves[i,ncol(DeltaYieldCurves)]  <- 1
}
DeltaYieldCurves[,ncol(DeltaYieldCurves)-1]  <- 1-DeltaYieldCurves[,ncol(DeltaYieldCurves)]

# replicate Non-policy vs policy day regression table from ES, p. 10
Tab_NPvsPdays <- matrix(nrow=10-1-1,ncol=10)
library(car)
Tab_NPvsPdays[,1] <- c("$\\alpha_n$", "", "$\\beta_n^{NP}$", "", "$\\beta_n^P$", "",
                       "$\\bar{R}^2$","$\\beta_n^{NP}$ = $\\beta_n^P$")
colnames(Tab_NPvsPdays) <- c(" ","6m","1y","2y","3y","5y","7y","10y","20y","30y")

for(k in 4:(ncol(DeltaYieldCurves)-2)){
  # regress change in n-maturity on NP-dummy*delta3m and P-dummy*delta3m    
  myreg <- lm(DeltaYieldCurves[,k]~DeltaYieldCurves$d3M:DeltaYieldCurves$NP + 
                DeltaYieldCurves$d3M:DeltaYieldCurves$P)
  # fill table
    j = k+1-3 
    Tab_NPvsPdays[1,j]  <- formatC(abs(round(summary(myreg)$coef[1,1],2)),format="f",digits=2) # intercept
    Tab_NPvsPdays[2,j]  <- paste0("(", format(unlist(
                            formatC(abs(round(summary(myreg)$coef[1,2],2)),format="f",digits=2)
                                      )),")") # std intercept
    Tab_NPvsPdays[3,j]  <- round(summary(myreg)$coef[2,1],2) # beta NP
    Tab_NPvsPdays[4,j]  <- paste0("(", format(unlist(
                            formatC(abs(round(summary(myreg)$coef[2,2],2)),format="f",digits=2)
                                      )),")") # std beta NP
    Tab_NPvsPdays[5,j]  <- round(summary(myreg)$coef[3,1],2) # beta P
    Tab_NPvsPdays[6,j]  <- paste0("(", format(unlist(
                            formatC(abs(round(summary(myreg)$coef[3,2],2)),format="f",digits=2)
                                      )),")") # std beta P
    Tab_NPvsPdays[7,j]  <- round(summary(myreg)$r.squared,2) # R^2
  # add D-W statistic?
    Tab_NPvsPdays[8,j]  <- formatC(abs(round(linearHypothesis(myreg,
          "DeltaYieldCurves$d3M:DeltaYieldCurves$NP=DeltaYieldCurves$d3M:DeltaYieldCurves$P")$Pr[2
                                      ],2)),format="f",digits=2)
}
# export table to latex format
library(xtable)
print(xtable(Tab_NPvsPdays, align="lrrrrrrrrrr", digits=2, type="latex", 
      caption="Yield curve response to short rate movements on policy days and non-policy days.",
      label = "tab:NPvsPdays"), 
      sanitize.text.function = function(x){x}, include.rownames=F,
      booktabs=TRUE, caption.placement="top", 
      file="Text/chapters/tables_graphs/NPvsPdays.tex")

# Old Stuff ---------------------------------------------------------------

# FOMC Dates - Full History Web Scrape ------------------------------------

# https://www.r-bloggers.com/fomc-dates-full-history-web-scrape/
# http://www.returnandrisk.com/2014/11/scraping-data-from-web-pages-fomc-dates.html
# funcions: https://github.com/returnandrisk/r-code/blob/master/FOMC%20Dates%20Functions.R

# read table from Excel and export to LaTeX
OMO_dates     <- read.xlsx("data/FED_OpenMarket_Operations.xlsx", sheetName = "OMOs")
# issue with dates an xtable, fix:
#xtable <- function(x, ...) {
#  for (i in which(sapply(x, function(y) !all(is.na(match(c("POSIXt","Date"),
#        class(y))))))) x[[i]] <- as.character(x[[i]])
#        xtable::xtable(x, ...)
#}
#print(xtable(as.data.frame(OMO_dates[,c(1,3:16)]), caption="Federal Funds Targets.", 
#             label = "tab:OMOs", align="rrrrrrrrrrrrrrrr", floating=TRUE,
#             digits=c(0,0,4,4,4,4,3,3,3,3,3,3,3,3,3,3)), booktabs=TRUE, caption.placement="top")

# BBG data ----------------------------------------------------------------
# from Bloomberg - data to be updated!
#YCGT                  <- read.csv(file="data/US_Treasury_Actives_Curve.csv", 
#                                  header=TRUE, sep=";", na.strings =".", 
#                                  stringsAsFactors=FALSE)
#YCGT[,1]              <- as.Date(YCGT[,1],"%d.%m.%Y") 
#colnames(YCGT)        <- c("Date","1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","30Y")

# !! how to treat values missing due to holidays/weekends etc?
# replace NAs with the last non-NA value - for instance!
# !! look up first NA levels; hard paste (outch)
#YCGT[1,2] = YCGT[1,3]# noooooooooooooo
#YCGT[1,5] = (YCGT[1,4]+YCGT[1,6])/2# noooooooooooooo
#YCGT[1,9] = (YCGT[1,8]+YCGT[1,10])/2# noooooooooooooo
# better extrapolate somehow
#for(j in 2:ncol(YCGT)){
#  for(i in 1:nrow(YCGT)){
#    if(is.na(YCGT[i,j])==T){
#      YCGT[i,j]               <- YCGT[i-1,j]
#    }
#  } 
#}
# truncate data to relevant time period
#YCGT                    <- YCGT[which(
#          YCGT[,1]=="2007-01-01"):which( # sample start
#          YCGT[,1]=="2016-12-31"),]      # sample end

# plot(strptime(YCGT[,1],'%d-%m-%dY'),YCGT[,2],type='l',xlab="Date",ylab="1M")


####################################################
##                      END                       ##
####################################################
