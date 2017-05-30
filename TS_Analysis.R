####################################################
##                    Thesis                      ##
####################################################

# rm(list=ls())
# setwd("C:/Users/Admin/Google Drive/Masterthesis")

###
# Read TS data
###

## 3-m T-bill rate (measure of policy innovation)

# c.f. class MP Ellingsen, Söderström, p. 25
# announcements: https://www.federalreserve.gov/feeds/h15.html
# 3-Month Treasury Bill: Secondary Market Rate (daily)
# source: https://fred.stlouisfed.org/series/DTB3

Tbill3m                 <- read.csv(file="data/Tbill3m.csv", header=TRUE, sep=",",
                                    na.strings =".", stringsAsFactors=FALSE)
Date                    <- as.Date(Tbill3m[,1],"%Y-%m-%d") 
colnames(Tbill3m)       <- c("Date","Tbill3m")
sum(is.na(x = Tbill3m)) # NAs - obviously before and after changes
# how to treat the NAs? -> CHECK!!
# replace NAs with the last non-NA value - for instance!
for(i in 1:nrow(Tbill3m)){
  if(is.na(Tbill3m[i,2])==T){
    Tbill3m[i,2]               <- Tbill3m[i-1,2]
  }
} 
# truncate data to relevant time period
Tbill3m                 <- Tbill3m[which(
                            Tbill3m[,1]=="2007-01-01"):which( # sample start
                            Tbill3m[,1]=="2016-12-30"),]      # sample end

# plot(strptime(Tbill3m[,1],'%Y-%m-%d'),Tbill3m[,2],type='l',xlab="Date",ylab="3mT-bill")

## Fed Interest Rate Decision

# Source: https://www.federalreserve.gov/monetarypolicy/openmarket.htm
# Source: https://www.investing.com/economic-calendar/interest-rate-decision-168
# Source: http://www.tradingeconomics.com/united-states/interest-rate

OMO                       <- read.csv(file="data/FRB_OMO.csv", header=TRUE, sep=";",
                                    na.strings =".", stringsAsFactors=FALSE)
OMO                       <- OMO[nrow(OMO):1, ]
OMO[,1]                   <- as.Date(OMO[,1],"%Y-%m-%d") 
colnames(OMO)             <- c("Date","Increase (in BP)","Decrease (in BP) Min",
                         "Decrease (in BP) Max","Level (in %) Min","Level (in %) Max")

# set up data frame with date from 3mTbill and corresponding interest rate targets
Rates                     <- data.frame(Tbill3m[,1])
Rates[,1]                 <- as.Date(Rates[,1])
Rates[,2]                 <- Tbill3m[,2]
for(j in 3:4){# fill min and max target in columns 3 & 4 (from columns 5 & 6)
  for(i in 1:nrow(Rates)){
    if(!length(which(Rates[i,1]==OMO[,1]))==T){# trick when date is not included
      Rates[i,j]                 <- NA
    }
    else{
      Rates[i,j]                 <- OMO[which(Rates[i,1]==OMO[,1]),2+j] # cols 5 & 6 
    }
  }
}
colnames(Rates)             <- c('Date','3mTbill','Target_min','Taget_max')
# replace NAs with the last value that is not NA in columns 3 & 4 of data frame
for(j in 3:ncol(Rates)){
  # if first value is NA, take the one from previous year
  if(is.na(Rates[1,j])==T){
    Rates[1,j]               <- OMO[1,2+j] # first value is from 2006, cols 5 & 6 
  }
  for(i in 1:nrow(Rates)){
    if(is.na(Rates[i,j])==T){
      Rates[i,j]               <- Rates[i-1,j]
    }
  } 
}

plot(Rates[,1],Rates[,2],type='l',xlab="Date",ylab="Interest Rates",col='orange')
lines(Rates[,1],Rates[,3], col='red')
lines(Rates[,1],Rates[,4], col='cornflowerblue')

# add decisions where nothing was changed for analysis!



####################################################
##                      END                       ##
####################################################
