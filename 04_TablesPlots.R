##########################################################
###                       Thesis                       ###
##########################################################

##########################################################
### --- Author: Manuel von Krosigk
### --- Date: 2017-08-08
### --- Description: create tables and plots from 03_Data
##########################################################

# Input: OMO, Tab_Class, YieldCurves

# Federal funds targets table ---------------------------------------------
OMO$Class[1] <- "NA"
OMO$Class[c(2,3,6,8,9,10,16,18,19,20,31)] <- "Exog"
OMO$Class[c(4,5,7,11:15,17,21:30,32,33,34)] <- "Endog"
OMO$Class[c(4,9,29)] <- "R"

# export table to latex format
library(xtable)
#OMO$Date<-as.character(OMO$Date) # fix date
#print(xtable(OMO[2:nrow(OMO),], align="llcrrrrrrrrrrrrrrrr", digits=c(
#                0,0,4,4,4,4,4,2,2,2,2,2,2,2,2,2,2,2,0), type="latex", 
#             caption="Federal funds target rate adjustments and corresponding yield curve movements.",
#             label = "tab:FEDfundstgt"), 
#      sanitize.text.function = function(x){x}, include.rownames=F,
#      booktabs=TRUE, caption.placement="top", floating.environment='sidewaystable',
#      size="\\fontsize{8pt}{9pt}\\selectfont",
#      file="Text/chapters/tables_graphs/FEDfundstgt.tex")
#OMO$Date                   <- as.Date(OMO$Date,"%Y-%m-%d") # back to normal

# Fed Funds vs Int Rate Plots ---------------------------------------------

#pdf("Text/chapters/tables_graphs/FedFundsIntRate.pdf", height=6, width=6) 
# latex: \includegraphics[width=0.98\textwidth]{chapters/tables_graphs/FedFundsIntRate.pdf} 
#par(mar = c(4, 4, 0.1, 0.1), cex.lab = 0.95, cex.axis = 0.9,
#    mgp = c(2, 0.7, 0), tcl = -0.3, mfrow=c(1,1))
plot(TargetRates$Date,TargetRates$Target_min*100, # lower bound
     type = 'l', xlab = "Date", ylab = "Interest Rate (percent)",
     lwd=1, col='cornflowerblue', pch=19)
lines(TargetRates$Date,TargetRates$Taget_max*100, lwd = 1, col='cornflowerblue')  # upper bound
lines(YieldCurves$Date,YieldCurves$`1M`, col = 'orange', lwd = 1) # 1M g'ment bond
# add QE dates
abline(v=c(TargetRates$Date[which(TargetRates$Date=="2008-11-25")], # QE1
           #TargetRates$Date[which(TargetRates$Date=="2009-03-18")], # QE1
           TargetRates$Date[which(TargetRates$Date=="2010-11-03")], # QE2
           TargetRates$Date[which(TargetRates$Date=="2011-09-21")], # Op Twist
           TargetRates$Date[which(TargetRates$Date=="2012-09-13")]), # QE3
       col = "darkgrey", untf = F, lwd = 1)
mtext("QE1", side = 1, line = 0, outer = FALSE, 
      at = TargetRates$Date[which(TargetRates$Date=="2008-11-25")],
      adj = NA, padj = NA, cex = 0.7, col = "darkgrey", font = NA, crt=90)
mtext("QE2", side = 1, line = 0, outer = FALSE, 
      at = TargetRates$Date[which(TargetRates$Date=="2010-11-03")],
      adj = NA, padj = NA, cex = 0.7, col = "darkgrey", font = NA, crt=90)
mtext("Twist", side = 1, line = 0, outer = FALSE, 
      at = TargetRates$Date[which(TargetRates$Date=="2011-09-21")],
      adj = NA, padj = NA, cex = 0.7, col = "darkgrey", font = NA, crt=90)
mtext("QE3", side = 1, line = 0, outer = FALSE, 
      at = TargetRates$Date[which(TargetRates$Date=="2012-09-13")],
      adj = NA, padj = NA, cex = 0.7, col = "darkgrey", font = NA, crt=90)
legend("toprigh", legend=c("Daily US treasury 1M rate", "Federal Funds target rate (range)"),
       col=c("orange", "cornflowerblue"), lty=1:2, cex=0.8, bg = "white")
#dev.off()


# Endog vs Exog Days Plots ------------------------------------------------

# c.f. p. 15ff ES paper

# Yield curve data only from 2002

DeltaYieldCurves <- data.frame(YieldCurves[2:nrow(YieldCurves),1], 
                               diff(as.matrix(YieldCurves[,2:ncol(YieldCurves)])),NA,NA,NA)
colnames(DeltaYieldCurves) <- c("Date","d1M","d3M","d6M","d1Y","d2Y","d3Y","d5Y","d7Y",
                                "d10Y","d20Y","d30Y", "NP", "End","Ex")

DeltaYieldCurves[,ncol(DeltaYieldCurves)-1]  <- 1-DeltaYieldCurves[,ncol(DeltaYieldCurves)]

# fill in end or ex classification into table
for(i in 1:nrow(DeltaYieldCurves)){
  if(length(which(OMO$Date==DeltaYieldCurves$Date[i])) == 0){
    DeltaYieldCurves$NP[i]    <- 1
    DeltaYieldCurves$End[i]   <- 0
    DeltaYieldCurves$Ex[i]    <- 0
  }
  else{
    if(OMO$Class[which(OMO$Date==DeltaYieldCurves$Date[i])]=="Endog"){
      DeltaYieldCurves$NP[i]    <- 0
      DeltaYieldCurves$End[i]   <- 1
      DeltaYieldCurves$Ex[i]    <- 0
    }
    if(OMO$Class[which(OMO$Date==DeltaYieldCurves$Date[i])]=="Exog"){
      DeltaYieldCurves$NP[i]    <- 0      
      DeltaYieldCurves$End[i]   <- 0
      DeltaYieldCurves$Ex[i]    <- 1
    }
  }
}

# all classified policy events
#pdf("Text/chapters/tables_graphs/ClassPolEvents.pdf", height=6, width=6) 
# latex: \includegraphics[width=0.98\textwidth]{Text/chapters/tables_graphs/ClassPolEvents.pdf} 
#par(mar = c(4, 4, 0.1, 0.1), cex.lab = 0.95, cex.axis = 0.9,
#    mgp = c(2, 0.7, 0), tcl = -0.3, mfrow=c(1,1))
plot(DeltaYieldCurves$d3M[which(DeltaYieldCurves$End==1|DeltaYieldCurves$Ex==1)],
     DeltaYieldCurves$d10Y[which(DeltaYieldCurves$End==1|DeltaYieldCurves$Ex==1)],
     xlab = "Change in 3-month rate", ylab = "Change in 10-year rate",
     pch=19, xlim = c(-0.5,0.1))
abline(h=0, v=0)
#dev.off()

# endog policy events
#pdf("Text/chapters/tables_graphs/EndPolEvents.pdf", height=6, width=6) 
#par(mar = c(4, 4, 0.1, 0.1), cex.lab = 0.95, cex.axis = 0.9,
#    mgp = c(2, 0.7, 0), tcl = -0.3, mfrow=c(1,1))
plot(DeltaYieldCurves$d3M[which(DeltaYieldCurves$End==1)],
     DeltaYieldCurves$d10Y[which(DeltaYieldCurves$End==1)],
     xlab = "Change in 3-month rate", ylab = "Change in 10-year rate",
     pch=19, xlim = c(-0.06,0.04))
abline(h=0, v=0)
text(DeltaYieldCurves$d3M[which(DeltaYieldCurves$End==1)],
     DeltaYieldCurves$d10Y[which(DeltaYieldCurves$End==1)],
     labels=DeltaYieldCurves$Date[which(DeltaYieldCurves$End==1)], cex= 0.7, pos=2)
#dev.off()

# exog policy events
#pdf("Text/chapters/tables_graphs/ExPolEvents.pdf", height=6, width=6) 
#par(mar = c(4, 4, 0.1, 0.1), cex.lab = 0.95, cex.axis = 0.9,
#    mgp = c(2, 0.7, 0), tcl = -0.3, mfrow=c(1,1))
plot(DeltaYieldCurves$d3M[which(DeltaYieldCurves$Ex==1)],
     DeltaYieldCurves$d10Y[which(DeltaYieldCurves$Ex==1)],
     xlab = "Change in 3-month rate", ylab = "Change in 10-year rate",
     pch=19, xlim = c(-0.55,0.1))
abline(h=0, v=0)
text(DeltaYieldCurves$d3M[which(DeltaYieldCurves$Ex==1)],
     DeltaYieldCurves$d10Y[which(DeltaYieldCurves$Ex==1)],
     labels=DeltaYieldCurves$Date[which(DeltaYieldCurves$End==1)], cex= 0.7, pos=2)
#dev.off()

# Endog vs Exog Days Reg --------------------------------------------------

# replicate endog vs exog day regression table from ES, p. 18
Tab_EndvsExdays  <- matrix(nrow=14-1-1-1,ncol=10)
library(car)
Tab_EndvsExdays[,1] <- c("$\\alpha_n$", "", "$\\beta_n^{NP}$", "", "$\\beta_n^{End}$", "",
                         "$\\beta_n^{Ex}$", "", "$\\bar{R}^2$", 
                         "$\\beta_n^{NP}$ = $\\beta_n^{End}$", "$\\beta_n^{End}$ = $\\beta_n^{Ex}$")
colnames(Tab_EndvsExdays) <- c(" ","6m","1y","2y","3y","5y","7y","10y","20y","30y")

library(lmtest)
library(sandwich)

# heteroskedastic standard errors until 10y
for(k in 4:(ncol(DeltaYieldCurves)-3-2)){
  # regress change in n-maturity on NP-dummy*delta3m + End-dummy*delta3m + Ex-dummy*delta3m
  myreg <- lm(DeltaYieldCurves[,k]~DeltaYieldCurves$d3M:DeltaYieldCurves$NP + 
                DeltaYieldCurves$d3M:DeltaYieldCurves$End+
                DeltaYieldCurves$d3M:DeltaYieldCurves$Ex)
#  print(bptest(myreg))
#  print(ncvTest(myreg))
  # fill table
  j = k+1-3 
  Tab_EndvsExdays[1,j]  <- formatC(abs(round(summary(myreg)$coef[1,1],2)),format="f",digits=2) # intercept
  Tab_EndvsExdays[2,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[1,4],2)),format="f",digits=2))),")")
  #  paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[1,4],2)),format="f",digits=2))),")") # p-V intercept
  Tab_EndvsExdays[3,j]  <- round(summary(myreg)$coef[2,1],2) # beta NP
  Tab_EndvsExdays[4,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[2,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[2,4],2)),format="f",digits=2))),")") # p-V beta NP
  Tab_EndvsExdays[5,j]  <- round(summary(myreg)$coef[3,1],2) # beta End
  Tab_EndvsExdays[6,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[3,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[3,4],2)),format="f",digits=2))),")") # p-V beta End
  Tab_EndvsExdays[7,j]  <- round(summary(myreg)$coef[4,1],2) # beta Ex
  Tab_EndvsExdays[8,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[4,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[4,4],2)),format="f",digits=2))),")") # p-V beta Ex
  Tab_EndvsExdays[9,j]  <- round(summary(myreg)$r.squared,2) # R^2
  # add D-W statistic?
  Tab_EndvsExdays[10,j]  <- formatC(abs(round(linearHypothesis(myreg, # test equality param NP & End
                                                               "DeltaYieldCurves$d3M:DeltaYieldCurves$NP=DeltaYieldCurves$d3M:DeltaYieldCurves$End")$Pr[2
                                                                                                                                                        ],2)),format="f",digits=2)
  Tab_EndvsExdays[11,j]  <- formatC(abs(round(linearHypothesis(myreg, # test equality param End & Ex
                                                               "DeltaYieldCurves$d3M:DeltaYieldCurves$End=DeltaYieldCurves$d3M:DeltaYieldCurves$Ex")$Pr[2
                                                                                                                                                        ],2)),format="f",digits=2)
}
# homoskedastic standard errors for 20y and 30y
for(k in (ncol(DeltaYieldCurves)-3-1):(ncol(DeltaYieldCurves)-3)){
  # regress change in n-maturity on NP-dummy*delta3m + End-dummy*delta3m + Ex-dummy*delta3m
  myreg <- lm(DeltaYieldCurves[,k]~DeltaYieldCurves$d3M:DeltaYieldCurves$NP + 
                DeltaYieldCurves$d3M:DeltaYieldCurves$End+
                DeltaYieldCurves$d3M:DeltaYieldCurves$Ex)
  #  print(bptest(myreg))
  #  print(ncvTest(myreg))
  # fill table
  j = k+1-3
  Tab_EndvsExdays[1,j]  <- formatC(abs(round(summary(myreg)$coef[1,1],2)),format="f",digits=2) # intercept  
  Tab_EndvsExdays[2,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[1,4],2)),format="f",digits=2))),")") # p-V intercept
  Tab_EndvsExdays[3,j]  <- round(summary(myreg)$coef[2,1],2) # beta NP  
  Tab_EndvsExdays[4,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[2,4],2)),format="f",digits=2))),")") # p-V beta NP
  Tab_EndvsExdays[5,j]  <- round(summary(myreg)$coef[3,1],2) # beta End  
  Tab_EndvsExdays[6,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[3,4],2)),format="f",digits=2))),")") # p-V beta End
  Tab_EndvsExdays[7,j]  <- round(summary(myreg)$coef[4,1],2) # beta Ex
  Tab_EndvsExdays[8,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[4,4],2)),format="f",digits=2))),")") # p-V beta Ex
  Tab_EndvsExdays[9,j]  <- round(summary(myreg)$r.squared,2) # R^2
  # add D-W statistic?
  Tab_EndvsExdays[10,j]  <- formatC(abs(round(linearHypothesis(myreg, # test equality param NP & End
                                                               "DeltaYieldCurves$d3M:DeltaYieldCurves$NP=DeltaYieldCurves$d3M:DeltaYieldCurves$End")$Pr[2],2)),format="f",digits=2)
  Tab_EndvsExdays[11,j]  <- formatC(abs(round(linearHypothesis(myreg, # test equality param End & Ex
                                                               "DeltaYieldCurves$d3M:DeltaYieldCurves$End=DeltaYieldCurves$d3M:DeltaYieldCurves$Ex")$Pr[2],2)),format="f",digits=2)  
}


# export table to latex format
library(xtable)
#print(xtable(Tab_EndvsExdays, align="lrrrrrrrrrr", digits=2, type="latex", 
#             caption="Regression results for yield curve response to short rate movements on classified policy days.",
#             label = "tab:EndogvsExogdays"), 
#      sanitize.text.function = function(x){x}, include.rownames=F,
#      booktabs=TRUE, caption.placement="top", size="\\small",
#      file="Text/chapters/tables_graphs/EndogvsExogdays.tex")


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

# heteroskedastic standard errors until 10y
for(k in 4:(ncol(DeltaYieldCurves)-2-2)){
  # regress change in n-maturity on NP-dummy*delta3m and P-dummy*delta3m    
  myreg <- lm(DeltaYieldCurves[,k]~DeltaYieldCurves$d3M:DeltaYieldCurves$NP + 
                DeltaYieldCurves$d3M:DeltaYieldCurves$P)
#  print(bptest(myreg))
#  print(ncvTest(myreg))  
  # fill table
  j = k+1-3 
  Tab_NPvsPdays[1,j]  <- formatC(abs(round(summary(myreg)$coef[1,1],2)),format="f",digits=2) # intercept
  Tab_NPvsPdays[2,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[1,4],2)),format="f",digits=2))),")") # p-V intercept
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[1,4],2)),format="f",digits=2))),")") # p-V intercept
  Tab_NPvsPdays[3,j]  <- round(summary(myreg)$coef[2,1],2) # beta NP
  Tab_NPvsPdays[4,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[2,4],2)),format="f",digits=2))),")") # p-V intercept
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[2,4],2)),format="f",digits=2))),")") # p-V beta NP
  Tab_NPvsPdays[5,j]  <- round(summary(myreg)$coef[3,1],2) # beta P
  Tab_NPvsPdays[6,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[3,4],2)),format="f",digits=2))),")") # p-V intercept
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[3,4],2)),format="f",digits=2))),")") # p-V beta P
  Tab_NPvsPdays[7,j]  <- round(summary(myreg)$r.squared,2) # R^2
  # add D-W statistic?
  Tab_NPvsPdays[8,j]  <- formatC(abs(round(linearHypothesis(myreg,"DeltaYieldCurves$d3M:DeltaYieldCurves$NP=DeltaYieldCurves$d3M:DeltaYieldCurves$P")$Pr[2
                                                                                                                                                   ],2)),format="f",digits=2)
}
# homoskedastic standard errors for 20y and 30y
for(k in (ncol(DeltaYieldCurves)-2-1):(ncol(DeltaYieldCurves)-2)){
  # regress change in n-maturity on NP-dummy*delta3m and P-dummy*delta3m    
  myreg <- lm(DeltaYieldCurves[,k]~DeltaYieldCurves$d3M:DeltaYieldCurves$NP + 
                DeltaYieldCurves$d3M:DeltaYieldCurves$P)
  #  print(bptest(myreg))
  #  print(ncvTest(myreg))  
  # fill table
  j = k+1-3 
  Tab_NPvsPdays[1,j]  <- formatC(abs(round(summary(myreg)$coef[1,1],2)),format="f",digits=2) # intercept
  Tab_NPvsPdays[2,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[1,4],2)),format="f",digits=2))),")") # p-V intercept
  Tab_NPvsPdays[3,j]  <- round(summary(myreg)$coef[2,1],2) # beta NP
  Tab_NPvsPdays[4,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[2,4],2)),format="f",digits=2))),")") # p-V beta NP
  Tab_NPvsPdays[5,j]  <- round(summary(myreg)$coef[3,1],2) # beta P
  Tab_NPvsPdays[6,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[3,4],2)),format="f",digits=2))),")") # p-V beta P
  Tab_NPvsPdays[7,j]  <- round(summary(myreg)$r.squared,2) # R^2
  # add D-W statistic?
  Tab_NPvsPdays[8,j]  <- formatC(abs(round(linearHypothesis(myreg,"DeltaYieldCurves$d3M:DeltaYieldCurves$NP=DeltaYieldCurves$d3M:DeltaYieldCurves$P")$Pr[2],2)),format="f",digits=2)
}

# export table to latex format
library(xtable)
#print(xtable(Tab_NPvsPdays, align="lrrrrrrrrrr", digits=2, type="latex", 
#             caption="Regression results for yield curve response to short rate movements on policy days and non-policy days.",
#             label = "tab:NPvsPdays"), 
#      sanitize.text.function = function(x){x}, include.rownames=F,
#      booktabs=TRUE, caption.placement="top", size="\\small",
#      file="Text/chapters/tables_graphs/NPvsPdays.tex")




# Policy Days vs Normal Days QE -------------------------------------------

# QE Non-policy vs policy day regression table

DeltaYieldCurves <- data.frame(YieldCurves[2:nrow(YieldCurves),1], 
                               diff(as.matrix(YieldCurves[,2:ncol(YieldCurves)])),NA,NA,NA)
colnames(DeltaYieldCurves) <- c("Date","d1M","d3M","d6M","d1Y","d2Y","d3Y","d5Y","d7Y",
                                "d10Y","d20Y","d30Y","NP","P","QE")

for(i in 1:nrow(DeltaYieldCurves)){# create policy day dummy
  if(length(which(OMO[,1]==DeltaYieldCurves[i,1])) == 0)
    DeltaYieldCurves$P[i]                       <- 0
  else
    DeltaYieldCurves$P[i]                       <- 1
}
DeltaYieldCurves$NP  <- 1-DeltaYieldCurves$P

# QE dates
QE_dates <- c(
  "2008-11-25", # QE1
  #("2009-03-18, # QE1)
  "2010-11-03", # QE2
  "2011-09-21", # Op Twist
  "2012-09-13") # QE3

for(i in 1:nrow(DeltaYieldCurves)){# create QE dummy
  if(length(which(QE_dates==DeltaYieldCurves[i,1])) == 0)
    DeltaYieldCurves$QE[i]                       <- 0
  else
    DeltaYieldCurves$QE[i]                       <- 1
}

# Regression and fill table
Tab_NPvsPdays_QE <- matrix(nrow=10-1-1+2,ncol=10)
library(car)
Tab_NPvsPdays_QE[,1] <- c("$\\alpha_n$", "", "$\\beta_n^{NP}$", "", "$\\beta_n^P$", "",
                          "$\\beta_n^{QE}$", "",
                       "$\\bar{R}^2$","$\\beta_n^{NP}$ = $\\beta_n^P$")
colnames(Tab_NPvsPdays_QE) <- c(" ","6m","1y","2y","3y","5y","7y","10y","20y","30y")

# heteroskedastic standard errors until 10y
for(k in 4:(ncol(DeltaYieldCurves)-3-2)){
  # regress change in n-maturity on NP-dummy*delta3m and P-dummy*delta3m,
  # control for QE    
  myreg <- lm(DeltaYieldCurves[,k]~DeltaYieldCurves$d3M:DeltaYieldCurves$NP + 
                DeltaYieldCurves$d3M:DeltaYieldCurves$P+
                DeltaYieldCurves$QE)
  # fill table
  j = k+1-3 
  Tab_NPvsPdays_QE[1,j]  <- formatC(abs(round(summary(myreg)$coef[1,1],2)),format="f",digits=2) # intercept
  Tab_NPvsPdays_QE[2,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[1,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[1,4],2)),format="f",digits=2))),")") # p-V intercept
  Tab_NPvsPdays_QE[3,j]  <- round(summary(myreg)$coef[3,1],2) # beta NP
  Tab_NPvsPdays_QE[4,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[3,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[3,4],2)),format="f",digits=2))),")") # p-V beta NP
  Tab_NPvsPdays_QE[5,j]  <- round(summary(myreg)$coef[4,1],2) # beta P
  Tab_NPvsPdays_QE[6,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[4,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[4,4],2)),format="f",digits=2))),")") # p-V beta P
  Tab_NPvsPdays_QE[7,j]  <- round(summary(myreg)$coef[2,1],2) # beta QE
  Tab_NPvsPdays_QE[8,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[2,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[2,4],2)),format="f",digits=2))),")") # p-V beta QE
  Tab_NPvsPdays_QE[9,j]  <- round(summary(myreg)$r.squared,2) # R^2
  # add D-W statistic?
  Tab_NPvsPdays_QE[10,j]  <- formatC(abs(round(linearHypothesis(myreg,"DeltaYieldCurves$d3M:DeltaYieldCurves$NP=DeltaYieldCurves$d3M:DeltaYieldCurves$P")$Pr[2],2)),format="f",digits=2)
}
# homoskedastic standard errors for 20y and 30y
for(k in (ncol(DeltaYieldCurves)-3-1):(ncol(DeltaYieldCurves)-3)){
  # regress change in n-maturity on NP-dummy*delta3m and P-dummy*delta3m,
  # control for QE    
  myreg <- lm(DeltaYieldCurves[,k]~DeltaYieldCurves$d3M:DeltaYieldCurves$NP + 
                DeltaYieldCurves$d3M:DeltaYieldCurves$P+
                DeltaYieldCurves$QE)
  # fill table
  j = k+1-3 
  Tab_NPvsPdays_QE[1,j]  <- formatC(abs(round(summary(myreg)$coef[1,1],2)),format="f",digits=2) # intercept
  Tab_NPvsPdays_QE[2,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[1,4],2)),format="f",digits=2))),")") # p-V intercept
  Tab_NPvsPdays_QE[3,j]  <- round(summary(myreg)$coef[3,1],2) # beta NP
  Tab_NPvsPdays_QE[4,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[3,4],2)),format="f",digits=2))),")") # p-V beta NP
  Tab_NPvsPdays_QE[5,j]  <- round(summary(myreg)$coef[4,1],2) # beta P
  Tab_NPvsPdays_QE[6,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[4,4],2)),format="f",digits=2))),")") # p-V beta P
  Tab_NPvsPdays_QE[7,j]  <- round(summary(myreg)$coef[2,1],2) # beta QE
  Tab_NPvsPdays_QE[8,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[2,4],2)),format="f",digits=2))),")") # p-V beta QE
  Tab_NPvsPdays_QE[9,j]  <- round(summary(myreg)$r.squared,2) # R^2
  # add D-W statistic?
  Tab_NPvsPdays_QE[10,j]  <- formatC(abs(round(linearHypothesis(myreg,"DeltaYieldCurves$d3M:DeltaYieldCurves$NP=DeltaYieldCurves$d3M:DeltaYieldCurves$P")$Pr[2],2)),format="f",digits=2)
}


# export table to latex format
library(xtable)
#print(xtable(Tab_NPvsPdays_QE, align="lrrrrrrrrrr", digits=2, type="latex", 
#             caption="Regression results for yield curve response to short rate movements on policy days and non-policy days after controlling for QE announcements.",
#             label = "tab:NPvsPdays_QE"), 
#      sanitize.text.function = function(x){x}, include.rownames=F,
#      booktabs=TRUE, caption.placement="top", size="\\small",
#      file="Text/chapters/tables_graphs/NPvsPdays_QE.tex")


# Exog vs Endog QE Reg ----------------------------------------------------

# QE endog vs exog day regression table
Tab_EndvsExdays_QE  <- matrix(nrow=14-1-1-1+2,ncol=10)
library(car)
Tab_EndvsExdays_QE[,1] <- c("$\\alpha_n$", "", "$\\beta_n^{NP}$", "", "$\\beta_n^{End}$", "",
                          "$\\beta_n^{Ex}$", "", "$\\beta_n^{QE}$", "", "$\\bar{R}^2$", 
                         "$\\beta_n^{NP}$ = $\\beta_n^{End}$", "$\\beta_n^{End}$ = $\\beta_n^{Ex}$")
colnames(Tab_EndvsExdays_QE) <- c(" ","6m","1y","2y","3y","5y","7y","10y","20y","30y")

# yield curve regressor
DeltaYieldCurves <- data.frame(YieldCurves[2:nrow(YieldCurves),1], 
                               diff(as.matrix(YieldCurves[,2:ncol(YieldCurves)])),NA,NA,NA,NA)
colnames(DeltaYieldCurves) <- c("Date","d1M","d3M","d6M","d1Y","d2Y","d3Y","d5Y","d7Y",
                                "d10Y","d20Y","d30Y", "NP", "End","Ex","QE")

DeltaYieldCurves[,ncol(DeltaYieldCurves)-1]  <- 1-DeltaYieldCurves[,ncol(DeltaYieldCurves)]

# fill in end or ex classification into regressor table
for(i in 1:nrow(DeltaYieldCurves)){
  if(length(which(OMO$Date==DeltaYieldCurves$Date[i])) == 0){
    DeltaYieldCurves$NP[i]    <- 1
    DeltaYieldCurves$End[i]   <- 0
    DeltaYieldCurves$Ex[i]    <- 0
  }
  else{
    if(OMO$Class[which(OMO$Date==DeltaYieldCurves$Date[i])]=="Endog"){
      DeltaYieldCurves$NP[i]    <- 0
      DeltaYieldCurves$End[i]   <- 1
      DeltaYieldCurves$Ex[i]    <- 0
    }
    if(OMO$Class[which(OMO$Date==DeltaYieldCurves$Date[i])]=="Exog"){
      DeltaYieldCurves$NP[i]    <- 0      
      DeltaYieldCurves$End[i]   <- 0
      DeltaYieldCurves$Ex[i]    <- 1
    }
  }
}

for(i in 1:nrow(DeltaYieldCurves)){# create QE dummy
  if(length(which(QE_dates==DeltaYieldCurves[i,1])) == 0)
    DeltaYieldCurves$QE[i]                       <- 0
  else
    DeltaYieldCurves$QE[i]                       <- 1
}

# Regression and fill table
# heteroskedastic standard errors until 10y
for(k in 4:(ncol(DeltaYieldCurves)-4-2)){
  # regress change in n-maturity on NP-dummy*delta3m + End-dummy*delta3m + Ex-dummy*delta3m
  # + QE dummy
  myreg <- lm(DeltaYieldCurves[,k]~DeltaYieldCurves$d3M:DeltaYieldCurves$NP + 
                DeltaYieldCurves$d3M:DeltaYieldCurves$End+
                DeltaYieldCurves$d3M:DeltaYieldCurves$Ex+
                DeltaYieldCurves$QE)
  # fill table
  j = k+1-3 
  Tab_EndvsExdays_QE[1,j]  <- formatC(abs(round(summary(myreg)$coef[1,1],2)),format="f",digits=2) # intercept
  Tab_EndvsExdays_QE[2,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[1,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[1,4],2)),format="f",digits=2))),")") # std intercept
  Tab_EndvsExdays_QE[3,j]  <- round(summary(myreg)$coef[3,1],2) # beta NP
  Tab_EndvsExdays_QE[4,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[3,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[3,4],2)),format="f",digits=2))),")") # std beta NP
  Tab_EndvsExdays_QE[5,j]  <- round(summary(myreg)$coef[4,1],2) # beta End
  Tab_EndvsExdays_QE[6,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[4,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[4,4],2)),format="f",digits=2))),")") # std beta End
  Tab_EndvsExdays_QE[7,j]  <- round(summary(myreg)$coef[5,1],2) # beta Ex
  Tab_EndvsExdays_QE[8,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[5,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[5,4],2)),format="f",digits=2))),")") # std beta Ex
  Tab_EndvsExdays_QE[9,j]  <- round(summary(myreg)$coef[2,1],2) # beta QE
  Tab_EndvsExdays_QE[10,j]  <- paste0("(", format(unlist(formatC(abs(round(coeftest(myreg, vcov. = vcovHC)[2,4],2)),format="f",digits=2))),")")
    #paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[2,4],2)),format="f",digits=2))),")") # std beta QE
  Tab_EndvsExdays_QE[11,j]  <- round(summary(myreg)$r.squared,2) # R^2
  # add D-W statistic?
  Tab_EndvsExdays_QE[12,j]  <- formatC(abs(round(linearHypothesis(myreg, # test equality param NP & End
                                                                  "DeltaYieldCurves$d3M:DeltaYieldCurves$NP=DeltaYieldCurves$d3M:DeltaYieldCurves$End")$Pr[2],2)),format="f",digits=2)
  Tab_EndvsExdays_QE[13,j]  <- formatC(abs(round(linearHypothesis(myreg, # test equality param End & Ex
                                                                  "DeltaYieldCurves$d3M:DeltaYieldCurves$End=DeltaYieldCurves$d3M:DeltaYieldCurves$Ex")$Pr[2],2)),format="f",digits=2)
}
# homoskedastic standard errors for 20y and 30y
for(k in (ncol(DeltaYieldCurves)-4-1):(ncol(DeltaYieldCurves)-4)){
  # regress change in n-maturity on NP-dummy*delta3m + End-dummy*delta3m + Ex-dummy*delta3m
  # + QE dummy
  myreg <- lm(DeltaYieldCurves[,k]~DeltaYieldCurves$d3M:DeltaYieldCurves$NP + 
                DeltaYieldCurves$d3M:DeltaYieldCurves$End+
                DeltaYieldCurves$d3M:DeltaYieldCurves$Ex+
                DeltaYieldCurves$QE)
  # fill table
  j = k+1-3 
  Tab_EndvsExdays_QE[1,j]  <- formatC(abs(round(summary(myreg)$coef[1,1],2)),format="f",digits=2) # intercept
  Tab_EndvsExdays_QE[2,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[1,4],2)),format="f",digits=2))),")") # std intercept
  Tab_EndvsExdays_QE[3,j]  <- round(summary(myreg)$coef[3,1],2) # beta NP
  Tab_EndvsExdays_QE[4,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[3,4],2)),format="f",digits=2))),")") # std beta NP
  Tab_EndvsExdays_QE[5,j]  <- round(summary(myreg)$coef[4,1],2) # beta End
  Tab_EndvsExdays_QE[6,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[4,4],2)),format="f",digits=2))),")") # std beta End
  Tab_EndvsExdays_QE[7,j]  <- round(summary(myreg)$coef[5,1],2) # beta Ex
  Tab_EndvsExdays_QE[8,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[5,4],2)),format="f",digits=2))),")") # std beta Ex
  Tab_EndvsExdays_QE[9,j]  <- round(summary(myreg)$coef[2,1],2) # beta QE
  Tab_EndvsExdays_QE[10,j]  <- paste0("(", format(unlist(formatC(abs(round(summary(myreg)$coef[2,4],2)),format="f",digits=2))),")") # std beta QE
  Tab_EndvsExdays_QE[11,j]  <- round(summary(myreg)$r.squared,2) # R^2
  # add D-W statistic?
  Tab_EndvsExdays_QE[12,j]  <- formatC(abs(round(linearHypothesis(myreg, # test equality param NP & End
                                                               "DeltaYieldCurves$d3M:DeltaYieldCurves$NP=DeltaYieldCurves$d3M:DeltaYieldCurves$End")$Pr[2],2)),format="f",digits=2)
  Tab_EndvsExdays_QE[13,j]  <- formatC(abs(round(linearHypothesis(myreg, # test equality param End & Ex
                                                               "DeltaYieldCurves$d3M:DeltaYieldCurves$End=DeltaYieldCurves$d3M:DeltaYieldCurves$Ex")$Pr[2],2)),format="f",digits=2)
}

# export table to latex format
library(xtable)
#print(xtable(Tab_EndvsExdays_QE, align="lrrrrrrrrrr", digits=2, type="latex", 
#             caption="Regression results for yield curve response to short rate movements on classified policy days after controlling for QE announcements.",
#             label = "tab:EndogvsExogdays_QE"), 
#      sanitize.text.function = function(x){x}, include.rownames=F,
#      booktabs=TRUE, caption.placement="top", size="\\small",
#      file="Text/chapters/tables_graphs/EndogvsExogdays_QE.tex")




####################################################
##                      END                       ##
####################################################
