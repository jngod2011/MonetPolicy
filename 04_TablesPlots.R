##########################################################
###                       Thesis                       ###
##########################################################

##########################################################
### --- Author: Manuel von Krosigk
### --- Date: 2017-08-08
### --- Description: create tables and plots from 03_Data
##########################################################

# Input: OMO, Tab_Class, YieldCurves

# Endog vs Exog Days Plots ------------------------------------------------
# c.f. p. 15ff ES paper

# WHICH CLASSIFICATION TO TAKE? -> eg KNN (later LDA)
OMO$Classification <- Tab_Class$KNN
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
    if(OMO$Classification[which(OMO$Date==DeltaYieldCurves$Date[i])]=="Endog"){
      DeltaYieldCurves$NP[i]    <- 0
      DeltaYieldCurves$End[i]   <- 1
      DeltaYieldCurves$Ex[i]    <- 0
    }
    if(OMO$Classification[which(OMO$Date==DeltaYieldCurves$Date[i])]=="Exog"){
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

for(k in 4:(ncol(DeltaYieldCurves)-3)){
  # regress change in n-maturity on NP-dummy*delta3m + End-dummy*delta3m + Ex-dummy*delta3m
  myreg <- lm(DeltaYieldCurves[,k]~DeltaYieldCurves$d3M:DeltaYieldCurves$NP + 
                DeltaYieldCurves$d3M:DeltaYieldCurves$End+
                DeltaYieldCurves$d3M:DeltaYieldCurves$Ex)
  # fill table
  j = k+1-3 
  Tab_EndvsExdays[1,j]  <- formatC(abs(round(summary(myreg)$coef[1,1],2)),format="f",digits=2) # intercept
  Tab_EndvsExdays[2,j]  <- paste0("(", format(unlist(
    formatC(abs(round(summary(myreg)$coef[1,2],2)),format="f",digits=2)
  )),")") # std intercept
  Tab_EndvsExdays[3,j]  <- round(summary(myreg)$coef[2,1],2) # beta NP
  Tab_EndvsExdays[4,j]  <- paste0("(", format(unlist(
    formatC(abs(round(summary(myreg)$coef[2,2],2)),format="f",digits=2)
  )),")") # std beta NP
  Tab_EndvsExdays[5,j]  <- round(summary(myreg)$coef[3,1],2) # beta End
  Tab_EndvsExdays[6,j]  <- paste0("(", format(unlist(
    formatC(abs(round(summary(myreg)$coef[3,2],2)),format="f",digits=2)
  )),")") # std beta End
  Tab_EndvsExdays[7,j]  <- round(summary(myreg)$coef[4,1],2) # beta Ex
  Tab_EndvsExdays[8,j]  <- paste0("(", format(unlist(
    formatC(abs(round(summary(myreg)$coef[4,2],2)),format="f",digits=2)
  )),")") # std beta Ex
  Tab_EndvsExdays[9,j]  <- round(summary(myreg)$r.squared,2) # R^2
  # add D-W statistic?
  Tab_EndvsExdays[10,j]  <- formatC(abs(round(linearHypothesis(myreg, # test equality param NP & End
                                                               "DeltaYieldCurves$d3M:DeltaYieldCurves$NP=DeltaYieldCurves$d3M:DeltaYieldCurves$End")$Pr[2
                                                                                                                                                        ],2)),format="f",digits=2)
  Tab_EndvsExdays[11,j]  <- formatC(abs(round(linearHypothesis(myreg, # test equality param End & Ex
                                                               "DeltaYieldCurves$d3M:DeltaYieldCurves$End=DeltaYieldCurves$d3M:DeltaYieldCurves$Ex")$Pr[2
                                                                                                                                                        ],2)),format="f",digits=2)
}

# export table to latex format
library(xtable)
#print(xtable(Tab_EndvsExdays, align="lrrrrrrrrrr", digits=2, type="latex", 
#             caption="Yield curve response to short rate movements on classified policy days.",
#             label = "tab:EndogvsExogdays"), 
#      sanitize.text.function = function(x){x}, include.rownames=F,
#      booktabs=TRUE, caption.placement="top", 
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
#print(xtable(Tab_NPvsPdays, align="lrrrrrrrrrr", digits=2, type="latex", 
#             caption="Yield curve response to short rate movements on policy days and non-policy days.",
#             label = "tab:NPvsPdays"), 
#      sanitize.text.function = function(x){x}, include.rownames=F,
#      booktabs=TRUE, caption.placement="top", 
#      file="Text/chapters/tables_graphs/NPvsPdays.tex")

