# Multiple regression analysis of monthly water balance components
#
# Clear workspace
rm(list=ls())
# read merged data file
pName      <- getwd()
fName      <- "/Superior/data/Monthly/SupMergedCMS.txt"
fullName   <- paste(pName,fName,sep="")
NBSrcDf    <- read.table(file=fullName,header=TRUE,sep="\t",
                         stringsAsFactors = FALSE)
# Convert character date strings to dates
NBSrcDf$DateSeq <- as.Date(NBSrcDf$DateSeq,format="%Y-%m-%d")
#
# Extract subset of data for model development
TT         <- 696                    # 58 years of monthly data
stmr       <- NBSrcDf$stmrCMS[1:TT] 
prec       <- NBSrcDf$precCMS[1:TT] 
rOff       <- NBSrcDf$rOffCMS[1:TT] 
evap       <- NBSrcDf$evapCMS[1:TT] 
dSto       <- NBSrcDf$dStoCMS[1:TT] 
divr       <- NBSrcDf$divrCMS[1:TT]
# dTim       <- as.numeric(NBSrcDf$DateSeq[1:TT])
# Evaluate the effectiveness of lag1 flows
lStmr      <- NBSrcDf$stmrCMS[2:(TT+1)]
# 
# Get time span for analysis
yearBeg <- NBSrcDf$DateSeq[1]
yearEnd <- NBSrcDf$DateSeq[TT]
#
# Define numeric vector for months
monthNum <- as.numeric(format(NBSrcDf$DateSeq[1:TT], "%m"))
# Initialize vector of colors
colorPnt <- rep(NA, length.out=length(monthNum))
# https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/palettes.html
# rainbow, heat.colors, terrain.colors, topo.colors, cm.colors
colorSet <- topo.colors(12)
for (i in 1:12){
  ndxColor <- which(monthNum==i)
  colorPnt[ndxColor] <- colorSet[i]
}
# 
# Develop multiple regression equation 
multReg <- lm(stmr ~ prec + rOff + evap + dSto + divr)
summary(multReg)
#
# Plot measured with predicted flow for St. Marys River
plot(stmr,predict(multReg),pch=20,col=colorPnt,cex=0.8,
     xlab=expression("Measured Average Monthly Flow in St. Marys River Flow, in" ~ m^{3}~s^{-1}),
     ylab=expression("MultReg Estimates of Monthly Flow, in   " ~m^{3}~s^{-1}),
     main=paste("Measured and Multiple Regression Estimates of Monthly Flow\n", 
                "of St. Marys River from ",yearBeg,' to ',yearEnd,sep=""),cex.main=0.8)
abline(0,1,col="red",lty="dashed")
legend("bottomright",legend=month.abb,col=colorPnt,pch=20,cex=0.8)
#
# Plot measured with flow residuals for St. Marys River
plot(stmr,residuals(multReg),pch=20,col=colorPnt,cex=0.8,
     xlab=expression("Measured Average Monthly Flow in St. Marys River Flow, in" ~ m^{3}%.%~s^{-1}),
     ylab=expression("Multiple Regression Flow Residuals, in   " ~m^{3}%.%~s^{-1}),
     main=paste("Measured and Multiple Regression Residuals of Monthly Flow\n", 
                "of St. Marys River from ",yearBeg,' to ',yearEnd,sep=""),cex.main=0.8)
abline(h=0,col="red",lty="dashed")
legend("bottomright",legend=month.abb,col=colorPnt,pch=20,cex=0.8)
#
# Plot measured with flow residuals for St. Marys River
plot(NBSrcDf$DateSeq[1:TT],residuals(multReg),pch=20,col=colorPnt,cex=0.8,
     xlab="Year",
     ylab=expression("Multiple Regression Flow Residuals, in   " ~m^{3}%.%~s^{-1}),
     main="Flow Residuals from Multiple Regression for St. Marys River with Time",cex.main=0.9)
abline(h=0,col="red",lty="dashed")
legend("bottomright",legend=month.abb,col=colorPnt,pch=20,cex=0.6)
#
# Assessing possible monthly seasonality in residuals
boxplot(residuals(multReg)~monthNum, notch=TRUE,
        col=colorSet,names=month.abb,
        main="Flow Residuals from Multiple Regression for St. Marys River by Month",
        ylab=expression("Residuals of Average Monthly Flow, in   " ~m^{3}~s^{-1}),
        xlab="Month")
abline(h=0,col="red",lty="dashed")
#
###  Assess autoregressive component  ### 
arMultReg   <- lm(stmr ~ lStmr + prec + rOff + divr + evap + dSto )
summary(arMultReg)
#
# Plot measured with predicted flow for St. Marys River
plot(stmr,predict(arMultReg),pch=20,col=colorPnt,cex=0.8,
     xlab=expression("Measured Average Monthly Flow in St. Marys River Flow, in" ~ m^{3}~s^{-1}),
     ylab=expression("AR MultiReg Estimates of Monthly Flow, in   " ~m^{3}~s^{-1}),
     main=paste("Measured and AR Multiple Regression Estimates of Monthly Flow\n", 
                "of St. Marys River from ",yearBeg,' to ',yearEnd,sep=""),cex.main=0.8)
abline(0,1,col="red",lty="dashed")
legend("bottomright",legend=month.abb,col=colorPnt,pch=20,cex=0.8)
#
# Plot measured with flow AR MultReg residuals for St. Marys River
plot(stmr,residuals(arMultReg),pch=20,col=colorPnt,cex=0.8,
     xlab=expression("Measured Average Monthly Flow in St. Marys River Flow, in" ~ m^{3}%.%~s^{-1}),
     ylab=expression("AR Multiple Regression Flow Residuals, in   " ~m^{3}%.%~s^{-1}),
     main=paste("Measured and AR Multiple Regression Residuals of Monthly Flow\n", 
                "of St. Marys River from ",yearBeg,' to ',yearEnd,sep=""),cex.main=0.8)
abline(h=0,col="red",lty="dashed")
legend("bottomright",legend=month.abb,col=colorPnt,pch=20,cex=0.8)
#
# Plot measured with AR MultReg flow residuals for St. Marys River
plot(NBSrcDf$DateSeq[1:TT],residuals(arMultReg),pch=20,col=colorPnt,cex=0.8,
     xlab="Year",
     ylab=expression("AR Multiple Regression Flow Residuals, in   " ~m^{3}%.%~s^{-1}),
     main="Flow Residuals from AR Multiple Regression for St. Marys River with Time",cex.main=0.9)
abline(h=0,col="red",lty="dashed")
legend("bottomright",legend=month.abb,col=colorPnt,pch=20,cex=0.6)
#
# Assessing possible monthly seasonality in AR MultReg residuals
boxplot(residuals(arMultReg) ~ monthNum, notch=TRUE,
        col=colorSet,names=month.abb,
        main="Flow Residuals from AR Multiple Regression for St. Marys River by Month",
        ylab=expression("Residuals of Average Monthly Flow, in   " ~m^{3}~s^{-1}),
        xlab="Month")
abline(h=0,col="red",lty="dashed")
#
# Develop regression models for JanFeb and MarDec
# Identify indices of observations in Jan and Feb
ndxJanFeb <- which(monthNum == 1 | monthNum == 2)
# Develop autoregressive model using Jan and Feb data
arMultRegJF  <- lm(stmr[ndxJanFeb] ~ lStmr[ndxJanFeb] + prec[ndxJanFeb] + rOff[ndxJanFeb]
                 + divr[ndxJanFeb] + evap[ndxJanFeb] + dSto[ndxJanFeb] )
summary(arMultRegJF)
#
# Identify indices of observations in Mar-Dec
ndxMarDec <- which(monthNum  > 2)
arMultRegMD  <- lm(stmr[ndxMarDec] ~ lStmr[ndxMarDec] + prec[ndxMarDec] + rOff[ndxMarDec]
                  + divr[ndxMarDec] + evap[ndxMarDec] + dSto[ndxMarDec] )
summary(arMultRegMD)
#
# anova(multReg,arMultRegMD)
multRegCI95 <- confint(multReg, level=0.95)
multRegCoef <- coef(multReg)
mRegCoefMat <- rbind(multRegCI95[,1],multRegCoef,multRegCI95[,2])

arMultRegCI95 <- confint(multReg, level=0.95)
arMultRegCoef <- coef(multReg)
arMRegCoefMat <- rbind(multRegCI95[,1],multRegCoef,multRegCI95[,2])


