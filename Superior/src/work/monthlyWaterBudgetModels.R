# Multiple regression analysis of monthly Lake Superior water balance components
# Multiple regression models are developed to estimate monthly flows on St. Marys
# River (stmr) based on 
#
# Clear workspace
rm(list=ls())
# read Net Basin Supply with residual and components Data frame (NBSrcDf)
pName      <- getwd()
fName      <- "/Superior/data/Monthly/SupMergedCMS.txt"
fullName   <- paste(pName,fName,sep="")
NBSrcDf    <- read.table(file=fullName,header=TRUE,sep="\t",
                         stringsAsFactors = FALSE)
# Convert character date strings to dates
NBSrcDf$DateSeq <- as.Date(NBSrcDf$DateSeq,format="%Y-%m-%d")
#
# Evaluate the effectiveness of lag1 flows
NBSrcDf$ar01Stmr    <- c(NBSrcDf$stmrCMS[2:nrow(NBSrcDf)],NA)
# 
#
# Develop multiple regression equation 
multReg    <- with(NBSrcDf, lm(stmrCMS ~ precCMS + rOffCMS + evapCMS + dStoCMS + divrCMS))
print(summary(multReg))
#
multRegAR1 <- with(NBSrcDf, lm(stmrCMS ~ precCMS + rOffCMS + evapCMS + dStoCMS + divrCMS + ar01Stmr))
print(summary(multRegAR1))

NBSrcDf$monthNum <- as.numeric(format(NBSrcDf$DateSeq,'%m'))
#
# Define matrices for model characterization
nParm    <- 6
coefMat6 <- matrix(NA,12,nParm)
r2Mat6   <- matrix(NA,12,1)
stdMat6  <- matrix(NA,12,nParm)
uCIMat6  <- matrix(NA,12,nParm)
lCIMat6  <- matrix(NA,12,nParm)
#
# Setup 4x3 plot frame for monthly data
par(mfrow=c(4,3),cex.lab=0.7,cex.main=0.9)
# Compute monthly regression with standard equation
for (i in 1:12){
  monDat <- subset(NBSrcDf,monthNum == i)
  monLm  <- with(monDat, lm(stmrCMS ~ precCMS + evapCMS + rOffCMS + dStoCMS + divrCMS ))
  coefMat6[i,] <- coef(monLm)
  r2Mat6[i]    <- summary(monLm)$r.squared
  stdMat6[i,]  <- summary(monLm)$coefficients[,2]
  uCIMat6[i,]  <- coefMat6[i,] + qt(.975,summary(monLm)$df[2]) * stdMat6[i,]
  lCIMat6[i,]  <- coefMat6[i,] + qt(.025,summary(monLm)$df[2]) * stdMat6[i,]
  print(summary(monLm))
  plot(monLm$model$stmrCMS,fitted.values(monLm),pch=20,col="blue",cex=0.7,
       main=paste("St. Marys Flow in",month.name[i],sep=" "),
       xlab="Measured Flow, cms", ylab="MultReg Estimated Flow, cms")
  abline(0,1,col="blue",lty="dashed")
}
#
# Reset graph to contain one plot
par(mfrow=c(1,1))
# Plot parameter values and uncertainties with season
for (i in 1:nParm){
  plot(coefMat6[,i],main=paste("Monthly Parameter Estimates with 95% Limits for",names(coef(monLm))[i],
                               "using MultReg Model",sep=" "),cex.main=0.95,
       xaxt = "n", ylab = paste("Parameter",names(coef(monLm))[i],sep=" "),
       ylim = c(min(lCIMat6[,i]),max(uCIMat6[,i])),xlab="Month")
  axis(1, 1:12, month.abb)
  points(uCIMat6[,i],pch=6)
  points(lCIMat6[,i],pch=2)
  abline(h=0,col="red",lty="dashed")
  for (j in 1:12){
    lines(c(j,j),c(uCIMat6[j,i],lCIMat6[j,i]),col="blue",lty="dotted")
  }
}
## Model with ar(1) stmr term
# Define matrices for model characterization
nParm    <- 7
coefMat7 <- matrix(NA,12,nParm)
r2Mat7   <- matrix(NA,12,1)
stdMat7  <- matrix(NA,12,nParm)
uCIMat7  <- matrix(NA,12,nParm)
lCIMat7  <- matrix(NA,12,nParm)
#
# Setup 4x3 plot frame for monthly data
par(mfrow=c(4,3),cex.lab=0.7,cex.main=0.9)
# Compute monthly regression with standard equation and AR(1) stmr term
for (i in 1:12){
  monDat <- subset(NBSrcDf,monthNum == i)
  monLm  <- with(monDat, lm(stmrCMS ~ precCMS + evapCMS + rOffCMS + dStoCMS + divrCMS + ar01Stmr))
  coefMat7[i,] <- coef(monLm)
  r2Mat7[i]    <- summary(monLm)$r.squared
  stdMat7[i,]  <- summary(monLm)$coefficients[,2]
  uCIMat7[i,]  <- coefMat7[i,] + qt(.975,summary(monLm)$df[2]) * stdMat7[i,]
  lCIMat7[i,]  <- coefMat7[i,] + qt(.025,summary(monLm)$df[2]) * stdMat7[i,]
  print(summary(monLm))
  plot(monLm$model$stmrCMS,fitted.values(monLm),pch=20,col="blue",cex=0.7,
       main=paste("St. Marys Flow in",month.name[i],sep=" "),
       xlab="Measured Flow, cms", ylab="AR_MultReg Estimated Flow, cms")
  abline(0,1,col="blue",lty="dashed")
}
#
# Reset graph to contain one plot
par(mfrow=c(1,1),cex.lab=1)
# Plot seasonal parameters for AR_MultReg Model
for (i in 1:nParm){
  plot(coefMat7[,i],main=paste("Monthly Parameter Estimates with 95% Limits for",names(coef(monLm))[i],
                               "using AR_MultReg Model",sep=" "),cex.main=0.95,
       xaxt = "n", ylab = paste("Parameter",names(coef(monLm))[i],sep=" "),
       xlab = "Month",
       ylim = c(min(lCIMat7[,i]),max(uCIMat7[,i])))
  axis(1, 1:12, month.abb)
  points(uCIMat7[,i],pch=6)
  points(lCIMat7[,i],pch=2)
  abline(h=0,col="red",lty="dashed")
  for (j in 1:12){
    lines(c(j,j),c(uCIMat7[j,i],lCIMat7[j,i]),col="blue",lty="dotted")
  }
  if (i == 7){
    abline(h=1,col="red",lty="dashed")
  }
}
# Plot of model r-squared
plot(r2Mat6,main= "Lake Superior Water Budget R2 Estimates for MultReg and AR_MultReg Models",cex.main=0.95,
     col="blue",pch=20,
     xaxt = "n", ylab = "R2 Value",xlab="Monthly Model",
     ylim = c(min(c(r2Mat6,r2Mat7)),max(c(r2Mat6,r2Mat7))))
axis(1, 1:12, month.abb)
points(r2Mat7,col="red",pch=20)
abline(h=summary(multReg)$r.squared, col="blue",lty="dotted")
abline(h=summary(multRegAR1)$r.squared, col="red",lty="dotted")
legend("bottomright",col=c("blue","red","blue","red"),
       lty=c(NA,NA,"dotted","dotted"),pch=c(20,20,NA,NA),
       legend=c("Monthly MultReg","Monthly AR_MultReg","MultReg","AR_MultReg"),
       cex=0.7)

