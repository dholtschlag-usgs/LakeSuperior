# Multiple regression analysis of monthly Lake Superior water balance components
# Multiple regression models are developed to estimate monthly flows on St. Marys
# "Level" Beginning of Month Lake Superior Water Levels
#
# Clear workspace
rm(list=ls())
# read Net Basin Supply with residual and components Data frame (NBSrcLevelDf)
pName      <- getwd()
fName      <- "/Superior/data/Monthly/SupMergedCMS.txt"
fullName   <- paste(pName,fName,sep="")
NBSrcLevelDf    <- read.table(file=fullName,header=TRUE,sep="\t",
                         stringsAsFactors = FALSE)
# Convert character date strings to dates
NBSrcLevelDf$DateSeq <- as.Date(NBSrcLevelDf$DateSeq,format="%Y-%m-%d")
#
# Evaluate the effectiveness of lag1 flows
NBSrcLevelDf$ar01Stmr    <- c(NBSrcLevelDf$stmrCMS[2:nrow(NBSrcLevelDf)],NA)
# 
# Read in table of Beginning of Month Water Levels on Lake Superior 
fName      <- "/Superior/data/Monthly/SupBOMLevelsM.txt"
fullName   <- paste(pName,fName,sep="")
LSupBOMLevelM <- read.table(file=fullName,header=TRUE,sep="\t",comment.char="#",
                           stringsAsFactors = FALSE)
#
dateBeg     <- as.Date(paste(LSupBOMLevelM$Year[1],'-01-01',sep="",format="%Y-%m-%d"))
dateEnd     <- as.Date(paste(tail(LSupBOMLevelM$Year,1),'-12-01',sep=""),
                       format="%Y-%m-%d")
DateSeq     <- seq(dateBeg, dateEnd, by="month")
# Convert data frame LSupBOMLevelM to nrow X 1 matrix
LSupBOMmat  <- matrix(unlist(t(LSupBOMLevelM[,2:13])),nrow=nrow(LSupBOMLevelM)*12,
                           ncol=1,byrow=FALSE)
LSupBOMDf   <- data.frame(DateSeq,LSupBOMmat)
colnames(LSupBOMDf) <- c("DateSeq","WaterLevelM")
#
# Plot the beginning of monthly water levels on Lake superior
plot(LSupBOMDf$DateSeq,LSupBOMDf$WaterLevelM,type="l",xlab="Year",
     ylab="Water Level, in IGLD 1985 meters",
     main="Beginning of Monthly Water Levels on Lake Superior")
# Merge data frames by DateSeq
NBSrcLevelDf  <- merge(NBSrcLevelDf,LSupBOMDf,by="DateSeq")
#
# Develop multiple regression equation to confirm data merge
multReg       <- with(NBSrcLevelDf, lm(stmrCMS ~ precCMS + rOffCMS + evapCMS + dStoCMS + divrCMS))
print(summary(multReg))
#
multRegLevel    <- with(NBSrcLevelDf, lm(stmrCMS ~ precCMS + rOffCMS + evapCMS + dStoCMS + divrCMS + WaterLevelM))
print(summary(multRegLevel))

multRegLevelAR  <- with(NBSrcLevelDf, lm(stmrCMS ~ precCMS + rOffCMS + evapCMS + 
                                           dStoCMS + divrCMS + WaterLevelM + ar01Stmr))
print(summary(multRegLevelAR))

multRegAR       <- with(NBSrcLevelDf, lm(stmrCMS ~ precCMS + rOffCMS + evapCMS + 
                                           dStoCMS + divrCMS + ar01Stmr))
print(summary(multRegAR))
#
NBSrcLevelDf$monthNum <- as.numeric(format(NBSrcLevelDf$DateSeq,'%m'))
#
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
  monDat <- subset(NBSrcLevelDf,monthNum == i)
  monLm  <- with(monDat, lm(stmrCMS ~ precCMS + evapCMS + rOffCMS + dStoCMS + divrCMS + WaterLevelM))
  coefMat7[i,] <- coef(monLm)
  r2Mat7[i]    <- summary(monLm)$r.squared
  stdMat7[i,]  <- summary(monLm)$coefficients[,2]
  uCIMat7[i,]  <- coefMat7[i,] + qt(.975,summary(monLm)$df[2]) * stdMat7[i,]
  lCIMat7[i,]  <- coefMat7[i,] + qt(.025,summary(monLm)$df[2]) * stdMat7[i,]
  print(summary(monLm))
  plot(monLm$model$stmrCMS,fitted.values(monLm),pch=20,col="blue",cex=0.7,
       main=paste("St. Marys Flow in",month.name[i],sep=" "),
       xlab="Measured Flow, cms", ylab="MultRegLevel Estimated Flow, cms")
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
  if (names(coef(monLm))[i] == "ar01Stmr"){
    abline(h=1,col="red",lty="dashed")
  }
}
#

# Define matrices for model characterization
nParm    <- 8
coefMat8 <- matrix(NA,12,nParm)
r2Mat8   <- matrix(NA,12,1)
stdMat8  <- matrix(NA,12,nParm)
uCIMat8  <- matrix(NA,12,nParm)
lCIMat8  <- matrix(NA,12,nParm)
#
# Setup 4x3 plot frame for monthly data
par(mfrow=c(4,3),cex.lab=0.7,cex.main=0.9)
# Compute monthly regression with standard equation and AR(1) stmr term
for (i in 1:12){
  monDat <- subset(NBSrcLevelDf,monthNum == i)
  monLm  <- with(monDat, lm(stmrCMS ~ precCMS + evapCMS + rOffCMS + dStoCMS + 
                              divrCMS + WaterLevelM + ar01Stmr))
  coefMat8[i,] <- coef(monLm)
  r2Mat8[i]    <- summary(monLm)$r.squared
  stdMat8[i,]  <- summary(monLm)$coefficients[,2]
  uCIMat8[i,]  <- coefMat8[i,] + qt(.975,summary(monLm)$df[2]) * stdMat8[i,]
  lCIMat8[i,]  <- coefMat8[i,] + qt(.025,summary(monLm)$df[2]) * stdMat8[i,]
  print(summary(monLm))
  plot(monLm$model$stmrCMS,fitted.values(monLm),pch=20,col="blue",cex=0.7,
       main=paste("St. Marys Flow in",month.name[i],sep=" "),
       xlab="Measured Flow, cms", ylab="MultRegLevelAR Estimated Flow, cms")
  abline(0,1,col="blue",lty="dashed")
}
#
# Reset graph to contain one plot
par(mfrow=c(1,1),cex.lab=1)
# Plot seasonal parameters for MultRegLevelAR Model
for (i in 1:nParm){
  plot(coefMat8[,i],main=paste("Monthly Parameter Estimates with 95% Limits for",names(coef(monLm))[i],
                               "using MultRegLevelAR Model",sep=" "),cex.main=0.95,
       xaxt = "n", ylab = paste("Parameter",names(coef(monLm))[i],sep=" "),
       xlab = "Month",
       ylim = c(min(lCIMat8[,i]),max(uCIMat8[,i])))
  axis(1, 1:12, month.abb)
  points(uCIMat8[,i],pch=6)
  points(lCIMat8[,i],pch=2)
  abline(h=0,col="red",lty="dashed")
  for (j in 1:12){
    lines(c(j,j),c(uCIMat8[j,i],lCIMat8[j,i]),col="blue",lty="dotted")
  }
  if (names(coef(monLm))[i] == "ar01Stmr"){
    abline(h=1,col="red",lty="dashed")
  }
}
#
# Define matrices for model characterization
nParm     <- 7
coefMat7a <- matrix(NA,12,nParm)
r2Mat7a   <- matrix(NA,12,1)
stdMat7a  <- matrix(NA,12,nParm)
uCIMat7a  <- matrix(NA,12,nParm)
lCIMat7a  <- matrix(NA,12,nParm)
#
# Setup 4x3 plot frame for monthly data
par(mfrow=c(4,3),cex.lab=0.7,cex.main=0.9)
# Compute monthly regression with standard equation and AR(1) stmr term
for (i in 1:12){
  monDat <- subset(NBSrcLevelDf,monthNum == i)
  monLm  <- with(monDat, lm(stmrCMS ~ precCMS + evapCMS + rOffCMS + dStoCMS + divrCMS + ar01Stmr))
  coefMat7a[i,] <- coef(monLm)
  r2Mat7a[i]    <- summary(monLm)$r.squared
  stdMat7a[i,]  <- summary(monLm)$coefficients[,2]
  uCIMat7a[i,]  <- coefMat7a[i,] + qt(.975,summary(monLm)$df[2]) * stdMat7a[i,]
  lCIMat7a[i,]  <- coefMat7a[i,] + qt(.025,summary(monLm)$df[2]) * stdMat7a[i,]
  print(summary(monLm))
  plot(monLm$model$stmrCMS,fitted.values(monLm),pch=20,col="blue",cex=0.7,
       main=paste("St. Marys Flow in",month.name[i],sep=" "),
       xlab="Measured Flow, cms", ylab="MultRegAR Estimated Flow, cms")
  abline(0,1,col="blue",lty="dashed")
}
#
# Reset graph to contain one plot
par(mfrow=c(1,1),cex.lab=1)
# Plot seasonal parameters for MultRegAR Model
for (i in 1:nParm){
  plot(coefMat7a[,i],main=paste("Monthly Parameter Estimates with 95% Limits for",names(coef(monLm))[i],
                               "using MultRegAR Model",sep=" "),cex.main=0.95,
       xaxt = "n", ylab = paste("Parameter",names(coef(monLm))[i],sep=" "),
       xlab = "Month",
       ylim = c(min(lCIMat7a[,i]),max(uCIMat7a[,i])))
  axis(1, 1:12, month.abb)
  points(uCIMat7a[,i],pch=6)
  points(lCIMat7a[,i],pch=2)
  abline(h=0,col="red",lty="dashed")
  for (j in 1:12){
    lines(c(j,j),c(uCIMat7a[j,i],lCIMat7a[j,i]),col="blue",lty="dotted")
  }
  if (names(coef(monLm))[i] == "ar01Stmr"){
    abline(h=1,col="red",lty="dashed")
  }
}
#
# Plot of model r-squared
plot(r2Mat7,main= "Lake Superior Water Budget R2 Estimates for MultRegLevel, MultRegLevelAR, and MultRegAR Models",cex.main=0.95,
     col="blue",pch=20,
     xaxt = "n", ylab = "R2 Value",xlab="Monthly Model",
     ylim = c(min(c(r2Mat7,r2Mat8,summary(multRegLevel)$r.squared)),
              max(c(r2Mat7,r2Mat8))))
axis(1, 1:12, month.abb)
points(r2Mat8,col="red",pch=20)
points(r2Mat7a,col="green",pch=5)
abline(h=summary(multRegLevel)$r.squared, col="blue",lty="dotted")
abline(h=summary(multRegLevelAR)$r.squared, col="red",lty="dotted")
abline(h=summary(multRegAR)$r.squared,col="green",lty="dotted")
legend("bottomright",col=c("blue","red","green","blue","red","green"),
       lty=c(NA,NA,NA,"dotted","dotted","dotted"),pch=c(20,20,5,NA,NA,NA),
       legend=c("Monthly MultRegLevel","Monthly MultRegLevelAR",
                "Monthly MultRegAR","MultRegLevel","MultRegLevelAR","MultRegAR"),
       cex=0.7)
#
