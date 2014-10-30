# Check residual-NBS for Lake Superior
# rNBS = ChangeInStorage - Inflow + Outflow
# Reads in change in Lake Superior storage and St. Marys River outflow  
#   and compares with computed rNBS
# locfit is used in density estimation
library(locfit)
#
# Set pathname to working directory
pName    <- getwd()
# Read change in storage data
fName    <- "/Superior/data/Monthly/SupStorageCMS.txt"
fullName <- paste(pName,fName,sep="")
df1      <- read.table(fullName, header = TRUE, sep = "\t", comment.char = "#")
mat1     <- as.matrix(df1[,2:13])
#
# Convert transposed matrix to vector
dStoCMS      <- as.vector(t(mat1))
storBegDate  <- as.Date(paste(df1[1,1],'01/01',sep="/"))
storEndDate  <- as.Date(paste(tail(df1[,1], n= 1),"12/01",sep="/"))
# Create date sequence
DateSeq  <- seq(from = storBegDate, to = storEndDate, by = "month")
# Create dataframe for prec
storDf       <- cbind.data.frame(DateSeq,dStoCMS)
# Plot net basin supply
par(mar=c(5,4.4,4,2)+0.1,las=1)
plot(    DateSeq,dStoCMS,type="l",
         main="Monthly Series of Changes in Lake Superior Storage",
         ylab=expression(paste("Change in Storage, in   ",m^3 %.% s^-1)),
         xlab="year",col="blue")
abline(h= 0, col="slategray", lty = "dashed")
# Clean up 
# 
# Read outflow data
fName    <- "/Superior/data/Monthly/SupStMarysCMS.txt"
fullName <- paste(pName,fName,sep="")
df1      <- read.table(fullName, header = TRUE, sep = "\t", comment.char = "#")
mat1     <- as.matrix(df1[,2:13])
# Convert transposed matrix to vector
stmrCMS  <- as.vector(t(mat1))
stmrBegDate  <- as.Date(paste(df1[1,1],'01/01',sep="/"))
stmrEndDate  <- as.Date(paste(tail(df1[,1], n= 1),"12/01",sep="/"))
# Create date sequence
stmrDateSeq  <- seq(from = stmrBegDate, to = stmrEndDate, by = "month")
# Create dataframe for prec
stmrDf       <- cbind.data.frame(stmrDateSeq,stmrCMS)
# Plot monthly runoff time series
par(mar=c(5,4.4,4,2)+0.1,las=1)
plot(stmrDateSeq,stmrCMS,type="l",
     main="Monthly Series of St. Marys River Outflows from Lake Superior",
     ylab=expression(paste("Outflow, in   ",m^3 %.% s^-1)),
     xlab="year",col="brown4")
#
# Read inflow diversions data
fName    <- "/Superior/data/Monthly/SupDiversionsCMS.txt"
fullName <- paste(pName,fName,sep="")
df1      <- read.table(fullName, header = TRUE, sep = "\t", comment.char = "#")
mat1     <- as.matrix(df1[,2:13])
# Convert transposed matrix to vector
divrCMS  <- as.vector(t(mat1))
divrBegDate  <- as.Date(paste(df1[1,1],'01/01',sep="/"))
divrEndDate  <- as.Date(paste(tail(df1[,1], n= 1),"12/01",sep="/"))
# Create date sequence
divrDateSeq  <- seq(from = divrBegDate, to = divrEndDate, by = "month")
# Create dataframe for diversions
divrDf       <- cbind.data.frame(divrDateSeq,divrCMS)
# Plot monthly diversion time series
par(mar=c(5,4.4,4,2)+0.1,las=1)
plot(divrDateSeq,divrCMS,type="l",
     main="Monthly Series of Diversions from Ogoki and Long Lake to Lake Superior",
     ylab=expression(paste("Diversions into Lake Superior, in   ",m^3 %.% s^-1)),
     xlab="year",col="brown4")
#
# Read computed residual NBS data
fName    <- "/Superior/data/Monthly/SupNBSresidCMS.txt"
fullName <- paste(pName,fName,sep="")
df1      <- read.table(fullName, header = TRUE, sep = "\t", comment.char = "#")
mat1     <- as.matrix(df1[,2:13])
# Convert transposed matrix to vector
NBSrCMS  <- as.vector(t(mat1))
NBSrBegDate  <- as.Date(paste(df1[1,1],'01/01',sep="/"))
NBSrEndDate  <- as.Date(paste(tail(df1[,1], n= 1),"12/01",sep="/"))
# Create date sequence
NBSrDateSeq  <- seq(from = NBSrBegDate, to = NBSrEndDate, by = "month")
# Create dataframe for rNBS
NBSrDf       <- cbind.data.frame(NBSrDateSeq,NBSrCMS)
# Plot monthly runoff time series
par(mar=c(5,4.4,4,2)+0.1,las=1)
plot(NBSrDateSeq,NBSrCMS,type="l",
     main="Monthly Series of Residual Net Basin Supply for Lake Superior",
     ylab=expression(paste("Residual Net Basin Supply, in   ",m^3 %.% s^-1)),
     xlab="Year",col="brown4")
abline(h= 0, col="slategray", lty = "dashed")
#
# Merge dataframe by date
# Merge change in storage and st. marys flow dataframes on date
storStmrDf <- merge(storDf,stmrDf, by.x = "DateSeq", by.y = "stmrDateSeq")
# Merge with diversions by date
storStrmDivrDf  <- merge(storStmrDf, divrDf, by.x = "DateSeq", by.y = "divrDateSeq")
# Merge with rNBS by date
storStmrDivrNBSrDf  <- merge(storStrmDivrDf, NBSrDf, by.x = "DateSeq", by.y = "NBSrDateSeq")
# Check rNBS as the sum of change in storage plus outflow
checkNBSrCMS <- storStmrDivrNBSrDf$dStoCMS + storStmrDivrNBSrDf$stmrCMS - storStmrDivrNBSrDf$divrCMS; 
# Simplify merged dataframe name
NBSrDf       <- storStmrDivrNBSrDf
#
par(mar=c(5,4.4,4,2)+0.1,las=1)
plot(checkNBSrCMS,NBSrDf$NBSrCMS,pch=20,cex=0.75,col="tan",
     xlab=expression(paste("Change In Storage - Inflow + Outflow, in   ",m^3 %.% s^-1)),
     ylab=expression(paste("Residual Net Basin Supply, in   ",m^3 %.% s^-1)),
     main="Relation Between Computed and Apparent Lake Superior Residual Net Basin Supply",
     cex.main = 0.9)
abline(0,1,col="red",lty="dashed")
#
# Density of Changes in Lake Storage
densdStoCMS <- density(NBSrDf$dStoCMS)
plot(densdStoCMS,xlab=expression(paste("Monthly Flow Equivalent, in  ",m^{3} %.% s^{-1})),
     ylab="Empirical Probability Density",
     main="Probability Density of Lake Superior Monthly Estimates of Change in Storage")
x1 <- min(which(densdStoCMS$x >= min(NBSrDf$dStoCMS) ))  
x2 <- max(which(densdStoCMS$x <  max(NBSrDf$dStoCMS) ))
with(densdStoCMS, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="tan"))
#
par(mar=c(5,5,4,2))
plot(DateSeq,cumsum(NBSrDf$dStoCMS),type="l",col="blue",
     xlab="Year",
     ylab=expression(paste("Cumulative Sum of Change in Storage (  ", m^{3} %.% s^{-1},")")),
     main="Series of Cumulative Sum of Monthly Changes in Lake Superior Storage")
abline(h=0,col="red",lty="dashed")

# Probability density of St. Marys River Outflows
densstmrCMS <- density(NBSrDf$stmrCMS)
plot(densstmrCMS,xlab=expression(paste("Monthly Flow Equivalent, in  ",m^{3} %.% s^{-1})),
     ylab="Empirical Probability Density",
     main="Probability Density of Lake Superior Monthly Outflows through St. Marys River")
x1 <- min(which(densstmrCMS$x >=  min(NBSrDf$stmrCMS) ))  
x2 <- max(which(densstmrCMS$x <   max(NBSrDf$stmrCMS) ))
with(densstmrCMS, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="tan"))
#
# Probability density of Monthly Diversions in Lake Superior
# library(locfit) is needed
locMod   <- locfit( ~ lp(NBSrDf$divrCMS))
# densdivrCMS <- density(NBSrDf$divrCMS)
# Quantiles to evalute 
locEval  <- seq(min(NBSrDf$divrCMS),max(NBSrDf$divrCMS),length.out = 500)
# Estimates for evaluated quantiles
locFit   <- predict(locMod,locEval)
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
plot(locEval,locFit,type="l",
     xlab=expression(paste("Monthly Flow Equivalent, in  ",m^{3} %.% s^{-1})),
     ylab="Empirical Probability Density",
     main="Probability Density of Monthly Diversions into Lake Superior\n from Long Lake and Ogoki Lake",
     cex.main=0.8,sub="Function locfit used in density approximation")
x1 <- min(which(locEval >=  min(NBSrDf$divrCMS) ))  
x2 <- max(which(locEval <   max(NBSrDf$divrCMS) ))
polygon(x=c(locEval,                rev(locEval),locEval[1]),
        y=c(rep(0,length(locEval)), rev(locFit ),0),
        col="tan")
# abline(v=0,col="red",lty="dashed")
#
pairs(NBSrDf,pch=20,cex=0.5,col="blue",
      labels=c("Date","Storage Change","St.Marys River","Diversions","rNBS"))
#
#
boxplot(NBSrDf$stmrCMS ~ format(NBSrDf$DateSeq, "%m"),
        names=unique(format(NBSrDf$DateSeq, "%b")),
        ylab=expression(paste("Precipitation,   ",m^{3} %.% s^{-1})),
        par(mar=c(5,4.4,4,2)+0.1),las=1,
        main="Monthly Distribution of St. Marys River Outflows",
        cex.main=0.9)
#
boxplot(NBSrDf$divrCMS ~ format(NBSrDf$DateSeq, "%m"),
        names=unique(format(NBSrDf$DateSeq, "%b")),
        ylab=expression(paste("Diversions,   ",m^{3} %.% s^{-1})),
        par(mar=c(5,5,4,2)+0.1),
        main="Monthly Distribution of Diversions from Ogoki and Long Lakes to Lake Superior",
        cex.main=0.9)
#
boxplot(NBSrDf$dStoCMS ~ format(NBSrDf$DateSeq, "%m"),
        names=unique(format(NBSrDf$DateSeq, "%b")),
        ylab=expression(paste("Change in Storage,   ",m^{3} %.% s^{-1})),
        par(mar=c(5,5,4,2)+0.1),
        main="Monthly Distribution of Changes in Storage of Lake Superior",
        cex.main=0.9)
abline(h=0,col="red",lty="dashed")
#
boxplot(NBSrDf$NBSrCMS ~ format(NBSrDf$DateSeq, "%m"),
        names=unique(format(NBSrDf$DateSeq, "%b")),
        ylab=expression(paste("Net Basin Supply,   ",m^{3} %.% s^{-1})),
        par(mar=c(5,5,4,2)+0.1),
        main="Monthly Distribution of Residuals Estimate of Net Basin Supply for Lake Superior",
        cex.main=0.9)
abline(h=0,col="red",lty="dashed")
#
# Clean up variables except selected 
# rm(list=setdiff(ls(), c("NBSrDf","NBScDf")))
#



