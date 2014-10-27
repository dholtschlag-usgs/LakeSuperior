# Check residual-NBS for Lake Superior
# rNBS = ChangeInStorage - Inflow + Outflow
# Reads in change in Lake Superior storage and St. Marys River outflow  
#   and compares with computed rNBS
#
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
plot(    DateSeq,dStoCMS,type="l",
         main="Change in Lake Superior Storage",
         ylab="cubic meters per second", xlab="year",col="blue")
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
plot(stmrDateSeq,stmrCMS,type="l",
     main="St. Marys River Outflow from Lake Superior",
     ylab="cubic meters per second", xlab="year",col="brown4")
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
plot(divrDateSeq,divrCMS,type="l",
     main="Diversions from Ogoki and Long Lake to Lake Superior",
     ylab="cubic meters per second", xlab="year",col="brown4")
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
plot(NBSrDateSeq,NBSrCMS,type="l",
     main="Residual Net Basin Supply for Lake Superior",
     ylab="cubic meters per second", xlab="year",col="brown4")
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
plot(checkNBSrCMS,NBSrDf$NBSrCMS,pch=20,cex=0.75,col="tan",
                      xlab="Change in Storage plus Outflow, in m^3/s",
                      ylab="Residual Net Basin Supply, in m^3/s",
                      main="Relation Between Computed and Apparent Lake Superior Residual Net Basin Supply",
                      cex.main = 0.8)
abline(0,1,col="red",lty="dashed")
#
# Density of Changes in Lake Storage
densdStoCMS <- density(NBSrDf$dStoCMS)
plot(densdStoCMS,xlab=expression(paste("Monthly Flow Equivalent, in  ",m^{3} %.% s^{-1})),
     ylab="Empirical Probability Density",
     main="Lake Superior Monthly Estimates of Change in Storage")
x1 <- min(which(densdStoCMS$x >= -5500))  
x2 <- max(which(densdStoCMS$x <   8000))
with(densdStoCMS, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="tan"))
#
par(mar=c(5,5,4,2))
plot(DateSeq,cumsum(NBSrDf$dStoCMS),type="l",col="blue",
     xlab="Year",
     ylab=expression(paste("Cumulative Sum of Change in Storage (  ", m^{3} %.% s^{-1},")")),
     main="Cumulative Sum of Monthly Changes in Lake Superior Storage")
abline(h=0,col="red",lty="dashed")

# Density of Changes in Lake Storage
densstmrCMS <- density(NBSrDf$stmrCMS)
plot(densstmrCMS,xlab=expression(paste("Monthly Flow Equivalent, in  ",m^{3} %.% s^{-1})),
     ylab="Empirical Probability Density",
     main="Lake Superior Monthly Outflows through St. Marys River")
x1 <- min(which(densstmrCMS$x >=  1000))  
x2 <- max(which(densstmrCMS$x <   3750))
with(densstmrCMS, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="tan"))

#
# Density of Monthly Diversions in Lake Superior
densdivrCMS <- density(NBSrDf$divrCMS)
plot(densdivrCMS,xlab=expression(paste("Monthly Flow Equivalent, in  ",m^{3} %.% s^{-1})),
     ylab="Empirical Probability Density",
     main="Monthly Diversions into Lake Superior from Long Lake and Ogoki Lake")
x1 <- min(which(densdivrCMS$x >=     0))  
x2 <- max(which(densdivrCMS$x <    450))
with(densdivrCMS, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="tan"))
#
# Clean up variables except selected 
rm(list=setdiff(ls(), c("NBSrDf","NBScDf")))
#



