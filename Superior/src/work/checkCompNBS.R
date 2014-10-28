# Check component-NBS for Lake Superior
# Reads in components, computes NBS and compares with coordinated value
# component_NBS = Precipitation + Runoff - Evaporation  
# Clear memory
# rm(list=ls())
#
# Set pathname to working directory
pName    <- getwd()
# Read precip data
fName    <- "/Superior/data/Monthly/SupPrecipCMS.txt"
fullName <- paste(pName,fName,sep="")
df1      <- read.table(fullName, header = TRUE, sep = "\t", comment.char = "#")
mat1     <- as.matrix(df1[,2:13])
#
# Convert transposed matrix to vector
precCMS      <- as.vector(t(mat1))
precBegDate  <- as.Date(paste(df1[1,1],'01/01',sep="/"))
precEndDate  <- as.Date(paste(tail(df1[,1], n= 1),"12/01",sep="/"))
# Create date sequence
    DateSeq  <- seq(from = precBegDate, to = precEndDate, by = "month")
# Create dataframe for prec
precDf       <- cbind.data.frame(DateSeq,precCMS)
# Plot net basin supply
plot(    DateSeq,precCMS,type="l",
     main="Monthly Series of Overlake Precipiation for Lake Superior",
     ylab="cubic meters per second", xlab="year",col="blue")
#
# Read evaporation data
fName    <- "/Superior/data/Monthly/SupEvapCMS.txt"
fullName <- paste(pName,fName,sep="")
df1      <- read.table(fullName, header = TRUE, sep = "\t", comment.char = "#")
mat1     <- as.matrix(df1[,2:13])
# Convert transposed matrix to vector
evapCMS  <- as.vector(t(mat1))
evapBegDate  <- as.Date(paste(df1[1,1],'01/01',sep="/"))
evapEndDate  <- as.Date(paste(tail(df1[,1], n= 1),"12/01",sep="/"))
# Create date sequence
evapDateSeq  <- seq(from = evapBegDate, to = evapEndDate, by = "month")
# Create dataframe for prec
evapDf       <- cbind.data.frame(evapDateSeq,evapCMS)
# Plot monthly evaporation series
plot(evapDateSeq,evapCMS,type="l",
     main="Monthly Series of Overlake Evaporation for Lake Superior",
     ylab="cubic meters per second", xlab="year",col="salmon")
abline(h= 0, col="slategray", lty = "dashed")
#
# Read runoff data
fName    <- "/Superior/data/Monthly/SupRunoffCMS.txt"
fullName <- paste(pName,fName,sep="")
df1      <- read.table(fullName, header = TRUE, sep = "\t", comment.char = "#")
mat1     <- as.matrix(df1[,2:13])
# Convert transposed matrix to vector
rOffCMS  <- as.vector(t(mat1))
rOffBegDate  <- as.Date(paste(df1[1,1],'01/01',sep="/"))
rOffEndDate  <- as.Date(paste(tail(df1[,1], n= 1),"12/01",sep="/"))
# Create date sequence
rOffDateSeq  <- seq(from = rOffBegDate, to = rOffEndDate, by = "month")
# Create dataframe for prec
rOffDf       <- cbind.data.frame(rOffDateSeq,rOffCMS)
# Plot monthly runoff time series
plot(rOffDateSeq,rOffCMS,type="l",
     main="Monthly Series of Land Surface Runoff to Lake Superior",
     ylab="cubic meters per second", xlab="year",col="brown4")
#
# Read component NBS data
fName    <- "/Superior/data/Monthly/SupNBScompCMS.txt"
fullName <- paste(pName,fName,sep="")
df1      <- read.table(fullName, header = TRUE, sep = "\t", comment.char = "#")
mat1     <- as.matrix(df1[,2:13])
# Convert transposed matrix to vector
NBScCMS       <- as.vector(t(mat1))
nbs_BegDate  <- as.Date(paste(df1[1,1],'01/01',sep="/"))
nbs_EndDate  <- as.Date(paste(tail(df1[,1], n= 1),"12/01",sep="/"))
# Create date sequence
nbs_DateSeq  <- seq(from = nbs_BegDate, to = nbs_EndDate, by = "month")
# Create dataframe for prec
nbs_Df       <- cbind.data.frame(nbs_DateSeq,NBScCMS)
# Plot monthly runoff time series
plot(nbs_DateSeq,NBScCMS,type="l",
     main="Monthly Series of Component Net Basin Supply for Lake Superior",
     ylab="cubic meters per second", xlab="year",col="steelblue")
abline(h= 0, col="slategray", lty = "dashed")
#
# Merge prec and evap dataframes on date
precEvapDf <- merge(precDf,evapDf, by.x = "DateSeq", by.y = "evapDateSeq")
# Merge rOff by date
precEvaprOffDf  <- merge(precEvapDf, rOffDf, by.x = "DateSeq", by.y = "rOffDateSeq")
# Merge NBScCMS in
NBScDf      <- merge(precEvaprOffDf, nbs_Df, by.x = "DateSeq", by.y = "nbs_DateSeq")
rm(list=c("precDf","evapDf","rOffDf","nbs_Df","precEvapDf","precEvaprOffDf"))
#
# Compute components estimate of net basin supply
nbsxCMS <- NBScDf$prec - NBScDf$evap + NBScDf$rOff
#
plot(NBScDf$NBScCMS,nbsxCMS,pch=20,cex=0.75,col="tan",
                      xlab="Net Basin Suppy, in m^3/s",
                      ylab="Prec - Evap + Runoff, in m^3/s",
                      main="Relation Between Computed and Apparent Lake Superior Net Basin Supply",
                      cex.main = 0.8)
#
abline(0,1,col="red",lty="dashed")
#
rm(list=setdiff(ls(),c("NBSrDf","NBScDf")))
#
# Density of Precipitation
densPrecCMS <- density(NBScDf$precCMS)
plot(densPrecCMS,xlab=expression(paste("Monthly Flow Equivalent, in  ",m^{3} %.% s^{-1})),
     ylab="Empirical Probability Density",
     main="Probability Density of Lake Superior Monthly Estimates of Precipitation",
     cex.main=0.9)
x1 <- min(which(densPrecCMS$x >= min(NBScDf$precCMS) ))  
x2 <- max(which(densPrecCMS$x <  max(NBScDf$precCMS) ))
with(densPrecCMS, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="tan"))
#
# Density of Evaporation
densEvapCMS <- density(NBScDf$evapCMS)
plot(densEvapCMS,xlab=expression(paste("Monthly Flow Equivalent, in  ",m^{3} %.% s^{-1})),
     ylab="Empirical Probability Density",
     main="Probability Density of Lake Superior Monthly Estimates of Evaporation",
     cex.main=0.9)
x1 <- min(which(densEvapCMS$x >=  min(NBScDf$evapCMS) ))  
x2 <- max(which(densEvapCMS$x <   max(NBScDf$evapCMS) ))
with(densEvapCMS, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="tan"))
#
# Density of Basin Runoff
densrOffCMS <- density(NBScDf$rOffCMS)
plot(densrOffCMS,xlab=expression(paste("Monthly Flow Equivalent, in  ",m^{3} %.% s^{-1})),
     ylab="Empirical Probability Density",
     main="Probability Density of Lake Superior Monthly Estimates of Basin Runoff",
     cex.main=0.9)
x1 <- min(which(densrOffCMS$x >=  min(NBScDf$rOffCMS) ))  
x2 <- max(which(densrOffCMS$x <   max(NBScDf$rOffCMS) ))
with(densrOffCMS, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="tan"))
#
# Density of Net Basin Supply
densNBSc <- density(NBScDf$NBScCMS)
plot(densNBSc,xlab=expression(paste("Monthly Flow, in  ",m^{3} %.% s^{-1})),
     ylab="Empirical Probability Density",
     main="Probability Density of Monthly Series of Lake Superior Components Estimate of Net Basin Supply",
     cex.main=0.9)
x1 <- min(which(densNBSc$x >= min(NBScDf$NBScCMS) ))  
x2 <- max(which(densNBSc$x <  max(NBScDf$NBScCMS) ))
with(densNBSc, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="tan"))

pairs(NBScDf,pch=20,cex=0.5,col="blue",
      labels=c("Date","Precipitation","Evaporation","Runoff","cNBS"))
#
boxplot(NBScDf$precCMS ~ format(NBScDf$DateSeq, "%m"),
        names=unique(format(NBScDf$DateSeq, "%b")),
        ylab=expression(paste("Precipitation,   ",m^{3} %.% s^{-1})),
        par(mar=c(5,4.4,4,2)+0.1),las=1,
        main="Monthly Distribution of Overlake Precipitation over Lake Superior",
        cex.main=0.9)
#
boxplot(NBScDf$evapCMS ~ format(NBScDf$DateSeq, "%m"),
        names=unique(format(NBScDf$DateSeq, "%b")),
        ylab=expression(paste("Evaporation,   ",m^{3} %.% s^{-1})),
        par(mar=c(5,5,4,2)+0.1),
        main="Monthly Distribution of Lake Superior Evaporation",
        cex.main=0.9)
#
boxplot(NBScDf$rOffCMS ~ format(NBScDf$DateSeq, "%m"),
        names=unique(format(NBScDf$DateSeq, "%b")),
        ylab=expression(paste("Basin Runoff,   ",m^{3} %.% s^{-1})),
        par(mar=c(5,5,4,2)+0.1),
        main="Monthly Distribution of Basin Runoff to Lake Superior",
        cex.main=0.9)
#
boxplot(NBScDf$NBScCMS ~ format(NBScDf$DateSeq, "%m"),
        names=unique(format(NBScDf$DateSeq, "%b")),
        ylab=expression(paste("Net Basin Supply,   ",m^{3} %.% s^{-1})),
        par(mar=c(5,5,4,2)+0.1),
        main="Monthly Distribution of Components Estimate of Net Basin Supply for Lake Superior",
        cex.main=0.9)

