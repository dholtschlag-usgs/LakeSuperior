# Read in data on monthly diversions to Lake Superior from Ogoki and Long Lac.
#
# Clear workspace
rm(list=ls())
# read merged data file
pName          <- getwd()
fName          <- "/Superior/data/Monthly/SupDiversionLongLac.txt"
fullName       <- paste(pName,fName,sep="")
DivLongLac     <- read.table(file=fullName, header=FALSE, sep="",
                           comment.char="#",col.names=c("Year",month.abb))
DivLongLacMat  <- as.matrix(DivLongLac[,2:13])

DivLongLacFlow <- as.vector(matrix(t(DivLongLacMat),912,1,byrow=TRUE))

DivLongLacDate <- seq(as.Date(paste(DivLongLac$Year[1],'-01-15',sep="")),
                     as.Date(paste(tail(DivLongLac$Year,n=1),'-12-15',sep="")),
                             by = "month")
DivLongLacDF   <- data.frame(DivLongLacDate,DivLongLacFlow)
colnames(DivLongLacDF) <- c("Date","FlowCMS")
# Replace -9999 missing data indicator with NA
ndxNA     <- which(DivLongLacDF$FlowCMS==-9999)
DivLongLacDF$FlowCMS[ndxNA] <- NA
plot(DivLongLacDF$Date,DivLongLacDF$FlowCMS,pch=20,cex=0.5,col="blue",
     main="Diversions into Lake Superior From Long Lac",
     xlab="Date",ylab="Flow, in cubic meter per second")
##
# Monthly Lake Ogoki River Diversions into Lake Superior
pName          <- getwd()
fName          <- "/Superior/data/Monthly/SupDiversionOgokim.txt"
fullName       <- paste(pName,fName,sep="")
DivOgokim      <- read.table(file=fullName, header=FALSE, sep="",
                             comment.char="#",col.names=c("Year",month.abb))
DivOgokimMat   <- as.matrix(DivOgokim[,2:13])
dimOgokimMat   <- dim(DivOgokimMat)
DivOgokimFlow  <- as.vector(matrix(t(DivOgokimMat),dimOgokimMat[1]*dimOgokimMat[2],
                                     1,byrow=TRUE))

DivOgokimDate  <- seq(as.Date(paste(DivOgokim$Year[1],'-01-15',sep="")),
                      as.Date(paste(tail(DivOgokim$Year,n=1),'-12-15',sep="")),
                      by = "month")
DivLongLacDF   <- data.frame(DivOgokimDate,DivOgokimFlow)
colnames(DivLongLacDF) <- c("Date","FlowCMS")
# Replace -9999 missing data indicator with NA
ndxNA     <- which(DivLongLacDF$FlowCMS==-9999)
DivLongLacDF$FlowCMS[ndxNA] <- NA
plot(DivLongLacDF$Date,DivLongLacDF$FlowCMS,pch=20,cex=0.5,col="blue",
     main="Diversions into Lake Superior From Ogoki River",
     xlab="Date",ylab="Flow, in cubic meter per second")
#
