# Uncertainty Estimation
# Runoff to Lake Superior
#
# Clear workspace
rm(list=ls())
# read Net Basin Supply with residual and components Data frame (NBSrcLevelDf)
pName      <- getwd()
fName      <- "/Superior/data/Monthly/SUPALL_PercentGaged_MonthlyTable.txt"
fullName   <- paste(pName,fName,sep="")
SUPGagedRODf    <- read.table(file=fullName,header=TRUE,sep="",
                              stringsAsFactors = FALSE, comment.char="#")
# Create beginning and ending date 
dateBeg     <- as.Date(paste(SUPGagedRODf$Year[1],'-01-15',sep="",format="%Y-%m-%d"))
dateEnd     <- as.Date(paste(tail(SUPGagedRODf$Year,1),'-12-15',sep=""),
                       format="%Y-%m-%d")
DateSeq     <- seq(dateBeg, dateEnd, by="month")
# Convert data frame LSupBOMLevelM to nrow X 1 matrix
LSupGageROmat  <- matrix(unlist(t(SUPGagedRODf[,2:13])),nrow=nrow(SUPGagedRODf)*12,
                      ncol=1,byrow=FALSE)
LSupGageRODf           <- data.frame(DateSeq,LSupGageROmat)
colnames(LSupGageRODf) <- c("DateSeq","PrctROGaged")
#
plot(LSupGageRODf$DateSeq,LSupGageRODf$PrctROGaged,type="l",col="blue",
     xlab="Year",ylab="Percent of Basin Runoff Gaged",
     main="Trend in the Percent of the Lake Superior Basin where Runoff is Gaged")
