# Analysis to describe the uncertainty of monthly flows from daily flows
#
# Read in daily values from Superior/data/Daily/Qd04116500.txt
pName       <- 'Superior/data/Daily/Qd04116500.txt'
dvTable     <- read.table(pName,sep="\t", header=TRUE,comment.char="#",
                          stringsAsFactors = FALSE)
dvTable$datetime    <- as.Date(dvTable$datetime)
# Drop first two columns for agency and gage
dvTable <- dvTable[-c(1:2)]
# Rename columns 
colnames(dvTable) <- c("datetime","flow","flow_cd")
# Create log flow
dvTable$lFlow     <- log(dvTable$flow)
# load zoo to get function yearmon
library(zoo)
dvTable$yearmmm <- as.yearmon(dvTable$datetime)
#
# Get the unique year-mo and the number of days
uniqYearMo <- unique(dvTable$yearmmm)
# Initialize data frame for stats
yearMoStats1160 <- data.frame(uniqYearMo)
yearMoStats1160$lFlowMean    <- 0;
yearMoStats1160$lFlowVarObs  <- 0;
yearMoStats1160$lFlowVarMean <- 0;
#
# allocate matrix to compute autocorrelation coefficients
acfMatrix <- matrix(NA,length(uniqYearMo),25)
# 
for (i in 1:length(uniqYearMo)){
  tmpVec <- subset(dvTable, select="lFlow", yearmmm==uniqYearMo[i])
  tmpAcf <- acf(tmpVec,lag.max = 25, plot=FALSE)
  acfMatrix[i,] <- tmpAcf$acf[2:26]
}
# Compute mean ACF at daily lags
acfDayMean <- colMeans(acfMatrix)
#    return upper quantiles of ACF for all lags
acfDayMax  <- apply(acfMatrix,2,quantile,probs=0.9)
#    return lower quantiles of ACF for all lags
acfDayMin  <- apply(acfMatrix,2,quantile,probs=0.1)
#
# Plot ACF with quantiles
plot(1:25,acfDayMean,ylim=c(-0.5,1),pch=20,col="blue",
     main="Autocorrelation Function of daily flows used to computed monthly mean",
     xlab="Lag, in days",ylab="Estimated ACF")
points(1:25,acfDayMax,pch=6,col="blue");
points(1:25,acfDayMin,pch=2,col="blue")
abline(h=0,col="red",lty="dashed")
legend("topright",legend=c("Expected","90% limit","10% limit"),
       col=c("blue","blue","blue"),pch=c(20,6,2))
# 
# Create dataframe containing unique 


# Compute mean from daily values and compare with monthly means
#
# Variance inflation for the standard error of the mean due to autocorrelation
varInflaACF <- function(nDays,acfVec){
  result    <- 1 + 2/nDays * (seq(nDays-1,nDays-length(acfVec),by=-1) %*% acfVec)
  return(result)
}
% varInflaACF <- 1 + 2/30 * (seq(29,25,by=-1) %*% acfDayMean[1:5])
# Store Year-Mon means, raw variances, and acf-adjusted variance
for (i in 1:length(uniqYearMo)){
  tmpVec <- subset(dvTable, select="lFlow", yearmmm==uniqYearMo[i])
  yearMoStats1160$lFlowMean[i]    <- mean(tmpVec$lFlow)
  yearMoStats1160$lFlowVarObs[i]  <- var(tmpVec$lFlow)
  yearMoStats1160$lFlowVarMean[i] <- yearMoStats1160$lFlowVarObs[i]/length(tmpVec$lFlow) *
    varInflaACF(nDays=length(tmpVec$lFlow),acfVec=acfDayMean[1:5])  
}
#
# Plot 
plot(yearMoStats1160$lFlowMean,sqrt(yearMoStats1160$lFlowVarObs),pch=20,col="blue")
points(yearMoStats1160$lFlowMean,sqrt(yearMoStats1160$lFlowVarMean),pch=20,col="red")

plot(sqrt(yearMoStats1160$lFlowVarObs),sqrt(yearMoStats1160$lFlowVarMean),pch=20,col="blue")
abline(0,1,col="red",lty="dashed")
subset(dvtable, select=flow)

# Compute acf from daily values for each month

# Composite ACF