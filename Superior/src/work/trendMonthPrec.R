
# Set path to target directory
setwd("C:/Home/Projects/GLWaterBudget/Analysis/Superior")



# Creat data frame for analysis
if (!exists("NBSrcDf")) {
  # source script to set up data files
  source("Superior/src/work/setupMonthlyDataFrame.R")
}

# Create observation matrix
obsMatrix <- t(as.matrix(NBSrcDf[,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

# Load the MARSS library
library(MARSS)

AICmatrix <- matrix(NA,12,6)
rownames(AICmatrix) <- month.abb
colnames(AICmatrix) <- c("Level","Linear trend","Stochastic level","Stochastic trend",
                         "Step trend 1998","Step 1973 & 1998")

dLevelxtT <- matrix(NA,12,63)
dTrendxtT <- matrix(NA,12,63)
sLevelxtT <- matrix(NA,12,63)
sTrendxtT <- matrix(NA,12,63)
step98xtT <- matrix(NA,12,63)
step73xtT <- matrix(NA,12,63)

rownames(dLevelxtT) <- month.abb
rownames(dTrendxtT) <- month.abb
rownames(sLevelxtT) <- month.abb
rownames(sTrendxtT) <- month.abb
rownames(step98xtT) <- month.abb
rownames(step73xtT) <- month.abb


# Select the unique month 2-digit codes from the data frame 
month.chr <- unique(NBSrcDf$Month)

# Create 3 x 4 plot frame
par(mfrow=c(3,4),mar=c(2,4.5,2,.35)+0.1)
for (i in 1:12){
  # Select by month
  
  stmrCMSmon <- subset(NBSrcDf, Month == month.chr[i])
  
  plot(stmrCMSmon$DateSeq,stmrCMSmon$precCMS,
       ylab=expression(paste("Precipitation,  ", m^3%.%s^-1)),
       main=month.name[as.numeric(month.chr[i])],
       col="blue", pch=20, xlab="Year")
  
  
  # Deterministic level
  mod.stmr.0 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                     B=matrix(1), U=matrix(0), Q=matrix(0),
                     x0=matrix("a"))
  
  dat       <- t(stmrCMSmon$precCMS)
  kem.0em   <- MARSS(dat, model=mod.stmr.0) 
  summary(kem.0em)
  
  kf0       <- MARSSkf(kem.0em)
  
  lines(stmrCMSmon$DateSeq,kf0$xtT,lty="dashed",col="red")
  
  # Deterministics slope
  mod.stmr.1 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                     B=matrix(1), U=matrix("u"), Q=matrix(0),
                     x0=matrix("a"))
  
  kem.1em   <- MARSS(dat, model=mod.stmr.1, silent=TRUE) 
  summary(kem.1em)
  # Compute linear trend
  kf1       <- MARSSkf(kem.1em)
  # Plot linear trend line
  lines(stmrCMSmon$DateSeq,kf1$xtT,lty="solid",col="black")
  
  
  
  # Stochastic level
  mod.stmr.2 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                     B=matrix(1), U=matrix(0), Q=matrix("Q"),
                     x0=matrix("pi"))
  
  kem.2em   <- MARSS(dat, model=mod.stmr.2, silent=TRUE) 
  summary(kem.2em)
  # Plot stochastic level
  kf2       <- MARSSkf(kem.2em)
  
  lines(stmrCMSmon$DateSeq,kf2$xtT,lty="solid",col="green")
  
  # Stochastic trend
  mod.stmr.3 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                     B=matrix(1), U=matrix("u"), Q=matrix("Q"),
                     x0=matrix("pi"))
  
  kem.3em   <- MARSS(dat, model=mod.stmr.3, silent=TRUE) 
  summary(kem.3em)
  # Plot stochastic level
  kf3       <- MARSSkf(kem.3em)
  
  lines(stmrCMSmon$DateSeq,kf3$xtT,lty="solid",col="orange")
  
  # How about a structural break in 1998
  n    <- 13
  c.row1 <- matrix( rep(1,63), 1, 63)
  c.row2 <- matrix( c(rep(0,63-n),rep(1,n)), 1, 63)
  c.in   <- rbind(c.row1,c.row2)
  
  C    <- matrix("Step") 
  
  C    <- matrix( c("Early","Late"), 1, 2)
  
  # Deterministic level
  mod.stmr.0 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                     B=matrix(0), U=matrix(0), Q=matrix(0),
                     x0=matrix(0), C=C, c=c.in)
  
  kem.4em   <- MARSS(dat, model=mod.stmr.0) 
  summary(kem.0em)
  
  kf4       <- MARSSkf(kem.4em)
  
  kfss4     <- MARSSkfss(kem.4em)
  
  lines(stmrCMSmon$DateSeq,kf4$xtT,lty="solid",col="magenta")
  
  ####
  
  # How about a structural break in 1972 and 1998
  m      <- 38
  c.row1 <- matrix( rep(1,63), 1, 63)
  c.row2 <- matrix( c(rep(0,63-m),rep(1,m)), 1, 63)
  c.row3 <- matrix( c(rep(0,63-n),rep(1,n)), 1, 63)
  c.in   <- rbind(c.row1,c.row2,c.row3)
  
  C    <- matrix( c("Early","Middle","Late"), 1, 3)
  
  # Deterministic level
  mod.stmr.0 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                     B=matrix(0), U=matrix(0), Q=matrix(0),
                     x0=matrix(0), C=C, c=c.in)
  
  kem.5em   <- MARSS(dat, model=mod.stmr.0) 
  summary(kem.5em)
  
  kf5       <- MARSSkf(kem.5em)
  
  kfss5     <- MARSSkfss(kem.5em)
  
  lines(stmrCMSmon$DateSeq,kf5$xtT,lty="solid",col="brown")

  # AIC matrix containing statistics for trends
  AICmatrix[i,] <- c(kem.0em$AICc,kem.1em$AICc,kem.2em$AICc,kem.3em$AICc,
                     kem.4em$AICc,kem.5em$AICc)
  
  dLevelxtT[i,] <- kf0$xtT
  dTrendxtT[i,] <- kf1$xtT
  sLevelxtT[i,] <- kf2$xtT
  sTrendxtT[i,] <- kf3$xtT
  step98xtT[i,] <- kf4$xtT
  step73xtT[i,] <- kf5$xtT
}
trndAll <- c(dLevelxtT,dTrendxtT,sLevelxtT,sTrendxtT,step98xtT,step73xtT)
# Plot the trend in monthly Precipitation for the minimum AIC
colorMonth <- c(1:6,9:14)
par(mfrow=c(1,1),mar=c(4,5,2,2)+0.1)
plot(stmrCMSmon$DateSeq,seq(1,63),type="n",
     ylim=c(min(trndAll), max(trndAll)),
     ylab=expression(paste("Precipitation, in  ",m^3%.% s^-1)),
     xlab="Year",
     main="Monthly Trends in Overlake Precipitation on Lake Superior")

for (i in 1:12) {
  ndxMin <- which.min(AICmatrix[i,])
  # Select the trend component with the minimum AICc 
  switch(ndxMin,
  '1' = {trnd <- dLevelxtT[i,]},
  '2' = {trnd <- dTrendxtT[i,]},
  '3' = {trnd <- sLevelxtT[i,]},
  '4' = {trnd <- sTrendxtT[i,]},
  '5' = {trnd <- step98xtT[i,]},
  '6' = {trnd <- step73xtT[i,]}
  )
  lines(stmrCMSmon$DateSeq,trnd,lty="solid",col=colorMonth[i])
  text(stmrCMSmon$DateSeq[4*(i-1)+2], trnd[4*(i-1)+1]+40, label=month.abb[i],
       col=colorMonth[i], cex=0.7)
}

