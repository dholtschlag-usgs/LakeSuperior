
# Read monthly data files and merge data by dates to common dataframe
## MARSS model with simple linear regression
#
# Setup monthly data frame
# generate components dataframe
# Clear variables
rm(list=ls())
library(MASS)
#
pName     <- getwd()
fName     <- "/Superior/src/work/setupMonthlyDataFrame.R"
fullName  <- paste(pName,fName,sep="")
source(fullName)
rm(list=setdiff(ls(),"NBSrcDf"))
#
# Load MARSS library
library(MARSS)
#
densStmr <- density(NBSrcDf$stmrCMS)
plot(densStmr,xlab=expression(paste("Monthly Flow, in ", m^{3} %.% s^{-1})),
     sub ="Kernel Density Estimation with the density {stats} Function",
     main="Probability Density of Lake Superior Monthly Outflows Through St. Marys River",
     cex.main=0.95)
x1 <- min(which(densStmr$x >= 1000))  
x2 <- max(which(densStmr$x <  4000))
with(densStmr, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="tan"))
#
## Regression Model with lm and MARSS
# Component NBS Eqn:
# NBS = Precipitation + Runoff - Evaporation
# Residual NBS Eqn:
# NBS = ChangeInStorage - Inflow + Outflow
# Water budget for Lake Superior
# Outflow = Prec + Runoff - Evap - ChangeInStorage + Div
#    stmr = prec + rOff   - evap - dSto            + divr  

# Taking subset to develop model, remaining 5 years are for testing
TT         <- 696                    # 58 years of monthly data
stmr       <- NBSrcDf$stmrCMS[1:TT] 
prec       <- NBSrcDf$precCMS[1:TT] 
rOff       <- NBSrcDf$rOffCMS[1:TT] 
evap       <- NBSrcDf$evapCMS[1:TT] 
dSto       <- NBSrcDf$dStoCMS[1:TT] 
divr       <- NBSrcDf$divrCMS[1:TT]
dTim       <- as.numeric(NBSrcDf$DateSeq[1:TT])
covariates <- rbind(prec,rOff,divr,evap,dSto)
#
yearBeg <- NBSrcDf$DateSeq[1]
yearEnd <- NBSrcDf$DateSeq[TT]
## Estimate linear model 
linearRegModel     <- lm(stmr ~ prec + rOff + evap + dSto + divr)
summary(linearRegModel)
plot(stmr,predict(linearRegModel),pch=20,col="blue",cex=0.8,
     xlab=expression("Measured Average Monthly Flow in St. Marys River Flow, in" ~ m^{3}~s^{-1}),
     ylab=expression("Estimated Average Monthly Flow, in " ~m^{3}~s^{-1}),
     main=paste("Measured and Linear Regression Estimates of Monthly Flow\n", 
                "of St. Marys River from ",yearBeg,' to ',yearEnd,sep=""),cex.main=0.8)
abline(0,1,col="red",lty="dashed")
#
plot(stmr,residuals(linearRegModel),pch=20,col="blue",cex=0.8,
     xlab=expression("Measured Average Monthly Flow in St. Marys River Flow, in" ~ m^{3}~s^{-1}),
     ylab=expression("Residuals of Average Monthly Flow, in   " ~m^{3}~s^{-1}),
     main=paste("Residuals of Measured and Linear Regression Estimates of Monthly Flow\n", 
                "with the Magnitude of St. Marys River from ",yearBeg,' to ',yearEnd,sep=""),
     cex.main=0.8)
abline(h=0,col="red",lty="dashed")
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
# Assessing possible trend in residuals 
plot(NBSrcDf$DateSeq[1:TT],residuals(linearRegModel),pch=20,col=colorPnt,
     xlab="Year",
     ylab=expression("Residuals of Average Monthly Flow, in   " ~m^{3}~s^{-1}),
     main="Residuals of Measured and Linear Regression Estimates of Monthly Flow with Time",
     cex.main=0.8)
abline(h=0,col="red",lty="dashed")
legend("topleft",legend=month.abb,col=colorPnt,pch=20,cex=0.6)
#
# Assessing possible monthly seasonality in residuals
boxplot(residuals(linearRegModel)~monthNum, notch=TRUE,
        col=colorSet,names=month.abb,
        main="Residuals of Linear Regression of St. Marys River Flow by Month",
        ylab=expression("Residuals of Average Monthly Flow, in   " ~m^{3}~s^{-1}),
        xlab="Month")
abline(h=0,col="red",lty="dashed")
#
# Fit a robust linear regression to assess possible improvement
rLinearRegModel     <- rlm(stmr ~ prec + rOff + evap + dSto + divr)
summary(rLinearRegModel)
plot(stmr,predict(rLinearRegModel),pch=20,col="blue",cex=0.8,
     xlab=expression("Measured Average Monthly Flow in St. Marys River Flow, in" ~ m^{3}~s^{-1}),
     ylab=expression("Estimated Average Monthly Flow, in " ~m^{3}~s^{-1}),
     main=paste("Measured and Linear Regression Estimates of Monthly Flow\n", 
                "of St. Marys River from ",yearBeg,' to ',yearEnd,sep=""),cex.main=0.8)
abline(0,1,col="red",lty="dashed")
#
plot(stmr,residuals(rLinearRegModel),pch=20,col=colorPnt,cex=0.8,
     xlab=expression("Measured Average Monthly Flow in St. Marys River Flow, in" ~ m^{3}~s^{-1}),
     ylab=expression("Residuals of Average Monthly Flow, in   " ~m^{3}~s^{-1}),
     main=paste("Residuals of Measured and Linear Regression Estimates of Monthly Flow\n", 
                "of St. Marys River from ",yearBeg,' to ',yearEnd,sep=""),
     cex.main=0.8)
abline(h=0,col="red",lty="dashed")
#
## MARSS Observation model only with data in original units
# Fix the state part of the model
Q <- x0 <- "zero" 
U <- "zero"
B <- Z <- "identity"
d <- covariates
A <- "unconstrained"
D <- "unconstrained"
y <- stmr # show relation between data & the equations
#
# Model specification
model.list <- list(B=B,U=U,Q=Q,Z=Z,A=A,D=D,d=d,x0=x0)
marssRegModel <- MARSS(y, model=model.list,control=list(conv.test.slope.tol=0.05));
# Compute standard errors of parameters
MARSSparamCIs(marssRegModel)
#
# Compare linear regression and MARSS model predictions
prec <- NBSrcDf$precCMS[1:TT] 
rOff <- NBSrcDf$rOffCMS[1:TT] 
evap <- NBSrcDf$evapCMS[1:TT] 
dSto <- NBSrcDf$dStoCMS[1:TT] 
divr <- NBSrcDf$divrCMS[1:TT] 

origData <- subset(NBSrcDf[1:TT,],select=c("precCMS","rOffCMS","evapCMS","dStoCMS","divrCMS"))
colnames(origData) <- c("prec","rOff","evap","dSto","divr")
#
# Develop both models based on zscores of variables
# (1) z-score the response variable stmr
stmrMean <- mean(stmr)
stmrStd  <- sd(stmr)
zStmr    <- t( (stmr - stmrMean)/stmrStd )
# (2) z-score the explanatory variables
the.mean   <- apply(covariates,1,mean,na.rm=TRUE)
the.sd     <- apply(covariates,1,sd  ,na.rm=TRUE)
zCovariates<- (covariates - the.mean) * 1/the.sd
#
#
## Linear model with z-scored variated without intercept
linearRegModelz     <- lm(t(zStmr) ~ 0 + t(zCovariates))
summary(linearRegModelz)
plot(zStmr,predict(linearRegModelz),
     xlab=expression("z-Scores of Monthly Flows of St. Marys River"),
     ylab=expression("z-Scores of Linear Regression Estimates"),
     pch=20, cex=0.8, col="blue",
     main=expression("Relation between Measured and Estimated Monthly Flows of St. Marys River"))
abline(0,1,col="red",lty="dashed")
#
plot(zStmr,residuals(linearRegModelz),
     xlab="z-Scores of Monthly Flows of St. Marys River",
     ylab="z-Scores of Linear Regression Residuals",
     pch=20, cex=0.8, col="blue",
     main="Relation between Measured and Residuals of Monthly Flow Estimates of St. Marys River",
                     cex.main=0.9)
abline(h=0,col="red",lty="dashed")


# Estimate linear model with MARSS

# Fix the state part of the model
Q <- U <- x0 <- "zero" 
B <- Z <- "identity"
d <- zCovariates
A <- "zero"
D <- "unconstrained"
y <- zStmr # show relation between data & the equations
#
# Model specification
model.list <- list(B=B,U=U,Q=Q,Z=Z,A=A,D=D,d=d,x0=x0)
marssRegModelz <- MARSS(y, model=model.list)
# Compute standard errors of parameters
MARSSparamCIs(marssRegModelz)
#
# ACF of model residuals
acf(linearRegModelz$residuals,main="Autocorrelation in Regression Residuals")
# 


