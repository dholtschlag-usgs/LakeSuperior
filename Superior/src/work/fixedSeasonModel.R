# MARSS model with time-varying parameters
#
# Clear workspace
rm(list=ls())
# Setup Lake Superior Data
pName     <- getwd()
fName     <- "/Superior/src/work/setupMonthlyDataFrame.R"
fullName  <- paste(pName,fName,sep="")
# This reads i
source(fullName)
#
library(MARSS)
# Budget Components
waterBudget <- c("stmr","prec","evap","rOff","dSto")

TT        <- 12*62  # 62 years of monthly data, max = 62 
NBSrcMtrx <- as.matrix(NBSrcDf[1:TT,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")])

dat       <- t(NBSrcMtrx)
#
# z.score data because we changed the mean when we subsampled
the.mean = apply(dat,1,mean,na.rm=TRUE)
the.sigma = sqrt(apply(dat,1,var,na.rm=TRUE))
# Replace original data with z-scores
# dat = (dat-the.mean)*(1/the.sigma)
# Replace original time series with demeaned values
dat <- (dat-the.mean)
# number of time periods/samples
TT = dim(dat)[2]
###################################################
### code chunk number 12: Covar_sec6_02_set-up-month-factors
###################################################
# number of "seasons" (e.g., 12 months per year)
period = 12
# first "season" (e.g., Jan = 1, July = 7)
per.1st = 1
# create factors for seasons
c.in = diag(period)
for(i in 2:(ceiling(TT/period))) {c.in = cbind(c.in,diag(period))}
# trim c.in to correct start & length
c.in = c.in[,(1:TT)+(per.1st-1)]
# better row names
rownames(c.in) = month.abb
#
C = matrix(month.abb,5,12,byrow=TRUE)
C

###################################################
### code chunk number 14: Covar_sec6_04_C-constrained2
###################################################
C = "unconstrained"

###################################################
### code chunk number 15: Covar_sec6_05_month-factor-marss-params
###################################################
# Each taxon has unique density-dependence
B = "diagonal and unequal"
# B = "identity"
# Assume independent process errors
Q = "diagonal and unequal"
# We have demeaned the data & are fitting a mean-reverting model
# by estimating a diagonal B, thus
# U = matrix(c(rowMeans(dat)),5,1)
# U = "unconstrained"
U = "zero"
# Each obs time series is associated with only one process
Z = "identity" 
# The data are demeaned & fluctuate around a mean
A = "zero" 
# We assume observation errors are independent, but they
# have similar variance due to similar collection methods
R = "diagonal and unequal"
# We are not including covariate effects in the obs equation
D = "zero"
d = "zero"
# Compute model
model.list = list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,C=C,c=c.in,D=D,d=d)
fixedSeasAR1diagRdiag.model = MARSS(dat,model=model.list,
                               control=list(maxit=2000))
# Compute parameter uncertainities
fixedSeasAR1diagRdiag.ParCI <- MARSSparamCIs(fixedSeasAR1diagRdiag.model)

# Plot the C matrix
source("Superior/src/work/plotCmatrixCI.R")
plotCmatrixCI.R(fixedSeasAR1diagRdiag.ParCI)


# Get the estimated seasonal effects
# rows are taxa, cols are seasonal effects
seas.1 = coef(seas.mod.1,type="matrix")$C
rownames(seas.1) = waterBudget
colnames(seas.1) = month.abb
#

matplot(t(seas.1),type="l",bty="n",xaxt="n", ylab="Fixed monthly", col=1:5)
axis(1,labels=month.abb, at=1:12,las=1,cex.axis=0.75)
legend("topright", lty=1:5, legend=waterBudget, cex=0.8, col=1:5)
abline(h=0,col="grey",lty="dotdash")

parmCIinfo <- MARSSparamCIs(seas.mod.1, method = "hessian", alpha = 0.05, nboot=1000)

loCI_Seas  <- parmCIinfo$par.lowCI$U;
hiCI_Seas  <- parmCIinfo$par.upCI$U
mean_Seas  <- coef(parmCIinfo)$C

plot(1:12,mean_Seas[1:12],col="black",type="s",lty="solid",xaxt="n",
     ylim=c(-1.5,1))
axis(1,labels=month.abb, at=seq(1,12,by=1))

lines(1:12,loCI_Seas[1:12],col="grey",type="s",lty="dashed")
lines(1:12,hiCI_Seas[1:12],col="grey",type="s",lty="dashed")

library(ggplot2)
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# dfc <- summarySE(df, measurevar="len", groupvars=c("supp","dose"))
# supp dose  N   len       sd        se       ci
#   OJ  0.5 10 13.23 4.459709 1.4102837 3.190283
#   OJ  1.0 10 22.70 3.910953 1.2367520 2.797727
#   OJ  2.0 10 26.06 2.655058 0.8396031 1.899314
#   VC  0.5 10  7.98 2.746634 0.8685620 1.964824
#   VC  1.0 10 16.77 2.515309 0.7954104 1.799343
#   VC  2.0 10 26.14 4.797731 1.5171757 3.432090

# pd <- position_dodge(.1)  # move them .05 to the left and right
# Use 95% confidence interval instead of SEM
# ggplot(dfc, aes(x=dose, y=len, colour=supp)) + 
#   geom_errorbar(aes(ymin=len-ci, ymax=len+ci), width=.1, position=pd) +
#   geom_line(position=pd) +
#   geom_point(position=pd)





# Component NBS Eqn:
# NBS = Precipitation + Runoff - Evaporation
# Residual NBS Eqn:
# NBS = ChangeInStorage - Inflow + Outflow
# Water budget for Lake Superior
# Outflow = Prec + Runoff - Evap - ChangeInStorage + Div
#    stmr = prec + rOff   - evap - dSto            + divr  
TT   <- 696                   # 58 years of monthly data
stmr <- NBSrcDf$stmrCMS[1:TT] 
prec <- NBSrcDf$precCMS[1:TT] 
rOff <- NBSrcDf$rOffCMS[1:TT] 
evap <- NBSrcDf$evapCMS[1:TT] 
dSto <- NBSrcDf$dStoCMS[1:TT] 
divr <- NBSrcDf$divrCMS[1:TT] 
#
# Load MARSS library
library(MARSS)
#
# Estimate linear model 
lm1     <- lm(stmr ~ prec + rOff + evap + dSto + divr)
summary(lm1)
# Linear model without intercept
lm2     <- lm(stmr ~ prec + rOff + evap + dSto + divr -1)
summary(lm2)
plot(stmr,predict(lm2))
#
# Estimate linear model with MARSS
# (1) z-score the response variable stmr
stmrMean <- mean(stmr)
stmrStd  <- sd(stmr)
zStmr    <- t( (stmr - stmrMean)/stmrStd )
# (2) z-score the explanatory variables
covariates <- rbind(prec,rOff,divr,evap,dSto)
the.mean   <- apply(covariates,1,mean,na.rm=TRUE)
the.sd     <- apply(covariates,1,sd  ,na.rm=TRUE)
zCovariates<- (covariates - the.mean) * 1/the.sd
#
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
kem <- MARSS(y, model=model.list)
# Compute standard errors of parameters
MARSSparamCIs(kem)
# Set up linear regression model on z-scores of 
tzCov <- t(zCovariates)
lm3 <- lm(t(y) ~ (tzCov -1))
summary(lm3)
acf(lm3$residuals,main="Autocorrelation in Regression Residuals")
## 
# MARSS regression with autocorrelated errors
#   x(t) = B x(t-1) + w(t), wt ~ N(0,Q)
#   y(t) = D(t) d(t) + x(t)
# MARSS model specifications
Q <- "unconstrained"
B <- "unconstrained"
A <- "zero"; U <- "zero"; x0 = "zero"
R <- "unconstrained"
d <- zCovariates;
D <- "unconstrained"
y <- zStmr # show relation between data & the equations
model.list   <- list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,D=D,d=d,x0=x0)
control.list <- list(maxit=5000)
kemAR2       <- MARSS(y, model=model.list, contro=control.list)
#
# ACF the state residual series (w(t))
acf(c(residuals(kemAR2)$model.residuals),
    main="Autocorrelation of the Model Residuals for AR Multiple Regression Model",
    xlab="Lag, in months")

kf.out <- MARSSkfss(kemAR2)
plot(zStmr,c(kf.out$xtT),pch=20,col="steelblue",
     main="Meaursed and Smoothed AR Regression Estimates of St. Marys River Flow",
     xlab="Z-Scores of Monthly Flow in St. Marys River, cubic meters per second",
     ylab="Z-Scores of Smoothed Monthly Flow Estimates for St. Marys River, m^3",
     cex = 0.60)
abline(0,1,col="salmon",lty="dashed",lwd=2)
#
## Process error only model
#  x(t) = x(t-1) + C c(t) + w(t), w(t) ~ N(0,Q)
R = A = U = "zero"
B = Z = "identity"
Q = "unconstrained"
C = "unconstrained"
x = zStmr # show relation between data & the equations
model.list <- list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,C=C,c=zCovariates)
kemStateModel <- MARSS(x, model=model.list)
MARSSparamCIs(kemStateModel)

#
# Process error model only with AR component
B <- "unconstrained"
model.list <- list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,C=C,c=zCovariates)
kemARStateModel <- MARSS(x, model=model.list)
MARSSparamCIs(kemARStateModel)
#
# Both Process and Obseration error Model
#  x(t) = B x(t-1) + C c(t) + w(t), w(t) ~ N(0,Q)
#  y(t) = x(t) + v(t), v(t)~N(0,R)
R = diag(0.002,1);  # Guess at the uncertainty in stmr flow (z-score units)   
A = U = "zero"
B = "unconstrained"
Z = "identity"
Q = "unconstrained"
C = "unconstrained"
x = zStmr # show relation between data & the equations
model.list <- list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,C=C,c=zCovariates)
kemARStateObsModel <- MARSS(x, model=model.list)
#
MARSSparamCIs(kemARStateObsModel)
#
################################################################
## Set up seasonal model with fixed factors (MARSSv3.9, p. 157)
# Create the c matrix
# number of "seasons" (e.g., 12 months per year)
period = 12
# first "season" (e.g., Jan = 1, July = 7)
per.1st = 1
# create factors for seasons
cin = diag(period)
for(i in 2:(ceiling(TT/period))) {cin = cbind(cin,diag(period))}
# trim c.in to correct start & length
cin = cin[,(1:TT)+(per.1st-1)]
# better row names
rownames(cin) = month.abb
#
# AR regression model with seasonal affects
# x(t) = B x(t-1) + C c(t) + w(t), w(t)~N(0,Q)
# y(t) = Z x(t  ) + D d(t) + v(t), v(t)~N(0,R)
#
B = "diagonal and unequal"
# Assume independent process errors
Q = "diagonal and unequal"
# We have demeaned the data & are fitting a mean-reverting model
# by estimating a diagonal B, thus
U = "zero"
# Each obs time series is associated with only one process
Z = "identity" 
# The data are demeaned & fluctuate around a mean
A = "zero" 
# We assume observation errors are independent, but they
# have similar variance due to similar collection methods
R = "diagonal and equal"
# We are not including covariate effects in the obs equation
D = "zero"
d = "zero"
C = "unconstrained"
rownames(vecDat) <- c("stmr","prec","rOff","divr","evap","dSto")
y = vecDat
model.list = list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,C=C,c=cin,D=D,d=d)
seaMARSS   <- MARSS(y,model.list,control=list(maxit=1500))

MARSSparamCIs(seaMARSS, method = "hessian", alpha = 0.05, nboot=1000)


