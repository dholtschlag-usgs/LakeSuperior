# MARSS model with time-varying parameters
#
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

