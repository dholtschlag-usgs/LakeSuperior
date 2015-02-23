# revStdMARSS.R

# Create augmented data frame with diversion inputs
source("Superior/src/work/setupMonthlyDataFrame.R")
#
library(MARSS)
# Setup fixed monthly component for model
# TT is the number of time steps
TT       <- 12*62  # 62 years of monthly data, max = 62 
# number of "seasons" (e.g., 12 months per year)
period   <- 12
# first "season" (e.g., Jan = 1, July = 7)
per.1st  <-  1
# create factors for seasons
c.in = diag(period)
for(i in 2:(ceiling(TT/period))) {c.in = cbind(c.in,diag(period))}
# trim c.in to correct start & length
c.in = c.in[,(1:TT)+(per.1st-1)]
# better row names
rownames(c.in) = month.abb
#


obsVec <- c("stmr","prec","evap","rOff","dSto")

obsMatrix <- t(as.matrix(NBSrcDf[1:TT,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))


###################################################
### Specify model structure
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# Z  <- matrix(list("z1",1,-1,1,1, 0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0, 0,0,0,0,1),5,5,byrow=TRUE)
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
B  = matrix(list("b11",0,0,0,0, 0,"b22",0,"b24",0, 0,0,"b33",0,0, 0,"b24",0,"b44",0, 
                 0,0,0,0,"b55"),5,5)
U  = "unequal"               # Unique u values
# U  = "zero"
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# R  = "diagonal and equal"    # All observation errors have the same variance
# Supercede to have different variances
# R  = "diagonal and unequal"
R  = matrix(list("r",0,0,0,0, 0,"10*r",0,0,0, 0,0,"10*r",0,0, 0,0,0,"5*r",0, 0,0,0,0,"5*r" ),5,5)
A  = "scaling"               # A set of scaling factors
# A  = "zero"
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
# D matrix sums diversions into Laker Superior to increment St. Marys flow
D  = matrix(c(1,1,0,0,0,0,0,0,0,0),5,2,byrow=TRUE)
# d  = "zero"                  # Data associated with obs inputs
d.in <- rbind(NBSrcDf$lLacCMS[1:TT],NBSrcDf$lOgoCMS[1:TT])
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
# model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d.in,V0=V0,tinitx=tinitx)
# 
# cntl.list  <- list(maxit=2500) # MCInit=TRUE,numInits=10,
# # s4o1Ar1SeaRun1.model = MARSS(obs,model=model.list,control=cntl.list)
# 
# start.time <- Sys.time()
# s5o5SeaAR1Dr <- MARSS(obsMatrix, method="BFGS",
#                      model=model.list, control=cntl.list)
# end.time   <- Sys.time()
# print(end.time - start.time)
# 
# 
# start.time <- Sys.time()
# s5o5SeaAR1Dr.ci    <- MARSSparamCIs(s5o5SeaAR1Dr)
# end.time   <- Sys.time()
# print(end.time - start.time)
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 149 iterations. 
# Log-likelihood: -35629.9 
# AIC: 71423.8   AICc: 71427.54   
# 
# ML.Est Std.Err low.CI up.CI
# R.r                      1.37e+05      NA     NA    NA
# B.b11                    1.00e+00      NA     NA    NA
# B.b22                    9.72e-01      NA     NA    NA
# B.b24                    2.83e-02      NA     NA    NA
# B.b33                    6.74e-01      NA     NA    NA
# B.b44                    7.92e-01      NA     NA    NA
# B.b55                   -4.22e-01      NA     NA    NA
# U.X.stmrCMS              1.84e-02      NA     NA    NA
# U.X.precCMS              4.07e-03      NA     NA    NA
# U.X.evapCMS              1.12e-03      NA     NA    NA
# U.X.rOffCMS              3.46e-03      NA     NA    NA
# U.X.dStoCMS             -1.10e-06      NA     NA    NA
# Q.(X.stmrCMS,X.stmrCMS)  3.25e-01      NA     NA    NA
# Q.(X.precCMS,X.precCMS)  1.72e+00      NA     NA    NA
# Q.(X.evapCMS,X.evapCMS)  9.89e+00      NA     NA    NA
# Q.(X.rOffCMS,X.rOffCMS)  4.34e+00      NA     NA    NA
# Q.(X.dStoCMS,X.dStoCMS)  1.68e+02      NA     NA    NA
# x0.X.stmrCMS             2.05e+03      NA     NA    NA
# x0.X.precCMS             2.12e+03      NA     NA    NA
# x0.X.evapCMS             4.06e+03      NA     NA    NA
# x0.X.rOffCMS             6.00e+02      NA     NA    NA
# x0.X.dStoCMS            -2.76e+03      NA     NA    NA
# C.(X.stmrCMS,Jan)        1.55e-03      NA     NA    NA
# C.(X.precCMS,Jan)        2.87e-04      NA     NA    NA
# C.(X.evapCMS,Jan)        3.41e-05      NA     NA    NA
# C.(X.rOffCMS,Jan)        2.69e-04      NA     NA    NA
# C.(X.dStoCMS,Jan)       -4.30e-05      NA     NA    NA
# C.(X.stmrCMS,Feb)        1.59e-03      NA     NA    NA
# C.(X.precCMS,Feb)        2.85e-04      NA     NA    NA
# C.(X.evapCMS,Feb)       -7.67e-05      NA     NA    NA
# C.(X.rOffCMS,Feb)        3.23e-04      NA     NA    NA
# C.(X.dStoCMS,Feb)        6.37e-05      NA     NA    NA
# C.(X.stmrCMS,Mar)        1.60e-03      NA     NA    NA
# C.(X.precCMS,Mar)        3.50e-04      NA     NA    NA
# C.(X.evapCMS,Mar)       -6.97e-05      NA     NA    NA
# C.(X.rOffCMS,Mar)        4.09e-04      NA     NA    NA
# C.(X.dStoCMS,Mar)        2.01e-04      NA     NA    NA
# C.(X.stmrCMS,Apr)        1.63e-03      NA     NA    NA
# C.(X.precCMS,Apr)        3.95e-04      NA     NA    NA
# C.(X.evapCMS,Apr)       -6.39e-05      NA     NA    NA
# C.(X.rOffCMS,Apr)        5.46e-04      NA     NA    NA
# C.(X.dStoCMS,Apr)        4.55e-04      NA     NA    NA
# C.(X.stmrCMS,May)        1.65e-03      NA     NA    NA
# C.(X.precCMS,May)        4.28e-04      NA     NA    NA
# C.(X.evapCMS,May)       -4.17e-05      NA     NA    NA
# C.(X.rOffCMS,May)        3.47e-04      NA     NA    NA
# C.(X.dStoCMS,May)        1.97e-04      NA     NA    NA
# C.(X.stmrCMS,Jun)        1.62e-03      NA     NA    NA
# C.(X.precCMS,Jun)        4.07e-04      NA     NA    NA
# C.(X.evapCMS,Jun)        9.79e-06      NA     NA    NA
# C.(X.rOffCMS,Jun)        1.56e-04      NA     NA    NA
# C.(X.dStoCMS,Jun)        1.47e-05      NA     NA    NA
# C.(X.stmrCMS,Jul)        1.62e-03      NA     NA    NA
# C.(X.precCMS,Jul)        3.67e-04      NA     NA    NA
# C.(X.evapCMS,Jul)        8.29e-05      NA     NA    NA
# C.(X.rOffCMS,Jul)        1.61e-04      NA     NA    NA
# C.(X.dStoCMS,Jul)       -9.43e-05      NA     NA    NA
# C.(X.stmrCMS,Aug)        1.58e-03      NA     NA    NA
# C.(X.precCMS,Aug)        3.60e-04      NA     NA    NA
# C.(X.evapCMS,Aug)        1.78e-04      NA     NA    NA
# C.(X.rOffCMS,Aug)        1.92e-04      NA     NA    NA
# C.(X.dStoCMS,Aug)       -1.56e-04      NA     NA    NA
# C.(X.stmrCMS,Sep)        1.48e-03      NA     NA    NA
# C.(X.precCMS,Sep)        3.50e-04      NA     NA    NA
# C.(X.evapCMS,Sep)        2.80e-04      NA     NA    NA
# C.(X.rOffCMS,Sep)        2.53e-04      NA     NA    NA
# C.(X.dStoCMS,Sep)       -1.71e-04      NA     NA    NA
# C.(X.stmrCMS,Oct)        1.40e-03      NA     NA    NA
# C.(X.precCMS,Oct)        2.93e-04      NA     NA    NA
# C.(X.evapCMS,Oct)        3.01e-04      NA     NA    NA
# C.(X.rOffCMS,Oct)        2.93e-04      NA     NA    NA
# C.(X.dStoCMS,Oct)       -1.40e-04      NA     NA    NA
# C.(X.stmrCMS,Nov)        1.37e-03      NA     NA    NA
# C.(X.precCMS,Nov)        2.77e-04      NA     NA    NA
# C.(X.evapCMS,Nov)        2.92e-04      NA     NA    NA
# C.(X.rOffCMS,Nov)        2.68e-04      NA     NA    NA
# C.(X.dStoCMS,Nov)       -1.46e-04      NA     NA    NA
# C.(X.stmrCMS,Dec)        1.31e-03      NA     NA    NA
# C.(X.precCMS,Dec)        2.70e-04      NA     NA    NA
# C.(X.evapCMS,Dec)        1.89e-04      NA     NA    NA
# C.(X.rOffCMS,Dec)        2.40e-04      NA     NA    NA
# C.(X.dStoCMS,Dec)       -1.82e-04      NA     NA    NA


