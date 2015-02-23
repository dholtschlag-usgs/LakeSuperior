# MARSS development for Lake Superior Water Budget
library(MARSS)

# Model of water budget comparable to example 5.2.1

obsVec <- c("stmr","prec","evap","rOff","dSto")

TT        <- 12*62  # 62 years of monthly data, max = 62 
obsMatrix <- t(as.matrix(NBSrcDf[1:TT,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

defltModel <- MARSS(obsMatrix)

# Warning! Reached maxit before parameters converged. Maxit was 500.
# neither abstol nor log-log convergence tests were passed.
# 
# MARSS fit is
# Estimation method: kem 
# Convergence test: conv.test.slope.tol = 0.5, abstol = 0.001
# WARNING: maxit reached at  500  iter before convergence.
# Neither abstol nor log-log convergence test were passed.
# The likelihood and params are not at the ML values.
# Try setting control$maxit higher.
# Log-likelihood: -30392.67 
# AIC: 60817.34   AICc: 60817.49   
# 
# Estimate
# R.diag                   1.64e+03
# U.X.stmrCMS             -4.97e-01
# U.X.precCMS              1.97e-01
# U.X.evapCMS              9.26e-01
# U.X.rOffCMS              6.59e-01
# U.X.dStoCMS              1.24e+00
# Q.(X.stmrCMS,X.stmrCMS)  7.00e+04
# Q.(X.precCMS,X.precCMS)  1.20e+06
# Q.(X.evapCMS,X.evapCMS)  8.44e+05
# Q.(X.rOffCMS,X.rOffCMS)  6.57e+05
# Q.(X.dStoCMS,X.dStoCMS)  4.26e+06
# x0.X.stmrCMS             2.15e+03
# x0.X.precCMS             2.11e+03
# x0.X.evapCMS             4.06e+03
# x0.X.rOffCMS             5.99e+02
# x0.X.dStoCMS            -2.76e+03

defltModel.bfgs <- MARSS(obsMatrix, method="BFGS")

# Success! Converged in 167 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 167 iterations. 
# Log-likelihood: -30389.05 
# AIC: 60810.09   AICc: 60810.24   
# 
# Estimate
# R.diag                   2.96e-02    # Note difference in R.diag with above unconverged
# U.X.stmrCMS             -4.95e-01
# U.X.precCMS              3.42e-01
# U.X.evapCMS              1.04e+00
# U.X.rOffCMS             -2.00e+00
# U.X.dStoCMS              1.31e+00
# Q.(X.stmrCMS,X.stmrCMS)  7.18e+04
# Q.(X.precCMS,X.precCMS)  1.21e+06
# Q.(X.evapCMS,X.evapCMS)  8.46e+05
# Q.(X.rOffCMS,X.rOffCMS)  6.67e+05
# Q.(X.dStoCMS,X.dStoCMS)  4.26e+06
# x0.X.stmrCMS             2.15e+03
# x0.X.precCMS             2.11e+03
# x0.X.evapCMS             4.05e+03
# x0.X.rOffCMS             6.28e+02
# x0.X.dStoCMS            -2.76e+03



defltModelrDiagUn.bfgs <- MARSS(obsMatrix, method="BFGS",model=list(R="diagonal and unequal"))
# Success! Converged in 290 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 290 iterations. 
# Log-likelihood: -30207.14 
# AIC: 60454.29   AICc: 60454.51   
# 
# Estimate
# R.(stmrCMS,stmrCMS)      2.99e+00
# R.(precCMS,precCMS)      7.60e+05
# R.(evapCMS,evapCMS)      5.49e-04
# R.(rOffCMS,rOffCMS)      7.22e+04
# R.(dStoCMS,dStoCMS)      7.05e+05
# U.X.stmrCMS             -8.30e-01
# U.X.precCMS              2.42e-02
# U.X.evapCMS              9.74e-01
# U.X.rOffCMS              6.99e-01
# U.X.dStoCMS              1.45e+00
# Q.(X.stmrCMS,X.stmrCMS)  7.18e+04
# Q.(X.precCMS,X.precCMS)  2.00e-04
# Q.(X.evapCMS,X.evapCMS)  8.47e+05
# Q.(X.rOffCMS,X.rOffCMS)  5.22e+05
# Q.(X.dStoCMS,X.dStoCMS)  2.83e+06
# x0.X.stmrCMS             2.15e+03
# x0.X.precCMS             2.04e+03
# x0.X.evapCMS             4.06e+03
# x0.X.rOffCMS             6.03e+02
# x0.X.dStoCMS            -2.76e+03

# # #
# Add fixed monthly component to model
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
# C = matrix(month.abb,5,12,byrow=TRUE)
# print(C)

###################################################
### Specify model structure
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
B  = "identity"              # No autoregressive component or interactions among x
U  = "unequal"               # Unique u values
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
R  = "diagonal and equal"    # All observation errors have the same variance
# Supercede to have different variances
R  = "diagonal and unequal"
A  = "scaling"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=500) # MCInit=TRUE,numInits=10,
# 
s5o5SeaAR1 <- MARSS(obsMatrix, method="BFGS",
                       model=model.list, control=cntl.list)

start.time  <- Sys.time()
s5o5SeaAR1.ci <- MARSSparamCIs(s5o5SeaAR1)
end.time    <- Sys.time()
print(end.time-start.time)

# Compute parameter confidence intervals
s5o5Sea.ci    <- MARSSparamCIs(s5o5Sea.bfgs1)

# Plot CI about seasonal components
source("Superior/src/work/plotCmatrixCI.R")
plotCmatrixCI(s5o5Sea.ci)


# Compute Kalman filter estimates 
s5o5Sea.kf    <- MARSSkf(s5o5Sea.bfgs1)

# Success! Converged in 425 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 425 iterations. 
# Log-likelihood: -28908.39 
# AIC: 57976.78   AICc: 57980.34   
# 
# Estimate
# R.(stmrCMS,stmrCMS)      8.00e-04
# R.(precCMS,precCMS)      5.81e+05
# R.(evapCMS,evapCMS)      2.41e+05
# R.(rOffCMS,rOffCMS)      1.69e+05
# R.(dStoCMS,dStoCMS)      1.72e+06
# U.X.stmrCMS             -4.98e-01
# U.X.precCMS              3.29e-03
# U.X.evapCMS             -1.44e-01
# U.X.rOffCMS              4.74e-01
# U.X.dStoCMS              1.39e-01
# Q.(X.stmrCMS,X.stmrCMS)  6.22e+04
# Q.(X.precCMS,X.precCMS)  3.86e-04
# Q.(X.evapCMS,X.evapCMS)  1.95e+03
# Q.(X.rOffCMS,X.rOffCMS)  1.92e+04
# Q.(X.dStoCMS,X.dStoCMS)  3.28e-01
# x0.X.stmrCMS             2.22e+03
# x0.X.precCMS             1.81e+03
# x0.X.evapCMS             3.86e+03
# x0.X.rOffCMS             7.33e+02
# x0.X.dStoCMS            -2.62e+03
# C.(X.stmrCMS,Jan)       -1.19e+02
# C.(X.precCMS,Jan)       -5.12e+01
# C.(X.evapCMS,Jan)       -6.62e+02
# C.(X.rOffCMS,Jan)       -1.11e+02
# C.(X.dStoCMS,Jan)        2.23e+02
# C.(X.stmrCMS,Feb)       -1.82e+01
# C.(X.precCMS,Feb)       -4.44e+02
# C.(X.evapCMS,Feb)       -1.32e+03
# C.(X.rOffCMS,Feb)        7.45e-01
# C.(X.dStoCMS,Feb)        6.54e+02
# C.(X.stmrCMS,Mar)       -3.27e+01
# C.(X.precCMS,Mar)        1.54e+02
# C.(X.evapCMS,Mar)       -5.06e+02
# C.(X.rOffCMS,Mar)        1.28e+02
# C.(X.dStoCMS,Mar)        1.54e+03
# C.(X.stmrCMS,Apr)        4.29e+01
# C.(X.precCMS,Apr)        2.65e+02
# C.(X.evapCMS,Apr)       -5.18e+02
# C.(X.rOffCMS,Apr)        1.67e+03
# C.(X.dStoCMS,Apr)        2.52e+03
# C.(X.stmrCMS,May)        2.11e+02
# C.(X.precCMS,May)        5.30e+02
# C.(X.evapCMS,May)       -4.60e+02
# C.(X.rOffCMS,May)        1.03e-04
# C.(X.dStoCMS,May)        5.38e+02
# C.(X.stmrCMS,Jun)        7.85e+01
# C.(X.precCMS,Jun)        2.38e+02
# C.(X.evapCMS,Jun)       -2.14e+02
# C.(X.rOffCMS,Jun)       -1.05e+03
# C.(X.dStoCMS,Jun)       -4.84e+02
# C.(X.stmrCMS,Jul)        5.88e+01
# C.(X.precCMS,Jul)       -1.67e+02
# C.(X.evapCMS,Jul)        2.01e+01
# C.(X.rOffCMS,Jul)       -4.09e+02
# C.(X.dStoCMS,Jul)       -9.22e+02
# C.(X.stmrCMS,Aug)        8.84e+01
# C.(X.precCMS,Aug)        6.10e+01
# C.(X.evapCMS,Aug)        3.66e+02
# C.(X.rOffCMS,Aug)       -3.06e+02
# C.(X.dStoCMS,Aug)       -1.06e+03
# C.(X.stmrCMS,Sep)       -5.53e+01
# C.(X.precCMS,Sep)        2.75e+02
# C.(X.evapCMS,Sep)        1.11e+03
# C.(X.rOffCMS,Sep)        4.61e+01
# C.(X.dStoCMS,Sep)       -9.28e+02
# C.(X.stmrCMS,Oct)       -9.89e+01
# C.(X.precCMS,Oct)       -3.70e+02
# C.(X.evapCMS,Oct)        7.76e+02
# C.(X.rOffCMS,Oct)        2.90e+02
# C.(X.dStoCMS,Oct)       -6.10e+02
# C.(X.stmrCMS,Nov)       -1.54e+01
# C.(X.precCMS,Nov)       -2.71e+02
# C.(X.evapCMS,Nov)        1.00e+03
# C.(X.rOffCMS,Nov)        9.53e+00
# C.(X.dStoCMS,Nov)       -6.91e+02
# C.(X.stmrCMS,Dec)       -1.41e+02
# C.(X.precCMS,Dec)       -2.20e+02
# C.(X.evapCMS,Dec)        4.10e+02
# C.(X.rOffCMS,Dec)       -2.76e+02
# C.(X.dStoCMS,Dec)       -7.81e+02

###################################################
### Specify model structure
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
B  = "identity"              # No autoregressive component or interactions among x
# Simple (no-cross corr) AR1 model 
B  = "diagonal and unequal"
#
U  = "unequal"               # Unique u values
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
R  = "diagonal and equal"    # All observation errors have the same variance
# Supercede to have different variances
R  = "diagonal and unequal"
A  = "scaling"               # A set of scaling factors
C  = "zero"                # Parameters associated with state inputs
c  = "zero"                  # Data associated with state inputs
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=500) # MCInit=TRUE,numInits=10,
# s4o1Ar1SeaRun1.model = MARSS(obs,model=model.list,control=cntl.list)
s5o5Ar1.bfgs1 <- MARSS(obsMatrix, method="BFGS",
                       model=model.list, control=cntl.list)


# Success! Converged in 328 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 328 iterations. 
# Log-likelihood: -29952.48 
# AIC: 59954.96   AICc: 59955.31   
# 
# Estimate
# R.(stmrCMS,stmrCMS)      4.35e+01
# R.(precCMS,precCMS)      7.33e+05
# R.(evapCMS,evapCMS)      8.51e+02
# R.(rOffCMS,rOffCMS)      5.26e+03
# R.(dStoCMS,dStoCMS)      7.36e+05
# B.(X.stmrCMS,X.stmrCMS)  8.63e-01
# B.(X.precCMS,X.precCMS)  6.44e-01
# B.(X.evapCMS,X.evapCMS)  7.61e-01
# B.(X.rOffCMS,X.rOffCMS)  5.03e-01
# B.(X.dStoCMS,X.dStoCMS)  6.29e-01
# U.X.stmrCMS              2.98e+02
# U.X.precCMS              7.27e+02
# U.X.evapCMS              3.55e+02
# U.X.rOffCMS              7.81e+02
# U.X.dStoCMS             -2.59e+00
# Q.(X.stmrCMS,X.stmrCMS)  6.75e+04
# Q.(X.precCMS,X.precCMS)  6.29e+04
# Q.(X.evapCMS,X.evapCMS)  7.39e+05
# Q.(X.rOffCMS,X.rOffCMS)  4.84e+05
# Q.(X.dStoCMS,X.dStoCMS)  2.50e+06
# x0.X.stmrCMS             1.91e+03
# x0.X.precCMS             2.06e+03
# x0.X.evapCMS             4.05e+03
# x0.X.rOffCMS             4.46e+02
# x0.X.dStoCMS            -2.74e+03
# 
# Standard errors have not been calculated. 
# Use MARSSparamCIs to compute CIs and bias estimates.
# 
# Warning messages:
#   1: In KFS(kfas.model, simplify = FALSE) :
#   Possible error in smoothing: Negative variances in V, 
# try changing the tolerance parameter tol of the model.
# 2: In KFS(kfas.model, simplify = FALSE) :
#   Possible error in smoothing: Negative variances in V, 
# try changing the tolerance parameter tol of the model.
# 3: In KFS(kfas.model, simplify = FALSE) :
#   Possible error in smoothing: Negative variances in V, 
# try changing the tolerance parameter tol of the model.
# 4: In KFS(kfas.model, simplify = FALSE) :
#   Possible error in smoothing: Negative variances in V, 
# try changing the tolerance parameter tol of the model.
# 5: In KFS(kfas.model, simplify = FALSE) :
#   Possible error in smoothing: Negative variances in V, 
# try changing the tolerance parameter tol of the model.

###################################################
### Specify model structure
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
# Simple (no-cross corr) AR1 model 
# Make A symmetrical
B  = matrix(c("b11","b12","b13","b14","b15",
              "b12","b22","b23","b24","b25",
              "b13","b23","b33","b34","b35",
              "b14","b24","b34","b44","b45",
              "b15","b25","b35","b45","b55"),5,5)
#
U  = "unequal"               # Unique u values
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
R  = "diagonal and equal"    # All observation errors have the same variance
# Supercede to have different variances
R  = "diagonal and unequal"
A  = "scaling"               # A set of scaling factors
C  = "zero"                # Parameters associated with state inputs
c  = "zero"                  # Data associated with state inputs
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=500) # MCInit=TRUE,numInits=10,
# s4o1Ar1SeaRun1.model = MARSS(obs,model=model.list,control=cntl.list)
s5o5Ar1sym.bfgs1 <- MARSS(obsMatrix, method="BFGS",
                       model=model.list, control=cntl.list)
# Returns error

###################################################
### Specify model structure
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
U  = "unequal"               # Unique u values
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# R  = "diagonal and equal"    # All observation errors have the same variance
# Supercede to have different variances
R  = "diagonal and unequal"
A  = "scaling"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=500) # MCInit=TRUE,numInits=10,
# s4o1Ar1SeaRun1.model = MARSS(obs,model=model.list,control=cntl.list)
s5o5SeaAR.bfgs2 <- MARSS(obsMatrix, method="BFGS",
                       model=model.list, control=cntl.list)

library(MARSS)
s5o5SeaAR.ci    <- MARSSparamCIs(s5o5SeaAR.bfgs1)

tmp             <- MARSSparamCIs(s5o5SeaAR.bfgs1, method="innovations", nboot=10)
# Plot CI about seasonal components
source("Superior/src/work/plotCmatrixCI.R")
plotCmatrixCI(s5o5SeaAR.ci)


# Compute Kalman filter estimates 
s5o5SeaAR.kf    <- MARSSkf(s5o5SeaAR.bfgs1)

#                        
# Success! Converged in 355 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 355 iterations. 
# Log-likelihood: -28809.06 
# AIC: 57788.12   AICc: 57792.15   
# 
# Estimate
# R.(stmrCMS,stmrCMS)      4.14e-03
# R.(precCMS,precCMS)      5.38e+05
# R.(evapCMS,evapCMS)      6.11e+04
# R.(rOffCMS,rOffCMS)      6.68e+04
# R.(dStoCMS,dStoCMS)      1.22e+06
# B.(X.stmrCMS,X.stmrCMS)  8.65e-01
# B.(X.precCMS,X.precCMS)  5.49e-01
# B.(X.evapCMS,X.evapCMS)  5.64e-01
# B.(X.rOffCMS,X.rOffCMS)  5.49e-01
# B.(X.dStoCMS,X.dStoCMS)  5.40e-01
# U.X.stmrCMS              2.71e+02
# U.X.precCMS              8.52e+02
# U.X.evapCMS              5.64e+02
# U.X.rOffCMS              6.57e+02
# U.X.dStoCMS             -3.55e+00
# Q.(X.stmrCMS,X.stmrCMS)  5.79e+04
# Q.(X.precCMS,X.precCMS)  3.21e+04
# Q.(X.evapCMS,X.evapCMS)  1.69e+05
# Q.(X.rOffCMS,X.rOffCMS)  1.35e+05
# Q.(X.dStoCMS,X.dStoCMS)  3.39e+05
# x0.X.stmrCMS             2.14e+03
# x0.X.precCMS             2.07e+03
# x0.X.evapCMS             4.21e+03
# x0.X.rOffCMS             4.42e+02
# x0.X.dStoCMS            -2.76e+03
# C.(X.stmrCMS,Jan)       -1.09e+02
# C.(X.precCMS,Jan)        5.45e+00
# C.(X.evapCMS,Jan)        3.54e+02
# C.(X.rOffCMS,Jan)       -2.30e+02
# C.(X.dStoCMS,Jan)       -8.56e+02
# C.(X.stmrCMS,Feb)       -2.44e+01
# C.(X.precCMS,Feb)       -5.97e+02
# C.(X.evapCMS,Feb)       -6.04e+02
# C.(X.rOffCMS,Feb)       -2.00e+02
# C.(X.dStoCMS,Feb)       -5.13e+02
# C.(X.stmrCMS,Mar)       -4.12e+01
# C.(X.precCMS,Mar)       -7.57e+01
# C.(X.evapCMS,Mar)       -3.82e+02
# C.(X.rOffCMS,Mar)       -4.31e+01
# C.(X.dStoCMS,Mar)        6.60e+02
# C.(X.stmrCMS,Apr)        3.22e+01
# C.(X.precCMS,Apr)        2.75e+01
# C.(X.evapCMS,Apr)       -6.07e+02
# C.(X.rOffCMS,Apr)        1.55e+03
# C.(X.dStoCMS,Apr)        2.90e+03
# C.(X.stmrCMS,May)        2.11e+02
# C.(X.precCMS,May)        4.82e+02
# C.(X.evapCMS,May)       -7.61e+02
# C.(X.rOffCMS,May)        6.65e+02
# C.(X.dStoCMS,May)        1.48e+03
# C.(X.stmrCMS,Jun)        1.07e+02
# C.(X.precCMS,Jun)        4.31e+02
# C.(X.evapCMS,Jun)       -7.15e+02
# C.(X.rOffCMS,Jun)       -3.92e+02
# C.(X.dStoCMS,Jun)        7.13e+02
# C.(X.stmrCMS,Jul)        9.86e+01
# C.(X.precCMS,Jul)        7.55e+01
# C.(X.evapCMS,Jul)       -5.69e+02
# C.(X.rOffCMS,Jul)       -2.35e+02
# C.(X.dStoCMS,Jul)        1.14e+02
# C.(X.stmrCMS,Aug)        1.37e+02
# C.(X.precCMS,Aug)        2.51e+02
# C.(X.evapCMS,Aug)       -2.17e+02
# C.(X.rOffCMS,Aug)       -2.98e+02
# C.(X.dStoCMS,Aug)       -4.25e+02
# C.(X.stmrCMS,Sep)        2.56e+00
# C.(X.precCMS,Sep)        5.73e+02
# C.(X.evapCMS,Sep)        6.74e+02
# C.(X.rOffCMS,Sep)       -9.47e+01
# C.(X.dStoCMS,Sep)       -7.33e+02
# C.(X.stmrCMS,Oct)       -5.11e+01
# C.(X.precCMS,Oct)       -6.28e+01
# C.(X.evapCMS,Oct)        8.31e+02
# C.(X.rOffCMS,Oct)        1.65e+02
# C.(X.dStoCMS,Oct)       -6.91e+02
# C.(X.stmrCMS,Nov)        1.89e+01
# C.(X.precCMS,Nov)       -4.29e+01
# C.(X.evapCMS,Nov)        1.38e+03
# C.(X.rOffCMS,Nov)        1.88e+01
# C.(X.dStoCMS,Nov)       -1.06e+03
# C.(X.stmrCMS,Dec)       -1.12e+02
# C.(X.precCMS,Dec)       -2.15e+02
# C.(X.evapCMS,Dec)        1.18e+03
# C.(X.rOffCMS,Dec)       -2.46e+02
# C.(X.dStoCMS,Dec)       -1.60e+03

###################################################
### Specify model structure: Z components estimated in s5o5Sea model
###################################################
library(MARSS)
# Default Model Structures (p. 30-31)
# Z  = "identity"              # Each y cooresponds to one x
# Change from identity model to use components
Z  <- matrix(list("z11","z12","z13","z14","z15",
                   0 ,   1 ,   0 ,   0 ,   0 ,
                   0 ,   0 ,   1 ,   0 ,   0 ,
                   0 ,   0 ,   0 ,   1 ,   0 ,
                   0 ,   0 ,   0 ,   0 ,   1),nrow=5, ncol=5, byrow=TRUE)
#
B  = "identity"              # No autoregressive component or interactions among x
U  = "unequal"               # Unique u values
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
R  = "diagonal and equal"    # All observation errors have the same variance
# Supercede to have different variances
R  = "diagonal and unequal"
# A  = "scaling"               # A set of scaling factors
A  <- "zero"
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=1000) # MCInit=TRUE,numInits=10,
# 
s5o5SeaZstmr <- MARSS(obsMatrix, method="BFGS",
                       model=model.list, control=cntl.list)

# Compute parameter confidence intervals
s5o5SeaZstmr.ci    <- MARSSparamCIs(s5o5SeaZstmr)

# Plot CI about seasonal components
source("Superior/src/work/plotCmatrixCI.R")
plotCmatrixCI(s5o5SeaZstmr.ci)

tmp <- MARSS(obsMatrix, method="kem",
             model=model.list, control=cntl.list)

# 
# Success! Converged in 739 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 739 iterations. 
# Log-likelihood: -28813.68 
# AIC: 57797.35   AICc: 57801.38   
# 
# Estimate
# Z.z11                2.80e+00
# Z.z12                2.67e+00
# Z.z13               -1.76e-01
# Z.z14                1.26e+00
# Z.z15               -4.75e-01
# R.(stmrCMS,stmrCMS)  4.11e-01
# R.(precCMS,precCMS)  5.83e+05
# R.(evapCMS,evapCMS)  2.42e+05
# R.(rOffCMS,rOffCMS)  1.74e+05
# R.(dStoCMS,dStoCMS)  1.22e+06
# U.X1                -8.65e-02
# U.X2                 2.28e-03
# U.X3                -1.90e-02
# U.X4                 8.14e-02
# U.X5                 1.01e+01
# Q.(X1,X1)            1.44e-07
# Q.(X2,X2)            6.00e-07
# Q.(X3,X3)            1.78e+03
# Q.(X4,X4)            1.30e+04
# Q.(X5,X5)            2.05e+05
# x0.X1               -1.70e+03
# x0.X2                1.85e+03
# x0.X3                3.76e+03
# x0.X4                1.06e+03
# x0.X5               -2.85e+03
# C.(X1,Jan)           9.59e+01
# C.(X2,Jan)          -9.36e+01
# C.(X3,Jan)          -6.32e+02
# C.(X4,Jan)          -1.40e+02
# C.(X5,Jan)           1.46e+02
# C.(X1,Feb)           4.59e+02
# C.(X2,Feb)          -4.93e+02
# C.(X3,Feb)          -1.31e+03
# C.(X4,Feb)          -1.78e+01
# C.(X5,Feb)           4.04e+02
# C.(X1,Mar)          -2.50e+01
# C.(X2,Mar)           1.57e+02
# C.(X3,Mar)          -5.21e+02
# C.(X4,Mar)           1.77e+02
# C.(X5,Mar)           1.46e+03
# C.(X1,Apr)          -5.25e+02
# C.(X2,Apr)           2.79e+02
# C.(X3,Apr)          -5.19e+02
# C.(X4,Apr)           1.64e+03
# C.(X5,Apr)           2.91e+03
# C.(X1,May)          -4.17e+02
# C.(X2,May)           5.42e+02
# C.(X3,May)          -4.62e+02
# C.(X4,May)           1.86e+01
# C.(X5,May)           3.67e+02
# C.(X1,Jun)           1.21e+02
# C.(X2,Jun)           2.52e+02
# C.(X3,Jun)          -2.16e+02
# C.(X4,Jun)          -1.03e+03
# C.(X5,Jun)          -6.84e+02
# C.(X1,Jul)           2.23e+02
# C.(X2,Jul)          -1.74e+02
# C.(X3,Jul)           2.61e+01
# C.(X4,Jul)          -4.36e+02
# C.(X5,Jul)          -9.67e+02
# C.(X1,Aug)          -6.08e+01
# C.(X2,Aug)           7.88e+01
# C.(X3,Aug)           3.73e+02
# C.(X4,Aug)          -2.95e+02
# C.(X5,Aug)          -1.01e+03
# C.(X1,Sep)          -3.68e+02
# C.(X2,Sep)           2.71e+02
# C.(X3,Sep)           1.10e+03
# C.(X4,Sep)           5.17e+01
# C.(X5,Sep)          -7.70e+02
# C.(X1,Oct)           1.26e+02
# C.(X2,Oct)          -3.23e+02
# C.(X3,Oct)           7.82e+02
# C.(X4,Oct)           2.78e+02
# C.(X5,Oct)          -4.16e+02
# C.(X1,Nov)           1.83e+02
# C.(X2,Nov)          -2.44e+02
# C.(X3,Nov)           9.88e+02
# C.(X4,Nov)           1.49e+01
# C.(X5,Nov)          -5.86e+02
# C.(X1,Dec)           1.89e+02
# C.(X2,Dec)          -2.51e+02
# C.(X3,Dec)           3.90e+02
# C.(X4,Dec)          -2.63e+02
# C.(X5,Dec)          -8.38e+02

###################################################
### Specify model structure for Fourier Seasonal approximation
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
B  = "identity"              # No autoregressive component or interactions among x
U  = "unequal"               # Unique u values
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
R  = "diagonal and equal"    # All observation errors have the same variance
# Supercede to have different variances
R  = "diagonal and unequal"
A  = "scaling"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
# c  = c.in
# Replace fixed seasonal structure with first-order Fourier Series
cos.t  <- cos(2 * 3.1415926 * seq(TT) / period)
sin.t  <- sin(2 * 3.1415926 * seq(TT) / period)
c.Four <- rbind(cos.t, sin.t)
#
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.Four,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=500) # MCInit=TRUE,numInits=10,
# 
s5o5Fourier <- MARSS(obsMatrix, method="BFGS",
                       model=model.list, control=cntl.list)
# Success! Converged in 337 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 337 iterations. 
# Log-likelihood: -29391.86 
# AIC: 58843.72   AICc: 58844.23   
# 
# Estimate
# R.(stmrCMS,stmrCMS)      6.37e+00
# R.(precCMS,precCMS)      6.09e+05
# R.(evapCMS,evapCMS)      3.19e+05
# R.(rOffCMS,rOffCMS)      4.78e+05
# R.(dStoCMS,dStoCMS)      2.00e+06
# U.X.stmrCMS              2.64e-02
# U.X.precCMS             -2.68e-02
# U.X.evapCMS             -7.99e-01
# U.X.rOffCMS              4.89e-01
# U.X.dStoCMS              2.47e-01
# Q.(X.stmrCMS,X.stmrCMS)  6.53e+04
# Q.(X.precCMS,X.precCMS)  8.38e-04
# Q.(X.evapCMS,X.evapCMS)  2.95e+03
# Q.(X.rOffCMS,X.rOffCMS)  1.24e+03
# Q.(X.dStoCMS,X.dStoCMS)  6.25e-04
# x0.X.stmrCMS             2.18e+03
# x0.X.precCMS             1.80e+03
# x0.X.evapCMS             3.91e+03
# x0.X.rOffCMS             7.02e+02
# x0.X.dStoCMS            -2.66e+03
# C.(X.stmrCMS,cos.t)     -1.12e+02
# C.(X.precCMS,cos.t)     -2.82e+02
# C.(X.evapCMS,cos.t)      1.83e+02
# C.(X.rOffCMS,cos.t)      8.20e+01
# C.(X.dStoCMS,cos.t)     -1.86e+02
# C.(X.stmrCMS,sin.t)      1.26e+01
# C.(X.precCMS,sin.t)      6.71e+01
# C.(X.evapCMS,sin.t)     -8.84e+02
# C.(X.rOffCMS,sin.t)      2.89e+02
# C.(X.dStoCMS,sin.t)      1.31e+03

###################################################
### Specify model structure
###################################################
# Default Model Structures (p. 30-31)
# Z  = "identity"              # Each y cooresponds to one x
Z <- matrix(list(   1 , "z12", "z13", "z14", "z15", 
                    0 ,   1  ,   0  ,   0 ,    0  ,
                    0 ,   0  ,   1  ,   0  ,   0  ,
                    0 ,   0  ,   0  ,   1  ,   0  ,
                    0 ,   0  ,   0  ,   0  ,   1) , nrow=5, ncol=5, byrow=TRUE)


# B  = "identity"              # No autoregressive component or interactions among x
B <- matrix(list("b11", 0 ,   0 ,   0 ,   0 , 
               0 ,  "b22" ,   0 ,   0 ,   0 ,
               0 ,   0 ,  "b33" ,   0 ,   0 ,
               0 ,   0 ,   0 ,   "b44" ,  0 ,
               0 ,   0 ,   0 ,   0 ,   "b55"), nrow=5, ncol=5, byrow=TRUE)

U  = "unequal"               # Unique u values
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
R  = "diagonal and equal"    # All observation errors have the same variance
# Supercede to have different variances
R  = "diagonal and unequal"
A  = "zero"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=500) # MCInit=TRUE,numInits=10,
# 
s5o5SeaARzaug.bfgs1 <- MARSS(obsMatrix, method="BFGS",
                         model=model.list, control=cntl.list)
#                        
# Not estimatible model

###################################################
### Specify model structure
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
# B  = "unconstrained"
U  = "unequal"               # Unique u values
# U <- "zero"
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# Q  <- "unconstrained"
# R  = "diagonal and equal"    # All observation errors have the same variance
# R  = "diagonal and unequal"
R <- matrix(list( (0.025*2180)^2,0,0,0,0,
            0,"r22",0,0,0,
            0,0,"r33",0,0,
            0,0,0,"r44",0,
            0,0,0,0,"r55"),5,5)
# Supercede to have different variances

A  = "scaling"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
U          <- "zero"
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=1000) # MCInit=TRUE,numInits=10,
# s4o1Ar1SeaRun1.model = MARSS(obs,model=model.list,control=cntl.list)

obsMatrixDmean <- obsMatrix - rowMeans(obsMatrix)

s5o5SeaARQx  <- MARSS(obsMatrix, method="BFGS",
                         model=model.list, control=cntl.list)
# MARSSkfas returned error.  Trying MARSSkfss.
# Returns error with nonzero (previously estimated) U or 
#  U <- "zero" and demeaned obsMatrixDmean
#
cntl.list  <- list(maxit=2000, safe=TRUE)
s5o5SeaARQkem  <- MARSS(obsMatrixDmean, method="kem",
                      model=model.list, control=cntl.list)

s5o5SeaAR.ci2    <- MARSSparamCIs(s5o5SeaAR.bfgs2)

s5o5SeaAR.ciParmBoot <- MARSSparamCIs(s5o5SeaAR.bfgs2,method="parametric",nboot=1000)

# # #  Measurement Uncertainties Based on Neff and Nicholas  # # #
###################################################
### Specify model structure: Low measurement uncertainties
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
# B  = "unconstrained"
U  = "unequal"               # Unique u values
# U <- "zero"
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# Q  <- "unconstrained"
# R  = "diagonal and equal"    # All observation errors have the same variance
# R  = "diagonal and unequal"
R <- matrix(list( (0.05*2182)^2,0,0,0,0,
                  0,(0.15*2047)^2,0,0,0,
                  0,0,(0.10*1403)^2,0,0,
                  0,0,0,(0.15*1576)^2,0,
                  0,0,0,0,95^2),5,5)
# Supercede to have different variances

A  = "scaling"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=1000) # MCInit=TRUE,numInits=10,
# 

# obsMatrixDmean <- obsMatrix - rowMeans(obsMatrix)
library(MARSS)
s5o5SeaARQ_NeffLo  <- MARSS(obsMatrix, method="BFGS",
                      model=model.list, control=cntl.list)
# MARSSkfas returned error.  Trying MARSSkfss.
# Returns error with nonzero (previously estimated) U or 
#  U <- "zero" and demeaned obsMatrixDmean
#
# # #  Measurement Uncertainties Based on Neff and Nicholas  # # #
###################################################
### Specify model structure: High measurement uncertainties
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
# B  = "unconstrained"
U  = "unequal"               # Unique u values
# U <- "zero"
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# Q  <- "unconstrained"
# R  = "diagonal and equal"    # All observation errors have the same variance
# R  = "diagonal and unequal"
R <- matrix(list( (0.15*2182)^2,0,0,0,0,
                  0,(0.45*2047)^2,0,0,0,
                  0,0,(0.35*1403)^2,0,0,
                  0,0,0,(0.35*1576)^2,0,
                  0,0,0,0,380^2),5,5)
# Supercede to have different variances

A  = "scaling"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=1000) # MCInit=TRUE,numInits=10,
# 

# obsMatrixDmean <- obsMatrix - rowMeans(obsMatrix)
library(MARSS)
s5o5SeaARQ_NeffHi1  <- MARSS(obsMatrix, method="BFGS",
                            model=model.list, control=cntl.list)
# 
# 
# s5o5SeaARQ_NeffHi.ci    <- MARSSparamCIs(s5o5SeaARQ_NeffHi)
# 
# Success! Converged in 454 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 454 iterations. 
# Log-likelihood: -29149.22 
# AIC: 58458.44   AICc: 58462.01   
# 
# Estimate
# B.(X.stmrCMS,X.stmrCMS)  9.23e-01
# B.(X.precCMS,X.precCMS)  7.25e-01
# B.(X.evapCMS,X.evapCMS)  8.92e-01
# B.(X.rOffCMS,X.rOffCMS)  9.17e-01
# B.(X.dStoCMS,X.dStoCMS)  3.20e-01
# U.X.stmrCMS              1.54e+02
# U.X.precCMS              5.19e+02
# U.X.evapCMS              1.38e+02
# U.X.rOffCMS              1.21e+02
# U.X.dStoCMS             -6.12e+00
# Q.(X.stmrCMS,X.stmrCMS)  2.47e+04
# Q.(X.precCMS,X.precCMS)  1.43e+02
# Q.(X.evapCMS,X.evapCMS)  1.37e+04
# Q.(X.rOffCMS,X.rOffCMS)  1.01e+04
# Q.(X.dStoCMS,X.dStoCMS)  1.53e+06
# x0.X.stmrCMS             2.18e+03
# x0.X.precCMS             2.06e+03
# x0.X.evapCMS             4.20e+03
# x0.X.rOffCMS             6.28e+02
# x0.X.dStoCMS            -2.77e+03
# C.(X.stmrCMS,Jan)       -1.21e+02
# C.(X.precCMS,Jan)       -5.33e+01
# C.(X.evapCMS,Jan)       -3.52e+02
# C.(X.rOffCMS,Jan)       -1.35e+02
# C.(X.dStoCMS,Jan)       -1.18e+03
# C.(X.stmrCMS,Feb)       -3.13e+01
# C.(X.precCMS,Feb)       -5.58e+02
# C.(X.evapCMS,Feb)       -1.16e+03
# C.(X.rOffCMS,Feb)        1.06e+01
# C.(X.dStoCMS,Feb)       -8.67e+02
# C.(X.stmrCMS,Mar)       -3.19e+01
# C.(X.precCMS,Mar)       -4.06e+01
# C.(X.evapCMS,Mar)       -4.59e+02
# C.(X.rOffCMS,Mar)        8.12e+01
# C.(X.dStoCMS,Mar)        2.94e+02
# C.(X.stmrCMS,Apr)        5.00e+01
# C.(X.precCMS,Apr)        1.40e+02
# C.(X.evapCMS,Apr)       -5.71e+02
# C.(X.rOffCMS,Apr)        1.70e+03
# C.(X.dStoCMS,Apr)        2.88e+03
# C.(X.stmrCMS,May)        2.17e+02
# C.(X.precCMS,May)        5.12e+02
# C.(X.evapCMS,May)       -5.41e+02
# C.(X.rOffCMS,May)        1.07e+02
# C.(X.dStoCMS,May)        1.82e+03
# C.(X.stmrCMS,Jun)        1.15e+02
# C.(X.precCMS,Jun)        3.33e+02
# C.(X.evapCMS,Jun)       -3.24e+02
# C.(X.rOffCMS,Jun)       -9.70e+02
# C.(X.dStoCMS,Jun)        1.14e+03
# C.(X.stmrCMS,Jul)        9.71e+01
# C.(X.precCMS,Jul)       -3.21e+01
# C.(X.evapCMS,Jul)       -1.28e+02
# C.(X.rOffCMS,Jul)       -3.55e+02
# C.(X.dStoCMS,Jul)        4.76e+02
# C.(X.stmrCMS,Aug)        1.17e+02
# C.(X.precCMS,Aug)        2.03e+02
# C.(X.evapCMS,Aug)        1.93e+02
# C.(X.rOffCMS,Aug)       -2.94e+02
# C.(X.dStoCMS,Aug)       -1.95e+02
# C.(X.stmrCMS,Sep)       -2.05e+01
# C.(X.precCMS,Sep)        4.65e+02
# C.(X.evapCMS,Sep)        1.04e+03
# C.(X.rOffCMS,Sep)        2.32e+01
# C.(X.dStoCMS,Sep)       -6.65e+02
# C.(X.stmrCMS,Oct)       -7.95e+01
# C.(X.precCMS,Oct)       -1.44e+02
# C.(X.evapCMS,Oct)        7.82e+02
# C.(X.rOffCMS,Oct)        2.76e+02
# C.(X.dStoCMS,Oct)       -7.29e+02
# C.(X.stmrCMS,Nov)       -2.06e+01
# C.(X.precCMS,Nov)       -1.24e+02
# C.(X.evapCMS,Nov)        1.11e+03
# C.(X.rOffCMS,Nov)       -1.24e+01
# C.(X.dStoCMS,Nov)       -1.15e+03
# C.(X.stmrCMS,Dec)       -1.37e+02
# C.(X.precCMS,Dec)       -1.83e+02
# C.(X.evapCMS,Dec)        5.50e+02
# C.(X.rOffCMS,Dec)       -3.10e+02
# C.(X.dStoCMS,Dec)       -1.83e+03
# 
# s5o5SeaAR.ciParmBoot <- MARSSparamCIs(s5o5SeaAR.bfgs2,method="parametric",nboot=1000)

# # #  Measurement Uncertainties Based on Neff and Nicholas  # # #
###################################################
### Specify model structure: Midlevel measurement uncertainties
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
# B  = "unconstrained"
U  = "unequal"               # Unique u values
# U <- "zero"
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# Q  <- "unconstrained"
# R  = "diagonal and equal"    # All observation errors have the same variance
# R  = "diagonal and unequal"
R <- matrix(list( (0.10*2182)^2,0,0,0,0,
                  0,(0.30*2047)^2,0,0,0,
                  0,0,(0.225*1403)^2,0,0,
                  0,0,0,(0.25*1576)^2,0,
                  0,0,0,0,277^2),5,5)
# Supercede to have different variances

A  = "scaling"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=1000) # MCInit=TRUE,numInits=10,
# 

# obsMatrixDmean <- obsMatrix - rowMeans(obsMatrix)
library(MARSS)
s5o5SeaARQ_NeffMid  <- MARSS(obsMatrix, method="BFGS",
                            model=model.list, control=cntl.list)

### Specify model structure: Midlevel measurement uncertainties
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
# B  = "unconstrained"
U  = "unequal"               # Unique u values
# U <- "zero"
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# Q  <- "unconstrained"
# R  = "diagonal and equal"    # All observation errors have the same variance
# R  = "diagonal and unequal"

R <- matrix(list( (0.05*2182)^2,0,0,0,0,
                  0,(0.15*2047)^2,0,0,0,
                  0,0,(0.10*1403)^2,0,0,
                  0,0,0,(0.15*1576)^2,0,
                  0,0,0,0,95^2),5,5)

# Supercede to have different variances

A  = "scaling"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=1000) # MCInit=TRUE,numInits=10,
# 

# obsMatrixDmean <- obsMatrix - rowMeans(obsMatrix)
library(MARSS)
s5o5SeaARQ_NeffLo2  <- MARSS(obsMatrix, method="BFGS",
                             model=model.list, control=cntl.list)

# # #  Measurement Uncertainties Based on Neff and Nicholas  # # #
###################################################
### Specify model structure: High measurement uncertainties
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
# B  = "unconstrained"
U  = "unequal"               # Unique u values
# U <- "zero"
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# Q  <- "unconstrained"
# R  = "diagonal and equal"    # All observation errors have the same variance
# R  = "diagonal and unequal"
R <- matrix(list( (0.05*2182)^2,0,0,0,0,
                  0,(0.15*2047)^2,0,0,0,
                  0,0,(0.15*1403)^2,0,0,
                  0,0,0,(0.10*1576)^2,0,
                  0,0,0,0,380^2),5,5)        # dStor large
# Supercede to have different variances

A  = "scaling"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=1000) # MCInit=TRUE,numInits=10,
# 

# obsMatrixDmean <- obsMatrix - rowMeans(obsMatrix)
library(MARSS)
s5o5SeaARQ_NeffLoNot5  <- MARSS(obsMatrix, method="BFGS",
                             model=model.list, control=cntl.list)

# Success! Converged in 404 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 404 iterations. 
# Log-likelihood: -28842.12 
# AIC: 57844.25   AICc: 57847.81   
# 
# Estimate
# B.(X.stmrCMS,X.stmrCMS)  8.77e-01
# B.(X.precCMS,X.precCMS)  8.26e-02
# B.(X.evapCMS,X.evapCMS)  4.67e-01
# B.(X.rOffCMS,X.rOffCMS)  4.51e-01
# B.(X.dStoCMS,X.dStoCMS)  1.99e-01
# U.X.stmrCMS              2.47e+02
# U.X.precCMS              1.73e+03
# U.X.evapCMS              6.91e+02
# U.X.rOffCMS              7.97e+02
# U.X.dStoCMS              4.40e+00
# Q.(X.stmrCMS,X.stmrCMS)  4.93e+04
# Q.(X.precCMS,X.precCMS)  4.89e+05
# Q.(X.evapCMS,X.evapCMS)  1.93e+05
# Q.(X.rOffCMS,X.rOffCMS)  1.88e+05
# Q.(X.dStoCMS,X.dStoCMS)  1.52e+06
# x0.X.stmrCMS             2.29e+03
# x0.X.precCMS             2.12e+03
# x0.X.evapCMS             4.31e+03
# x0.X.rOffCMS             5.61e+02
# x0.X.dStoCMS            -2.78e+03
# C.(X.stmrCMS,Jan)       -1.11e+02
# C.(X.precCMS,Jan)       -1.15e+02
# C.(X.evapCMS,Jan)        5.92e+02
# C.(X.rOffCMS,Jan)       -2.74e+02
# C.(X.dStoCMS,Jan)       -1.59e+03
# C.(X.stmrCMS,Feb)       -2.82e+01
# C.(X.precCMS,Feb)       -6.69e+02
# C.(X.evapCMS,Feb)       -4.34e+02
# C.(X.rOffCMS,Feb)       -2.36e+02
# C.(X.dStoCMS,Feb)       -1.25e+03
# C.(X.stmrCMS,Mar)       -2.43e+01
# C.(X.precCMS,Mar)       -3.62e+02
# C.(X.evapCMS,Mar)       -3.37e+02
# C.(X.rOffCMS,Mar)       -5.80e+01
# C.(X.dStoCMS,Mar)        1.16e+00
# C.(X.stmrCMS,Apr)        3.02e+01
# C.(X.precCMS,Apr)       -1.11e+02
# C.(X.evapCMS,Apr)       -6.20e+02
# C.(X.rOffCMS,Apr)        1.54e+03
# C.(X.dStoCMS,Apr)        2.97e+03
# C.(X.stmrCMS,May)        2.06e+02
# C.(X.precCMS,May)        4.48e+02
# C.(X.evapCMS,May)       -8.31e+02
# C.(X.rOffCMS,May)        8.41e+02
# C.(X.dStoCMS,May)        2.28e+03
# C.(X.stmrCMS,Jun)        1.04e+02
# C.(X.precCMS,Jun)        6.16e+02
# C.(X.evapCMS,Jun)       -8.35e+02
# C.(X.rOffCMS,Jun)       -2.29e+02
# C.(X.dStoCMS,Jun)        1.63e+03
# C.(X.stmrCMS,Jul)        1.02e+02
# C.(X.precCMS,Jul)        3.85e+02
# C.(X.evapCMS,Jul)       -7.17e+02
# C.(X.rOffCMS,Jul)       -2.13e+02
# C.(X.dStoCMS,Jul)        8.58e+02
# C.(X.stmrCMS,Aug)        1.27e+02
# C.(X.precCMS,Aug)        4.86e+02
# C.(X.evapCMS,Aug)       -3.69e+02
# C.(X.rOffCMS,Aug)       -3.21e+02
# C.(X.dStoCMS,Aug)        2.15e+01
# C.(X.stmrCMS,Sep)       -1.17e+01
# C.(X.precCMS,Sep)        7.93e+02
# C.(X.evapCMS,Sep)        5.65e+02
# C.(X.rOffCMS,Sep)       -1.39e+02
# C.(X.dStoCMS,Sep)       -6.29e+02
# C.(X.stmrCMS,Oct)       -6.19e+01
# C.(X.precCMS,Oct)        3.02e+02
# C.(X.evapCMS,Oct)        8.29e+02
# C.(X.rOffCMS,Oct)        1.29e+02
# C.(X.dStoCMS,Oct)       -8.21e+02
# C.(X.stmrCMS,Nov)        2.88e+01
# C.(X.precCMS,Nov)        1.18e+02
# C.(X.evapCMS,Nov)        1.47e+03
# C.(X.rOffCMS,Nov)        1.35e+01
# C.(X.dStoCMS,Nov)       -1.32e+03
# C.(X.stmrCMS,Dec)       -1.13e+02
# C.(X.precCMS,Dec)       -1.60e+02
# C.(X.evapCMS,Dec)        1.38e+03
# C.(X.rOffCMS,Dec)       -2.57e+02
# C.(X.dStoCMS,Dec)       -2.15e+03

# # #  Measurement Uncertainties Based on Neff and Nicholas  # # #
###################################################
### Specify model structure: Low half measurement uncertainties
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
# B  = "unconstrained"
U  = "unequal"               # Unique u values
# U <- "zero"
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# Q  <- "unconstrained"
# R  = "diagonal and equal"    # All observation errors have the same variance
# R  = "diagonal and unequal"
R <- matrix(list( (0.05*2182)^2,0,0,0,0,
                  0,(0.15*2047)^2,0,0,0,
                  0,0,(0.15*1403)^2,0,0,
                  0,0,0,(0.10*1576)^2,0,
                  0,0,0,0,268^2),5,5)        # dStor large
# Supercede to have different variances

A  = "scaling"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=1000) # MCInit=TRUE,numInits=10,
# 

# obsMatrixDmean <- obsMatrix - rowMeans(obsMatrix)
library(MARSS)
s5o5SeaARQ_NeffLo5Half  <- MARSS(obsMatrix, method="BFGS",
                                model=model.list, control=cntl.list)

s5o5SeaARQ_NeffLo5Half.ci <- MARSSparamCIs(s5o5SeaARQ_NeffLo5Half)
# Results from above operation to compute CI are singular
# 
# Success! Converged in 491 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 491 iterations. 
# Log-likelihood: -30117.13 
# AIC: 60394.26   AICc: 60397.82   
# 
# Estimate
# B.(X.stmrCMS,X.stmrCMS)  8.50e-01
# B.(X.precCMS,X.precCMS) -1.64e-01
# B.(X.evapCMS,X.evapCMS)  7.71e-01
# B.(X.rOffCMS,X.rOffCMS)  4.70e-01
# B.(X.dStoCMS,X.dStoCMS)  6.66e-01
# U.X.stmrCMS              3.04e+02
# U.X.precCMS              2.24e+03
# U.X.evapCMS              2.96e+02
# U.X.rOffCMS              7.77e+02
# U.X.dStoCMS              1.92e+03
# Q.(X.stmrCMS,X.stmrCMS)  5.26e+04
# Q.(X.precCMS,X.precCMS)  5.00e+05
# Q.(X.evapCMS,X.evapCMS)  1.90e+05
# Q.(X.rOffCMS,X.rOffCMS)  1.87e+05
# Q.(X.dStoCMS,X.dStoCMS)  1.08e+08
# x0.X.stmrCMS             2.30e+03
# x0.X.precCMS             2.12e+03
# x0.X.evapCMS             4.27e+03
# x0.X.rOffCMS             5.21e+02
# x0.X.dStoCMS            -3.41e+03
# C.(X.stmrCMS,Jan)       -3.42e+01
# C.(X.precCMS,Jan)       -9.63e+01
# C.(X.evapCMS,Jan)       -2.13e+02
# C.(X.rOffCMS,Jan)       -2.31e+02
# C.(X.dStoCMS,Jan)        6.65e+02
# C.(X.stmrCMS,Feb)       -6.94e+01
# C.(X.precCMS,Feb)       -6.21e+02
# C.(X.evapCMS,Feb)       -9.91e+02
# C.(X.rOffCMS,Feb)       -1.88e+02
# C.(X.dStoCMS,Feb)        3.23e+02
# C.(X.stmrCMS,Mar)        3.91e+01
# C.(X.precCMS,Mar)       -5.35e+02
# C.(X.evapCMS,Mar)       -4.53e+02
# C.(X.rOffCMS,Mar)       -2.90e+01
# C.(X.dStoCMS,Mar)        4.48e+02
# C.(X.stmrCMS,Apr)       -7.65e+01
# C.(X.precCMS,Apr)       -2.81e+02
# C.(X.evapCMS,Apr)       -5.12e+02
# C.(X.rOffCMS,Apr)        1.51e+03
# C.(X.dStoCMS,Apr)        3.92e+02
# C.(X.stmrCMS,May)        2.32e+02
# C.(X.precCMS,May)        3.03e+02
# C.(X.evapCMS,May)       -5.71e+02
# C.(X.rOffCMS,May)        7.32e+02
# C.(X.dStoCMS,May)       -2.87e+02
# C.(X.stmrCMS,Jun)        3.94e+01
# C.(X.precCMS,Jun)        6.60e+02
# C.(X.evapCMS,Jun)       -4.33e+02
# C.(X.rOffCMS,Jun)       -2.93e+02
# C.(X.dStoCMS,Jun)       -1.53e+02
# C.(X.stmrCMS,Jul)        1.82e+02
# C.(X.precCMS,Jul)        5.45e+02
# C.(X.evapCMS,Jul)       -2.56e+02
# C.(X.rOffCMS,Jul)       -2.14e+02
# C.(X.dStoCMS,Jul)       -1.38e+02
# C.(X.stmrCMS,Aug)        5.74e+01
# C.(X.precCMS,Aug)        5.74e+02
# C.(X.evapCMS,Aug)        1.09e+02
# C.(X.rOffCMS,Aug)       -3.03e+02
# C.(X.dStoCMS,Aug)       -3.90e+01
# C.(X.stmrCMS,Sep)        6.21e+01
# C.(X.precCMS,Sep)        8.97e+02
# C.(X.evapCMS,Sep)        8.82e+02
# C.(X.rOffCMS,Sep)       -1.17e+02
# C.(X.dStoCMS,Sep)        7.74e+01
# C.(X.stmrCMS,Oct)       -7.38e+01
# C.(X.precCMS,Oct)        5.66e+02
# C.(X.evapCMS,Oct)        8.20e+02
# C.(X.rOffCMS,Oct)        1.40e+02
# C.(X.dStoCMS,Oct)        1.97e+02
# C.(X.stmrCMS,Nov)        3.96e+01
# C.(X.precCMS,Nov)        2.83e+02
# C.(X.evapCMS,Nov)        1.16e+03
# C.(X.rOffCMS,Nov)        1.47e+01
# C.(X.dStoCMS,Nov)        1.92e+02
# C.(X.stmrCMS,Dec)       -9.35e+01
# C.(X.precCMS,Dec)       -5.43e+01
# C.(X.evapCMS,Dec)        7.51e+02
# C.(X.rOffCMS,Dec)       -2.40e+02
# C.(X.dStoCMS,Dec)        2.43e+02

###################################################
### Specify model structure
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
U  = "zero"               # Unique u values
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# R  = "diagonal and equal"    # All observation errors have the same variance
# Supercede to have different variances
R  = "diagonal and unequal"
A  = "zero"               # A set of scaling factors
# C  = "zero"                # Parameters associated with state inputs
# Supersede C to contain parameters of possible seasonal effects
C  = "unconstrained"
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.in
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=500) # MCInit=TRUE,numInits=10,
# s4o1Ar1SeaRun1.model = MARSS(obs,model=model.list,control=cntl.list)
start.time    <- Sys.time()
s5o5SeaARUA0.bfgs2 <- MARSS(obsMatrix, method="BFGS",
                         model=model.list, control=cntl.list)
end.time      <- Sys.time()
print(end.time-start.time)
# 
start.time    <- Sys.time()
s5o5SeaARUA0.InnoCI <- MARSSparamCIs(s5o5SeaARUA0.bfgs2, method="hessian", nboot=10)
end.time      <- Sys.time()
print(end.time-start.time)

# Update the initial conditions 
start.time    <- Sys.time()
s5o5SeaARUA0.mc <- MARSSmcinit(s5o5SeaARUA0.bfgs2)
end.time      <- Sys.time()
print(end.time-start.time)
# Error in eigen(par.random, only.values = TRUE) : 
#   non-square matrix in 'eigen'



# Success! Converged in 494 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 494 iterations. 
# Log-likelihood: -28826.55 
# AIC: 57813.1   AICc: 57816.66   
# 
# 
# ML.Est  Std.Err    low.CI     up.CI
# R.(stmrCMS,stmrCMS)      4.29e+00       NA        NA        NA
# R.(precCMS,precCMS)      5.88e+05 2.24e+01        NA        NA
# R.(evapCMS,evapCMS)      9.55e+04 2.67e+01        NA        NA
# R.(rOffCMS,rOffCMS)      1.22e+05 1.80e+01        NA        NA
# R.(dStoCMS,dStoCMS)      1.04e+06 7.97e+01        NA        NA
# B.(X.stmrCMS,X.stmrCMS)  8.27e-01 1.70e-02  7.94e-01  8.61e-01
# B.(X.precCMS,X.precCMS)  8.92e-01 1.01e-02  8.73e-01  9.12e-01
# B.(X.evapCMS,X.evapCMS)  6.70e-01       NA        NA        NA
# B.(X.rOffCMS,X.rOffCMS)  7.60e-01 9.27e-03  7.42e-01  7.78e-01
# B.(X.dStoCMS,X.dStoCMS)  5.37e-01 5.55e-02  4.28e-01  6.46e-01
# Q.(X.stmrCMS,X.stmrCMS)  5.70e+04 6.13e+00  5.14e+04  6.29e+04
# Q.(X.precCMS,X.precCMS)  5.33e+03 3.36e+01  5.09e+01  1.93e+04
# Q.(X.evapCMS,X.evapCMS)  1.24e+05 2.66e+01  9.01e+04  1.64e+05
# Q.(X.rOffCMS,X.rOffCMS)  6.17e+04 1.97e+01  4.40e+04  8.23e+04
# Q.(X.dStoCMS,X.dStoCMS)  5.20e+05 1.25e+02  2.27e+05  9.33e+05
# x0.X.stmrCMS             2.13e+03 2.91e+02  1.56e+03  2.70e+03
# x0.X.precCMS             1.93e+03 4.33e+02  1.08e+03  2.78e+03
# x0.X.evapCMS             4.19e+03 6.87e+02  2.85e+03  5.54e+03
# x0.X.rOffCMS             5.18e+02 5.10e+02 -4.81e+02  1.52e+03
# x0.X.dStoCMS            -2.75e+03 2.21e+03 -7.09e+03  1.58e+03
# C.(X.stmrCMS,Jan)        2.52e+02 4.71e+01  1.60e+02  3.44e+02
# C.(X.precCMS,Jan)        1.99e+02 1.29e+02 -5.27e+01  4.51e+02
# C.(X.evapCMS,Jan)        5.42e+02 5.23e+01  4.40e+02  6.45e+02
# C.(X.rOffCMS,Jan)        1.73e+02       NA        NA        NA
# C.(X.dStoCMS,Jan)       -8.46e+02 2.08e+02 -1.25e+03 -4.38e+02
# C.(X.stmrCMS,Feb)        3.29e+02 4.78e+01  2.35e+02  4.23e+02
# C.(X.precCMS,Feb)       -3.80e+02 1.05e+02 -5.85e+02 -1.74e+02
# C.(X.evapCMS,Feb)       -3.80e+02 6.75e+01 -5.12e+02 -2.47e+02
# C.(X.rOffCMS,Feb)        2.90e+02 7.57e+01  1.42e+02  4.38e+02
# C.(X.dStoCMS,Feb)       -4.78e+02 1.90e+02 -8.50e+02 -1.06e+02
# C.(X.stmrCMS,Mar)        3.12e+02 4.33e+01  2.27e+02  3.97e+02
# C.(X.precCMS,Mar)        2.99e+02 7.89e+01  1.44e+02  4.54e+02
# C.(X.evapCMS,Mar)        5.99e+01       NA        NA        NA
# C.(X.rOffCMS,Mar)        3.44e+02 7.05e+01  2.06e+02  4.82e+02
# C.(X.dStoCMS,Mar)        7.07e+02 1.73e+02  3.68e+02  1.05e+03
# C.(X.stmrCMS,Apr)        3.81e+02 4.58e+01  2.91e+02  4.71e+02
# C.(X.precCMS,Apr)        4.23e+02 1.17e+02  1.93e+02  6.52e+02
# C.(X.evapCMS,Apr)       -1.48e+02       NA        NA        NA
# C.(X.rOffCMS,Apr)        2.01e+03 6.58e+01  1.88e+03  2.14e+03
# C.(X.dStoCMS,Apr)        2.89e+03 1.73e+02  2.56e+03  3.23e+03
# C.(X.stmrCMS,May)        5.54e+02 4.73e+01  4.61e+02  6.47e+02
# C.(X.precCMS,May)        7.84e+02 1.39e+02  5.11e+02  1.06e+03
# C.(X.evapCMS,May)       -2.61e+02 9.88e+01 -4.54e+02 -6.69e+01
# C.(X.rOffCMS,May)        6.70e+02 7.05e+01  5.32e+02  8.08e+02
# C.(X.dStoCMS,May)        1.48e+03 2.36e+02  1.02e+03  1.95e+03
# C.(X.stmrCMS,Jun)        4.61e+02 4.55e+01  3.72e+02  5.50e+02
# C.(X.precCMS,Jun)        4.90e+02 1.44e+02  2.08e+02  7.73e+02
# C.(X.evapCMS,Jun)       -1.65e+02       NA        NA        NA
# C.(X.rOffCMS,Jun)       -3.55e+02 3.40e+01 -4.22e+02 -2.88e+02
# C.(X.dStoCMS,Jun)        6.94e+02 2.18e+02  2.66e+02  1.12e+03
# C.(X.stmrCMS,Jul)        4.55e+02 4.25e+01  3.72e+02  5.39e+02
# C.(X.precCMS,Jul)        3.05e+01       NA        NA        NA
# C.(X.evapCMS,Jul)       -1.22e+01       NA        NA        NA
# C.(X.rOffCMS,Jul)        5.60e+01       NA        NA        NA
# C.(X.dStoCMS,Jul)        8.47e+01 8.35e+01 -7.90e+01  2.48e+02
# C.(X.stmrCMS,Aug)        4.94e+02 4.68e+01  4.03e+02  5.86e+02
# C.(X.precCMS,Aug)        3.25e+02 2.00e+02 -6.72e+01  7.16e+02
# C.(X.evapCMS,Aug)        3.18e+02 7.08e+01  1.79e+02  4.57e+02
# C.(X.rOffCMS,Aug)        1.69e+01       NA        NA        NA
# C.(X.dStoCMS,Aug)       -4.53e+02 1.74e+02 -7.95e+02 -1.11e+02
# C.(X.stmrCMS,Sep)        3.68e+02 4.99e+01  2.70e+02  4.66e+02
# C.(X.precCMS,Sep)        6.26e+02 1.87e+02  2.59e+02  9.93e+02
# C.(X.evapCMS,Sep)        1.21e+03 6.62e+01  1.08e+03  1.34e+03
# C.(X.rOffCMS,Sep)        3.19e+02 6.00e+01  2.01e+02  4.36e+02
# C.(X.dStoCMS,Sep)       -7.55e+02 1.59e+02 -1.07e+03 -4.44e+02
# C.(X.stmrCMS,Oct)        3.15e+02 5.32e+01  2.11e+02  4.19e+02
# C.(X.precCMS,Oct)       -1.29e+02       NA        NA        NA
# C.(X.evapCMS,Oct)        1.19e+03 6.36e+01  1.07e+03  1.32e+03
# C.(X.rOffCMS,Oct)        5.85e+02 6.80e+01  4.52e+02  7.19e+02
# C.(X.dStoCMS,Oct)       -7.14e+02 1.62e+02 -1.03e+03 -3.96e+02
# C.(X.stmrCMS,Nov)        3.79e+02 5.08e+01  2.80e+02  4.79e+02
# C.(X.precCMS,Nov)       -9.88e+00       NA        NA        NA
# C.(X.evapCMS,Nov)        1.71e+03 5.98e+01  1.60e+03  1.83e+03
# C.(X.rOffCMS,Nov)        3.62e+02 4.25e+01  2.79e+02  4.45e+02
# C.(X.dStoCMS,Nov)       -1.07e+03 1.77e+02 -1.42e+03 -7.26e+02
# C.(X.stmrCMS,Dec)        2.53e+02 4.48e+01  1.65e+02  3.41e+02
# C.(X.precCMS,Dec)       -1.77e+01       NA        NA        NA
# C.(X.evapCMS,Dec)        1.41e+03 4.49e+01  1.32e+03  1.50e+03
# C.(X.rOffCMS,Dec)        6.94e+01       NA        NA        NA
# C.(X.dStoCMS,Dec)       -1.59e+03 1.94e+02 -1.97e+03 -1.21e+03
# 
# CIs calculated at alpha = 0.05 via method=hessian 

