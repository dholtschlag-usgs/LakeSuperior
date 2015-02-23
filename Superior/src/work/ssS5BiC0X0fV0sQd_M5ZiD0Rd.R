# Simple model with 5 states and 5 measurements

# Lessons Learned:
# Estimating a very simple model as random walk process with no covariates or season
#   Both process and meaurement variance estimated as is drift component

# Set path to target directory
setwd("C:/Home/Projects/GLWaterBudget/Analysis/Superior")

# source script to set up data files
source("Superior/src/work/setupMonthlyDataFrame.R")

# Specify time period of analysis
TT        <- 12*62  # 62 years of monthly data, max = 62 

# Create observation matrix
obsMatrix <- t(as.matrix(NBSrcDf[(1:TT)+1,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

# Load MARSS package
library(MARSS)


###################################################
### Specify model structure
###################################################
# State Equation Specifications
B      = "identity"              # No autoregressive component or interactions among x
U      = "unequal"               # Unique drift u values
C      = "zero"                  # No state covariate parameters
c.in   = "zero"                  # No state covariates
Q      = "diagonal and unequal"  # Process errors are independent but have different variances             # Initial states are different unknowns
x0     = matrix(c(NBSrcDf$stmrCMS[1],NBSrcDf$precCMS[1],NBSrcDf$evapCMS[1],
                  NBSrcDf$rOffCMS[1],NBSrcDf$dStoCMS[1]),5,1)                  # Initial states are fixed but unknown
V0     <- diag(40000,5)
# Measurement Equation Specifications
Z      = "identity"              # Each state identically corresponds to one measurement
A      = "zero"                  # Measurement bias is zero  
D      = "zero"                  # No measurement covariate parameters
d      = "zero"                  # No measurement covariates
R      = "diagonal and unequal"  # Measurement errors are unique unknowns and independent
tinitx = 0                       # The initial state refers to t=0 instead of t=1

#
model.list <- list(B=B, U=U, C=C, c=c.in, Q=Q, x0=x0, V0=V0,
                   Z=Z, A=A, D=D, d=d   , R=R, tinitx=tinitx)

cntl.list  <- list(maxit=1000)  
# 

s5BiC0_m5ZiD0.kem <- MARSS(obsMatrix, method="kem",
                       model=model.list, control=list(maxit=500))


start.time         <- Sys.time()
s5BiC0_m5ZiD0.BFGS <- MARSS(obsMatrix, method="BFGS",
                           model=model.list, control=cntl.list, 
                           inits=s5BiC0_m5ZiD0.kem$par)
end.time           <- Sys.time()
print(end.time - start.time)

# Compute parameter confidence intervals
s5BiC0_m5ZiD0.ci    <- MARSSparamCIs(s5BiC0_m5ZiD0.BFGS)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 73 iterations. 
# Log-likelihood: -30211.61 
# AIC: 60453.21   AICc: 60453.34   
# 
# ML.Est  Std.Err    low.CI    up.CI
# R.(stmrCMS,stmrCMS)      6.02e-01       NA        NA       NA
# R.(precCMS,precCMS)      7.63e+05 2.27e+01        NA       NA
# R.(evapCMS,evapCMS)      1.74e+02       NA        NA       NA
# R.(rOffCMS,rOffCMS)      6.89e+04 8.15e+01        NA       NA
# R.(dStoCMS,dStoCMS)      6.18e+05 1.06e+02        NA       NA
# U.X.stmrCMS             -1.87e-01 9.55e-02 -0.374177 3.05e-05
# U.X.precCMS              2.59e-03 1.67e-03 -0.000687 5.86e-03
# U.X.evapCMS             -1.12e+00       NA        NA       NA
# U.X.rOffCMS              6.05e-01       NA        NA       NA
# U.X.dStoCMS             -1.43e-01       NA        NA       NA
# Q.(X.stmrCMS,X.stmrCMS)  7.19e+04 6.95e+00        NA       NA
# Q.(X.precCMS,X.precCMS)  8.33e-06       NA        NA       NA
# Q.(X.evapCMS,X.evapCMS)  8.48e+05 2.39e+01        NA       NA
# Q.(X.rOffCMS,X.rOffCMS)  5.28e+05 5.56e+01        NA       NA
# Q.(X.dStoCMS,X.dStoCMS)  3.03e+06 9.16e+01        NA       NA
# 

# Lessons learned: Specifying uncertain (V0 = diag(40000,5)) but known initial 
#  conditions causes problems computing parameter uncertainties.

###################################################
### Specify model structure
###################################################
# State Equation Specifications
B      = "identity"              # No autoregressive component or interactions among x
U      = "unequal"               # Unique drift u values
C      = "zero"                  # No state covariate parameters
c.in   = "zero"                  # No state covariates
Q      = "diagonal and unequal"  # Process errors are independent but have different variances
#x0     = "unequal"               # Initial states are different unknowns
x0     = matrix(c(NBSrcDf$stmrCMS[1],NBSrcDf$precCMS[1],NBSrcDf$evapCMS[1],
                  NBSrcDf$rOffCMS[1],NBSrcDf$dStoCMS[1]),5,1)
V0     = "zero"                  # Initial states are fixed but unknown
# Measurement Equation Specifications
Z      = "identity"              # Each state identically corresponds to one measurement
A      = "zero"                  # Measurement bias is zero  
D      = "zero"                  # No measurement covariate parameters
d      = "zero"                  # No measurement covariates
R      = "diagonal and unequal"  # Measurement errors are unique unknowns and independent
tinitx = 0                       # The initial state refers to t=0 instead of t=1

#
model.list <- list(B=B, U=U, C=C, c=c.in, Q=Q, x0=x0, V0=V0,
                   Z=Z, A=A, D=D, d=d   , R=R, tinitx=tinitx)

cntl.list  <- list(maxit=1000)  
# 

s5BiC0_m5ZiD0.kem <- MARSS(obsMatrix, method="kem",
                           model=model.list, control=list(maxit=500))


start.time         <- Sys.time()
s5BiC0_m5ZiD0.BFGS <- MARSS(obsMatrix, method="BFGS",
                            model=model.list, control=cntl.list, 
                            inits=s5BiC0_m5ZiD0.kem$par)
end.time           <- Sys.time()
print(end.time - start.time)

# Compute parameter confidence intervals
s5BiC0_m5ZiD0.ci    <- MARSSparamCIs(s5BiC0_m5ZiD0.BFGS)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 48 iterations. 
# Log-likelihood: -30210.1 
# AIC: 60450.21   AICc: 60450.34   
# 
# ML.Est  Std.Err low.CI   up.CI
# R.(stmrCMS,stmrCMS)      2.26e-03       NA     NA      NA
# R.(precCMS,precCMS)      7.67e+05  22.7282     NA      NA
# R.(evapCMS,evapCMS)      5.67e-01   0.1982     NA      NA
# R.(rOffCMS,rOffCMS)      6.89e+04  13.7586     NA      NA
# R.(dStoCMS,dStoCMS)      6.18e+05 100.1407     NA      NA
# U.X.stmrCMS             -2.06e-01       NA     NA      NA
# U.X.precCMS             -1.37e-01   0.0902 -0.313  0.0401
# U.X.evapCMS             -1.19e+00   0.4892 -2.147 -0.2292
# U.X.rOffCMS              6.11e-01       NA     NA      NA
# U.X.dStoCMS             -1.35e-01       NA     NA      NA
# Q.(X.stmrCMS,X.stmrCMS)  7.19e+04   6.9445     NA      NA
# Q.(X.precCMS,X.precCMS)  8.94e-09       NA     NA      NA
# Q.(X.evapCMS,X.evapCMS)  8.48e+05  23.8672     NA      NA
# Q.(X.rOffCMS,X.rOffCMS)  5.27e+05  30.0552     NA      NA
# Q.(X.dStoCMS,X.dStoCMS)  3.03e+06  88.0771     NA      NA

# Lessons learned: Specifying fixed (V0="zero") but known initial conditions causes
# problems estimating parameter uncertainties

###################################################
### Specify model structure
###################################################
# State Equation Specifications
B      = "identity"              # No autoregressive component or interactions among x
U      = "unequal"               # Unique drift u values
C      = "zero"                  # No state covariate parameters
c.in   = "zero"                  # No state covariates
Q      = "diagonal and unequal"  # Process errors are independent but have different variances
x0     = "unequal"               # Initial states are different unknowns
V0     = "zero"                  # Initial states are fixed but unknown
# Measurement Equation Specifications
Z      = "identity"              # Each state identically corresponds to one measurement
A      = "zero"                  # Measurement bias is zero  
D      = "zero"                  # No measurement covariate parameters
d      = "zero"                  # No measurement covariates
R      = "diagonal and unequal"  # Measurement errors are unique unknowns and independent
tinitx = 0                       # The initial state refers to t=0 instead of t=1

#
model.list <- list(B=B, U=U, C=C, c=c.in, Q=Q, x0=x0, V0=V0,
                   Z=Z, A=A, D=D, d=d   , R=R, tinitx=tinitx)

cntl.list  <- list(maxit=1000)  
# 

s5BiC0_m5ZiD0.kem <- MARSS(obsMatrix, method="kem",
                           model=model.list, control=list(maxit=500))


start.time         <- Sys.time()
s5BiC0_m5ZiD0.BFGS <- MARSS(obsMatrix, method="BFGS",
                            model=model.list, control=cntl.list, 
                            inits=s5BiC0_m5ZiD0.kem$par)
end.time           <- Sys.time()
print(end.time - start.time)


# Compute parameter confidence intervals
s5BiC0_m5ZiD0.ci    <- MARSSparamCIs(s5BiC0_m5ZiD0.BFGS)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 46 iterations. 
# Log-likelihood: -30208.7 
# AIC: 60457.41   AICc: 60457.64   
# 
# ML.Est Std.Err low.CI up.CI
# R.(stmrCMS,stmrCMS)      4.49e-04      NA     NA    NA
# R.(precCMS,precCMS)      7.64e+05      NA     NA    NA
# R.(evapCMS,evapCMS)      1.29e-04      NA     NA    NA
# R.(rOffCMS,rOffCMS)      6.91e+04      NA     NA    NA
# R.(dStoCMS,dStoCMS)      6.21e+05      NA     NA    NA
# U.X.stmrCMS             -1.62e-01      NA     NA    NA
# U.X.precCMS              4.62e-03      NA     NA    NA
# U.X.evapCMS              2.29e-01      NA     NA    NA
# U.X.rOffCMS              5.17e-01      NA     NA    NA
# U.X.dStoCMS             -8.38e-01      NA     NA    NA
# Q.(X.stmrCMS,X.stmrCMS)  7.19e+04      NA     NA    NA
# Q.(X.precCMS,X.precCMS)  1.55e-09      NA     NA    NA
# Q.(X.evapCMS,X.evapCMS)  8.47e+05      NA     NA    NA
# Q.(X.rOffCMS,X.rOffCMS)  5.27e+05      NA     NA    NA
# Q.(X.dStoCMS,X.dStoCMS)  3.03e+06      NA     NA    NA
# x0.X.stmrCMS             2.12e+03      NA     NA    NA
# x0.X.precCMS             2.05e+03      NA     NA    NA
# x0.X.evapCMS             3.01e+03      NA     NA    NA
# x0.X.rOffCMS             6.70e+02      NA     NA    NA
# x0.X.dStoCMS            -2.24e+03      NA     NA    NA

# Lessons learned: Specifying fixed (V0="zero") but unknown initial conditions 
# causes problems estimating parameter uncertainties.  Of the above options,
# however, specifed uncertain initial state conditions seems to be the best option
# in terms of plausibility and parameter uncertainties, although AICc is not minimum.

