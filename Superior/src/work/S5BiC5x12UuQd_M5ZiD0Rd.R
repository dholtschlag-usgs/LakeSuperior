# State-space model with 5 states and 5 measurements and fixed seasonal covariates

# Model ID:
#  S5BiC5x12UuQd_M5ZiD0Rd

# Lessons Learned: The fixed seasonal component contributes significantly to the
# model accuracy.  Process variance is erratic among components.  It is not 
# clear why some state components tract so well and why others do not. The
# magnitudes of the drift components look okay.  Measurement variance for
# stmrCMS is too low, others look better. 
# 

# Set path to target directory
setwd("C:/Home/Projects/GLWaterBudget/Analysis/Superior")

# source script to set up data files
source("Superior/src/work/setupMonthlyDataFrame.R")

# Specify time period of analysis
TT        <- 12*62  # 62 years of monthly data, max = 62 

# Create observation matrix
obsMatrix <- t(as.matrix(NBSrcDf[(1:TT)+1,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

# Setup fixed monthly component for model
# number of "seasons" (e.g., 12 months per year)
period = 12
# first "season" (e.g., Jan = 1, July = 7)
per.1st = 2
# create factors for seasons
c.in = diag(period)
for(i in 2:(ceiling((TT+1)/period))) {c.in = cbind(c.in,diag(period))}
# trim c.in to correct start & length
c.in = c.in[,(1:TT)+(per.1st-1)]
# better row names
rownames(c.in) = month.abb

# Load MARSS package
library(MARSS)

###################################################
### Specify model structure
###################################################
# State Equation Specifications
B      = "identity"              # No autoregressive component or interactions among x
U      = "unequal"               # Unique drift u values
C      = "unconstrained"         # Full state covariate parameters for season
# c.in   = "zero"                  # No state covariates
c.in   = c.in                    # Fixed season starting in February
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

s5BiC12x5_m5ZiD0.kem <- MARSS(obsMatrix, method="kem",
                           model=model.list, control=list(maxit=500))


start.time         <- Sys.time()
s5BiC0_m5ZiD0.BFGS <- MARSS(obsMatrix, method="BFGS",
                            model=model.list, control=cntl.list) 
                            # inits=s5BiC12x5_m5ZiD0.kem$par)
end.time           <- Sys.time()
print(end.time - start.time)

# Compute parameter confidence intervals
s5BiC0_m5ZiD0.BFGS.ci    <- MARSSparamCIs(s5BiC0_m5ZiD0.BFGS)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 506 iterations. 
# Log-likelihood: -28916.23 
# AIC: 57982.47   AICc: 57985.6   
# 
# ML.Est  Std.Err    low.CI     up.CI
# R.(stmrCMS,stmrCMS)      3.02e-01 6.85e-01  6.28e-01  3.58e+00
# R.(precCMS,precCMS)      5.81e+05 1.97e+01  5.23e+05  6.41e+05
# R.(evapCMS,evapCMS)      2.36e+05 1.38e+01  2.10e+05  2.63e+05
# R.(rOffCMS,rOffCMS)      1.71e+05 1.54e+01  1.47e+05  1.97e+05
# R.(dStoCMS,dStoCMS)      1.69e+06 3.39e+01  1.52e+06  1.86e+06
# U.X.stmrCMS             -1.82e-01 1.61e-01 -4.98e-01  1.33e-01
# U.X.precCMS             -3.44e-02       NA        NA        NA
# U.X.evapCMS             -5.90e-01       NA        NA        NA
# U.X.rOffCMS              3.98e-01 1.48e-01  1.07e-01  6.89e-01
# U.X.dStoCMS              1.08e-01       NA        NA        NA
# Q.(X.stmrCMS,X.stmrCMS)  6.24e+04 6.48e+00        NA        NA
# Q.(X.precCMS,X.precCMS)  5.04e-06       NA        NA        NA
# Q.(X.evapCMS,X.evapCMS)  3.31e+03 1.16e+01        NA        NA
# Q.(X.rOffCMS,X.rOffCMS)  1.82e+04 1.85e+01        NA        NA
# Q.(X.dStoCMS,X.dStoCMS)  6.87e-06 9.65e-03        NA        NA
# C.(X.stmrCMS,Jan)       -1.17e+02 3.36e+01 -1.82e+02 -5.07e+01
# C.(X.precCMS,Jan)        4.36e+01       NA        NA        NA
# C.(X.evapCMS,Jan)       -6.01e+02 8.46e+01 -7.67e+02 -4.36e+02
# C.(X.rOffCMS,Jan)       -1.38e+02 9.09e+01 -3.16e+02  3.99e+01
# C.(X.dStoCMS,Jan)        1.56e+01       NA        NA        NA
# C.(X.stmrCMS,Feb)       -1.91e+01 5.30e+01 -1.23e+02  8.48e+01
# C.(X.precCMS,Feb)       -5.45e+02 9.68e+01 -7.35e+02 -3.55e+02
# C.(X.evapCMS,Feb)       -1.35e+03 8.85e+01 -1.52e+03 -1.17e+03
# C.(X.rOffCMS,Feb)       -1.78e+01 1.17e+01 -4.06e+01  5.09e+00
# C.(X.dStoCMS,Feb)        6.13e+02 2.04e+02  2.14e+02  1.01e+03
# C.(X.stmrCMS,Mar)       -3.34e+01 3.19e+01 -9.59e+01  2.91e+01
# C.(X.precCMS,Mar)        1.67e+02 4.79e+01  7.30e+01  2.61e+02
# C.(X.evapCMS,Mar)       -5.33e+02 9.28e+01 -7.15e+02 -3.51e+02
# C.(X.rOffCMS,Mar)        1.98e+02 6.95e+01  6.18e+01  3.34e+02
# C.(X.dStoCMS,Mar)        1.52e+03 1.99e+02  1.13e+03  1.91e+03
# C.(X.stmrCMS,Apr)        4.35e+01 2.82e+01 -1.18e+01  9.87e+01
# C.(X.precCMS,Apr)        2.33e+02 1.31e+02 -2.35e+01  4.90e+02
# C.(X.evapCMS,Apr)       -5.14e+02 9.31e+01 -6.97e+02 -3.32e+02
# C.(X.rOffCMS,Apr)        1.61e+03 6.70e+01  1.48e+03  1.74e+03
# C.(X.dStoCMS,Apr)        2.74e+03 2.36e+02  2.28e+03  3.20e+03
# C.(X.stmrCMS,May)        2.14e+02 3.15e+01  1.52e+02  2.75e+02
# C.(X.precCMS,May)        5.31e+02 1.50e+02  2.38e+02  8.24e+02
# C.(X.evapCMS,May)       -4.42e+02 1.08e+02 -6.53e+02 -2.30e+02
# C.(X.rOffCMS,May)        3.19e+01 2.04e+01 -8.17e+00  7.19e+01
# C.(X.dStoCMS,May)        4.20e+02 2.93e+02 -1.55e+02  9.96e+02
# C.(X.stmrCMS,Jun)        7.98e+01 3.12e+01  1.87e+01  1.41e+02
# C.(X.precCMS,Jun)        2.60e+02 1.36e+02 -6.95e+00  5.27e+02
# C.(X.evapCMS,Jun)       -2.15e+02 1.00e+02 -4.11e+02 -1.77e+01
# C.(X.rOffCMS,Jun)       -1.03e+03 6.60e+01 -1.16e+03 -8.98e+02
# C.(X.dStoCMS,Jun)       -5.94e+02 2.78e+02 -1.14e+03 -4.88e+01
# C.(X.stmrCMS,Jul)        5.97e+01 2.95e+01  1.78e+00  1.18e+02
# C.(X.precCMS,Jul)       -1.53e+02 1.47e+02 -4.41e+02  1.36e+02
# C.(X.evapCMS,Jul)        1.63e+01       NA        NA        NA
# C.(X.rOffCMS,Jul)       -4.56e+02 7.53e+01 -6.04e+02 -3.09e+02
# C.(X.dStoCMS,Jul)       -9.43e+02 2.39e+02 -1.41e+03 -4.75e+02
# C.(X.stmrCMS,Aug)        8.93e+01 3.22e+01  2.62e+01  1.52e+02
# C.(X.precCMS,Aug)        6.17e+01 4.66e+01 -2.96e+01  1.53e+02
# C.(X.evapCMS,Aug)        3.63e+02 7.94e+01  2.07e+02  5.18e+02
# C.(X.rOffCMS,Aug)       -2.87e+02 6.64e+01 -4.17e+02 -1.57e+02
# C.(X.dStoCMS,Aug)       -1.02e+03 1.88e+02 -1.39e+03 -6.53e+02
# C.(X.stmrCMS,Sep)       -5.64e+01 3.51e+01 -1.25e+02  1.24e+01
# C.(X.precCMS,Sep)        2.94e+02 1.65e+02 -3.03e+01  6.18e+02
# C.(X.evapCMS,Sep)        1.09e+03 8.79e+01  9.19e+02  1.26e+03
# C.(X.rOffCMS,Sep)        5.50e+01 2.05e+01  1.48e+01  9.52e+01
# C.(X.dStoCMS,Sep)       -8.29e+02 1.09e+02 -1.04e+03 -6.16e+02
# C.(X.stmrCMS,Oct)       -1.01e+02 3.34e+01 -1.66e+02 -3.54e+01
# C.(X.precCMS,Oct)       -3.76e+02 1.18e+02 -6.08e+02 -1.45e+02
# C.(X.evapCMS,Oct)        8.06e+02 8.72e+01  6.35e+02  9.77e+02
# C.(X.rOffCMS,Oct)        2.73e+02 5.43e+01  1.67e+02  3.80e+02
# C.(X.dStoCMS,Oct)       -4.55e+02 2.61e+02 -9.67e+02  5.76e+01
# C.(X.stmrCMS,Nov)       -1.66e+01 2.49e+01 -6.53e+01  3.22e+01
# C.(X.precCMS,Nov)       -2.32e+02 1.21e+02 -4.69e+02  5.09e+00
# C.(X.evapCMS,Nov)        1.01e+03 8.93e+01  8.31e+02  1.18e+03
# C.(X.rOffCMS,Nov)        9.83e+00 5.41e+00 -7.76e-01  2.04e+01
# C.(X.dStoCMS,Nov)       -6.16e+02 2.41e+02 -1.09e+03 -1.42e+02
# C.(X.stmrCMS,Dec)       -1.43e+02 3.28e+01 -2.08e+02 -7.91e+01
# C.(X.precCMS,Dec)       -2.84e+02 1.10e+02 -5.00e+02 -6.83e+01
# C.(X.evapCMS,Dec)        3.69e+02 8.63e+01  2.00e+02  5.39e+02
# C.(X.rOffCMS,Dec)       -2.50e+02 8.09e+01 -4.09e+02 -9.19e+01
# C.(X.dStoCMS,Dec)       -8.51e+02 1.22e+02 -1.09e+03 -6.11e+02
