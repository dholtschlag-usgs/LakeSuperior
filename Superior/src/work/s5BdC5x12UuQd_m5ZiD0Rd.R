# State-space model with 5 states and 5 measurements, fixed seasons, and AR(1)

# Lessons Learned: Estimating a mean reverting process with a diagonal B matrix,
# fixed seasonal component, diagonal Q and R matrices, drift (level) U vector 
# unconstrained (and hard to interpret), specified initial conditions and 
# uncertainty. Numerical difficulties maybe associated with estimation on
# original units rather than z-scores.

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
# B      = "identity"              # No autoregressive component or interactions among x
B      = "diagonal and unequal"
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

s5BdC12x5_m5ZiD0.kem <- MARSS(obsMatrix, method="kem",
                              model=model.list, control=list(maxit=500))

# Estimate with BFGS
start.time         <- Sys.time()
s5BdC5x12UuQd_m5ZiD0Rd.BFGS <- MARSS(obsMatrix, method="BFGS",
                            model=model.list, control=cntl.list) 
end.time           <- Sys.time()
print(end.time - start.time)

# Compute parameter confidence intervals
s5BdC5x12UuQd_m5ZiD0Rd.BFGS.ci    <- MARSSparamCIs(s5BdC5x12UuQd_m5ZiD0Rd.BFGS)

s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kf    <- MARSSkf(s5BdC5x12UuQd_m5ZiD0Rd.BFGS)

names(s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kf)

s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kfss  <- MARSSkfss(s5BdC5x12UuQd_m5ZiD0Rd.BFGS)

compName <- c("St. Marys River Streamflow", "Overlake Precipitation",
              "Overlake Evaporation", "Basin Runoff",
              "Change in Lake Storage")

for (i in 1:5){
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[(1:TT)+1,"DateSeq"], s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kfss$xtt1[i,],
       type="l",col="blue", xlab="Year", ylab=paste0(compName[i],", in CMS"),
       main=paste0("Monthly Estimates of ",compName[i]," State"))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[(1:TT)+1,"DateSeq"], s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kfss$xtt[i,],
        type="l",col="red")
  # lines(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,"stmrCMS"],
  #       type="l",col="green")
  # Compute the difference between the two state estimates
  xtt1_xtt <- s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kfss$xtt1[i,] -
    s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kfss$xtt[i,]
  legend("topright",legend=c("x[t]|[t-1]","x[t]|[t]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("The variance of x[t]|[t-1] - x[t]|[t] equals Q",i,i," =",
                   format(var(xtt1_xtt),digits=5)))
  
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[(1:TT)+1,"DateSeq"], s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kfss$xtt1[i,],
       type="l",col="blue", xlab="Year", ylab=paste0(compName[i],", in CMS"),
       main=paste0("Monthly Estimates of ",compName[i]," State"))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[(1:TT)+1,"DateSeq"], s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kfss$xtT[i,],
        type="l",col="red")
  # lines(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,"stmrCMS"],
  #       type="l",col="green")
  # Compute the difference between the two state estimates
  xtt1_xtT <- s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kfss$xtt1[i,] -
    s5BdC5x12UuQd_m5ZiD0Rd.BFGS.kfss$xtT[i,]
  legend("topright",legend=c("x[t]|[t-1]","x[t]|[t]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("The variance of x[t]|[t-1] - x[t]|[T] equals equals Q",i,i," =",
                   format(var(xtt1_xtT),digits=5)))
}

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 845 iterations. 
# Log-likelihood: -28808.57 
# AIC: 57777.15   AICc: 57780.71   
# 
# ML.Est  Std.Err    low.CI     up.CI
# R.(stmrCMS,stmrCMS)      1.42e-03       NA        NA        NA
# R.(precCMS,precCMS)      4.61e+05  94.4594        NA        NA
# R.(evapCMS,evapCMS)      1.63e+03       NA        NA        NA
# R.(rOffCMS,rOffCMS)      8.27e+04  43.2958        NA        NA
# R.(dStoCMS,dStoCMS)      1.66e+06  33.2793        NA        NA
# B.(X.stmrCMS,X.stmrCMS)  8.59e-01   0.0187  8.22e-01     0.896
# B.(X.precCMS,X.precCMS)  2.14e-01   0.0706  7.58e-02     0.353
# B.(X.evapCMS,X.evapCMS)  4.11e-01   0.0341  3.44e-01     0.478
# B.(X.rOffCMS,X.rOffCMS)  6.16e-01   0.0757  4.67e-01     0.764
# B.(X.dStoCMS,X.dStoCMS) -1.56e-01       NA        NA        NA
# U.X.stmrCMS              2.84e+02  47.5302  1.91e+02   377.041
# U.X.precCMS              1.48e+03 210.1419  1.07e+03  1896.243
# U.X.evapCMS              7.61e+02       NA        NA        NA
# U.X.rOffCMS              5.59e+02 100.6570  3.62e+02   756.771
# U.X.dStoCMS             -1.16e+01   6.6906 -2.48e+01     1.464
# Q.(X.stmrCMS,X.stmrCMS)  5.81e+04   6.2447        NA        NA
# Q.(X.precCMS,X.precCMS)  1.17e+05 184.3138        NA        NA
# Q.(X.evapCMS,X.evapCMS)  2.40e+05  11.4877        NA        NA
# Q.(X.rOffCMS,X.rOffCMS)  1.13e+05  46.4898        NA        NA
# Q.(X.dStoCMS,X.dStoCMS)  3.42e+02       NA        NA        NA
# C.(X.stmrCMS,Jan)       -1.02e+02  35.2053 -1.71e+02   -32.570
# C.(X.precCMS,Jan)       -6.82e+01  54.8452 -1.76e+02    39.340
# C.(X.evapCMS,Jan)        6.90e+02       NA        NA        NA
# C.(X.rOffCMS,Jan)       -2.14e+02  69.1341 -3.50e+02   -78.894
# C.(X.dStoCMS,Jan)       -2.52e+03       NA        NA        NA
# C.(X.stmrCMS,Feb)       -2.05e+01  26.0262 -7.15e+01    30.504
# C.(X.precCMS,Feb)       -6.09e+02  92.5619 -7.91e+02  -427.997
# C.(X.evapCMS,Feb)       -3.53e+02       NA        NA        NA
# C.(X.rOffCMS,Feb)       -1.67e+02  70.7286 -3.06e+02   -28.360
# C.(X.dStoCMS,Feb)       -2.03e+03       NA        NA        NA
# C.(X.stmrCMS,Mar)       -3.94e+01  38.3963 -1.15e+02    35.904
# C.(X.precCMS,Mar)       -2.85e+02 116.7569 -5.14e+02   -56.513
# C.(X.evapCMS,Mar)       -3.38e+02       NA        NA        NA
# C.(X.rOffCMS,Mar)       -5.36e+00       NA        NA        NA
# C.(X.dStoCMS,Mar)       -6.65e+02       NA        NA        NA
# C.(X.stmrCMS,Apr)        3.10e+01  32.0345 -3.18e+01    93.815
# C.(X.precCMS,Apr)       -1.22e+02       NA        NA        NA
# C.(X.evapCMS,Apr)       -6.33e+02       NA        NA        NA
# C.(X.rOffCMS,Apr)        1.56e+03  66.6709  1.43e+03  1693.070
# C.(X.dStoCMS,Apr)        2.65e+03 161.5069  2.34e+03  2971.430
# C.(X.stmrCMS,May)        2.09e+02  37.2777  1.36e+02   281.933
# C.(X.precCMS,May)        3.89e+02 123.9558  1.46e+02   631.766
# C.(X.evapCMS,May)       -8.77e+02       NA        NA        NA
# C.(X.rOffCMS,May)        5.62e+02 139.9736  2.88e+02   836.666
# C.(X.dStoCMS,May)        3.31e+03       NA        NA        NA
# C.(X.stmrCMS,Jun)        1.04e+02  35.0339  3.53e+01   172.648
# C.(X.precCMS,Jun)        5.36e+02  66.7093  4.06e+02   667.078
# C.(X.evapCMS,Jun)       -9.07e+02       NA        NA        NA
# C.(X.rOffCMS,Jun)       -4.85e+02 133.5447 -7.47e+02  -223.324
# C.(X.dStoCMS,Jun)        2.76e+03       NA        NA        NA
# C.(X.stmrCMS,Jul)        9.59e+01  35.0793  2.72e+01   164.668
# C.(X.precCMS,Jul)        3.10e+02       NA        NA        NA
# C.(X.evapCMS,Jul)       -7.98e+02  28.5827 -8.54e+02  -742.119
# C.(X.rOffCMS,Jul)       -2.69e+02  78.9219 -4.24e+02  -114.456
# C.(X.dStoCMS,Jul)        1.73e+03       NA        NA        NA
# C.(X.stmrCMS,Aug)        1.36e+02  35.4509  6.67e+01   205.679
# C.(X.precCMS,Aug)        4.06e+02       NA        NA        NA
# C.(X.evapCMS,Aug)       -4.33e+02  29.5546 -4.91e+02  -374.873
# C.(X.rOffCMS,Aug)       -3.08e+02  43.9821 -3.95e+02  -222.252
# C.(X.dStoCMS,Aug)        5.66e+02  95.1188  3.80e+02   752.553
# C.(X.stmrCMS,Sep)        3.21e+00       NA        NA        NA
# C.(X.precCMS,Sep)        7.16e+02  80.4110  5.59e+02   873.712
# C.(X.evapCMS,Sep)        5.15e+02       NA        NA        NA
# C.(X.rOffCMS,Sep)       -7.35e+01       NA        NA        NA
# C.(X.dStoCMS,Sep)       -4.76e+02 187.1073 -8.42e+02  -108.798
# C.(X.stmrCMS,Oct)       -4.91e+01  30.1442 -1.08e+02     9.943
# C.(X.precCMS,Oct)        2.37e+02       NA        NA        NA
# C.(X.evapCMS,Oct)        8.63e+02       NA        NA        NA
# C.(X.rOffCMS,Oct)        1.88e+02  47.5540  9.45e+01   280.920
# C.(X.dStoCMS,Oct)       -1.03e+03 153.8756 -1.33e+03  -726.161
# C.(X.stmrCMS,Nov)        2.28e+01  26.4871 -2.91e+01    74.753
# C.(X.precCMS,Nov)        1.23e+02       NA        NA        NA
# C.(X.evapCMS,Nov)        1.53e+03       NA        NA        NA
# C.(X.rOffCMS,Nov)        2.03e+01       NA        NA        NA
# C.(X.dStoCMS,Nov)       -1.66e+03 130.3263 -1.91e+03 -1399.879
# C.(X.stmrCMS,Dec)       -1.08e+02  33.4641 -1.73e+02   -42.007
# C.(X.precCMS,Dec)       -1.49e+02       NA        NA        NA
# C.(X.evapCMS,Dec)        1.50e+03       NA        NA        NA
# C.(X.rOffCMS,Dec)       -2.50e+02  68.9840 -3.86e+02  -115.149
# C.(X.dStoCMS,Dec)       -2.66e+03       NA        NA        NA

# There were 50 or more warnings (use warnings() to see the first 50)