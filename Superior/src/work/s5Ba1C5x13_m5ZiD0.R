# State-space model with 5 states and 5 measurements, fixed seasons, and AR(1)

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
# Augment c.in with Long Lac diversion
c.ina <- rbind(c.in,NBSrcDf[(1:TT)+1,"lLacCMS"])

# Descriptive row names
rownames(c.ina) = c(month.abb,"lLacCMS")
# Load MARSS package
library(MARSS)

###################################################
### Specify model structure
###################################################
# State Equation Specifications
# B matrix to pick up any intra-component and interactions between precip and runoff
B  <- matrix(list("B0101",0      ,0      ,0      ,0      ,
                  0      ,"B0202",0      ,"B0204",0      ,
                  0      ,0      ,"B0303",0      ,0      ,
                  0      ,"B0402",0      ,"B0404",0      ,
                  0      ,0      ,0      ,0      ,"B0505"),
             5,5,byrow=TRUE)

U      = "unequal"               # Unique drift u values
# Full state covariate parameters for season while 
C  <- matrix(list("C0101","C0102","C0103","C0104","C0105","C0106","C0107","C0108","C0109","C0110","C0111","C0112",0,
                  "C0101","C0202","C0203","C0204","C0205","C0206","C0207","C0208","C0209","C0210","C0211","C0212",0,
                  "C0301","C0302","C0303","C0304","C0305","C0306","C0307","C0308","C0309","C0310","C0311","C0312",0,
                  "C0401","C0402","C0403","C0404","C0405","C0406","C0407","C0408","C0409","C0410","C0411","C0412",0,
                  "C0501","C0502","C0503","C0504","C0505","C0506","C0507","C0508","C0509","C0510","C0511","C0512","C0513"),
             5,13,byrow=TRUE)

c.in   = c.ina                   # Fixed season starting in February augmented with Long Lac
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

# s5BdC5x13_m5ZiD0.kem <- MARSS(obsMatrix, method="kem",
#                               model=model.list, control=list(maxit=500))
# Stopped at iter=1 in MARSSkem at U update. denom is not invertible.
# par, kf, states, iter, loglike are the last values before the error.
# Try control$safe=TRUE which uses a slower but slightly more robust algorithm.
# Use control$trace=1 to generate a more detailed error report. See user guide for insight.


start.time            <- Sys.time()
s5Ba1C5x13_m5ZiD0.BFGS <- MARSS(obsMatrix, method="BFGS",
                               model=model.list, control=cntl.list) 
# inits=s5BiC12x5_m5ZiD0.kem$par)
end.time           <- Sys.time()
print(end.time - start.time)

# Compute parameter confidence intervals
s5Ba1C5x13_m5ZiD0.ci    <- MARSSparamCIs(s5Ba1C5x13_m5ZiD0.BFGS)

# Use MARSSkf to compute Kalman statistics
s5Ba1C5x13_m5ZiD0.BFGS.kfss    <- MARSSkfss(s5Ba1C5x13_m5ZiD0.BFGS)

compName <- c("St. Marys River Streamflow", "Overlake Precipitation",
              "Overlake Evaporation", "Basin Runoff",
              "Change in Lake Storage")

xtt1 <- s5Ba1C5x13_m5ZiD0.BFGS.kfss$xtt1
xtt  <- s5Ba1C5x13_m5ZiD0.BFGS.kfss$xtt
xtT  <- s5Ba1C5x13_m5ZiD0.BFGS.kfss$xtT

for (i in 1:5){
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[(1:TT)+1,"DateSeq"], xtt1[i,],
       type="l",col="blue", xlab="Year", ylab=paste0(compName[i],", in CMS"),
       main=paste0("Monthly Estimates of ",compName[i]," State"))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[(1:TT)+1,"DateSeq"], xtt[i,],
        type="l",col="red")
  # lines(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,"stmrCMS"],
  #       type="l",col="green")
  # Compute the difference between the two state estimates
  xtt1_xtt <- xtt1[i,] - xtt[i,]
  legend("topright",legend=c("x[t]|[t-1]","x[t]|[t]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("The variance of x[t]|[t-1] - x[t]|[t] equals Q",i,i," =",
                   format(var(xtt1_xtt),digits=5)))
  
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[(1:TT)+1,"DateSeq"], xtt1[i,],
       type="l",col="blue", xlab="Year", ylab=paste0(compName[i],", in CMS"),
       main=paste0("Monthly Estimates of ",compName[i]," State"))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[(1:TT)+1,"DateSeq"], xtT[i,],
        type="l",col="red")
  # lines(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,"stmrCMS"],
  #       type="l",col="green")
  # Compute the difference between the two state estimates
  xtt1_xtT <- xtt1[i,] - xtT[i,]
  legend("topright",legend=c("x[t]|[t-1]","x[t]|[t]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("The variance of x[t]|[t-1] - x[t]|[T] equals equals Q",i,i," =",
                   format(var(xtt1_xtT),digits=5)))
}

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 1264 iterations. 
# Log-likelihood: -28763.56 
# AIC: 57691.12   AICc: 57694.86   
# 
# ML.Est  Std.Err    low.CI     up.CI
# R.(stmrCMS,stmrCMS)      8.59e-02       NA        NA        NA
# R.(precCMS,precCMS)      2.19e+05 8.38e+01        NA        NA
# R.(evapCMS,evapCMS)      1.55e+02 1.19e+01        NA        NA
# R.(rOffCMS,rOffCMS)      1.47e+05 3.58e+01        NA        NA
# R.(dStoCMS,dStoCMS)      1.05e+06 1.48e+02        NA        NA
# B.B0101                  8.60e-01 1.92e-02  8.23e-01  8.98e-01
# B.B0202                  6.04e-03 8.28e-03 -1.02e-02  2.23e-02
# B.B0402                  2.72e-01 4.67e-02  1.80e-01  3.63e-01
# B.B0303                  4.04e-01 4.01e-02  3.25e-01  4.82e-01
# B.B0204                  3.38e-02 1.97e-02 -4.78e-03  7.23e-02
# B.B0404                  7.21e-01 8.32e-02  5.58e-01  8.84e-01
# B.B0505                  1.67e-01 6.08e-02  4.81e-02  2.87e-01
# U.X.stmrCMS              2.91e+02 5.06e+01  1.92e+02  3.90e+02
# U.X.precCMS              1.83e+03 7.20e+01  1.69e+03  1.98e+03
# U.X.evapCMS              7.72e+02 3.88e+02  1.22e+01  1.53e+03
# U.X.rOffCMS             -1.07e+02       NA        NA        NA
# U.X.dStoCMS             -5.62e+02 6.10e+01 -6.81e+02 -4.42e+02
# Q.(X.stmrCMS,X.stmrCMS)  5.81e+04 6.25e+00  5.23e+04  6.41e+04
# Q.(X.precCMS,X.precCMS)  3.56e+05 6.67e+01  2.17e+05  5.30e+05
# Q.(X.evapCMS,X.evapCMS)  2.42e+05 1.28e+01  2.18e+05  2.67e+05
# Q.(X.rOffCMS,X.rOffCMS)  2.11e+04 7.10e+01  3.88e+01  8.10e+04
# Q.(X.dStoCMS,X.dStoCMS)  5.48e+05 1.99e+02  1.23e+05  1.28e+06
# C.C0101                 -1.05e+02 5.83e+01 -2.20e+02  9.00e+00
# C.C0301                  7.23e+02 4.74e+02 -2.07e+02  1.65e+03
# C.C0401                 -1.55e+02 1.45e+02 -4.39e+02  1.29e+02
# C.C0501                 -1.77e+03 2.19e+02 -2.20e+03 -1.34e+03
# C.C0102                 -3.15e+01 3.09e+01 -9.21e+01  2.92e+01
# C.C0202                 -6.65e+02 1.38e+02 -9.37e+02 -3.94e+02
# C.C0302                 -3.44e+02 4.71e+02 -1.27e+03  5.79e+02
# C.C0402                 -8.51e+01 1.26e+02 -3.32e+02  1.62e+02
# C.C0502                 -1.41e+03 2.06e+02 -1.82e+03 -1.01e+03
# C.C0103                 -4.66e+01 4.04e+01 -1.26e+02  3.27e+01
# C.C0203                 -4.54e+02 1.29e+02 -7.08e+02 -2.00e+02
# C.C0303                 -3.10e+02 4.30e+02 -1.15e+03  5.32e+02
# C.C0403                  2.32e+02 9.02e+01  5.54e+01  4.09e+02
# C.C0503                 -1.25e+02 6.08e+01 -2.44e+02 -5.88e+00
# C.C0104                  2.42e+01 3.04e+01 -3.55e+01  8.38e+01
# C.C0204                 -2.19e+02 1.40e+02 -4.94e+02  5.55e+01
# C.C0304                 -6.42e+02 4.14e+02 -1.45e+03  1.69e+02
# C.C0404                  1.73e+03 9.93e+01  1.54e+03  1.93e+03
# C.C0504                  2.84e+03 1.43e+02  2.56e+03  3.12e+03
# C.C0105                  2.04e+02 3.61e+01  1.33e+02  2.75e+02
# C.C0205                  2.49e+02 5.67e+01  1.38e+02  3.60e+02
# C.C0305                 -8.85e+02 4.02e+02 -1.67e+03 -9.71e+01
# C.C0405                  4.72e+02 1.85e+02  1.09e+02  8.35e+02
# C.C0505                  2.10e+03 1.90e+02  1.72e+03  2.47e+03
# C.C0106                  9.30e+01 3.45e+01  2.53e+01  1.61e+02
# C.C0206                  5.67e+02 9.47e+01  3.81e+02  7.53e+02
# C.C0306                 -9.08e+02 3.91e+02 -1.67e+03 -1.42e+02
# C.C0406                 -7.35e+02 1.64e+02 -1.06e+03 -4.13e+02
# C.C0506                  1.42e+03 1.71e+02  1.09e+03  1.76e+03
# C.C0107                  8.39e+01 3.51e+01  1.52e+01  1.53e+02
# C.C0207                  4.66e+02 1.17e+02  2.37e+02  6.95e+02
# C.C0307                 -8.01e+02 3.84e+02 -1.55e+03 -4.88e+01
# C.C0407                 -4.88e+02 1.16e+02 -7.15e+02 -2.60e+02
# C.C0507                  8.60e+02 1.62e+02  5.42e+02  1.18e+03
# C.C0108                  1.25e+02 3.36e+01  5.94e+01  1.91e+02
# C.C0208                  5.35e+02 1.08e+02  3.23e+02  7.47e+02
# C.C0308                 -4.54e+02 3.79e+02 -1.20e+03  2.90e+02
# C.C0408                 -4.32e+02 7.54e+01 -5.80e+02 -2.85e+02
# C.C0508                  1.75e+02       NA        NA        NA
# C.C0109                 -1.07e+01       NA        NA        NA
# C.C0209                  8.72e+02 1.16e+02  6.44e+02  1.10e+03
# C.C0309                  5.20e+02 3.86e+02 -2.37e+02  1.28e+03
# C.C0409                 -1.73e+02 9.28e+01 -3.55e+02  8.55e+00
# C.C0509                 -4.45e+02 4.83e+01 -5.40e+02 -3.51e+02
# C.C0110                 -6.25e+01 3.51e+01 -1.31e+02  6.26e+00
# C.C0210                  4.88e+02 1.14e+02  2.65e+02  7.11e+02
# C.C0310                  8.29e+02 4.31e+02 -1.50e+01  1.67e+03
# C.C0410                 -1.67e+01 3.88e+01 -9.27e+01  5.94e+01
# C.C0510                 -7.71e+02 1.48e+02 -1.06e+03 -4.82e+02
# C.C0111                  1.34e+01 1.47e+01 -1.55e+01  4.23e+01
# C.C0211                  2.12e+02 3.30e+01  1.47e+02  2.77e+02
# C.C0311                  1.53e+03 4.37e+02  6.76e+02  2.39e+03
# C.C0411                 -1.32e+02 1.31e+02 -3.89e+02  1.25e+02
# C.C0511                 -1.30e+03 1.50e+02 -1.59e+03 -1.00e+03
# C.C0112                 -1.24e+02 3.47e+01 -1.92e+02 -5.58e+01
# C.C0212                 -8.72e+01 5.07e+01 -1.87e+02  1.21e+01
# C.C0312                  1.51e+03 4.67e+02  5.96e+02  2.43e+03
# C.C0412                 -3.27e+02 8.09e+01 -4.85e+02 -1.68e+02
# C.C0512                 -2.14e+03 1.81e+02 -2.50e+03 -1.79e+03
# C.C0513                  1.51e+01 2.74e+00  9.71e+00  2.04e+01

