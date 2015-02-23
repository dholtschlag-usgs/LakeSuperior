# State-space model with 5 states and 5 measurements and fixed seasonal covariates

# Model ID:
#  S5BiC5x4UuQd_M5ZiD0Rd

# Lessons Learned: The first-order Fourier approximation to season was highly
# significant, but not close to the 60 parameter fixed approximation. 
# 

# Set path to target directory
setwd("C:/Home/Projects/GLWaterBudget/Analysis/Superior")

# source script to set up data files
source("Superior/src/work/setupMonthlyDataFrame.R")

# Specify time period of analysis
period    <- 12
TT        <- period*62  # 62 years of monthly data, max = 62 

# Create observation matrix
obsMatrix <- t(as.matrix(NBSrcDf[(1:TT)+1,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

# Setup Fourier component for seasonality
# number of "seasons" (e.g., 12 months per year)
# create factors for seasons
cos1.t  <- cos(2 * 3.1415926 * seq(TT) / period)
sin1.t  <- sin(2 * 3.1415926 * seq(TT) / period)

cos2.t  <- cos(2 * 2 * 3.1415926 * seq(TT) / period)
sin2.t  <- sin(2 * 2 * 3.1415926 * seq(TT) / period)
c.Four <- rbind(cos1.t, sin1.t, cos2.t, sin2.t)

# Load MARSS package
library(MARSS)

###################################################
### Specify model structure
###################################################
# State Equation Specifications
B      = "identity"              # No autoregressive component or interactions among x
U      = "unequal"               # Unique drift u values
C      = "unconstrained"         # Full state covariate parameters for season
c.in   = c.Four                  # Fixed season starting in February
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

S5BiC5x4UuQd_M5ZiD0Rd.kem <- MARSS(obsMatrix, method="kem",
                              model=model.list, control=list(maxit=500))


start.time         <- Sys.time()
S5BiC5x4UuQd_M5ZiD0Rd.BFGS <- MARSS(obsMatrix, method="BFGS",
                            model=model.list, control=cntl.list) #,
                            # inits=S5BiC5x4UuQd_M5ZiD0Rd.BFGS$par)
end.time           <- Sys.time()
print(end.time - start.time)

# Compute parameter confidence intervals
S5BiC5x4UuQd_M5ZiD0Rd.BFGS.ci    <- MARSSparamCIs(S5BiC5x4UuQd_M5ZiD0Rd.BFGS)

# Use MARSSkf to compute Kalman statistics
S5BiC5x4UuQd_M5ZiD0Rd.BFGS.kfss    <- MARSSkfss(S5BiC5x4UuQd_M5ZiD0Rd.BFGS)

compName <- c("St. Marys River Streamflow", "Overlake Precipitation",
              "Overlake Evaporation", "Basin Runoff",
              "Change in Lake Storage")

for (i in 1:5){
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[(1:TT)+1,"DateSeq"], S5BiC5x4UuQd_M5ZiD0Rd.BFGS.kfss$xtt1[i,],
       type="l",col="blue", xlab="Year", ylab=paste0(compName[i],", in CMS"),
       main=paste0("Monthly Estimates of ",compName[i]," State"))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[(1:TT)+1,"DateSeq"], S5BiC5x4UuQd_M5ZiD0Rd.BFGS.kfss$xtt[i,],
        type="l",col="red")
  # lines(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,"stmrCMS"],
  #       type="l",col="green")
  # Compute the difference between the two state estimates
  xtt1_xtt <- S5BiC5x4UuQd_M5ZiD0Rd.BFGS.kfss$xtt1[i,] -
    S5BiC5x4UuQd_M5ZiD0Rd.BFGS.kfss$xtt[i,]
  legend("topright",legend=c("x[t]|[t-1]","x[t]|[t]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("The variance of x[t]|[t-1] - x[t]|[t] equals Q",i,i," =",
                   format(var(xtt1_xtt),digits=5)))
  
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[(1:TT)+1,"DateSeq"], S5BiC5x4UuQd_M5ZiD0Rd.BFGS.kfss$xtt1[i,],
       type="l",col="blue", xlab="Year", ylab=paste0(compName[i],", in CMS"),
       main=paste0("Monthly Estimates of ",compName[i]," State"))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[(1:TT)+1,"DateSeq"], S5BiC5x4UuQd_M5ZiD0Rd.BFGS.kfss$xtT[i,],
        type="l",col="red")
  # lines(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,"stmrCMS"],
  #       type="l",col="green")
  # Compute the difference between the two state estimates
  xtt1_xtT <- S5BiC5x4UuQd_M5ZiD0Rd.BFGS.kfss$xtt1[i,] -
    S5BiC5x4UuQd_M5ZiD0Rd.BFGS.kfss$xtT[i,]
  legend("topright",legend=c("x[t]|[t-1]","x[t]|[t]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("The variance of x[t]|[t-1] - x[t]|[T] equals equals Q",i,i," =",
                   format(var(xtt1_xtT),digits=5)))
}

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 331 iterations. 
# Log-likelihood: -29406.62 
# AIC: 58863.23   AICc: 58863.59   
# 
# ML.Est Std.Err   low.CI   up.CI
# R.(stmrCMS,stmrCMS)      6.50e-05      NA       NA      NA
# R.(precCMS,precCMS)      6.04e+05  20.198       NA      NA
# R.(evapCMS,evapCMS)      3.17e+05  15.271       NA      NA
# R.(rOffCMS,rOffCMS)      4.65e+05  18.491       NA      NA
# R.(dStoCMS,dStoCMS)      2.07e+06  37.335       NA      NA
# U.X.stmrCMS              4.73e-01      NA       NA      NA
# U.X.precCMS             -1.54e-01      NA       NA      NA
# U.X.evapCMS             -4.89e-01      NA       NA      NA
# U.X.rOffCMS              5.80e-01      NA       NA      NA
# U.X.dStoCMS              3.04e-01   0.368   -0.416    1.02
# Q.(X.stmrCMS,X.stmrCMS)  6.50e+04   6.560       NA      NA
# Q.(X.precCMS,X.precCMS)  3.19e+01      NA       NA      NA
# Q.(X.evapCMS,X.evapCMS)  2.49e+03  10.968       NA      NA
# Q.(X.rOffCMS,X.rOffCMS)  1.93e+03   8.558       NA      NA
# Q.(X.dStoCMS,X.dStoCMS)  1.45e-03      NA       NA      NA
# C.(X.stmrCMS,cos1.t)    -9.49e+01  11.059 -116.534  -73.18
# C.(X.precCMS,cos1.t)    -2.09e+02  20.553 -248.785 -168.22
# C.(X.evapCMS,cos1.t)    -2.79e+02  15.354 -309.286 -249.10
# C.(X.rOffCMS,cos1.t)     2.13e+02  18.354  177.212  249.16
# C.(X.dStoCMS,cos1.t)     4.65e+02  38.587  389.474  540.73
# C.(X.stmrCMS,sin1.t)     7.04e+01      NA       NA      NA
# C.(X.precCMS,sin1.t)     1.88e+02  19.364  149.751  225.66
# C.(X.evapCMS,sin1.t)    -8.56e+02  15.239 -886.328 -826.59
# C.(X.rOffCMS,sin1.t)     2.07e+02  18.652  170.307  243.42
# C.(X.dStoCMS,sin1.t)     1.20e+03  36.620 1126.553 1270.10
