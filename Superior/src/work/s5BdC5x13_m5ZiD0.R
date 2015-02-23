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
B      = "diagonal and unequal"  # Diagonal AR component
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
s5BdC5x13_m5ZiD0.BFGS <- MARSS(obsMatrix, method="BFGS",
                            model=model.list, control=cntl.list) 
# inits=s5BiC12x5_m5ZiD0.kem$par)
end.time           <- Sys.time()
print(end.time - start.time)

# Compute parameter confidence intervals
s5BdC5x13_m5ZiD0.BFGS.ci    <- MARSSparamCIs(s5BdC5x13_m5ZiD0.BFGS)

s5BdC5x13_m5ZiD0.BFGS.kf  <- MARSSkf(s5BdC5x13_m5ZiD0.BFGS)

s5BdC5x13_m5ZiD0.BFGS.kfss  <- MARSSkfss(s5BdC5x13_m5ZiD0.BFGS)


compName <- c("St. Marys River Streamflow", "Overlake Precipitation",
              "Overlake Evaporation", "Basin Runoff",
              "Change in Lake Storage")

xtt1 <- s5BdC5x13_m5ZiD0.BFGS.kfss$xtt1
xtt  <- s5BdC5x13_m5ZiD0.BFGS.kfss$xtt
xtT  <- s5BdC5x13_m5ZiD0.BFGS.kfss$xtT

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

dbName <- c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")

for (i in 1:5){
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,dbName[i]],
       type="l",col="blue", xlab="Year", ylab=paste0(compName[i],", in CMS"),
       main=paste0("Monthly Estimates of ",compName[i]," State"))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[(1:TT)+1,"DateSeq"], xtt[i,],
        type="l",col="red")
  # lines(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,"stmrCMS"],
  #       type="l",col="green")
  # Compute the difference between the two state estimates
  y_xtt <- NBSrcDf[(1:TT)+1,dbName[i]] - xtt[i,]
  legend("topright",legend=c("y[t]","x[t]|[t]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("The variance of x[t]|[t-1] - x[t]|[t] equals R",i,i," = ",
                   format(var(y_xtt),digits=5)))
  
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,dbName[i]],
       type="l",col="blue", xlab="Year", ylab=paste0(compName[i],", in CMS"),
       main=paste0("Monthly Estimates of ",compName[i]," State"))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[(1:TT)+1,"DateSeq"], xtT[i,],
        type="l",col="red")
  # lines(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,"stmrCMS"],
  #       type="l",col="green")
  # Compute the difference between the two state estimates
  y_xtT <- NBSrcDf[(1:TT)+1,dbName[i]] - xtT[i,]
  legend("topright",legend=c("y[t]","x[t]|[T]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("The variance of y[t] - x[t]|[T] equals equals R",i,i," =",
                   format(var(y_xtT),digits=5)))
}

# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 908 iterations. 
# Log-likelihood: -28790.38 
# AIC: 57740.76   AICc: 57744.32   
# 
# ML.Est  Std.Err    low.CI    up.CI
# R.(stmrCMS,stmrCMS)      1.56e-03       NA        NA       NA
# R.(precCMS,precCMS)      3.76e+05  82.3297        NA       NA
# R.(evapCMS,evapCMS)      2.22e+01       NA        NA       NA
# R.(rOffCMS,rOffCMS)      7.19e+04  38.0087        NA       NA
# R.(dStoCMS,dStoCMS)      1.25e+06       NA        NA       NA
# B.(X.stmrCMS,X.stmrCMS)  8.61e-01   0.0176     0.827    0.896
# B.(X.precCMS,X.precCMS)  1.21e-01       NA        NA       NA
# B.(X.evapCMS,X.evapCMS)  4.15e-01   0.0307     0.355    0.476
# B.(X.rOffCMS,X.rOffCMS)  5.82e-01   0.0591     0.466    0.697
# B.(X.dStoCMS,X.dStoCMS) -2.82e-02       NA        NA       NA
# U.X.stmrCMS              2.86e+02  24.8259   237.418  334.734
# U.X.precCMS              1.66e+03 126.4283  1413.978 1909.567
# U.X.evapCMS              7.56e+02 142.6854   476.402 1035.719
# U.X.rOffCMS              6.10e+02  78.7458   455.534  764.212
# U.X.dStoCMS             -6.70e+02       NA        NA       NA
# Q.(X.stmrCMS,X.stmrCMS)  5.80e+04   6.2457        NA       NA
# Q.(X.precCMS,X.precCMS)  2.04e+05 107.4736        NA       NA
# Q.(X.evapCMS,X.evapCMS)  2.42e+05  12.7530        NA       NA
# Q.(X.rOffCMS,X.rOffCMS)  1.27e+05  36.9130        NA       NA
# Q.(X.dStoCMS,X.dStoCMS)  3.42e+05       NA        NA       NA
# C.C0101                 -1.09e+02  22.3663  -152.870  -65.195
# C.C0301                  6.88e+02 195.7288   304.099 1071.342
# C.C0401                 -2.24e+02  69.7407  -360.640  -87.261
# C.C0501                 -2.31e+03       NA        NA       NA
# C.C0102                 -2.82e+01       NA        NA       NA
# C.C0202                 -6.06e+02 137.1031  -874.454 -337.019
# C.C0302                 -3.58e+02 180.0347  -711.346   -5.623
# C.C0402                 -1.62e+02  61.0337  -281.929  -42.682
# C.C0502                 -1.85e+03       NA        NA       NA
# C.C0103                 -4.46e+01  19.4314   -82.636   -6.466
# C.C0203                 -3.58e+02 138.4867  -629.074  -86.216
# C.C0303                 -3.25e+02 157.8117  -633.965  -15.354
# C.C0403                  3.00e+00       NA        NA       NA
# C.C0503                 -3.99e+02       NA        NA       NA
# C.C0104                  2.73e+01       NA        NA       NA
# C.C0204                 -1.50e+02 106.4395  -358.768   58.467
# C.C0304                 -6.33e+02 163.6018  -954.038 -312.731
# C.C0404                  1.56e+03  72.4215  1420.133 1704.020
# C.C0504                  2.92e+03       NA        NA       NA
# C.C0105                  2.02e+02  22.2952   158.258  245.654
# C.C0205                  3.73e+02 145.6992    87.313  658.443
# C.C0305                 -8.71e+02 158.8148 -1182.462 -559.920
# C.C0405                  6.16e+02 124.5389   371.858  860.042
# C.C0505                  2.62e+03       NA        NA       NA
# C.C0106                  9.87e+01  23.3270    52.972  144.412
# C.C0206                  5.74e+02 105.1641   367.933  780.169
# C.C0306                 -8.94e+02 157.9102 -1203.462 -584.465
# C.C0406                 -4.38e+02 114.9458  -663.667 -213.088
# C.C0506                  1.92e+03       NA        NA       NA
# C.C0107                  8.96e+01  18.7753    52.780  126.378
# C.C0207                  3.82e+02  79.9429   225.372  538.742
# C.C0307                 -7.85e+02 154.4154 -1087.899 -482.602
# C.C0407                 -2.68e+02  71.6407  -408.010 -127.183
# C.C0507                  1.28e+03       NA        NA       NA
# C.C0108                  1.27e+02  23.6118    80.566  173.123
# C.C0208                  4.53e+02  87.2954   281.853  624.045
# C.C0308                 -4.32e+02 149.4527  -724.563 -138.719
# C.C0408                 -3.15e+02  75.8346  -463.882 -166.616
# C.C0508                  4.56e+02       NA        NA       NA
# C.C0109                 -5.37e+00   4.6855   -14.553    3.814
# C.C0209                  7.49e+02 105.2709   542.550  955.204
# C.C0309                  5.23e+02 148.5509   231.391  813.700
# C.C0409                 -9.08e+01  49.9816  -188.753    7.171
# C.C0509                 -3.65e+02       NA        NA       NA
# C.C0110                 -5.73e+01  22.1813  -100.769  -13.820
# C.C0210                  3.21e+02  10.7664   299.435  341.639
# C.C0310                  8.41e+02 162.4135   522.857 1159.506
# C.C0410                  1.66e+02  72.3618    23.948  307.601
# C.C0510                 -9.11e+02       NA        NA       NA
# C.C0111                  1.21e+01  22.6943   -32.407   56.553
# C.C0211                  1.46e+02 126.4925  -102.085  393.756
# C.C0311                  1.52e+03 171.6678  1182.706 1855.632
# C.C0411                  1.29e+01  14.1142   -14.744   40.583
# C.C0511                 -1.53e+03       NA        NA       NA
# C.C0112                 -1.16e+02  18.3792  -151.786  -79.741
# C.C0212                 -1.32e+02  89.4737  -307.452   43.278
# C.C0312                  1.48e+03 189.3763  1112.850 1855.191
# C.C0412                 -2.52e+02  69.1033  -387.023 -116.143
# C.C0512                 -2.50e+03       NA        NA       NA
# C.C0513                  1.81e+01   2.9951    12.187   23.927
