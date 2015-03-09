# State-space model with 5 states and 5 measurements, fixed seasons, and AR(1)

# Lessons Learned: Computing on the z-scores of the measurement data seems to 
# have improved the numerics because the augment B matrix was computed and most
# of the C matrix had 

# Set path to target directory
setwd("C:/Home/Projects/GLWaterBudget/Analysis/Superior")

# source script to set up data files
source("Superior/src/work/setupMonthlyDataFrame.R")

# Specify time period of analysis
TT        <- 12*62  # 62 years of monthly data, max = 62 

# Create observation matrix
obsMatrix <- t(as.matrix(NBSrcDf[,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

pairs(t(obsMatrix))

obsMatrix[4,] <- log10(obsMatrix[4,])
# obsMatrix[3,] <- log10(obsMatrix[3,])

zObsMatrix    <- t(scale(t(obsMatrix)))

library(ggplot2)
plotmatrix(t(zObsMatris)) + geom_smooth(method="lm")

library(GGally)
ggpairs(t(zObsMatrix)) + geom_smooth

# z-score the obsMatrix with scale to improve stability (?)
zObsMatrix <- t(scale(t(obsMatrix)))

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
B  <- matrix(list("B11",0    ,0    ,0    ,0    ,
                  0    ,"B22",0    ,"B24","B25",
                  0    ,0    ,"B33",0    ,0    ,
                  0    ,"B42",0    ,"B44",0    ,
                  0    ,"B52",0    ,0    ,"B55"),
             5,5,byrow=TRUE)

U      = "unequal"               # Unique drift u values
# U      = "zero"
# Full state covariate parameters for season while 
C  <- matrix(list("C0101","C0102","C0103","C0104","C0105","C0106","C0107","C0108","C0109","C0110","C0111","C0112",0,
                  "C0101","C0202","C0203","C0204","C0205","C0206","C0207","C0208","C0209","C0210","C0211","C0212",0,
                  "C0301","C0302","C0303","C0304","C0305","C0306","C0307","C0308","C0309","C0310","C0311","C0312",0,
                  "C0401","C0402","C0403","C0404","C0405","C0406","C0407","C0408","C0409","C0410","C0411","C0412",0,
                  "C0501","C0502","C0503","C0504","C0505","C0506","C0507","C0508","C0509","C0510","C0511","C0512","C0513"),
             5,13,byrow=TRUE)

c.in   = c.ina                   # Fixed season starting in February augmented with Long Lac
Q      = "diagonal and unequal"  # Process errors are independent but have different variances             # Initial states are different unknowns
# Re-estimate with z-scores of starting values
x0     = matrix(c(zObsMatrix['stmrCMS',1],zObsMatrix['precCMS',1],zObsMatrix['evapCMS',1],
                  zObsMatrix['rOffCMS',1],zObsMatrix['dStoCMS',1]),5,1) 
V0     <- diag(10,5)
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
s5Ba2C5x13_m5ZiD0_zScore.BFGS <- MARSS(zObsMatrix[,(1:TT)+1], method="BFGS",
                                model=model.list)
#                               control=list(safe=TRUE,trace=1)) 
# inits=s5BiC12x5_m5ZiD0.kem$par)
end.time           <- Sys.time()
print(end.time - start.time)

# Compute parameter confidence intervals
s5Ba2C5x13_m5ZiD0_zScore.BFGS.ci    <- MARSSparamCIs(s5Ba2C5x13_m5ZiD0_zScore.BFGS)

s5Ba2C5x13_m5ZiD0_zScore.BFGS.kfss  <- MARSSkfss(s5Ba2C5x13_m5ZiD0_zScore.BFGS)


compName <- c("St. Marys River Streamflow", "Overlake Precipitation",
              "Overlake Evaporation", "Basin Runoff",
              "Changes in Lake Storage")

xtt1 <- s5Ba2C5x13_m5ZiD0_zScore.BFGS.kfss$xtt1
xtt  <- s5Ba2C5x13_m5ZiD0_zScore.BFGS.kfss$xtt
xtT  <- s5Ba2C5x13_m5ZiD0_zScore.BFGS.kfss$xtT
parQ <- s5Ba2C5x13_m5ZiD0_zScore.BFGS$par$Q
parR <- s5Ba2C5x13_m5ZiD0_zScore.BFGS$par$R

par(mar=c(5,4,2,2)+0.1)
for (i in 1:5){
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[3:(TT+1),"DateSeq"], xtt1[i,-1],
       type="l",col="blue", xlab="Year", 
       ylab=paste0("Z-Scores of ",compName[i],", in CMS"),
       ylim=c(min(c(xtt1[i,-1],xtt[i,-1])),
              max(c(xtt1[i,-1],xtt[i,-1]))),
       main=paste0("Monthly Estimates of ",compName[i]," State"))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[3:(TT+1),"DateSeq"], xtt[i,-1],
        type="l",col="red")
  # lines(NBSrcDf[(1:TT)+1,"DateSeq"], NBSrcDf[(1:TT)+1,"stmrCMS"],
  #       type="l",col="green")
  # Compute the difference between the two state estimates
  xtt1_xtt <- xtt1[i,-1] - xtt[i,-1]
  legend("topright",legend=c("x[t]|[t-1]","x[t]|[t]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("Var[ x[t]|[t-1] - x[t]|[t] ] = ",
                   format(var(xtt1_xtt),digits=5),", MLE(Q",i,i,") = " ,
                   format(parQ[i,1],digits=6)),cex.sub=0.9)
  
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[3:(TT+1),"DateSeq"], xtt1[i,-1],
       type="l",col="blue", xlab="Year", 
       ylab=paste0("Z-Scores of ",compName[i],", in CMS"),
       ylim=c(min(c(xtt1[i,-1],xtT[i,-1])),
              max(c(xtt1[i,-1],xtT[i,-1]))),
       main=paste0("Monthly Estimates of ",compName[i]," State"))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[3:(TT+1),"DateSeq"], xtT[i,-1],
        type="l",col="red")
  # Compute the difference between the two state estimates
  xtt1_xtT <- xtt1[i,-1] - xtT[i,-1]
  legend("topright",legend=c("x[t]|[t-1]","x[t]|[T]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("Var[ x[t]|[t-1] - x[t]|[T] ] = ",
                   format(var(xtt1_xtT),digits=5),", MLE(Q",i,i,") = ",
                   format(parQ[i,1],digits=6)),cex.sub=0.9)
}

dbName <- c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")

par(mar=c(5,4,2,2)+0.1)
for (i in 1:5){
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[2:(TT+1),"DateSeq"], zObsMatrix[i,2:(TT+1)],
       type="l",col="blue", xlab="Year", 
       ylab=paste0("Z-Scores of ",compName[i],", in CMS"),
       main=paste0("Monthly Estimates of ",compName[i]," State"),
       ylim=c(min(c(zObsMatrix[i,2:(TT+1)],xtt[i,-1])),
              max(c(zObsMatrix[i,2:(TT+1)],xtt[i,-1]))))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[3:(TT+1),"DateSeq"], xtt[i,-1],
        type="l",col="red")
  # Compute the difference between the two state estimates
  y_xtt <- zObsMatrix[i,3:(TT+1)] - xtt[i,-1]
  legend("topright",legend=c("y[t]","x[t]|[t]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("Var[ y[t] - x[t]|[t] ] = ",
                   format(var(y_xtt),digits=5),", MLE(R",i,i,") = ",
                   format(parR[i,1],digits=6)),cex.sub=0.9)
  
  # Plot the expected value of Xt conditioned on the data up to time t-1, as xtt1
  plot(NBSrcDf[(1:TT)+1,"DateSeq"], zObsMatrix[i,2:(TT+1)],
       type="l",col="blue", xlab="Year", 
       ylab=paste0("Z-Scores of ",compName[i],", in CMS"),
       main=paste0("Monthly Estimates of ",compName[i]," State"),
       ylim=c(min(c(zObsMatrix[i,2:(TT+1)],xtT[i,-1])),
              max(c(zObsMatrix[i,2:(TT+1)],xtT[i,-1]))))
  # Plot the expected value of Xt conditioned on the data up to time t, as xtt
  lines(NBSrcDf[(1:TT)+1,"DateSeq"], xtT[i,],
        type="l",col="red")
  # Compute the difference between the two state estimates
  y_xtT <- zObsMatrix[i,2:(TT+1)] - xtT[i,]
  legend("topright",legend=c("y[t]","x[t]|[T]"),
         col=c("blue","red"), lty=c("solid","solid", cex=0.8))
  title(sub=paste0("Var[ y[t] - x[t]|[T] ] = ",
                   format(var(y_xtT),digits=5),", MLE(R",i,i,") = ",
                   format(parR[i,1],digits=6)),cex.sub=0.9)
}


# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 2187 iterations. 
# Log-likelihood: -3207.695 
# AIC: 6573.39   AICc: 6576.863   
# 
# ML.Est  Std.Err    low.CI     up.CI
# R.(stmrCMS,stmrCMS)      8.79e-07 0.004317        NA        NA
# R.(precCMS,precCMS)      6.80e-01 0.022429        NA        NA
# R.(evapCMS,evapCMS)      1.50e-05       NA        NA        NA
# R.(rOffCMS,rOffCMS)      4.88e-04 0.018333        NA        NA
# R.(dStoCMS,dStoCMS)      1.75e-01 0.026033        NA        NA
# B.B11                    8.61e-01 0.018821  0.824262  0.898038
# B.B22                    3.14e-01 0.052682  0.211106  0.417614
# B.B42                    1.58e+00 0.165460  1.259780  1.908370
# B.B52                    1.29e+00 0.158305  0.982278  1.602823
# B.B33                    4.10e-01 0.033404  0.344183  0.475124
# B.B24                    3.77e-02 0.011169  0.015838  0.059621
# B.B44                    2.34e-01 0.044157  0.147549  0.320640
# B.B25                   -1.79e-03 0.001832 -0.005383  0.001798
# B.B55                   -3.98e-02 0.018351 -0.075730 -0.003795
# Q.(X.stmrCMS,X.stmrCMS)  2.30e-01 0.012410  0.207268  0.253928
# Q.(X.precCMS,X.precCMS)  8.88e-02 0.028241  0.058835  0.124794
# Q.(X.evapCMS,X.evapCMS)  1.29e-01 0.009335  0.116608  0.142938
# Q.(X.rOffCMS,X.rOffCMS)  9.64e-02 0.037934  0.055733  0.148054
# Q.(X.dStoCMS,X.dStoCMS)  3.35e-03 0.052420  0.002014  0.025792
# C.C0101                 -2.29e-01 0.043005 -0.312809 -0.144233
# C.C0301                  4.64e-01 0.069912  0.327121  0.601171
# C.C0401                 -8.34e-02 0.125758 -0.329893  0.163070
# C.C0501                 -6.49e-01 0.095741 -0.836934 -0.461638
# C.C0102                 -8.97e-02 0.060843 -0.208951  0.029550
# C.C0202                 -7.06e-01 0.118653 -0.938538 -0.473427
# C.C0302                 -3.15e-01 0.059075 -0.430731 -0.199160
# C.C0402                  2.62e-02 0.021700 -0.016302  0.068759
# C.C0502                 -3.78e-01 0.051690 -0.478881 -0.276262
# C.C0103                 -1.22e-01 0.062218 -0.244143 -0.000253
# C.C0203                 -3.50e-01 0.128108 -0.600863 -0.098690
# C.C0303                 -2.86e-01 0.045994 -0.376293 -0.196001
# C.C0403                  1.04e+00 0.213240  0.622457  1.458344
# C.C0503                  8.69e-01 0.188603  0.499480  1.238790
# C.C0104                  2.18e-02 0.069966 -0.115366  0.158897
# C.C0204                 -1.71e-01 0.107029 -0.380990  0.038557
# C.C0304                 -5.09e-01 0.046307 -0.600181 -0.418661
# C.C0404                  2.73e+00 0.201913  2.331125  3.122610
# C.C0504                  2.04e+00 0.176891  1.692613  2.386014
# C.C0105                  3.71e-01 0.061417  0.251002  0.491752
# C.C0205                  2.69e-01 0.121281  0.031758  0.507172
# C.C0305                 -6.88e-01 0.049904 -0.785699 -0.590077
# C.C0405                  1.87e+00 0.190695  1.497631  2.245140
# C.C0505                  1.85e+00 0.131358  1.591398  2.106314
# C.C0106                  1.65e-01 0.061755  0.043635  0.285710
# C.C0206                  3.61e-01 0.065143  0.233139  0.488494
# C.C0306                 -7.09e-01 0.055310 -0.817556 -0.600745
# C.C0406                 -3.60e-01 0.205881 -0.763543  0.043495
# C.C0506                  8.24e-01 0.167939  0.495111  1.153419
# C.C0107                  1.47e-01 0.060985  0.026974  0.266030
# C.C0207                  1.47e-01 0.104728 -0.057842  0.352682
# C.C0307                 -6.31e-01 0.058424 -0.745817 -0.516799
# C.C0407                 -1.04e+00 0.099080 -1.229855 -0.841467
# C.C0507                  4.83e-02 0.044640 -0.039189  0.135797
# C.C0108                  2.21e-01 0.061667  0.100274  0.342005
# C.C0208                  2.87e-01 0.112905  0.065220  0.507800
# C.C0308                 -3.69e-01 0.058175 -0.483237 -0.255196
# C.C0408                 -1.01e+00 0.160798 -1.321429 -0.691114
# C.C0508                 -2.08e-01 0.137644 -0.477575  0.061981
# C.C0109                 -4.35e-02 0.062015 -0.165059  0.078036
# C.C0209                  6.09e-01 0.128486  0.357196  0.860854
# C.C0309                  3.29e-01 0.053083  0.225342  0.433422
# C.C0409                 -9.59e-01 0.181249 -1.314338 -0.603854
# C.C0509                 -7.00e-01 0.152053 -0.997827 -0.401789
# C.C0110                 -1.47e-01 0.061541 -0.268048 -0.026812
# C.C0210                  8.10e-02 0.130969 -0.175713  0.337675
# C.C0310                  5.75e-01 0.045684  0.485216  0.664293
# C.C0410                 -1.14e+00 0.222516 -1.580613 -0.708366
# C.C0510                 -1.32e+00 0.209329 -1.731942 -0.911387
# C.C0111                 -8.67e-03 0.053711 -0.113937  0.096604
# C.C0211                 -3.91e-02       NA        NA        NA
# C.C0311                  1.07e+00 0.049551  0.975633  1.169870
# C.C0411                 -5.58e-01 0.185508 -0.921160 -0.193984
# C.C0511                 -1.05e+00 0.146897 -1.340097 -0.764273
# C.C0112                 -2.65e-01 0.061351 -0.384831 -0.144340
# C.C0212                 -2.73e-01 0.091357 -0.452356 -0.094243
# C.C0312                  1.05e+00 0.063215  0.927104  1.174902
# C.C0412                 -5.01e-01 0.061554 -0.621678 -0.380393
# C.C0512                 -1.16e+00 0.077312 -1.313742 -1.010686
# C.C0513                 -2.76e-04 0.000359 -0.000981  0.000428

