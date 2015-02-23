# revStdMARSS.R

# Create augmented data frame with diversion inputs
source("Superior/src/work/setupMonthlyDataFrame.R")
#
library(MARSS)
# Setup fixed monthly component for model
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


obsVec <- c("stmr","prec","evap","rOff","dSto")

TT        <- 12*62  # 62 years of monthly data, max = 62 
obsMatrix <- t(as.matrix(NBSrcDf[1:TT,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

# Try demean the obsMatrix

dMeanObsMatrix <- obsMatrix - rowMeans(obsMatrix)

###################################################
### Specify model structure
###################################################
# Default Model Structures (p. 30-31)
Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
B  = "diagonal and unequal"
# U  = "unequal"               # Unique u values
U = "zero"
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# R  = "diagonal and equal"    # All observation errors have the same variance
# Supercede to have different variances
R  = "diagonal and unequal"
# R  = "diagonal and equal"
A  = "scaling"               # A set of scaling factors
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
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d.in,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=1000) # MCInit=TRUE,numInits=10,
# s4o1Ar1SeaRun1.model = MARSS(obs,model=model.list,control=cntl.list)

start.time <- Sys.time()
s5o5SeaAR1rD2 <- MARSS(dMeanObsMatrix, method="BFGS",
                    model=model.list, control=cntl.list)
end.time   <- Sys.time()
print(end.time - start.time)


start.time     <- Sys.time()
s5o5SeaAR1D.ci    <- MARSSparamCIs(s5o5SeaAR1D)
end.time       <- Sys.time()
print(end.time - start.time)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 316 iterations. 
# Log-likelihood: -28837.31 
# AIC: 57844.62   AICc: 57848.64   
# 
# ML.Est  Std.Err    low.CI     up.CI
# R.(stmrCMS,stmrCMS)      1.36e-01 7.48e-01  1.21e+00  3.37e+00
# R.(precCMS,precCMS)      5.78e+05 2.26e+01  5.13e+05  6.48e+05
# R.(evapCMS,evapCMS)      2.53e+04 1.04e+02  2.05e+03  1.32e+05
# R.(rOffCMS,rOffCMS)      6.72e+04 5.04e+01  2.57e+04  1.28e+05
# R.(dStoCMS,dStoCMS)      1.07e+06 7.52e+01  7.83e+05  1.39e+06
# B.(X.stmrCMS,X.stmrCMS)  8.38e-01 2.12e-02  7.97e-01  8.80e-01
# B.(X.precCMS,X.precCMS)  6.12e-01 8.25e-02  4.50e-01  7.74e-01
# B.(X.evapCMS,X.evapCMS)  4.89e-01 5.85e-02  3.74e-01  6.03e-01
# B.(X.rOffCMS,X.rOffCMS)  5.97e-01 6.74e-02  4.65e-01  7.29e-01
# B.(X.dStoCMS,X.dStoCMS)  5.27e-01 3.76e-02  4.53e-01  6.00e-01
# U.X.stmrCMS              3.03e+02 4.95e+01  2.06e+02  4.01e+02
# U.X.precCMS              7.35e+02 1.74e+02  3.94e+02  1.08e+03
# U.X.evapCMS              6.62e+02 1.65e+02  3.38e+02  9.86e+02
# U.X.rOffCMS              5.82e+02 8.23e+01  4.21e+02  7.44e+02
# U.X.dStoCMS             -4.19e+00       NA        NA        NA
# Q.(X.stmrCMS,X.stmrCMS)  6.20e+04 6.49e+00  5.58e+04  6.85e+04
# Q.(X.precCMS,X.precCMS)  4.49e+03 6.24e+01  3.06e+03  3.58e+04
# Q.(X.evapCMS,X.evapCMS)  2.19e+05 4.47e+01  1.45e+05  3.09e+05
# Q.(X.rOffCMS,X.rOffCMS)  1.33e+05 4.67e+01  7.43e+04  2.08e+05
# Q.(X.dStoCMS,X.dStoCMS)  4.92e+05 1.17e+02  2.23e+05  8.67e+05
# x0.X.stmrCMS             2.08e+03 3.02e+02  1.49e+03  2.68e+03
# x0.X.precCMS             2.08e+03 1.08e+03 -2.74e+01  4.19e+03
# x0.X.evapCMS             4.18e+03 1.09e+03  2.06e+03  6.31e+03
# x0.X.rOffCMS             4.93e+02 9.41e+02 -1.35e+03  2.34e+03
# x0.X.dStoCMS            -2.76e+03 2.68e+03 -8.01e+03  2.49e+03
# C.(X.stmrCMS,Jan)       -8.78e+01 3.64e+01 -1.59e+02 -1.64e+01
# C.(X.precCMS,Jan)        4.75e+01 8.06e+00  3.17e+01  6.33e+01
# C.(X.evapCMS,Jan)        5.29e+02 2.47e+02  4.49e+01  1.01e+03
# C.(X.rOffCMS,Jan)       -2.50e+02 6.32e+01 -3.74e+02 -1.26e+02
# C.(X.dStoCMS,Jan)       -8.68e+02 2.19e+02 -1.30e+03 -4.39e+02
# C.(X.stmrCMS,Feb)       -2.67e-01 6.27e-01 -1.50e+00  9.61e-01
# C.(X.precCMS,Feb)       -6.03e+02 1.25e+02 -8.48e+02 -3.58e+02
# C.(X.evapCMS,Feb)       -4.70e+02 2.22e+02 -9.04e+02 -3.56e+01
# C.(X.rOffCMS,Feb)       -2.00e+02 3.48e+01 -2.69e+02 -1.32e+02
# C.(X.dStoCMS,Feb)       -5.15e+02 1.92e+02 -8.91e+02 -1.38e+02
# C.(X.stmrCMS,Mar)       -1.29e+01 2.99e+01 -7.15e+01  4.58e+01
# C.(X.precCMS,Mar)       -3.15e+01       NA        NA        NA
# C.(X.evapCMS,Mar)       -3.59e+02 1.84e+02 -7.19e+02  1.41e+00
# C.(X.rOffCMS,Mar)       -4.72e+00 1.07e+00 -6.82e+00 -2.61e+00
# C.(X.dStoCMS,Mar)        6.76e+02 1.28e+02  4.24e+02  9.27e+02
# C.(X.stmrCMS,Apr)        4.96e+01 3.50e+01 -1.90e+01  1.18e+02
# C.(X.precCMS,Apr)        6.30e+01 1.39e+01  3.56e+01  9.03e+01
# C.(X.evapCMS,Apr)       -6.17e+02 1.74e+02 -9.58e+02 -2.75e+02
# C.(X.rOffCMS,Apr)        1.56e+03 6.01e+01  1.45e+03  1.68e+03
# C.(X.dStoCMS,Apr)        2.94e+03 1.73e+02  2.60e+03  3.28e+03
# C.(X.stmrCMS,May)        1.15e+02 3.51e+01  4.66e+01  1.84e+02
# C.(X.precCMS,May)        5.14e+02 1.45e+02  2.31e+02  7.98e+02
# C.(X.evapCMS,May)       -8.11e+02 1.72e+02 -1.15e+03 -4.73e+02
# C.(X.rOffCMS,May)        6.16e+02 1.24e+02  3.73e+02  8.58e+02
# C.(X.dStoCMS,May)        1.51e+03 1.95e+02  1.13e+03  1.89e+03
# C.(X.stmrCMS,Jun)        3.13e+01 7.06e+01 -1.07e+02  1.70e+02
# C.(X.precCMS,Jun)        3.94e+02 1.19e+02  1.60e+02  6.28e+02
# C.(X.evapCMS,Jun)       -8.01e+02 1.73e+02 -1.14e+03 -4.62e+02
# C.(X.rOffCMS,Jun)       -4.50e+02 1.31e+02 -7.06e+02 -1.94e+02
# C.(X.dStoCMS,Jun)        7.27e+02 3.22e+02  9.63e+01  1.36e+03
# C.(X.stmrCMS,Jul)        1.46e+02 3.48e+01  7.81e+01  2.15e+02
# C.(X.precCMS,Jul)       -1.29e+01 1.21e+01 -3.66e+01  1.07e+01
# C.(X.evapCMS,Jul)       -6.74e+02 1.78e+02 -1.02e+03 -3.26e+02
# C.(X.rOffCMS,Jul)       -2.75e+02 6.49e+01 -4.02e+02 -1.48e+02
# C.(X.dStoCMS,Jul)        1.16e+02 1.01e+02 -8.13e+01  3.13e+02
# C.(X.stmrCMS,Aug)        1.65e+02 4.02e+01  8.65e+01  2.44e+02
# C.(X.precCMS,Aug)        2.16e+02 1.27e+02 -3.32e+01  4.66e+02
# C.(X.evapCMS,Aug)       -3.20e+02 1.76e+02 -6.64e+02  2.45e+01
# C.(X.rOffCMS,Aug)       -3.02e+02 5.72e+01 -4.14e+02 -1.90e+02
# C.(X.dStoCMS,Aug)       -4.33e+02 2.17e+02 -8.58e+02 -7.95e+00
# C.(X.stmrCMS,Sep)        2.46e+01 5.00e+01 -7.34e+01  1.22e+02
# C.(X.precCMS,Sep)        5.62e+02 1.28e+02  3.10e+02  8.13e+02
# C.(X.evapCMS,Sep)        5.87e+02 1.70e+02  2.53e+02  9.20e+02
# C.(X.rOffCMS,Sep)       -6.96e+01 2.26e+01 -1.14e+02 -2.54e+01
# C.(X.dStoCMS,Sep)       -7.48e+02 1.78e+02 -1.10e+03 -3.98e+02
# C.(X.stmrCMS,Oct)       -4.28e+01 3.66e+01 -1.15e+02  2.90e+01
# C.(X.precCMS,Oct)       -1.39e+02 7.49e+01 -2.85e+02  8.31e+00
# C.(X.evapCMS,Oct)        8.36e+02 1.78e+02  4.86e+02  1.19e+03
# C.(X.rOffCMS,Oct)        1.85e+02 4.61e+01  9.48e+01  2.75e+02
# C.(X.dStoCMS,Oct)       -7.08e+02 1.80e+02 -1.06e+03 -3.54e+02
# C.(X.stmrCMS,Nov)        1.53e+01 1.87e+01 -2.15e+01  5.20e+01
# C.(X.precCMS,Nov)       -7.82e+01 4.78e+01 -1.72e+02  1.55e+01
# C.(X.evapCMS,Nov)        1.44e+03 1.98e+02  1.05e+03  1.82e+03
# C.(X.rOffCMS,Nov)        2.38e+01       NA        NA        NA
# C.(X.dStoCMS,Nov)       -1.08e+03 1.53e+02 -1.38e+03 -7.75e+02
# C.(X.stmrCMS,Dec)       -1.01e+02 3.81e+01 -1.75e+02 -2.61e+01
# C.(X.precCMS,Dec)       -1.98e+02 1.23e+02 -4.40e+02  4.28e+01
# C.(X.evapCMS,Dec)        1.33e+03 2.32e+02  8.71e+02  1.78e+03
# C.(X.rOffCMS,Dec)       -2.53e+02 5.97e+01 -3.70e+02 -1.36e+02
# C.(X.dStoCMS,Dec)       -1.62e+03 1.90e+02 -1.99e+03 -1.25e+03


Z  = "identity"              # Each y cooresponds to one x
# B  = "identity"              # No autoregressive component or interactions among x
 B  = "diagonal and unequal"
# U  = "unequal"               # Unique u values
 U = "zero"
Q  = "diagonal and unequal"  # Process errors are independent but have different variances
# R  = "diagonal and equal"    # All observation errors have the same variance
 # Supercede to have different variances
 R  = "diagonal and unequal"
# R  = "diagonal and equal"
A  = "scaling"               # A set of scaling factors
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
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.in,D=D,d=d.in,V0=V0,tinitx=tinitx)
 
cntl.list  <- list(maxit=1000) # MCInit=TRUE,numInits=10,
# s4o1Ar1SeaRun1.model = MARSS(obs,model=model.list,control=cntl.list)
 
start.time <- Sys.time()
s5o5SeaAR1rD2 <- MARSS(dMeanObsMatrix, method="BFGS", model=model.list, control=cntl.list)

start.time <- Sys.time()
s5o5SeaAR1rD2.ci <- MARSSparamCIs(s5o5SeaAR1rD2)
end.time <- Sys.time()
print(end.time-start.time)
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 387 iterations. 
# Log-likelihood: -28848.33 
# AIC: 57856.66   AICc: 57860.22   
# 
# ML.Est  Std.Err    low.CI     up.CI
# R.(stmrCMS,stmrCMS)      1.34e+00       NA        NA        NA
# R.(precCMS,precCMS)      5.25e+05       NA        NA        NA
# R.(evapCMS,evapCMS)      4.72e+04  52.4201        NA        NA
# R.(rOffCMS,rOffCMS)      1.00e+05  20.5479        NA        NA
# R.(dStoCMS,dStoCMS)      9.09e+05 102.2957        NA        NA
# B.(X.stmrCMS,X.stmrCMS)  8.50e-01       NA        NA        NA
# B.(X.precCMS,X.precCMS)  3.10e-01       NA        NA        NA
# B.(X.evapCMS,X.evapCMS)  5.44e-01   0.0450     0.456     0.632
# B.(X.rOffCMS,X.rOffCMS)  6.66e-01       NA        NA        NA
# B.(X.dStoCMS,X.dStoCMS)  5.03e-01   0.0595     0.386     0.620
# Q.(X.stmrCMS,X.stmrCMS)  6.23e+04   5.0288        NA        NA
# Q.(X.precCMS,X.precCMS)  4.66e+04       NA        NA        NA
# Q.(X.evapCMS,X.evapCMS)  1.94e+05  34.2614        NA        NA
# Q.(X.rOffCMS,X.rOffCMS)  9.41e+04  21.6496        NA        NA
# Q.(X.dStoCMS,X.dStoCMS)  8.38e+05 136.1883        NA        NA
# x0.X.stmrCMS             3.67e-01   0.9010    -1.399     2.133
# x0.X.precCMS             7.13e+01       NA        NA        NA
# x0.X.evapCMS             2.82e+03 703.7752  1440.370  4199.118
# x0.X.rOffCMS            -1.00e+03 223.5352 -1441.278  -565.037
# x0.X.dStoCMS            -2.75e+03       NA        NA        NA
# C.(X.stmrCMS,Jan)       -1.39e+02       NA        NA        NA
# C.(X.precCMS,Jan)       -6.84e+01       NA        NA        NA
# C.(X.evapCMS,Jan)        3.65e+02 110.6616   148.255   582.041
# C.(X.rOffCMS,Jan)       -2.73e+02  58.5015  -388.005  -158.683
# C.(X.dStoCMS,Jan)       -8.74e+02 220.5053 -1306.443  -442.078
# C.(X.stmrCMS,Feb)       -4.40e+01       NA        NA        NA
# C.(X.precCMS,Feb)       -7.83e+02  93.8250  -967.307  -599.520
# C.(X.evapCMS,Feb)       -6.42e+02  92.6418  -823.596  -460.447
# C.(X.rOffCMS,Feb)       -1.94e+02  31.5517  -256.052  -132.372
# C.(X.dStoCMS,Feb)       -5.01e+02 152.1674  -799.404  -202.919
# C.(X.stmrCMS,Mar)       -6.64e+01  24.6862  -114.813   -18.045
# C.(X.precCMS,Mar)       -3.47e+02       NA        NA        NA
# C.(X.evapCMS,Mar)       -4.14e+02  63.7735  -538.499  -288.511
# C.(X.rOffCMS,Mar)       -3.61e+01       NA        NA        NA
# C.(X.dStoCMS,Mar)        7.14e+02 184.6421   352.349  1076.133
# C.(X.stmrCMS,Apr)       -2.11e+00       NA        NA        NA
# C.(X.precCMS,Apr)       -2.29e+02       NA        NA        NA
# C.(X.evapCMS,Apr)       -6.89e+02  65.3176  -817.271  -561.230
# C.(X.rOffCMS,Apr)        1.54e+03  50.1182  1439.730  1636.189
# C.(X.dStoCMS,Apr)        2.99e+03 178.7036  2639.429  3339.934
# C.(X.stmrCMS,May)        6.89e+01       NA        NA        NA
# C.(X.precCMS,May)        3.31e+02 101.6795   131.771   530.348
# C.(X.evapCMS,May)       -8.45e+02  73.8822  -989.751  -700.138
# C.(X.rOffCMS,May)        4.46e+02       NA        NA        NA
# C.(X.dStoCMS,May)        1.54e+03 249.5414  1052.478  2030.662
# C.(X.stmrCMS,Jun)       -3.10e+01       NA        NA        NA
# C.(X.precCMS,Jun)        4.08e+02  93.8124   224.217   591.955
# C.(X.evapCMS,Jun)       -7.85e+02  87.1034  -955.789  -614.350
# C.(X.rOffCMS,Jun)       -6.04e+02       NA        NA        NA
# C.(X.dStoCMS,Jun)        7.35e+02 224.5271   294.793  1174.923
# C.(X.stmrCMS,Jul)        9.81e+01  18.4477    61.899   134.213
# C.(X.precCMS,Jul)        6.87e+01  41.9181   -13.422   150.894
# C.(X.evapCMS,Jul)       -6.33e+02  91.1875  -811.450  -454.001
# C.(X.rOffCMS,Jul)       -3.42e+02  67.2353  -474.195  -210.637
# C.(X.dStoCMS,Jul)        1.06e+02 109.0148  -107.549   319.781
# C.(X.stmrCMS,Aug)        1.21e+02       NA        NA        NA
# C.(X.precCMS,Aug)        2.32e+02       NA        NA        NA
# C.(X.evapCMS,Aug)       -2.89e+02  85.8444  -457.235  -120.731
# C.(X.rOffCMS,Aug)       -3.48e+02  62.8815  -471.623  -225.132
# C.(X.dStoCMS,Aug)       -4.53e+02 159.2383  -765.241  -141.038
# C.(X.stmrCMS,Sep)       -2.82e+01       NA        NA        NA
# C.(X.precCMS,Sep)        6.46e+02  87.8728   474.138   818.593
# C.(X.evapCMS,Sep)        6.45e+02  80.1156   488.222   802.270
# C.(X.rOffCMS,Sep)       -7.45e+01       NA        NA        NA
# C.(X.dStoCMS,Sep)       -7.71e+02 171.0249 -1106.513  -436.108
# C.(X.stmrCMS,Oct)       -9.53e+01       NA        NA        NA
# C.(X.precCMS,Oct)        2.42e+01       NA        NA        NA
# C.(X.evapCMS,Oct)        7.71e+02  63.9987   645.590   896.460
# C.(X.rOffCMS,Oct)        1.86e+02  52.2321    83.537   288.283
# C.(X.dStoCMS,Oct)       -7.31e+02 166.5110 -1057.515  -404.804
# C.(X.stmrCMS,Nov)       -2.29e+01       NA        NA        NA
# C.(X.precCMS,Nov)       -1.92e+01       NA        NA        NA
# C.(X.evapCMS,Nov)        1.36e+03  71.8910  1220.426  1502.233
# C.(X.rOffCMS,Nov)       -7.36e-01       NA        NA        NA
# C.(X.dStoCMS,Nov)       -1.10e+03 185.0505 -1463.112  -737.728
# C.(X.stmrCMS,Dec)       -1.52e+02  29.6949  -210.288   -93.886
# C.(X.precCMS,Dec)       -2.65e+02  82.7075  -427.526  -103.319
# C.(X.evapCMS,Dec)        1.17e+03 103.3056   971.882  1376.832
# C.(X.rOffCMS,Dec)       -2.92e+02  47.0925  -383.812  -199.213
# C.(X.dStoCMS,Dec)       -1.64e+03 193.3976 -2022.848 -1264.744
