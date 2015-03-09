#
# Set path to target directory
setwd("C:/Home/Projects/GLWaterBudget/Analysis/Superior")

# source script to set up data files
source("Superior/src/work/setupMonthlyDataFrame.R")

# Source scripts to setup innovations
source("Superior/src/work/trendAnnualStmr.R")
source("Superior/src/work/trendAnnualPrec.R")
source("Superior/src/work/trendAnnualEvap.R")
source("Superior/src/work/trendAnnualRoff.R")
source("Superior/src/work/trendAnnualdSto.R")

# Create observation matrix
dtrndMatrix <- t(rbind(stmrInno,precInno,evapInno,rOffInno,dStoInno))
colnames(dtrndMatrix) <- c('strmInno',"precInno","evapInno","rOffInno","dStoInno")

library(GGally)
ggpairs(dtrndMatrix, lower=list(continuous = "smooth",
                                method = "loess"),
        upper=list(continuous = "density"),
        title="Relation Among Detrended Lake Superior Water Balance Components")

# Default model
library(MARSS)
ssAnnualDetrendDefault <- MARSS(t(dtrndMatrix))

ssAnnualDetrendDefault <- MARSS(t(dtrndMatrix), method="BFGS",
                                inits=ssAnnualDetrendDefault$par)

# Compute confidence intervals
ssAnnualDetrendDefault.ci <- MARSSparamCIs(ssAnnualDetrendDefault)

# The drift term should be small since components were detrended
B   <- "identity"
U   <- "zero"
x0  <- "unequal"
Q   <- "diagonal and unequal"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5BiU0xuQduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                      model = model.lst)

ssAnnDtrnds5BiU0xuQduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                      model = model.lst,
                                      inits = ssAnnDtrnds5BiU0xuQduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5BiU0xuQduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5BiU0xuQduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 33 iterations. 
# Log-likelihood: -2221.976 
# AIC: 4473.953   AICc: 4475.558   
# 
# ML.Est  Std.Err    low.CI    up.CI
# R.(strmInno,strmInno)      7.43e+04 7.00e+01  18321.50 1.68e+05
# R.(precInno,precInno)      5.70e+04 2.12e+01  38844.02 7.85e+04
# R.(evapInno,evapInno)      4.80e+04 1.94e+01  32748.42 6.61e+04
# R.(rOffInno,rOffInno)      6.13e+04 2.20e+01  41827.94 8.46e+04
# R.(dStoInno,dStoInno)      1.70e+05 3.67e+01 115708.95 2.34e+05
# Q.(X.strmInno,X.strmInno)  8.73e+03 1.11e+02        NA       NA
# Q.(X.precInno,X.precInno)  7.14e-06       NA        NA       NA
# Q.(X.evapInno,X.evapInno)  5.02e-08 1.26e-03        NA       NA
# Q.(X.rOffInno,X.rOffInno)  2.74e-06 9.33e-03        NA       NA
# Q.(X.dStoInno,X.dStoInno)  1.13e-05 1.43e-02        NA       NA
# x0.X.strmInno             -8.26e+00       NA        NA       NA
# x0.X.precInno              1.91e-02       NA        NA       NA
# x0.X.evapInno             -8.27e-01       NA        NA       NA
# x0.X.rOffInno             -1.78e-01       NA        NA       NA
# x0.X.dStoInno              1.10e+00 3.22e+00     -5.21 7.41e+00


# The drift term should be small since components were detrended
B   <- "identity"
U   <- "zero"
x0  <- "zero"
Q   <- "diagonal and unequal"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5BiU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                      model = model.lst)

ssAnnDtrnds5BiU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                      model = model.lst,
                                      inits = ssAnnDtrnds5BiU0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5BiU0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5BiU0x0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 16 iterations. 
# Log-likelihood: -2221.973 
# AIC: 4463.946   AICc: 4464.67   
# 
# ML.Est Std.Err low.CI  up.CI
# R.(strmInno,strmInno)     7.72e+04 309.726 108368 783065
# R.(precInno,precInno)     5.71e+04  21.305  38877  78784
# R.(evapInno,evapInno)     4.82e+04  19.586  32827  66542
# R.(rOffInno,rOffInno)     6.16e+04  22.151  41938  85043
# R.(dStoInno,dStoInno)     1.70e+05  36.825 115815 234901
# Q.(X.strmInno,X.strmInno) 7.18e+03 528.214     NA     NA
# Q.(X.precInno,X.precInno) 7.28e-03   0.383     NA     NA
# Q.(X.evapInno,X.evapInno) 5.21e-04      NA     NA     NA
# Q.(X.rOffInno,X.rOffInno) 1.13e-02   0.370     NA     NA
# Q.(X.dStoInno,X.dStoInno) 5.36e-03   0.467     NA     NA

# Test that the drift term is zero
B   <- "identity"
U   <- "unequal"
x0  <- "zero"
Q   <- "diagonal and unequal"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5BiUux0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                      model = model.lst)

ssAnnDtrnds5BiUux0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                      model = model.lst,
                                      inits = ssAnnDtrnds5BiUux0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5BiUux0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5BiUux0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 23 iterations. 
# Log-likelihood: -2221.927 
# AIC: 4473.854   AICc: 4475.459   
# 
# ML.Est  Std.Err    low.CI     up.CI
# R.(strmInno,strmInno)      7.70e+04 130.4194  4.80e+02  2.84e+05
# R.(precInno,precInno)      5.71e+04  21.2913  3.89e+04  7.87e+04
# R.(evapInno,evapInno)      4.82e+04  19.5874  3.28e+04  6.65e+04
# R.(rOffInno,rOffInno)      6.16e+04  22.1428  4.19e+04  8.50e+04
# R.(dStoInno,dStoInno)      1.70e+05  36.8042  1.16e+05  2.35e+05
# U.X.strmInno              -1.43e+00       NA        NA        NA
# U.X.precInno              -8.20e-02   0.4620 -9.88e-01  8.24e-01
# U.X.evapInno               1.39e-04       NA        NA        NA
# U.X.rOffInno               1.21e-01       NA        NA        NA
# U.X.dStoInno              -3.01e-01   0.0775 -4.53e-01 -1.49e-01
# Q.(X.strmInno,X.strmInno)  7.26e+03 221.4531        NA        NA
# Q.(X.precInno,X.precInno)  1.30e-04   0.0779        NA        NA
# Q.(X.evapInno,X.evapInno)  1.59e-05       NA        NA        NA
# Q.(X.rOffInno,X.rOffInno)  3.29e-05       NA        NA        NA
# Q.(X.dStoInno,X.dStoInno)  2.04e-04       NA        NA        NA

# Results indicate that both drift and initial conditions are zero

# Test the diagonal and unequal AR component
B   <- "diagonal and unequal"
U   <- "zero"
x0  <- "zero"
Q   <- "diagonal and unequal"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5BduU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                      model = model.lst)

ssAnnDtrnds5BduU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                      model = model.lst,
                                      inits = ssAnnDtrnds5BduU0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5BduU0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5BduU0x0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 4 iterations. 
# Log-likelihood: -2207.381 
# AIC: 4444.762   AICc: 4446.367   
# 
# ML.Est Std.Err    low.CI     up.CI
# R.(strmInno,strmInno)      2.30e+03 122.657  3.71e+04  8.31e+04
# R.(precInno,precInno)      2.49e+04 207.312  6.18e+04  3.18e+05
# R.(evapInno,evapInno)      9.69e+03 231.887  1.27e+05  3.06e+05
# R.(rOffInno,rOffInno)      3.76e+03 164.257  6.79e+04  1.47e+05
# R.(dStoInno,dStoInno)      4.08e+03 168.286  7.07e+04  1.55e+05
# B.(X.strmInno,X.strmInno)  3.96e-01   0.121  1.58e-01  6.34e-01
# B.(X.precInno,X.precInno) -1.92e-01   0.349 -8.75e-01  4.92e-01
# B.(X.evapInno,X.evapInno)  1.62e-01   0.188 -2.06e-01  5.31e-01
# B.(X.rOffInno,X.rOffInno)  2.58e-01   0.137 -1.14e-02  5.27e-01
# B.(X.dStoInno,X.dStoInno) -3.82e-01   0.124 -6.25e-01 -1.38e-01
# Q.(X.strmInno,X.strmInno)  8.01e+04  33.430  4.73e+04  1.21e+05
# Q.(X.precInno,X.precInno)  3.09e+04 189.724  3.84e+04  3.00e+05
# Q.(X.evapInno,X.evapInno)  3.74e+04 120.195  1.78e+03  1.84e+05
# Q.(X.rOffInno,X.rOffInno)  5.40e+04  48.683  1.88e+04  1.07e+05
# Q.(X.dStoInno,X.dStoInno)  1.42e+05  44.757  8.40e+04  2.16e+05

# Results: Diag and unequal AR component significant

# Test the diagonal and unequal AR component with off sel diag components
B   <- "unconstrained"
U   <- "zero"
x0  <- "zero"
Q   <- "diagonal and unequal"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5BuU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                       model = model.lst)

ssAnnDtrnds5BuU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                       model = model.lst,
                                       inits = ssAnnDtrnds5BuU0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5BuU0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5BuU0x0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 216 iterations. 
# Log-likelihood: -2112.944 
# AIC: 4295.888   AICc: 4304.92   
# 
# ML.Est Std.Err  low.CI  up.CI
# R.(strmInno,strmInno)      7418.907 13.8190      NA     NA
# R.(precInno,precInno)     12236.641      NA      NA     NA
# R.(evapInno,evapInno)     44189.128 18.2093      NA     NA
# R.(rOffInno,rOffInno)     13612.812 12.6039      NA     NA
# R.(dStoInno,dStoInno)      1021.540 50.4688      NA     NA
# B.(1,1)                       2.334  0.5130  1.3290 3.3399
# B.(2,1)                       0.636  0.2465  0.1528 1.1189
# B.(3,1)                      -0.153      NA      NA     NA
# B.(4,1)                       1.331  0.0727  1.1884 1.4735
# B.(5,1)                      -0.142      NA      NA     NA
# B.(1,2)                       0.500  0.2740 -0.0366 1.0373
# B.(2,2)                      -0.308      NA      NA     NA
# B.(3,2)                       0.117      NA      NA     NA
# B.(4,2)                       0.379      NA      NA     NA
# B.(5,2)                      -0.953      NA      NA     NA
# B.(1,3)                       3.463      NA      NA     NA
# B.(2,3)                       4.646      NA      NA     NA
# B.(3,3)                      -1.140  0.6117 -2.3386 0.0591
# B.(4,3)                       4.945      NA      NA     NA
# B.(5,3)                       9.211      NA      NA     NA
# B.(1,4)                      -2.867      NA      NA     NA
# B.(2,4)                      -0.560      NA      NA     NA
# B.(3,4)                       0.061      NA      NA     NA
# B.(4,4)                      -1.467      NA      NA     NA
# B.(5,4)                       1.069      NA      NA     NA
# B.(1,5)                       1.275      NA      NA     NA
# B.(2,5)                       0.613      NA      NA     NA
# B.(3,5)                      -0.176      NA      NA     NA
# B.(4,5)                       0.817      NA      NA     NA
# B.(5,5)                       0.567      NA      NA     NA
# Q.(X.strmInno,X.strmInno)  4965.145 22.5738      NA     NA
# Q.(X.precInno,X.precInno)  9139.979      NA      NA     NA
# Q.(X.evapInno,X.evapInno)  1329.183      NA      NA     NA
# Q.(X.rOffInno,X.rOffInno)     1.802      NA      NA     NA
# Q.(X.dStoInno,X.dStoInno)  2700.905 35.3519      NA     NA

# Test the diagonal and unequal AR component with sel off diag components

B   <- matrix(list("b11",0    ,0    ,0    ,0    ,
                    0   ,"b22",0    ,0    ,0    ,
                    0      ,0    ,"b33",0    ,0    ,
                   "b41"   ,0    ,0    ,"b44",0    ,
                    0   ,0    ,0    ,0    ,"b55"),5,5,byrow=TRUE)

U   <- "zero"
x0  <- "zero"
Q   <- "diagonal and unequal"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                      model = model.lst)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                      model = model.lst,
                                      inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5Bds1U0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 58 iterations. 
# Log-likelihood: -2186.836 
# AIC: 4407.672   AICc: 4409.733   
# 
# ML.Est Std.Err    low.CI     up.CI
# R.(strmInno,strmInno)      1.89e+04  26.608  7.27e+03  3.59e+04
# R.(precInno,precInno)      2.47e+04 281.547  1.56e+05  5.02e+05
# R.(evapInno,evapInno)      9.47e+03 236.578  1.34e+05  3.15e+05
# R.(rOffInno,rOffInno)      2.25e+04  18.317  1.30e+04  3.45e+04
# R.(dStoInno,dStoInno)      3.76e+03 176.899  8.14e+04  1.67e+05
# B.b11                     -2.97e+00   1.523 -5.96e+00  1.07e-02
# B.b41                     -2.08e+00   1.110 -4.25e+00  9.54e-02
# B.b22                     -1.91e-01   0.445 -1.06e+00  6.81e-01
# B.b33                      1.62e-01   0.191 -2.13e-01  5.37e-01
# B.b14                      4.89e+00   2.275  4.34e-01  9.35e+00
# B.b44                      3.37e+00   1.557  3.19e-01  6.42e+00
# B.b55                     -3.81e-01   0.123 -6.23e-01 -1.40e-01
# Q.(X.strmInno,X.strmInno)  5.78e+00      NA        NA        NA
# Q.(X.precInno,X.precInno)  3.11e+04 255.342        NA        NA
# Q.(X.evapInno,X.evapInno)  3.76e+04 121.194        NA        NA
# Q.(X.rOffInno,X.rOffInno)  2.89e+03  25.781        NA        NA
# Q.(X.dStoInno,X.dStoInno)  1.43e+05  44.844        NA        NA


# Systematically replace individual components
U   <- "zero"
x0  <- "zero"
Q   <- "diagonal and unequal"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

B <- matrix(list("b11",    0,    0,    0,    0,
                 0,"b22",    0,    0,    0,
                 0,    0,"b33",    0,    0,
                 0,    0,    0,"b44",    0,
                 0,    0,    0,    0,"b55"),5,5)

AICc <- rep(NA,25)
k    <- 0

for (i in 1:5) {
  for (j in 1:5) {
    # if (i == j) {print}
    B1 <- B
    B1[j,i] <- paste0("b",j,i)
    # print(B1)
    
    model.lst <- list(B=B1, U=U, x0=x0, Q=Q, 
                      Z=Z, A=A,        R=R)
    
    ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                            model = model.lst)
    
    ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                            model = model.lst,
                                            inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)
    k       <- k + 1
    AICc[k] <- ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$AICc
  }
}

# Success! Converged in 4 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 4 iterations. 
# Log-likelihood: -2198.122 
# AIC: 4428.244   AICc: 4430.069   
# 
# Estimate
# R.(strmInno,strmInno)      2.55e+03
# R.(precInno,precInno)      2.49e+04
# R.(evapInno,evapInno)      9.69e+03
# R.(rOffInno,rOffInno)      3.76e+03
# R.(dStoInno,dStoInno)      1.41e+03
# B.b11                      4.29e-01
# B.b22                     -1.92e-01
# B.b33                      1.62e-01
# B.b44                      2.58e-01
# B.b15                      3.65e-01
# B.b55                     -3.77e-01
# Q.(X.strmInno,X.strmInno)  5.85e+04
# Q.(X.precInno,X.precInno)  3.09e+04
# Q.(X.evapInno,X.evapInno)  3.74e+04
# Q.(X.rOffInno,X.rOffInno)  5.40e+04
# Q.(X.dStoInno,X.dStoInno)  1.45e+05




# Systematically replace individual components
U   <- "zero"
x0  <- "zero"
Q   <- "diagonal and unequal"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

B <- matrix(list("b11",    0,    0,    0,    0,
                 "b21","b22",    0,    0,"b25",
                     0,    0,"b33",    0,    0,
                     0,    0,    0,"b44",    0,
                 "b51",    0,    0,    0,"b55"),5,5)

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                        model = model.lst)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                        model = model.lst,
                                        inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5Bds1U0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 31 iterations. 
# Log-likelihood: -2185.109 
# AIC: 4406.219   AICc: 4408.53   
# 
# ML.Est Std.Err    low.CI     up.CI
# R.(strmInno,strmInno)      3991.684 171.980  7.50e+04  1.60e+05
# R.(precInno,precInno)     35821.532  21.079  2.19e+04  5.32e+04
# R.(evapInno,evapInno)      9691.527 221.528  1.13e+05  2.84e+05
# R.(rOffInno,rOffInno)      3757.545 171.407  7.54e+04  1.58e+05
# R.(dStoInno,dStoInno)     21636.899  83.988  3.07e+02  9.72e+04
# B.b11                         0.546   0.154  2.43e-01  8.48e-01
# B.b21                        -0.870   0.334 -1.52e+00 -2.15e-01
# B.b22                        -0.565   0.121 -8.02e-01 -3.28e-01
# B.b25                        -3.083   0.721 -4.50e+00 -1.67e+00
# B.b33                         0.162   0.192 -2.13e-01  5.38e-01
# B.b44                         0.258   0.140 -1.58e-02  5.31e-01
# B.b51                         0.637   0.177  2.89e-01  9.85e-01
# B.b55                         0.336   0.156  3.01e-02  6.41e-01
# Q.(X.strmInno,X.strmInno) 42689.360  80.853  2.32e+03  1.33e+05
# Q.(X.precInno,X.precInno) 15000.522  25.678  5.21e+03  2.99e+04
# Q.(X.evapInno,X.evapInno) 37406.169 115.288  1.06e+03  1.76e+05
# Q.(X.rOffInno,X.rOffInno) 54013.291  50.278  1.79e+04  1.10e+05
# Q.(X.dStoInno,X.dStoInno)  1916.330 120.158  3.68e+04  7.80e+04


# Systematically replace individual components
U   <- "zero"
x0  <- "zero"
Q   <- "unconstrained"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

B <- matrix(list("b11",    0,    0,    0,    0,
                 "b21","b22",    0,    0,"b25",
                 0,    0,"b33",    0,    0,
                 0,    0,    0,"b44",    0,
                 "b51",    0,    0,    0,"b55"),5,5)

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                        model = model.lst)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                        model = model.lst,
                                        inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5Bds1U0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu)


# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 85 iterations. 
# Log-likelihood: -2097.011 
# AIC: 4250.023   AICc: 4255.701   
# 
# ML.Est Std.Err    low.CI     up.CI
# R.(strmInno,strmInno)  4.48e+02 42.6360        NA        NA
# R.(precInno,precInno)  5.08e+03  2.8277        NA        NA
# R.(evapInno,evapInno)  4.27e+03 39.0455        NA        NA
# R.(rOffInno,rOffInno)  2.32e+03  4.1889        NA        NA
# R.(dStoInno,dStoInno)  2.24e+03      NA        NA        NA
# B.b11                  3.54e-01  0.0948  1.69e-01  5.40e-01
# B.b21                  1.79e-01  0.1720 -1.58e-01  5.16e-01
# B.b22                 -2.45e-01  0.0950 -4.32e-01 -5.93e-02
# B.b25                 -1.07e+00  0.0779 -1.22e+00 -9.14e-01
# B.b33                  3.29e-01  0.2287 -1.20e-01  7.77e-01
# B.b44                  2.90e-01  0.0752  1.42e-01  4.37e-01
# B.b51                  2.51e-01  0.1134  2.82e-02  4.73e-01
# B.b55                  4.17e-02  0.0407 -3.80e-02  1.21e-01
# Q.(1,1)                5.74e+04 32.5558  4.17e+04  1.20e+05
# Q.(2,1)                2.92e+04 13.9795  1.72e+04  6.28e+04
# Q.(3,1)               -9.87e+03 19.6288 -2.62e+04  1.32e+04
# Q.(4,1)                4.26e+04 11.3034  2.51e+04  6.37e+04
# Q.(5,1)                3.04e+04 11.0179  2.35e+04  5.87e+04
# Q.(2,2)                4.83e+04 11.5775  3.83e+04  7.49e+04
# Q.(3,2)               -1.27e+03 20.8271 -1.87e+04  2.08e+04
# Q.(4,2)                2.90e+04 11.6699  1.02e+04  4.03e+04
# Q.(5,2)                6.04e+04 10.1524  2.90e+04  5.61e+04
# Q.(3,3)                4.43e+04 22.0075  3.65e+04  6.59e+04
# Q.(4,3)               -1.10e+04 12.5732 -1.91e+04  6.54e+03
# Q.(5,3)               -2.07e+04  8.9982 -2.53e+04 -1.60e+03
# Q.(4,4)                5.36e+04  8.7513  2.67e+04  4.92e+04
# Q.(5,4)                5.91e+04  6.1901  2.09e+04  4.24e+04
# Q.(5,5)                1.27e+05  6.8979  3.05e+04  5.21e+04




# Experiment with unestimated components
U   <- "zero"
x0  <- "zero"
Q   <- "equalvarcov"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

B <- matrix(list("b11",    0,    0,    0,    0,
                 "b21","b22",    0,    0,"b25",
                 0,    0,"b33",    0,    0,
                 0,    0,    0,"b44",    0,
                 "b51",    0,    0,    0,"b55"),5,5)

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                        model = model.lst)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                        model = model.lst,
                                        inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)

# MARSS fit is
# Estimation method: kem 
# Convergence test: conv.test.slope.tol = 0.5, abstol = 0.001
# WARNING: Abstol convergence only no log-log convergence.
# maxit (=500) reached before log-log convergence.
# The likelihood and params might not be at the ML values.
# Try setting control$maxit higher.
# Log-likelihood: -2153.22 
# AIC: 4336.439   AICc: 4338.044   
# 
# Estimate
# R.(strmInno,strmInno)   637.247
# R.(precInno,precInno) 19092.701
# R.(evapInno,evapInno) 96293.301
# R.(rOffInno,rOffInno) 10463.180
# R.(dStoInno,dStoInno) 43264.881
# B.b11                    -0.112
# B.b21                     1.726
# B.b22                    -0.430
# B.b25                    -1.515
# B.b33                    -0.275
# B.b44                     0.250
# B.b51                    -0.628
# B.b55                     0.224
# Q.diag                43159.086
# Q.offdiag             39199.395
# 
# Standard errors have not been calculated. 
# Use MARSSparamCIs to compute CIs and bias estimates.
# 
# Convergence warnings
# Warning: the  R.(strmInno,strmInno)  parameter value has not converged.
# Type MARSSinfo("convergence") for more info on this warning.
# 
# > 
#   > ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
#                                             +                                         model = model.lst,
#                                             +                                         inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)
# 
# Errors were caught in is.marssMODEL()
# 
# Errors were caught in is.marssMODEL_marxss()
# The variance-covariance matrix Q is not properly constrained.
# when method=BFGS, no constraints can be put on varcov blocks except being diagonal at t=1
# Error: Stopped in MARSS.marxss() due to problem(s) with model specification.




# Re-estimated AR components with Q unconstrained

# Systematically replace individual components
U   <- "zero"
x0  <- "zero"
Q   <- "unconstrained"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

B <- matrix(list("b11",    0,    0,    0,    0,
                 0,"b22",    0,    0,    0,
                 0,    0,"b33",    0,    0,
                 0,    0,    0,"b44",    0,
                 0,    0,    0,    0,"b55"),5,5)

AICc <- rep(NA,25)
k    <- 0

for (i in 1:5) {
  for (j in 1:5) {
    # if (i == j) {print}
    B1 <- B
    B1[i,j] <- paste0("b",j,i)
    # print(B1)
    
    model.lst <- list(B=B1, U=U, x0=x0, Q=Q, 
                      Z=Z, A=A,        R=R)
    
    ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                            model = model.lst)
    
    ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                            model = model.lst,
                                            inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)
    k       <- k + 1
    AICc[k] <- ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$AICc
  }
}

# Systematically replace individual components
U   <- "zero"
x0  <- "zero"
Q   <- "unconstrained"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

B <- matrix(list("b11",    0,    0,    0,    0,
                     0,"b22",    0,    0,    0,
                     0,    0,"b33",    0,    0,
                  "b41",    0,    0,"b44",    0,
                     0,    0,    0,    0,"b55"),5,5)

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                        model = model.lst)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                        model = model.lst,
                                        inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5Bds1U0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 169 iterations. 
# Log-likelihood: -2106.279 
# AIC: 4264.558   AICc: 4269.433   
# 
# ML.Est Std.Err  low.CI  up.CI
# R.(strmInno,strmInno)  5.84e-01  2.6570      NA     NA
# R.(precInno,precInno)  1.56e+03      NA      NA     NA
# R.(evapInno,evapInno)  1.67e+03      NA      NA     NA
# R.(rOffInno,rOffInno)  9.85e+03      NA      NA     NA
# R.(dStoInno,dStoInno)  4.00e+02      NA      NA     NA
# B.b11                 -2.52e-01      NA      NA     NA
# B.b22                 -5.01e-02  0.1139 -0.2733  0.173
# B.b33                  2.25e-01  0.1264 -0.0226  0.473
# B.b41                  1.12e+00      NA      NA     NA
# B.b44                  4.60e-01  0.0914  0.2812  0.639
# B.b55                 -2.93e-01  0.0414 -0.3743 -0.212
# Q.(1,1)                6.64e+04 28.7353      NA     NA
# Q.(2,1)                3.25e+04 19.3966      NA     NA
# Q.(3,1)               -1.05e+04 14.6873      NA     NA
# Q.(4,1)                4.83e+04  7.6883      NA     NA
# Q.(5,1)                3.11e+04 12.1098      NA     NA
# Q.(2,2)                5.55e+04 22.6763      NA     NA
# Q.(3,2)               -2.48e+03 12.7280      NA     NA
# Q.(4,2)                3.65e+04      NA      NA     NA
# Q.(5,2)                6.82e+04 13.3108      NA     NA
# Q.(3,3)                4.57e+04 18.0601      NA     NA
# Q.(4,3)               -1.15e+04  7.1523      NA     NA
# Q.(5,3)               -2.23e+04  3.8956      NA     NA
# Q.(4,4)                5.12e+04  9.4610      NA     NA
# Q.(5,4)                6.73e+04  4.1675      NA     NA
# Q.(5,5)                1.51e+05      NA      NA     NA

# Systematically replace individual components
U   <- "zero"
x0  <- "zero"
Q   <- "unconstrained"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

B <- matrix(list("b11",    0,    0,    0,    0,
                     0,"b22",    0,    0,    0,
                 "b31",    0,"b33",    0,    0,
                 "b41",    0,    0,"b44",    0,
                     0,     0,    0,    0,"b55"),5,5)

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                        model = model.lst)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                        model = model.lst,
                                        inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5Bds1U0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 160 iterations. 
# Log-likelihood: -2106.28 
# AIC: 4266.56   AICc: 4271.829   
# 
# ML.Est Std.Err    low.CI     up.CI
# R.(strmInno,strmInno)  5.42e-01  4.7856        NA        NA
# R.(precInno,precInno)  1.84e+03      NA        NA        NA
# R.(evapInno,evapInno)  1.84e+03      NA        NA        NA
# R.(rOffInno,rOffInno)  9.85e+03  7.1307        NA        NA
# R.(dStoInno,dStoInno)  4.51e+02      NA        NA        NA
# B.b11                 -2.54e-01      NA        NA        NA
# B.b22                 -5.44e-02  0.1541 -3.56e-01  2.48e-01
# B.b31                 -1.14e-02  0.0525 -1.14e-01  9.16e-02
# B.b33                  2.27e-01  0.1261 -2.05e-02  4.74e-01
# B.b41                  1.12e+00      NA        NA        NA
# B.b44                  4.60e-01  0.0908  2.82e-01  6.38e-01
# B.b55                 -2.96e-01  0.0434 -3.82e-01 -2.11e-01
# Q.(1,1)                6.63e+04 25.3028  5.28e+04  1.30e+05
# Q.(2,1)                3.27e+04 29.6587  8.48e+03  8.57e+04
# Q.(3,1)               -1.06e+04 18.4760 -2.66e+04  1.20e+04
# Q.(4,1)                4.82e+04 12.3474  2.65e+04  6.78e+04
# Q.(5,1)                3.10e+04 10.8195  2.02e+04  6.03e+04
# Q.(2,2)                5.58e+04 18.8445  3.84e+04  1.00e+05
# Q.(3,2)               -2.52e+03 16.0522 -1.69e+04  1.65e+04
# Q.(4,2)                3.67e+04  9.6114  1.61e+04  5.35e+04
# Q.(5,2)                6.84e+04 13.7187  1.99e+04  5.57e+04
# Q.(3,3)                4.55e+04 18.8200  3.99e+04  6.28e+04
# Q.(4,3)               -1.14e+04  8.9941 -1.99e+04  3.07e+03
# Q.(5,3)               -2.25e+04 12.6101 -2.03e+04  1.50e+03
# Q.(4,4)                5.13e+04 10.0274  2.55e+04  4.88e+04
# Q.(5,4)                6.74e+04  4.4028  1.50e+04  3.57e+04
# Q.(5,5)                1.51e+05  6.2082  3.05e+04  4.92e+04

# Systematically replace individual components
U   <- "zero"
x0  <- "zero"
Q   <- "unconstrained"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

B <- matrix(list("b11",    0,    0,    0,    0,
                     0,"b22",    0,    0,    0,
                 "b31",    0,"b33",    0,    0,
                     0,    0,    0,"b44",    0,
                 "b51",     0,    0,    0,"b55"),5,5)

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                        model = model.lst)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                        model = model.lst,
                                        inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5Bds1U0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 176 iterations. 
# Log-likelihood: -2106.336 
# AIC: 4266.673   AICc: 4271.941   
# 
# ML.Est Std.Err  low.CI  up.CI
# R.(strmInno,strmInno)  3.98e+00      NA      NA     NA
# R.(precInno,precInno)  7.85e+02      NA      NA     NA
# R.(evapInno,evapInno)  5.87e+02      NA      NA     NA
# R.(rOffInno,rOffInno)  1.06e+04 15.5656      NA     NA
# R.(dStoInno,dStoInno)  5.47e+01      NA      NA     NA
# B.b11                  4.55e-01  0.0745  0.3090  0.601
# B.b22                  1.01e-01  0.0831 -0.0617  0.264
# B.b31                  7.45e-02      NA      NA     NA
# B.b33                  2.35e-01  0.1113  0.0164  0.453
# B.b44                  4.96e-01  0.0796  0.3401  0.652
# B.b51                  4.06e-01  0.0478  0.3120  0.499
# B.b55                 -2.32e-01  0.0325 -0.2953 -0.168
# Q.(1,1)                6.10e+04 21.3356      NA     NA
# Q.(2,1)                3.22e+04      NA      NA     NA
# Q.(3,1)               -1.12e+04  7.2789      NA     NA
# Q.(4,1)                4.73e+04  9.9210      NA     NA
# Q.(5,1)                3.50e+04 10.1768      NA     NA
# Q.(2,2)                5.97e+04 19.4975      NA     NA
# Q.(3,2)               -2.38e+03  0.1915      NA     NA
# Q.(4,2)                3.71e+04      NA      NA     NA
# Q.(5,2)                7.36e+04 10.4173      NA     NA
# Q.(3,3)                4.68e+04 18.8581      NA     NA
# Q.(4,3)               -1.22e+04  6.6153      NA     NA
# Q.(5,3)               -2.23e+04      NA      NA     NA
# Q.(4,4)                4.94e+04  8.8583      NA     NA
# Q.(5,4)                6.62e+04  4.3575      NA     NA
# Q.(5,5)                1.54e+05  7.4050      NA     NA

# Systematically replace individual components
U   <- "zero"
x0  <- "zero"
Q <- matrix(list("q11","q21","q31","q41","q51",
                 "q21","q22","q32","q42","q52",
                 "q31","q32","q33","q43","q53",
                 "q41","q42","q43","q44","q54",
                 "q51","q52","q53","q54","q55"))

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

B <- matrix(list("b11",    0,    0,    0,    0,
                 "b21","b22",    0,    0,"b25",
                 0,    0,"b33",    0,    0,
                 0,    0,    0,"b44",    0,
                 "b51",    0,    0,    0,"b55"),5,5)

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                        model = model.lst)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                        model = model.lst,
                                        inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)

# Compute parameter uncertainties
ssAnnDtrnds5Bds1U0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 85 iterations. 
# Log-likelihood: -2097.011 
# AIC: 4250.023   AICc: 4255.701   
# 
# ML.Est Std.Err    low.CI     up.CI
# R.(strmInno,strmInno)  4.48e+02 42.6360        NA        NA
# R.(precInno,precInno)  5.08e+03  2.8277        NA        NA
# R.(evapInno,evapInno)  4.27e+03 39.0455        NA        NA
# R.(rOffInno,rOffInno)  2.32e+03  4.1889        NA        NA
# R.(dStoInno,dStoInno)  2.24e+03      NA        NA        NA
# B.b11                  3.54e-01  0.0948  1.69e-01  5.40e-01
# B.b21                  1.79e-01  0.1720 -1.58e-01  5.16e-01
# B.b22                 -2.45e-01  0.0950 -4.32e-01 -5.93e-02
# B.b25                 -1.07e+00  0.0779 -1.22e+00 -9.14e-01
# B.b33                  3.29e-01  0.2287 -1.20e-01  7.77e-01
# B.b44                  2.90e-01  0.0752  1.42e-01  4.37e-01
# B.b51                  2.51e-01  0.1134  2.82e-02  4.73e-01
# B.b55                  4.17e-02  0.0407 -3.80e-02  1.21e-01
# Q.q11                  5.74e+04 32.5558  4.17e+04  1.20e+05
# Q.q21                  2.92e+04 13.9795  1.72e+04  6.28e+04
# Q.q31                 -9.87e+03 19.6288 -2.62e+04  1.32e+04
# Q.q41                  4.26e+04 11.3034  2.51e+04  6.37e+04
# Q.q51                  3.04e+04 11.0179  2.35e+04  5.87e+04
# Q.q22                  4.83e+04 11.5775  3.83e+04  7.49e+04
# Q.q32                 -1.27e+03 20.8271 -1.87e+04  2.08e+04
# Q.q42                  2.90e+04 11.6699  1.02e+04  4.03e+04
# Q.q52                  6.04e+04 10.1524  2.90e+04  5.61e+04
# Q.q33                  4.43e+04 22.0075  3.65e+04  6.59e+04
# Q.q43                 -1.10e+04 12.5732 -1.91e+04  6.54e+03
# Q.q53                 -2.07e+04  8.9982 -2.53e+04 -1.60e+03
# Q.q44                  5.36e+04  8.7513  2.67e+04  4.92e+04
# Q.q54                  5.91e+04  6.1901  2.09e+04  4.24e+04
# Q.q55                  1.27e+05  6.8979  3.05e+04  5.21e+04

# Systematically replace individual components
U   <- "zero"
x0  <- "zero"
Q <- matrix(list("q11","q21",    0,"q41","q51",
                 "q21","q22",    0,"q42","q52",
                     0,    0,"q33","q43","q53",
                 "q41","q42","q43","q44","q54",
                 "q51","q52","q53","q54","q55"),5,5)

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

B <- matrix(list("b11",    0,    0,    0,    0,
                 "b21","b22",    0,    0,"b25",
                 0,    0,"b33",    0,    0,
                 0,    0,    0,"b44",    0,
                 "b51",    0,    0,    0,"b55"),5,5)

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                        model = model.lst)

ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                        model = model.lst,
                                        inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5Bds1U0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu)

Bfull   <- matrix(c("b11","b12","b13","b14","b15",
                    "b21","b22","b23","b24","b25",
                    "b31","b32","b33","b34","b35",
                    "b41","b42","b43","b44","b45",
                    "b51","b52","b53","b54","b55"),5,5)

Boff <- matrix(c("b12","b13","b14","b15",
             "b21","b23","b24","b25",
             "b31","b32","b34","b35",
             "b41","b42","b43","b45",
             "b51","b52","b53","b54"),5,4)

Bdiag <- matrix(list("b11",    0,    0,    0,    0,
                    0,"b22",    0,    0,    0,
                    0,    0,"b33",    0,    0,
                    0,    0,    0,"b44",    0,
                    0,    0,    0,    0,"b55"),5,5)

# Specify sampling parameters
nSample <- 2000; nParm <- 2;
# Allocate matrices to store results
tmpMat <- matrix(NA,nSample,nParm); tmpVec <- matrix(NA,nSample,1)

# Conduct sampling
for (i in 1:nSample) {
  tmpMat[i,] <- sort(unlist(sample(Boff, nParm, replace=FALSE)))
  tmpVec[i]  <- str_c( tmpMat[i,1:nParm], collapse="")
}
# 
tmp <- unique(tmpVec)

AICc <- matrix(NA,length(tmp),1)
k    <- 0

for (i in 1:length(tmp)){
  # Populate off diag elements of AR matrix
  B1 <- Bdiag
  for (j in 1:nParm) {
    ndx <- match(substr(tmp[i],1+(3*(j-1)),3+(3*(j-1))),Bfull)
    B1[ndx] <- Bfull[ndx]
  }
  
  # Specify model
  model.lst <- list(B=B1, U=U, x0=x0, Q=Q, 
                    Z=Z, A=A,        R=R)
  
  ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                          model = model.lst)
  
  ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                          model = model.lst,
                                          inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)
  k       <- k + 1
  AICc[k] <- ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$AICc
}

# The drift term should be small since components were detrended
B   <- "identity"
U   <- "zero"
x0  <- "zero"
Q   <- "unconstrained"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5BiU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                      model = model.lst)

ssAnnDtrnds5BiU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                      model = model.lst,
                                      inits = ssAnnDtrnds5BiU0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5BiU0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5BiU0x0QduZiA0Rdu)

# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 166 iterations. 
# Log-likelihood: -2169.794 
# AIC: 4379.588   AICc: 4382.445   
# 
# ML.Est Std.Err low.CI up.CI
# R.(strmInno,strmInno)  2.72e-03  0.4226     NA    NA
# R.(precInno,precInno)  1.59e+04      NA     NA    NA
# R.(evapInno,evapInno)  4.43e+04 18.4509     NA    NA
# R.(rOffInno,rOffInno)  1.21e+04 23.0856     NA    NA
# R.(dStoInno,dStoInno)  8.78e-01  1.6746     NA    NA
# Q.(1,1)                1.18e+05 77.5282     NA    NA
# Q.(2,1)                1.51e+04 16.1684     NA    NA
# Q.(3,1)               -1.25e+04 34.6483     NA    NA
# Q.(4,1)                6.28e+04 30.8552     NA    NA
# Q.(5,1)               -3.24e+04 27.0991     NA    NA
# Q.(2,2)                8.48e+04 18.7592     NA    NA
# Q.(3,2)               -2.17e+04 25.5857     NA    NA
# Q.(4,2)                6.20e+04 25.1108     NA    NA
# Q.(5,2)                1.86e+05 18.0093     NA    NA
# Q.(3,3)                6.20e+03      NA     NA    NA
# Q.(4,3)               -1.97e+04      NA     NA    NA
# Q.(5,3)               -4.26e+04      NA     NA    NA
# Q.(4,4)                6.94e+04 25.4005     NA    NA
# Q.(5,4)                1.08e+05  5.0482     NA    NA
# Q.(5,5)                4.48e+05  0.0825     NA    NA

# The drift term should be small since components were detrended
B   <- "diagonal and unequal"
U   <- "zero"
x0  <- "zero"
Q   <- "unconstrained"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5BiU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                      model = model.lst)

ssAnnDtrnds5BiU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                      model = model.lst,
                                      inits = ssAnnDtrnds5BiU0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5BiU0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5BiU0x0QduZiA0Rdu)



# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 94 iterations. 
# Log-likelihood: -2122.007 
# AIC: 4294.014   AICc: 4298.512   
# 
# ML.Est Std.Err  low.CI up.CI
# R.(strmInno,strmInno)      1.37e+02      NA      NA    NA
# R.(precInno,precInno)      9.82e+03      NA      NA    NA
# R.(evapInno,evapInno)      1.49e+03 57.5549      NA    NA
# R.(rOffInno,rOffInno)      3.76e+02      NA      NA    NA
# R.(dStoInno,dStoInno)      1.35e+04      NA      NA    NA
# B.(X.strmInno,X.strmInno)  1.31e-01  0.1060 -0.0767 0.339
# B.(X.precInno,X.precInno) -1.46e-01  0.1748 -0.4888 0.196
# B.(X.evapInno,X.evapInno)  1.97e-01  0.1234 -0.0450 0.439
# B.(X.rOffInno,X.rOffInno)  9.11e-02  0.0727 -0.0515 0.234
# B.(X.dStoInno,X.dStoInno) -1.45e-01  0.1249 -0.3893 0.100
# Q.(1,1)                    8.82e+04 33.7310      NA    NA
# Q.(2,1)                    2.85e+04 13.1462      NA    NA
# Q.(3,1)                   -1.09e+04 13.4047      NA    NA
# Q.(4,1)                    5.24e+04 11.0971      NA    NA
# Q.(5,1)                    6.97e+03 11.6200      NA    NA
# Q.(2,2)                    4.65e+04      NA      NA    NA
# Q.(3,2)                   -1.26e+03 15.4060      NA    NA
# Q.(4,2)                    3.03e+04      NA      NA    NA
# Q.(5,2)                    6.50e+04 10.4986      NA    NA
# Q.(3,3)                    4.54e+04 15.4580      NA    NA
# Q.(4,3)                   -1.07e+04 14.0711      NA    NA
# Q.(5,3)                   -2.04e+04 19.0644      NA    NA
# Q.(4,4)                    5.71e+04  7.9084      NA    NA
# Q.(5,4)                    4.97e+04      NA      NA    NA
# Q.(5,5)                    1.41e+05      NA      NA    NA

# The drift term should be small since components were detrended
B   <- "unconstrained"
U   <- "zero"
x0  <- "zero"
Q   <- "unconstrained"

Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5BfuU0x0QfsZiA0Rdu <- MARSS(t(dtrndMatrix),
                                      model = model.lst)

ssAnnDtrnds5BfuU0x0QfsZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                      model = model.lst,
                                      inits = ssAnnDtrnds5BfuU0x0QfsZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5BfuU0x0QfsZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5BfuU0x0QfsZiA0Rdu)

ssAnnDtrnds5BfuU0x0QfsZiA0Rdu.kf <- MARSSkf(ssAnnDtrnds5BfuU0x0QfsZiA0Rdu)



# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 852 iterations. 
# Log-likelihood: -2071.59 
# AIC: 4233.181   AICc: 4248.571   
# 
# ML.Est Std.Err low.CI   up.CI
# R.(strmInno,strmInno)  7.37e-02  1.1061     NA      NA
# R.(precInno,precInno)  1.61e-04      NA     NA      NA
# R.(evapInno,evapInno)  3.61e+04 14.5077     NA      NA
# R.(rOffInno,rOffInno)  9.01e+03  7.6750     NA      NA
# R.(dStoInno,dStoInno)  2.49e-03  0.2414     NA      NA
# B.(1,1)                1.98e+00  0.8587  0.296  3.6625
# B.(2,1)               -7.60e-01  0.2460 -1.242 -0.2781
# B.(3,1)                6.85e-01  0.5563 -0.405  1.7753
# B.(4,1)                4.16e-01  0.3045 -0.181  1.0128
# B.(5,1)               -2.72e+00  0.3020 -3.316 -2.1321
# B.(1,2)               -6.65e-01  0.4541 -1.555  0.2247
# B.(2,2)                5.66e-01  0.4131 -0.243  1.3759
# B.(3,2)               -3.11e-01  0.3084 -0.916  0.2932
# B.(4,2)               -4.06e-01  0.2017 -0.801 -0.0105
# B.(5,2)                3.17e-01  0.2686 -0.210  0.8432
# B.(1,3)                1.08e+00  0.9974 -0.875  3.0351
# B.(2,3)               -6.43e-01  0.7092 -2.034  0.7466
# B.(3,3)               -3.23e-01  0.2121 -0.739  0.0922
# B.(4,3)                1.61e+00  0.0950  1.428  1.8003
# B.(5,3)                1.32e+00  0.0822  1.154  1.4768
# B.(1,4)               -1.93e+00  0.8934 -3.686 -0.1837
# B.(2,4)                6.69e-01  0.6866 -0.676  2.0152
# B.(3,4)               -1.22e+00  0.7806 -2.750  0.3103
# B.(4,4)                2.27e-01  0.2877 -0.337  0.7906
# B.(5,4)                4.07e+00  0.3628  3.363  4.7847
# B.(1,5)                1.30e+00  0.5392  0.238  2.3520
# B.(2,5)               -5.94e-01  0.2031 -0.992 -0.1958
# B.(3,5)                4.17e-01  0.3435 -0.256  1.0901
# B.(4,5)                3.72e-01  0.1319  0.113  0.6302
# B.(5,5)               -1.57e+00  0.2431 -2.048 -1.0950
# Q.(1,1)                5.11e+04 13.0859     NA      NA
# Q.(2,1)                3.58e+04 92.6923     NA      NA
# Q.(3,1)               -1.27e+04 35.2879     NA      NA
# Q.(4,1)                4.01e+04 21.4398     NA      NA
# Q.(5,1)                4.06e+04 55.4808     NA      NA
# Q.(2,2)                5.40e+04 55.7047     NA      NA
# Q.(3,2)               -3.31e+03      NA     NA      NA
# Q.(4,2)                3.64e+04 28.2652     NA      NA
# Q.(5,2)                6.60e+04 47.4831     NA      NA
# Q.(3,3)                8.61e+03 14.2646     NA      NA
# Q.(4,3)               -9.69e+03 17.8710     NA      NA
# Q.(5,3)               -8.71e+03  1.5625     NA      NA
# Q.(4,4)                3.49e+04 18.4227     NA      NA
# Q.(5,4)                4.42e+04  5.8397     NA      NA
# Q.(5,5)                8.91e+04      NA     NA      NA

# The drift term should be small since components were detrended
B   <- "diagonal and unequal"
U   <- "zero"
x0  <- "zero"
Q   <- "unconstrained"

Z   <- matrix(list("z11","z12","z13","z14","z15",
                      0 ,    1,    0,    0,    0,
                      0 ,    0,    1,    0,    0,
                      0 ,    0,    0,    1,    0,
                      0 ,    0,    0,    0,    1),5,5,byrow=TRUE)
A   <- "zero"
R   <- "diagonal and unequal"

model.lst <- list(B=B, U=U, x0=x0, Q=Q, 
                  Z=Z, A=A,        R=R)

ssAnnDtrnds5BiU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                      model = model.lst)

ssAnnDtrnds5BiU0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                      model = model.lst,
                                      inits = ssAnnDtrnds5BiU0x0QduZiA0Rdu$par)

# Compute parameter estimates
ssAnnDtrnds5BiU0x0QduZiA0Rdu.ci <- MARSSparamCIs(ssAnnDtrnds5BiU0x0QduZiA0Rdu)

ssAnnDtrnds5BiU0x0QduZiA0Rdu.kf <- MARSSkf(ssAnnDtrnds5BiU0x0QduZiA0Rdu)

ssAnnDtrnds5BiU0x0QduZiA0Rdu.kfss <- MARSSkfss(ssAnnDtrnds5BiU0x0QduZiA0Rdu)



points(ssAnnDtrnds5BiU0x0QduZiA0Rdu.kfss$xtT[1,], col="green", pch=3)



par(mfrow=c(1,1),mar=c(4,5,0.5,2))
plot(seq(1948,2010), stmrInno, pch=20, col="blue", xlab="year", 
     ylab=expression(paste("Detrended Streamflow, in  ",m^3%.% s^-1)))

lines(seq(1948,2010), ssAnnDtrnds5BiU0x0QduZiA0Rdu.kfss$xtt[1,], 
      col="black")



plot(years,dlm3$states[1,],col="red",type="l",
     ylim=c(min(dlm3$states[1,]-2*dlm3$states.se[1,]),
            max(dlm3$states[1,]+2*dlm3$states.se[1,])),
     ylab=expression(italic(alpha)[italic(t)]))
lines(years,dlm3$states[1,]+dlm3$states.se[1,],lty="dotted", col="red" )
lines(years,dlm3$states[1,]-dlm3$states.se[1,],lty="dotted", col="red" )

plot(years,dlm3$states[2,],col="blue",type="l",
     ylim=c(min(dlm3$states[2,]-2*dlm3$states.se[2,]),
            max(dlm3$states[2,]+2*dlm3$states.se[2,])),
     ylab=expression(italic(beta)[italic(t)]))
lines(years,dlm3$states[2,]+dlm3$states.se[2,],lty="dotted", col="blue" )
lines(years,dlm3$states[2,]-dlm3$states.se[2,],lty="dotted", col="blue" )
abline(h=0, lty="dashed", col="red")


