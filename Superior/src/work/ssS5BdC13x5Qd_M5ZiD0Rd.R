# MARSS development for Lake Superior Water Budget

# Compile the data frame for analysis if needed
if (!exists("NBSrcDf", mode="list")) { source("setupMonthlyDataFrame.R")}

# Load the MARSS library
library(MARSS)

# Name Observation Series
obsVec <- c("stmr","prec","evap","rOff","dSto")

TT        <- 12*62  # 62 years of monthly data, max = 62 
obsMatrix <- t(as.matrix(NBSrcDf[1:TT,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

# Plot relation between PrcLk and PrcLd
#
with(NBSrcDf, plot(precCMS,PrcLdCMS, pch=20, col=Month,cex=0.8,
                   xlab="Overlake Precipitation, in cubic meters per second",
                   ylab="Overland Precipitation, in cubic meters per second",
                   cex.main=1.0,
                   main=paste0("Relation between Overlake and Overland\n",
                          "Precipitation for Lake Superior Basin")))
abline(0,1,col="red",lty="dashed")
legend("topleft",col=c(seq(1,12),2),pch=c(rep(20,12),NA),
       legend=c(month.abb,"Agreement"),cex=0.8,
       lty=c(rep(NA,12),"dashed"))

par(mar= c(5, 5, 4, 2) + 0.1.)
boxplot(PrcLdCMS-precCMS ~ Month, data = NBSrcDf, notch=TRUE, col="blue",
        names=month.abb,ylab="Overland minus Overlake Precipitation, in cms\n",
        cex.main=0.9,
        main="Monthly Differences between Lake Superior Overland and Overlake Precipitation")
abline(0,1,col="red",lty="dashed")

library(ggplot2)
qplot(precCMS, PrcLdCMS, data=NBSrcDf, colour = format(DateSeq,"%b"))

# Does the relation between overlake and overland precip vary by month

tmp    <- lm(PrcLdCMS ~ precCMS,         data=NBSrcDf)
tmpMth <- lm(PrcLdCMS ~ precCMS + Month, data=NBSrcDf)
anova(tmp,tmpMth)

NBSrcDf$month <- factor(NBSrcDf$Month, ordered = is.ordered(NBSrcDf$Month), labels=month.abb)

g <- ggplot(NBSrcDf, aes(precCMS, PrcLdCMS))
## Add layers
g1 <- g  + geom_point() + facet_wrap(~ month, ncol=4, nrow=3) 
g2 <- g1 + geom_smooth(method = "lm") + geom_abline(intercept=0, slope=1, colour="red")
g3 <- g2 + labs(title = "Relation Between Overlake and Overland Precipitation on Lake Superior Basin by Month") 
g3 + xlab("Overlake Precipitation, in cubic meters per second") + ylab("Overland Precipitation, in cubic meters per second")


+ facet_wrap(bmicat ~ no2dec, nrow = 3, ncol = 4)
+ geom_smooth(method="lm", se=FALSE, col="steelblue")
+ theme_bw(base_family = "Avenir", base_size = 10)
+ labs(x = expression("log " * PM[2.5])
       + labs(y = "Nocturnal Symptoms???)
 + labs(title = "MAACS Cohort???)
       
       


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
# Augment c.in with PrcLd
c.inp <- rbind(c.in,t(as.matrix(NBSrcDf[2:(TT+1),"PrcLdCMS"])))
rownames(c.inp) <- c(month.abb,"PrcLd")
# C = matrix(month.abb,5,12,byrow=TRUE)
# print(C)

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
# C  = "unconstrained"
C  <- matrix(list("C0101","C0102","C0103","C0104","C0105","C0106","C0107","C0108","C0109","C0110","C0111","C0112",0,
                  "C0101","C0202","C0203","C0204","C0205","C0206","C0207","C0208","C0209","C0210","C0211","C0212","C0213",
                  "C0301","C0302","C0303","C0304","C0305","C0306","C0307","C0308","C0309","C0310","C0311","C0312",0,
                  "C0401","C0402","C0403","C0404","C0405","C0406","C0407","C0408","C0409","C0410","C0411","C0412",0,
                  "C0501","C0502","C0503","C0504","C0505","C0506","C0507","C0508","C0509","C0510","C0511","C0512",0),
             5,13,byrow=TRUE)
#
# c  = "zero"                  # Data associated with state inputs
# Supersede c with c.in, defined above
c  = c.inp
D  = "zero"                  # Parameters associated with obs inputs
d  = "zero"                  # Data associated with obs inputs
pi = "unequal"               # All initial states are different
V0 = "zero"                  # The initial condition is fixed but unknown
tinitx = 0                   # The initial state refers to t=0 instead of t=1
#
model.list <- list(Z=Z,B=B,U=U,Q=Q,R=R,A=A,C=C,c=c.inp,D=D,d=d,V0=V0,tinitx=tinitx)

cntl.list  <- list(maxit=500) # MCInit=TRUE,numInits=10,
# s4o1Ar1SeaRun1.model = MARSS(obs,model=model.list,control=cntl.list)
s5o5SeaARprcLd <- MARSS(obsMatrix, method="BFGS",
                         model=model.list, control=cntl.list)

library(MARSS)
s5o5SeaARprcLd.ci    <- MARSSparamCIs(s5o5SeaARprcLd)

tmp             <- MARSSparamCIs(s5o5SeaAR.bfgs1, method="innovations", nboot=10)
# Plot CI about seasonal components
source("Superior/src/work/plotCmatrixCI.R")
plotCmatrixCI(s5o5SeaAR.ci)


# Compute Kalman filter estimates 
s5o5SeaARprcLd.kf    <- MARSSkf(s5o5SeaARprcLd)

#                        
# Success! Converged in 355 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 355 iterations. 
# Log-likelihood: -28809.06 
# AIC: 57788.12   AICc: 57792.15   



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
library(MARSS)

# s5o5SeaARprcLd <- MARSS(obsMatrix, method="BFGS",
#                         model=model.list, control=cntl.list)
#
# Success! Converged in 468 iterations.
# Function MARSSkfas used for likelihood calculation.
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 468 iterations. 
# Log-likelihood: -28725.13 
# AIC: 57620.25   AICc: 57624.28   
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
# 6: In KFS(kfas.model, simplify = FALSE) :
#   Possible error in smoothing: Negative variances in V, 
# try changing the tolerance parameter tol of the model.
# 7: In KFS(kfas.model, simplify = FALSE) :
#   Possible error in smoothing: Negative variances in V, 
# try changing the tolerance parameter tol of the model.
# s5o5SeaARprcLd.ci    <- MARSSparamCIs(s5o5SeaARprcLd)

# s5o5SeaARprcLd.ci
# 
# MARSS fit is
# Estimation method: BFGS 
# Estimation converged in 468 iterations. 
# Log-likelihood: -28725.13 
# AIC: 57620.25   AICc: 57624.28   
# 
# ML.Est  Std.Err    low.CI     up.CI
# R.(stmrCMS,stmrCMS)      1.15e-01 1.13e+00  3.49e+00  6.48e+00
# R.(precCMS,precCMS)      4.46e+05 1.87e+01  3.99e+05  4.97e+05
# R.(evapCMS,evapCMS)      5.91e+04 9.40e+01  3.48e+03  1.83e+05
# R.(rOffCMS,rOffCMS)      7.21e+04 4.06e+01  3.57e+04  1.21e+05
# R.(dStoCMS,dStoCMS)      9.16e+05 2.06e+02  3.07e+05  1.85e+06
# B.(X.stmrCMS,X.stmrCMS)  8.63e-01 6.58e-02  7.34e-01  9.92e-01
# B.(X.precCMS,X.precCMS)  7.24e-01 3.14e-02  6.62e-01  7.85e-01
# B.(X.evapCMS,X.evapCMS)  5.05e-01 1.13e-01  2.83e-01  7.27e-01
# B.(X.rOffCMS,X.rOffCMS)  5.71e-01 5.78e-02  4.57e-01  6.84e-01
# B.(X.dStoCMS,X.dStoCMS)  5.23e-01 1.46e-01  2.38e-01  8.09e-01
# U.X.stmrCMS              1.87e+02 7.61e+01  3.75e+01  3.36e+02
# U.X.precCMS             -1.02e+02 7.02e+01 -2.39e+02  3.56e+01
# U.X.evapCMS              6.35e+02 2.71e+02  1.04e+02  1.17e+03
# U.X.rOffCMS              6.23e+02 8.38e+01  4.59e+02  7.88e+02
# U.X.dStoCMS             -3.97e+00 3.54e+00 -1.09e+01  2.98e+00
# Q.(X.stmrCMS,X.stmrCMS)  5.74e+04 6.49e+00  5.15e+04  6.37e+04
# Q.(X.precCMS,X.precCMS)  3.56e+01 7.03e+01  1.74e+04  2.06e+04
# Q.(X.evapCMS,X.evapCMS)  1.77e+05 6.65e+01  8.46e+04  3.04e+05
# Q.(X.rOffCMS,X.rOffCMS)  1.30e+05 3.79e+01  8.19e+04  1.89e+05
# Q.(X.dStoCMS,X.dStoCMS)  6.88e+05 2.95e+02  6.29e+04  1.98e+06
# x0.X.stmrCMS             2.03e+03 2.81e+02  1.48e+03  2.59e+03
# x0.X.precCMS             1.51e+03 6.45e+02  2.48e+02  2.78e+03
# x0.X.evapCMS             4.20e+03 1.13e+03  1.99e+03  6.42e+03
# x0.X.rOffCMS             5.77e+02 7.54e+02 -9.01e+02  2.06e+03
# x0.X.dStoCMS            -2.69e+03 3.53e+03 -9.62e+03  4.23e+03
# C.C0101                 -2.03e+01 3.45e+01 -8.79e+01  4.72e+01
# C.C0301                  5.08e+02 2.34e+02  5.04e+01  9.66e+02
# C.C0401                 -2.28e+02 6.37e+01 -3.53e+02 -1.03e+02
# C.C0501                 -8.39e+02 3.03e+02 -1.43e+03 -2.46e+02
# C.C0102                  7.71e+01 8.26e+01 -8.47e+01  2.39e+02
# C.C0202                 -3.86e+02 1.27e+02 -6.35e+02 -1.38e+02
# C.C0302                 -5.23e+02 1.86e+02 -8.88e+02 -1.59e+02
# C.C0402                 -1.53e+02 6.27e+01 -2.76e+02 -3.03e+01
# C.C0502                 -4.83e+02 4.90e+02 -1.44e+03  4.78e+02
# C.C0103                  6.22e+01 6.60e+01 -6.72e+01  1.92e+02
# C.C0203                  4.56e+01 3.11e+01 -1.53e+01  1.07e+02
# C.C0303                 -3.21e+02 1.54e+02 -6.23e+02 -1.84e+01
# C.C0403                 -1.16e+01 8.16e+00 -2.76e+01  4.36e+00
# C.C0503                  6.86e+02 2.33e+02  2.29e+02  1.14e+03
# C.C0104                  1.30e+02 7.99e+01 -2.64e+01  2.87e+02
# C.C0204                 -1.11e+02 1.05e+03 -2.16e+03  1.94e+03
# C.C0304                 -6.05e+02 1.95e+02 -9.87e+02 -2.24e+02
# C.C0404                  1.48e+03 7.94e+01  1.33e+03  1.64e+03
# C.C0504                  2.88e+03 1.74e+02  2.54e+03  3.23e+03
# C.C0105                  2.92e+02 7.22e+01  1.51e+02  4.34e+02
# C.C0205                  1.38e+02 5.50e+02 -9.40e+02  1.22e+03
# C.C0305                 -8.08e+02 2.19e+02 -1.24e+03 -3.78e+02
# C.C0405                  6.22e+02 1.15e+02  3.97e+02  8.48e+02
# C.C0505                  1.48e+03 4.36e+02  6.21e+02  2.33e+03
# C.C0106                  1.89e+02 8.97e+01  1.37e+01  3.65e+02
# C.C0206                 -1.02e+00 8.79e+00 -1.82e+01  1.62e+01
# C.C0306                 -7.94e+02 2.75e+02 -1.33e+03 -2.55e+02
# C.C0406                 -3.77e+02 1.14e+02 -6.02e+02 -1.53e+02
# C.C0506                  7.00e+02 2.36e+02  2.38e+02  1.16e+03
# C.C0107                  1.78e+02 9.72e+01 -1.23e+01  3.69e+02
# C.C0207                 -4.49e+02 4.97e+02 -1.42e+03  5.25e+02
# C.C0307                 -6.77e+02 2.84e+02 -1.23e+03 -1.20e+02
# C.C0407                 -2.42e+02 9.37e+01 -4.26e+02 -5.84e+01
# C.C0507                  9.75e+01 2.04e+02 -3.03e+02  4.98e+02
# C.C0108                  2.11e+02 1.02e+02  1.18e+01  4.10e+02
# C.C0208                 -3.00e+02 2.56e+02 -8.01e+02  2.01e+02
# C.C0308                 -3.39e+02 2.98e+02 -9.23e+02  2.46e+02
# C.C0408                 -3.03e+02 6.47e+01 -4.29e+02 -1.76e+02
# C.C0508                 -4.39e+02 1.15e+02 -6.65e+02 -2.13e+02
# C.C0109                  8.47e+01 1.17e+02 -1.45e+02  3.14e+02
# C.C0209                  3.22e+02 1.47e+02  3.50e+01  6.10e+02
# C.C0309                  6.14e+02 2.64e+02  9.59e+01  1.13e+03
# C.C0409                 -9.17e+01 8.28e+01 -2.54e+02  7.06e+01
# C.C0509                 -7.44e+02 1.93e+02 -1.12e+03 -3.65e+02
# C.C0110                  3.76e+01 1.49e+02 -2.54e+02  3.30e+02
# C.C0210                 -3.64e+02 4.54e+02 -1.26e+03  5.26e+02
# C.C0310                  8.03e+02 1.68e+02  4.73e+02  1.13e+03
# C.C0410                  1.54e+02 8.20e+01 -6.75e+00  3.15e+02
# C.C0510                 -7.02e+02 2.22e+02 -1.14e+03 -2.67e+02
# C.C0111                  1.06e+02 1.12e+02 -1.13e+02  3.25e+02
# C.C0211                 -1.06e+02 3.36e+02 -7.64e+02  5.53e+02
# C.C0311                  1.45e+03 1.48e+02  1.16e+03  1.74e+03
# C.C0411                  1.31e+01 1.13e+01 -9.01e+00  3.53e+01
# C.C0511                 -1.06e+03 2.57e+02 -1.56e+03 -5.54e+02
# C.C0112                 -1.24e+01 7.75e+01 -1.64e+02  1.39e+02
# C.C0212                 -3.90e+01 2.21e+02 -4.71e+02  3.93e+02
# C.C0312                  1.32e+03 1.97e+02  9.38e+02  1.71e+03
# C.C0412                 -2.43e+02 7.65e+01 -3.93e+02 -9.35e+01
# C.C0512                 -1.58e+03 2.82e+02 -2.14e+03 -1.03e+03
# C.C0213                  3.67e-01 3.83e-02  2.92e-01  4.42e-01

