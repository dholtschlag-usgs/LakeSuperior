
# Set path to target directory
setwd("C:/Home/Projects/GLWaterBudget/Analysis/Superior")

# Creat data frame for analysis
if (!exists("NBSrcDf")) {
  # source script to set up data files
  source("Superior/src/work/setupMonthlyDataFrame.R")
}

# Create observation matrix
obsMatrix <- t(as.matrix(NBSrcDf[,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

# Aggregate by year

stmrCMSyr <- aggregate(NBSrcDf$stmrCMS,
                       by=list(format(NBSrcDf$DateSeq,'%Y')),
                       FUN=mean)

stmrCMSyr$Year <- as.numeric(stmrCMSyr$Group.1)


plot(stmrCMSyr$x,
     ylab="Log10 Annual Flow, in CMS",
     main="Flow from Lake Superior through St. Marys River",
     col="blue",pch=20)

m       <- 2

library(MARSS)

dat     <- matrix(stmrCMSyr$x, nrow=1)

# Set up linear deterministic model
mod.stmr.1 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                   B=matrix(1), U=matrix("u"), Q=matrix(0),
                   x0=matrix("a"))

# Standardize the response because the var(stmrCMS) >> var(year)
dat       <- matrix( (stmrCMSyr$x - mean(stmrCMSyr$x))/sd(stmrCMSyr$x), nrow=1 )

years      <- seq(1948,2010)

# Plot the standardized St. Marys river flow
plot(years, t(dat), type="p", col="blue", pch=20)

kem.1em   <- MARSS(dat, model=mod.stmr.1, silent=TRUE) 
print(kem.1em)
# Compute linear trend
kf1       <- MARSSkf(kem.1em)
# Plot linear trend line
lines(years, t(kf1$xtT),type="l", lty="dotdash",col="black")

# Dynamic linear model 
B       <- diag(m)
U       <- matrix(0, nrow=m, ncol=1)
Q       <- matrix(list(0),m,m)
diag(Q) <- c("q1","q2")
TT      <- 63

Z       <- array(NA, c(1,m,TT))
Z[1,1,] <- rep(1,TT)
Z[1,2,] <- matrix( c( seq(-31,31))/sd(seq(-31,31)), nrow=1)


A       <- matrix(0)
R       <- matrix("r")

inits.list <- list(x0=matrix(c(0, 0), nrow=m))

mod.list   <- list(B=B, U=U, Z=Z, A=A, R=R)

dlm1       <- MARSS(dat, model=mod.list, inits = inits.list)

dlm2       <- MARSS(dat, method="BFGS", model=mod.list,
                    inits = dlm1$par)

dlm2.ci    <- MARSSparamCIs(dlm2)
print(dlm2.ci)

dlm2.kf    <- MARSSkf(dlm2)

dlm2.kfss  <- MARSSkfss(dlm2)


par(mfrow=c(2,1),mar=c(4,6,0.5,2))
plot(years,dlm2$states[1,],col="red",type="l",
     ylim=c(min(dlm2$states[1,]-2*dlm2$states.se[1,]),
            max(dlm2$states[1,]+2*dlm2$states.se[1,])),
     ylab=expression(italic(alpha)[italic(t)]))
lines(years,dlm2$states[1,]+dlm2$states.se[1,],lty="dotted", col="red" )
lines(years,dlm2$states[1,]-dlm2$states.se[1,],lty="dotted", col="red" )

plot(years,dlm2$states[2,],col="blue",type="l",
     ylim=c(min(dlm2$states[2,]-2*dlm2$states.se[2,]),
            max(dlm2$states[2,]+2*dlm2$states.se[2,])),
     ylab=expression(italic(beta)[italic(t)]))
lines(years,dlm2$states[2,]+dlm2$states.se[2,],lty="dotted", col="blue" )
lines(years,dlm2$states[2,]-dlm2$states.se[2,],lty="dotted", col="blue" )
abline(h=0, lty="dashed", col="red")


lines(years,dlm2$states[2,],lty="dotted", col="blue" )

points(years,t(dlm2$ytT), pch=4, col="red")

#################################################################################################


# Dynamic linear model contrained by reason
B       <- diag(m)
U       <- matrix(0, nrow=m, ncol=1)
Q       <- matrix(list(0),m,m)
# diag(Q) <- c(0,1e-04)
diag(Q) <- c("q1","q2")
TT      <- 63

Z       <- array(NA, c(1,m,TT))
Z[1,1,] <- rep(1,TT)
Z[1,2,] <- matrix( c( seq(-31,31))/sd(seq(-31,31)), nrow=1)


A       <- matrix(0)
R       <- matrix("r")

inits.list <- list(x0=matrix(c(0, 0), nrow=m))

mod.list   <- list(B=B, U=U, Z=Z, A=A, R=R)

dlm1       <- MARSS(dat, model=mod.list, inits = inits.list)

dlm3       <- MARSS(dat, method="BFGS", model=mod.list,
                    inits = dlm1$par)

dlm3.ci    <- MARSSparamCIs(dlm3)
print(dlm2.ci)

dlm3.kf    <- MARSSkf(dlm3)

dlm3.kfss  <- MARSSkfss(dlm3)

dlm3.hatyt  <- MARSShatyt(dlm3)

par(mfrow=c(2,1),mar=c(4,6,0.5,2))
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


lines(years,dlm3$states[2,],lty="dotted", col="blue" )

points(years,t(dlm3$ytT), pch=2, col="black")



