library(MARSS)

data(SalmonSurvCUI)

years <- SalmonSurvCUI[,1]

dat   <- matrix(SalmonSurvCUI[,2],nrow=1)

CUI   <- SalmonSurvCUI[,3]

# z-score the CUI
CUI.z <- matrix( (CUI - mean(CUI))/sd(CUI), nrow=1)

m     <- dim(CUI.z)[1] + 1

CUI.zts <- ts(data=t(CUI.z), start=years[1])
dat.ts  <- ts(data=t(dat),   start=years[1])

plot.ts(dat.ts)
plot.ts(CUI.zts)

datI    <- ts.intersect(CUI.zts, dat.ts)

plot(datI,yax.flip=TRUE, main="Test data", type="both", pch=20, 
     col="blue" )

B       <- diag(m)
U       <- matrix(0, nrow=m, ncol=1)
Q       <- matrix(list(0),m,m)
diag(Q) <- c("q1","q2")
TT      <- 42

Z       <- array(NA, c(1,m,TT))
Z[1,1,] <- rep(1,TT)
Z[1,2,] <- CUI.z

A       <- matrix(0)
R       <- matrix("r")

inits.list <- list(x0=matrix(c(0, 0), nrow=m))

mod.list   <- list(B=B, U=U, Z=Z, A=A, R=R)

dlm1       <- MARSS(dat, inits=inits.list, model=mod.list)
print(dlm1)

dlm1.ci    <- MARSSparamCIs(dlm1)

dlm1.kf    <- MARSSkf(dlm1)

dlm1.kfss  <- MARSSkfss(dlm1)

par(mfrow=c(2,1),mar=c(4,4,0.5,2))
plot(years,dlm1$states[1,],col="red",type="l",
     ylim=c(min(dlm1$states[1,]-2*dlm1$states.se[1,]),
            max(dlm1$states[1,]+2*dlm1$states.se[1,])),
     ylab=expression(italic(alpha)[italic(t)]))
lines(years,dlm1$states[1,]+dlm1$states.se[1,],lty="dotted", col="red" )
lines(years,dlm1$states[1,]-dlm1$states.se[1,],lty="dotted", col="red" )

plot(years,dlm1$states[2,],col="blue",type="l",
     ylim=c(min(dlm1$states[2,]-2*dlm1$states.se[2,]),
            max(dlm1$states[2,]+2*dlm1$states.se[2,])),
     ylab=expression(italic(beta)[italic(t)]))
lines(years,dlm1$states[2,]+dlm1$states.se[2,],lty="dotted", col="blue" )
lines(years,dlm1$states[2,]-dlm1$states.se[2,],lty="dotted", col="blue" )

par(mfrow=c(1,1))
plot(years, t(dat), col="blue", type="b")
points(years,t(dlm1$ytT), pch=4, col="red")
