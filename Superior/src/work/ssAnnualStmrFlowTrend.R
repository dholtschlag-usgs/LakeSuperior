
# Set path to target directory
setwd("C:/Home/Projects/GLWaterBudget/Analysis/Superior")

# source script to set up data files
source("Superior/src/work/setupMonthlyDataFrame.R")

# Create observation matrix
obsMatrix <- t(as.matrix(NBSrcDf[,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

pairs(t(obsMatrix))


plot(NBSrcDf$DateSeq[seq(from=1,to=756,by=12)],
     log10(NBSrcDf$stmrCMS[seq(from=1,to=756,by=12)]),
     type="l",col="blue",
     ylim=c(min(log10(NBSrcDf$stmrCMS)),
            max(log10(NBSrcDf$stmrCMS))),
     ylab="log10 Flow, in CMS",
     xlab="Year",
     main="Monthly Flows of St. Marys River from Lake Superior")
for (i in 2:12){
  lines(NBSrcDf$DateSeq[seq(from=i,to=756,by=12)],
        log10(NBSrcDf$stmrCMS[seq(from=i,to=756,by=12)]),
        type="l",col=i)
}

# Aggregate by year

stmrCMSyr <- aggregate(NBSrcDf$stmrCMS,
                       by=list(format(NBSrcDf$DateSeq,'%Y')),
                       FUN=mean)

stmrCMSyr$Year <- as.numeric(stmrCMSyr$Group.1)


plot(stmrCMSyr$Year,stmrCMSyr$x,
     ylab="Log10 Annual Flow, in CMS",
     main="Flow from Lake Superior through St. Marys River",
     col="blue",pch=20)


lmTrnd <- lm(stmrCMSyr$x ~ stmrCMSyr$Year)
summary(lmTrnd)
abline(reg=lmTrnd,col="red",lty="dashed")

library(MARSS)

par(mar=c(5,4,2,2)+0.1)
plot(stmrCMSyr$Year,stmrCMSyr$x,
     ylab="Annual Flow, in CMS",
     main="Annual Flow from Lake Superior through St. Marys River",
     col="blue",pch=20,xlab="Year",
     sub="No transformation of streamflow and linear trend.", 
     cex.sub=0.8)


# Deterministic level
mod.stmr.0 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                   B=matrix(1), U=matrix(0), Q=matrix(0),
                   x0=matrix("a"))

dat       <- t(stmrCMSyr$x)
kem.0em   <- MARSS(dat, model=mod.stmr.0, silent=TRUE) 
summary(kem.0em)

kf0       <- MARSSkf(kem.0em)

lines(stmrCMSyr$Year,kf0$xtT,lty="dashed",col="red")

# Deterministics slope
mod.stmr.1 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                   B=matrix(1), U=matrix("u"), Q=matrix(0),
                   x0=matrix("a"))

dat       <- t(stmrCMSyr$x)
kem.1em   <- MARSS(dat, model=mod.stmr.1, silent=TRUE) 
summary(kem.1em)
# Compute linear trend
kf1       <- MARSSkf(kem.1em)
# Plot linear trend line
lines(stmrCMSyr$Year,kf1$xtT,lty="dotdash",col="black")



# Stochastic level
mod.stmr.2 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                   B=matrix(1), U=matrix(0), Q=matrix("Q"),
                   x0=matrix("pi"))

dat       <- t(stmrCMSyr$x)
kem.2em   <- MARSS(dat, model=mod.stmr.2, silent=TRUE) 
summary(kem.2em)
# Plot stochastic level
kf2       <- MARSSkf(kem.2em)

lines(stmrCMSyr$Year,kf2$xtT,lty="solid",col="green")

# Stochastic trend
mod.stmr.3 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                   B=matrix(1), U=matrix("u"), Q=matrix("Q"),
                   x0=matrix("pi"))

dat       <- t(stmrCMSyr$x)
kem.3em   <- MARSS(dat, model=mod.stmr.3, silent=TRUE) 
summary(kem.3em)
# Plot stochastic level
kf3       <- MARSSkf(kem.3em)

lines(stmrCMSyr$Year,kf3$xtT,lty="solid",col="orange")

# How about a structural break in 1998
n    <- 13
c.row1 <- matrix( rep(1,63), 1, 63)
c.row2 <- matrix( c(rep(0,63-n),rep(1,n)), 1, 63)
c.in   <- rbind(c.row1,c.row2)

C    <- matrix("Step") 

C    <- matrix( c("Early","Late"), 1, 2)

# Deterministic level
mod.stmr.0 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                   B=matrix(0), U=matrix(0), Q=matrix(0),
                   x0=matrix(0), C=C, c=c.in)

dat       <- t(stmrCMSyr$x)
kem.4em   <- MARSS(dat, model=mod.stmr.0, silent=TRUE) 
summary(kem.0em)

kf4       <- MARSSkf(kem.4em)

kfss4     <- MARSSkfss(kem.4em)

lines(stmrCMSyr$Year,kf4$xtT,lty="solid",col="magenta")

####

# How about a structural break in 1972 and 1998
m      <- 38
c.row1 <- matrix( rep(1,63), 1, 63)
c.row2 <- matrix( c(rep(0,63-m),rep(1,m)), 1, 63)
c.row3 <- matrix( c(rep(0,63-n),rep(1,n)), 1, 63)
c.in   <- rbind(c.row1,c.row2,c.row3)

C    <- matrix( c("Early","Middle","Late"), 1, 3)

# Deterministic level
mod.stmr.0 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                   B=matrix(0), U=matrix(0), Q=matrix(0),
                   x0=matrix(0), C=C, c=c.in)

dat       <- t(stmrCMSyr$x)
kem.5em   <- MARSS(dat, model=mod.stmr.0, silent=TRUE) 
summary(kem.5em)

kf5       <- MARSSkf(kem.5em)

kfss5     <- MARSSkfss(kem.5em)

lines(stmrCMSyr$Year,kf5$xtT,lty="solid",col="brown")


####


legend("topright",
       legend=c("Data",
                paste0("Level: AICc= ",format(kem.0em$AICc,digits=4)),
                paste0("Linear trend: AICc= ",format(kem.1em$AICc,digits=4)),
                paste0("Stochastic level: AICc= ",format(kem.2em$AICc,digits=4)),
                paste0("Stochastic trend: AICc= ",format(kem.3em$AICc,digits=4)),
                paste0("Step trend 1998: AICc= ", format(kem.4em$AICc,digits=4)),
                paste0("Steps 1973,1998: AICc= ", format(kem.5em$AICc,digits=4))),
       col=c("blue","red","black","green","orange","magenta","brown"),
       pch=c(20,NA,NA,NA,NA,NA,NA), cex=0.7,
       lty=c(NA,"dashed","dotdash","solid","solid","dotted","twodash") )

# Plot density of innovations
stmrInno <- kfss4$Innov
denInnov <- density(stmrInno)
#
plot(denInnov,xlab="Residuals from Step Trend 1998, in CMS",
     main="Distribution of St. Marys River Flow Residuals from Step Trend 1998",
     type = "n", xlim=c(-1250, 1250), ylim=c(0,1.5e-3))
polygon(denInnov, col = "wheat")
abline(v=mean(kfss4$Innov), col="red", lty="dashed")

normDensity <- dnorm(seq(from=-1000, to=1000, by = 10),
                     mean = mean(kfss4$Innov),
                     sd   =   sd(kfss4$Innov))

lines(seq(from=-1000, to=1000, by = 10), normDensity, col="blue")

legend("topright",legend=c("Empirical","Zero","Normal"),
       col=c("black","red","blue"), cex=0.8,
       lty=c("solid","dashed","solid"),
       pch=c(NA) )
