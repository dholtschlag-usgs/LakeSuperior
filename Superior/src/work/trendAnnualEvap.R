
# Set path to target directory
setwd("C:/Home/Projects/GLWaterBudget/Analysis/Superior")

# Creat data frame for analysis
if (!exists("NBSrcDf")) {
  # source script to set up data files
  source("Superior/src/work/setupMonthlyDataFrame.R")
}

# Create observation matrix
obsMatrix <- t(as.matrix(NBSrcDf[,c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS")]))

pairs(t(obsMatrix))

# Aggregate by year

evapCMSyr <- aggregate(NBSrcDf$evapCMS,
                       by=list(format(NBSrcDf$DateSeq,'%Y')),
                       FUN=mean)

evapCMSyr$Year <- as.numeric(evapCMSyr$Group.1)

library(MARSS)

par(mar=c(5,4,2,2)+0.1)
plot(evapCMSyr$Year,evapCMSyr$x,
     ylab="Annual Evaporation, in CMS",
     main="Annual Overlake Evaporation on Lake Superior",
     col="blue",pch=20,xlab="Year",
     cex.sub=0.8)


# Deterministic level
mod.stmr.0 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                   B=matrix(1), U=matrix(0), Q=matrix(0),
                   x0=matrix("a"))

dat       <- t(evapCMSyr$x)
kem.0em   <- MARSS(dat, model=mod.stmr.0, silent=TRUE) 
summary(kem.0em)

kf0       <- MARSSkf(kem.0em)
# Used to compute Innovations
kfss0     <- MARSSkfss(kem.0em)


lines(evapCMSyr$Year,kf0$xtT,lty="dashed",col="red")

# Deterministics slope
mod.stmr.1 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                   B=matrix(1), U=matrix("u"), Q=matrix(0),
                   x0=matrix("a"))

kem.1em   <- MARSS(dat, model=mod.stmr.1, silent=TRUE) 
summary(kem.1em)
# Compute linear trend
kf1       <- MARSSkf(kem.1em)

# Used to compute Innovations
kfss1     <- MARSSkfss(kem.1em)

# Plot linear trend line
lines(evapCMSyr$Year,kf1$xtT,lty="dotdash",col="black")




# How about a structural break in 1998
n    <- 13
c.row1 <- matrix( rep(1,63), 1, 63)
c.row2 <- matrix( c(rep(0,63-n),rep(1,n)), 1, 63)
c.in   <- rbind(c.row1,c.row2)

C    <- matrix( c("Early","Late"), 1, 2)

# Deterministic level
mod.stmr.0 <- list(Z=matrix(1), A=matrix(0), R=matrix("r"),
                   B=matrix(0), U=matrix(0), Q=matrix(0),
                   x0=matrix(0), C=C, c=c.in)

kem.4em   <- MARSS(dat, model=mod.stmr.0, silent=TRUE) 
summary(kem.4em)

kf4       <- MARSSkf(kem.4em)

kfss4     <- MARSSkfss(kem.4em)

lines(evapCMSyr$Year,kf4$xtT,lty="solid",col="magenta")

####


legend("topleft",
       legend=c("Data",
                paste0("Level: AICc= ",format(kem.0em$AICc,digits=4)),
                paste0("Linear trend: AICc= ",format(kem.1em$AICc,digits=4)),
                paste0("Step trend 1998: AICc= ", format(kem.4em$AICc,digits=4))),
       col=c("blue","red","black","magenta"),
       pch=c(20,NA,NA,NA,NA), cex=0.7,
       lty=c(NA,"dashed","dotdash","dotted") )

# Plot density of innovations
par(mar=c(5,4,2,2))
evapInno <- kfss1$Innov

denInnov <- density(evapInno)
#
plot(denInnov,xlab="Evaporation Residuals from Linear Trend, in CMS",
     main="Distribution of Annual Overlake Evaporation on Lake Superior from Linear Trend",
     type = "n", xlim=c(-700, 700), ylim=c(0,2e-3), cex.main=1, cex.axis = 0.8 )
polygon(denInnov, col = "wheat")
abline(v=mean(kfss1$Innov), col="red", lty="dashed")

normDensity <- dnorm(seq(from=-700, to=700, by = 10),
                     mean = mean(kfss1$Innov),
                     sd   =   sd(kfss1$Innov))

lines(seq(from=-700, to=700, by = 10), normDensity, col="blue")

legend("topright",legend=c("Empirical","Zero","Normal"),
       col=c("black","red","blue"), cex=0.8,
       lty=c("solid","dashed","solid"),
       pch=c(NA) )

