# Plot major water budget components


par(mfrow=c(5,1),mar = c(2.1, 4, 1.5, 2) + 0.1)




with(NBSrcDf, plot(DateSeq, dStoCMS, type="l", col="salmon",
                   xlab="Year",
                   ylab="CMS", cex.main=0.9,
                   main="Change in Lake Superior Storage"))

# Set plot parameters for all paired time series and monthly boxplot
par(mfrow=c(2,1),mar=c(2, 4, 1, 2) + 0.1)

# Plot St. Marys River Flow
with(NBSrcDf, plot(DateSeq,stmrCMS, type="l",col="blue",
                   xlab="Year",cex.lab=0.90, cex.axis=0.90,
                   ylab="Flow, in CMS", cex.main=0.95, 
                   main="Flow of St. Marys River at Sault Ste. Marie, Ontario"))
#
boxplot(stmrCMS ~ Month, data = NBSrcDf,
        names=month.abb, col="lightblue", ylab="Flow, in CMS",
        notch=TRUE, cex.main=0.95, cex.lab=0.90, cex.axis=0.90,
        main="Average Monthly Flow of St. Marys River, 1948-2010")
abline(h=mean(NBSrcDf$stmrCMS),col="red",lty="dashed")

# Plot of Lake Superior Overlake Precipitation
with(NBSrcDf, plot(DateSeq, precCMS, type="l", col="blue",
                   xlab="Year",cex.lab=0.90, cex.axis=0.90,
                   ylab="Flow, in CMS", cex.main=0.95,
                   main="Lake Superior Overlake Precipitation"))

boxplot(precCMS ~ Month, data = NBSrcDf, type="l", col="lightblue",
        names=month.abb, ylab="Flow, in CMS",
        notch=TRUE, cex.main=0.95, cex.lab=0.90, cex.axis=0.90,
        main="Lake Superior Average Monthly Overlake Precipitation, 1948-2010")
abline(h=mean(NBSrcDf$precCMS),col="red",lty="dashed")


# Plot of Lake Superior Overlake Evaporation
with(NBSrcDf, plot(DateSeq, evapCMS, type="l", col="blue",
                   xlab="Year", cex.lab=0.90, cex.axis=0.90,
                   ylab="Flow, in CMS", cex.main=0.95,
                   main="Lake Superior Overlake Evaporation"))

boxplot(evapCMS ~ Month, data = NBSrcDf,
        names=month.abb, col="lightblue", ylab="Flow, in CMS",
        notch=TRUE, cex.main=0.95, cex.lab=0.90, cex.axis=0.90,
        main="Lake Superior Average Monthly Overlake Evaporation, 1948-2010")
abline(h=mean(NBSrcDf$evapCMS),col="red",lty="dashed")


# Plot of Basin Runoff to Lake Superior
with(NBSrcDf, plot(DateSeq, rOffCMS, type="l", col="blue",
                   xlab="Year", cex.lab=0.90, cex.axis=0.90,
                   ylab="Flow, in CMS",  cex.main=0.95,
                   main="Basin Runoff to Lake Superior"))

boxplot(rOffCMS ~ Month, data = NBSrcDf,
        names=month.abb, col="lightblue", ylab="Flow, in CMS",
        notch=TRUE, cex.main=0.95, cex.lab=0.90, cex.axis=0.90,
        main="Average Monthly Basin Runoff to Lake Superior, 1948-2010")
abline(h=mean(NBSrcDf$rOffCMS),col="red",lty="dashed")

# Plot of Monthly Change in Lake Superior Storage
with(NBSrcDf, plot(DateSeq, dStoCMS, type="l", col="blue",
                   xlab="Year", cex.lab=0.90, cex.axis=0.90,
                   ylab="Flow, in CMS", cex.main=0.95,
                   main="Monthly Changes in Lake Superior Storage"))

boxplot(dStoCMS ~ Month, data = NBSrcDf,
        names=month.abb, col="lightblue", ylab="Flow, in CMS",
        notch=TRUE, cex.main=0.95,
        main="Lake Superior Average Monthly Change in Storage, 1948-2010")
abline(h=mean(NBSrcDf$dStoCMS),col="red",lty="dashed")





par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
plot(NBSrcDf$DateSeq[1:744],NBSrcDf$stmrCMS[1:744],type="l",col="blue",
     xlab="Year",ylab="St. Marys River, CMS",
     main="Measured and Kalman Filter Estimates of St. Marys River Outflow ")
lines(NBSrcDf$DateSeq[1:744],s5o5SeaAR.kf$xtt1[1,],col="salmon")
legend("topright",legend=c("Measured","Modeled xtt1"),col=c("blue","salmon"),
       lty=c("solid","solid"),cex=0.8)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
plot(NBSrcDf$DateSeq[1:744],NBSrcDf$precCMS[1:744],type="l",col="blue",
     xlab="Year",ylab="Precipitation, CMS",
     main="Measured and Kalman Filter Estimates of Overlake Precipitation ")
lines(NBSrcDf$DateSeq[1:744],s5o5SeaAR.kf$xtt1[2,],col="salmon")
legend("topright",legend=c("Measured","Modeled xtt1"),col=c("blue","salmon"),
       lty=c("solid","solid"),cex=0.8)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
plot(NBSrcDf$DateSeq[1:744],NBSrcDf$evapCMS[1:744],type="l",col="blue",
     xlab="Year",ylab="Evaporation, CMS",
     main="Measured and Kalman Filter Estimates of Overlake Evaporation ")
lines(NBSrcDf$DateSeq[1:744],s5o5SeaAR.kf$xtt1[3,],col="salmon")
legend("topright",legend=c("Measured","Modeled xtt1"),col=c("blue","salmon"),
       lty=c("solid","solid"),cex=0.8)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
plot(NBSrcDf$DateSeq[1:744],NBSrcDf$rOffCMS[1:744],type="l",col="blue",
     xlab="Year",ylab="Runoff, CMS",
     main="Measured and Kalman Filter Estimates of Runoff to Lake Superior ")
lines(NBSrcDf$DateSeq[1:744],s5o5SeaAR.kf$xtt1[4,],col="salmon")
legend("topright",legend=c("Measured","Modeled xtt1"),col=c("blue","salmon"),
       lty=c("solid","solid"),cex=0.8)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
plot(NBSrcDf$DateSeq[1:744],NBSrcDf$dStoCMS[1:744],type="l",col="blue",
     xlab="Year",ylab="Change in Lake Storage, CMS",
     main="Measured and Kalman Filter Estimates of Change in Lake Storage ")
lines(NBSrcDf$DateSeq[1:744],s5o5SeaAR.kf$xtt1[5,],col="salmon")
legend("topright",legend=c("Measured","Modeled xtt1"),col=c("blue","salmon"),
       lty=c("solid","solid"),cex=0.8)

MARSSkfss()
library(MARSS)
s5o5SeaAR.ss    <- MARSSkfss(s5o5SeaAR.bfgs1)

plot(NBSrcDf$dStoCMS[1:744],s5o5SeaAR.kf$xtT[5,],pch=20,col="blue",cex=0.8,
     xlab="Measured change in lake storage, CMS",
     ylab="Estimated change in lake storage, CMS",
     main="Relation between estimates and measured changes in lake storage")
points(NBSrcDf$dStoCMS[1:744],s5o5SeaAR.ss$xtt[5,],pch=20,col="salmon",cex=0.8)
points(NBSrcDf$dStoCMS[1:744],s5o5SeaAR.kf$xtt1[5,],pch=20,col="green",cex=0.8)
abline(0,1,col="red",lty="dashed")
legend("bottomright",legend=c("x[ t ] | y[ T ]","x[ t ] | y[ t ]",
                              "x[ t ] | y[ t-1 ]","Line of agreement"),
        cex=1,col=c("blue","salmon","green","red"),pch=c(20,20,20,NA),
       lty=c(NA,NA,NA,"dashed"))


