# Simulation to assess the uncertainty of estimating monthly flow at 04119000 
#   Grand River at Grand Rapids, MI on the basis of monthly flow at 04116000 Grand River at Ionia, MI
#   with a drainage correction factor of 4900/2840 -> 1.7193
#
# Clear workspace
rm(list=ls())
# Get current working directory to set path name
pName      <- getwd()
#
# Source function readMonthlyFlow
source("Superior/src/work/readMonthlyFlows.R")
#
# Create data frames of monthly flows at streamgages 
#   using assign for dynamic assignment of variable names
name1190     <- "04119000 Grand River at Grand Rapids, MI"
assign(paste0("Q",substr(name1190,1,8)),readMonthlyFlows(substr(name1190,1,8)))
name1160   <- "04116000 Grand River at Ionia, MI"
assign(paste0("Q",substr(name1160,1,8)),readMonthlyFlows(substr(name1160,1,8)))
name1165   <- "04116500 Flat River near Smyrna, MI"
assign(paste0("Q",substr(name1165,1,8)),readMonthlyFlows(substr(name1165,1,8)))
name1180   <- "04118000 Thornapple River near Caledonia, MI"
assign(paste0("Q",substr(name1180,1,8)),readMonthlyFlows(substr(name1180,1,8)))
name1185   <- "04118500 Rouge River near Rockford, MI"
assign(paste0("Q",substr(name1185,1,8)),readMonthlyFlows(substr(name1185,1,8)))
#
# Set up Grand River estimation data set 
#   Merge data frame on Year and Month
GrandRiver <- merge(Q04119000, Q04116000)
GrandRiver <- merge(GrandRiver,Q04116500)
GrandRiver <- merge(GrandRiver,Q04118000)
GrandRiver <- merge(GrandRiver,Q04118500)
# 
# Create field containing month number
GrandRiver$month_nu <- as.numeric(format(GrandRiver$cDate,"%m"))
#
# Drainage Areas at Streamgages, in sq. miles
drnArea1190 <- 4900
drnArea1160 <- 2840
drnArea1165 <-  528
drnArea1180 <-  773
drnArea1185 <-  234
# The gaged drainage area upstream from 04119000 is
drnAreaGaged    <- drnArea1160 + drnArea1165 + drnArea1180 + drnArea1185
print(paste("The gaged drainage area upstream from 04119000 is",format(drnAreaGaged,dig=4),
            "square miles.",sep=" "))
# 
print(paste("The drainage area at 04119000 is",format(drnArea1190,di=4),
            "square miles.",sep=" "))
#
# Multiply monthly flows at 1160 by ratio of drnArea[1190/1160] for inital estimate
#
# Set up Grand River prediction data set
gr1190Pred <- Q04119000[677:nrow(Q04119000),]
# Grand River at Ionia, MI is the persistent upstream gage
gr1190Pred <- merge(gr1190Pred,Q04116000)
# 
# Plot time series of monthly flows from 1952-1982
plot(Q04119000$cDate,Q04119000$o4119000,log="y",cex=0.7,pch=20,col="blue",
     ylim=c(80,25000),xlim=c(as.Date("01/01/1930","%d/%m/%Y"),
                             as.Date("31/10/2013","%d/%m/%Y")),
     xlab="Year",ylab="Flow, in cubic feet per second",
     main="Time Series of Measured Flows in Grand River Basin")
points(Q04116000$cDate,Q04116000$o4116000,cex=0.7,pch=20,col="orange")
points(Q04116500$cDate,Q04116500$o4116500,cex=0.7,pch=20,col="green")
points(Q04118000$cDate,Q04118000$o4118000,cex=0.7,pch=20,col="black")
points(Q04118500$cDate,Q04118500$o4118500,cex=0.7,pch=20,col="salmon")
legend("bottomleft",
       legend=c("04119000","04116000","04116500","04118000","04118500"),
       col=c("blue","orange","green","black","salmon"),
       pch=c(20),cex=0.8,horiz=FALSE)
#
# Compute a flow estimate for Grand River at Grand Rapids, MI by adjusting 
#   Grand River at Ionia, MI by drainage area ratio
gr1190Pred$pred1190_ratio1160  <- gr1190Pred$o4116000 * drnArea1190/drnArea1160
#
# Plot the monthly flow estimate with the measured value for 04119000
par(mar=c(5,5,4,2)+0.1)
plot(GrandRiver$o4119000,GrandRiver$o4116000* drnArea1190/drnArea1160,
     pch=20,col="blue",cex=0.65,cex.main=1,cex.lab=0.8,cex.axis=0.8,
     log="xy",xlab=paste0("Measured flow at ",name1190,", in cfs"),
     ylab=paste0("Estimated flow based on Grand River at Ionia, MI\n",
                 "adjusted for drainage area at Grand Rapids, in cfs"),
     main=paste("Relation between measured and estimated monthly at Grand River at Grand Rapids, MI\n",
                "based on drainage area adjusted monthly flow at Grand River at Ionia, MI"))
points(gr1190Pred$o4119000,gr1190Pred$pred1190_ratio1160,
       col="green",pch=20,cex=0.65)
abline(0,1,col="red",lty="dashed")
legend("topleft",legend=c("March 1952 - March 1982","April 1982 - Sep. 2013","Line of agreement"),
       col=c("blue","green","red"),pch=c(20,20,NA),lty=c(NA,NA,"dashed"),cex=0.8)
#
# Compute residuals of drainage area ratio methoc
gr1190Pred$resid1190_ratio1160 <- gr1190Pred$o4119000 - gr1190Pred$pred1190_ratio1160
# Compute month indicator
gr1190Pred$month_nu <- as.numeric(format(gr1190Pred$cDate,"%m"))
#
paste("The bias of the ratio estimator is ",
      format(mean(gr1190Pred$resid1190_ratio1160),digits=4),", in cfs",sep="")
paste("The variance of the ratio estimator is ",
      format(var(gr1190Pred$resid1190_ratio1160),digits=4),", in cfs",sep="")
# RMSE is the square root of the bias squared plus the variance
rmse1190_ratio1160 <- sqrt(mean(gr1190Pred$resid1190_ratio1160)^2 + 
                             var(gr1190Pred$resid1190_ratio1160))
paste("The RMSE of the 1190_1160 ratio estimator for the prediction interval is ",
      format(rmse1190_ratio1160,digits=4),", in cfs",sep="")
#
# Boxplot of Prediction errors by month
par(cex.lab=0.8,cex.main=0.9)
boxplot(gr1190Pred$resid1190_ratio1160 ~ 
          gr1190Pred$month_nu, xlab="Month",ylab="Flow Residuals, cfs",
        axes=FALSE,main=paste0("Monthly Distribution of Flow Residuals for Grand River at Grand Rapids, MI based on a Drainage Area \n",
                               "Adjustment of Monthly Flows at Grand River at Ionia, MI for Prediction Period 1982-04 to 2013-09"),
        sub=paste("The RMSE for the prediction interval is ",
                  format(rmse1190_ratio1160,digits=4),", in cfs",sep=""),
        cex.sub=0.7,notch=F)
axis(side=1,at=1:12,labels=month.abb)
axis(side=2)
abline(h=0,col="red",lty="solid")
#
## Develop regression estimator of 1190 given 1160 in natural log space
log1160     <- log(GrandRiver$o4116000)
lm1190_1160 <- lm(log(GrandRiver$o4119000) ~ log1160)
summary(lm1190_1160)
# Predict flows at 1190 with regression on 1160
log1160     <- log10(gr1190Pred$o4116000)
# 
gr1190Pred$pred1190_regres1160
# 
model1190_given1160 <- predict(lm1190_1160,
                              newdata=as.data.frame(log1160),se.fit=TRUE,
                              interval="prediction")
# Smearing error correction as the mean antilog of the residuals
meanExpResid <- mean(exp(lm1190_1160$residuals))
# Compute prediction in arithmetic space corrected for transformation bias
gr1190Pred$pred1190_regres1160 <- exp(model1190_given1160$fit[,1]) * meanExpResid
# Compute residuals of drainage area ratio methoc
gr1190Pred$resid1190_regres1160 <- gr1190Pred$o4119000 - gr1190Pred$pred1190_regres1160
# Compute month indicator
gr1190Pred$month_nu <- as.numeric(format(gr1190Pred$cDate,"%m"))
#
paste("The bias of the simple regression estimator is ",
      format(mean(gr1190Pred$resid1190_regres1160),digits=4),", in cfs",sep="")
paste("The variance of the simple regression estimator is ",
      format(var(gr1190Pred$resid1190_regres1160),digits=4),", in cfs",sep="")
# RMSE is the square root of the bias squared plus the variance
rmse1190_regres1160 <- sqrt(mean(gr1190Pred$resid1190_regres1160)^2 + 
                             var(gr1190Pred$resid1190_regres1160))
# Root mean square error of the simple regression estimator
paste("The RMSE of the 1190_1160 simple regression estimator for the prediction interval is ",
      format(rmse1190_regres1160,digits=4),", in cfs",sep="")
#
# Boxplot of Prediction errors by month
par(cex.lab=0.8,cex.main=0.9)
boxplot(gr1190Pred$resid1190_regres1160 ~ 
          gr1190Pred$month_nu, xlab="Month",ylab="Flow Residuals, cfs",
        axes=FALSE,main=paste0("Monthly Distribution of Flow Residuals for Grand River at Grand Rapids, MI based on a Simple Linear\n",
                               "Regression of Monthly Flows at Grand River at Ionia, MI for Prediction Period 1982-04 to 2013-09"),
        sub=paste("The RMSE for the prediction interval is ",
                  format(rmse1190_regres1160, digits=4),", in cfs",sep=""),
        cex.sub=0.7,notch=TRUE)
axis(side=1,at=1:12,labels=month.abb)
axis(side=2)
abline(h=0,col="red",lty="solid")
#
## Start multiple regression estimate
# Develop linear model for pred1165_regres1160
log1160     <- log10(GrandRiver$o4116000)
lm1165_ll60 <- lm(log10(GrandRiver$o4116500) ~ log1160 )
summary(lm1165_ll60)
month_no    <- as.factor(GrandRiver$month_nu)
lm1165_1160mon <- lm(log10(GrandRiver$o4116500) ~ log1160 + month_no)
summary(lm1165_1160mon)
anova(lm1165_ll60,lm1165_1160mon)

pacfResid65  <- pacf(residuals(lm1165_1160mon),
                     main="Partial ACF of log Flow residuals at 04116500")

# Predict flows at 1165 with regression on 1160 with newdata
log1160             <- log10(gr1190Pred$o4116000)
month_no            <- gr1190Pred$month_nu
newData             <- as.data.frame(cbind(log1160,month_no))
newData$month_no    <- as.factor(newData$month_no)
model1165_given1160 <- predict(lm1165_1160mon,newdata=newData,se.fit=TRUE,
                               interval="prediction")
# Smearing error correction as the mean antilog of the residuals
meanExpResid <- mean(exp(lm1165_1160mon$residuals))
# Compute prediction in arithmetic space corrected for transformation bias
gr1190Pred$pred1165_regres1160 <- exp(model1165_given1160$fit[,1]) * meanExpResid
# Dont know measured values during predicted interval so cannot compute residuals 
# Develop linear model for pred1180_regres1160
log1160        <- log10(GrandRiver$o4116000)
lm1180_1160    <- lm(log10(GrandRiver$o4118000) ~ log1160)
summary(lm1180_1160)

month_no       <- as.factor(GrandRiver$month_nu)
lm1180_1160mon <- lm(log10(GrandRiver$o4118000) ~ log1160 + month_no)
summary(lm1180_1160mon)
anova(lm1180_1160,lm1180_1160mon)

pacfResid80  <- pacf(residuals(lm1180_1160),
                     main="Partial ACF of log Flow residuals at 04118000")

# Predict flows at 1165 with regression on 1160 with newdata
log1160     <- log10(gr1190Pred$o4116000)
model1180_given1160 <- predict(lm1180_1160,
                               newdata=as.data.frame(log1160),se.fit=TRUE,
                               interval="prediction")
# Smearing error correction as the mean antilog of the residuals
meanExpResid <- mean(exp(lm1180_1160$residuals))
# Compute prediction in arithmetic space corrected for transformation bias
gr1190Pred$pred1180_regres1160 <- exp(model1180_given1160$fit[,1]) * meanExpResid
#
## Develop linear model for pred1185_regres1160
log1160     <- log10(GrandRiver$o4116000)
lm1185_1160 <- lm(log10(GrandRiver$o4118500) ~ log1160)
summary(lm1185_1160)
#
month_no       <- as.factor(GrandRiver$month_nu)
lm1185_1160mon <- lm(log10(GrandRiver$o4118500) ~ log1160 + month_no)
summary(lm1185_1160mon)
anova(lm1185_1160,lm1185_1160mon)
#
pacfResid85  <- pacf(residuals(lm1185_1160mon),
                     main="Partial ACF of log Flow residuals at 04118500")

# Predict flows at 1185 with regression on 1160 with newdata
log1160             <- log10(gr1190Pred$o4116000)
month_no            <- gr1190Pred$month_nu
newData             <- as.data.frame(cbind(log1160,month_no))
newData$month_no    <- as.factor(newData$month_no)
model1185_given1160 <- predict(lm1185_1160mon,newdata=newData,se.fit=TRUE,
# Add predicted values at 1165, 1180, and 1185, to measured values at 1160 (log10 Q)
pred1190_multReg    <- ((10^model1165_given1160$fit[,1] + 10^model1180_given1160$fit[,1] +
                          10^model1185_given1160$fit[,1] + gr1190Pred$o4116000) * 
                          drnArea1190/drnAreaGaged)
#
resid1190_multReg   <- log10(gr1190Pred$o4119000) - log10(pred1190_multReg)
#
boxplot(resid1190_multReg ~ gr1190Pred$month_nu,xlab="Month",
        ylab="Flow Residual, in log10 CFS",notch=TRUE,
        axes=FALSE,
        main=paste("Distribution of flow residuals at 04119000 given regression estimates for 04116500,\n",
                   " 04118000, 04118500, and measured flows at 04116000 for the period Apr 1982 to Sep 2013",sep=""),
        sub=paste("The bias and RMSE for the prediction interval are ",
                  format(mean(resid1190_multReg),digits=4)," and ",
                  format(sqrt(mean(resid1190_multReg)^2 + 
                                var(resid1190_multReg)),digits=4),
                  " in log10 cfs, respectively.",sep=""),
                  cex.sub=0.8)
#
axis(side=1,at=1:12,labels=month.abb)
axis(side=2)
abline(h=0,col="red",lty="dashed")

resid1190_ratio <- log10(gr1190Pred$o4119000) - log10(drnArea1190/drnArea1160 *
                                            gr1190Pred$o4116000);
boxplot(resid1190_ratio ~ gr1190Pred$month_nu,xlab="Month",
        ylab="Flow Residual, in log10 CFS",notch=TRUE,
        axes=FALSE,
        main=paste("Distribution of flow residuals at 04119000 given drainage area adjusted \n",
                   " measured flows at 04116000 for the period Apr 1982 to Sep 2013",sep=""),
        sub=paste("The bias and RMSE for the prediction interval are ",
                  format(mean(resid1190_ratio),digits=4)," and ",
                  format(sqrt(mean(resid1190_ratio)^2 + 
                                var(resid1190_ratio)),digits=4),
                  " in log10 cfs, respectively.",sep=""),
        cex.sub=0.8)
#
axis(side=1,at=1:12,labels=month.abb)
axis(side=2)
abline(h=0,col="red",lty="dashed")

plot(gr1190Pred$cDate,gr1190Pred$o4119000,log="y",type="l",col="blue",
     ylim=c(500,15000),xlab="Year",ylab="Flow, in CFS",
     main=paste("Hydrographs of Measured and Estimated Flows at 04119000\n",
                "Grand River at Grand Rapids, MI for the period Apr 1982 to Sep 2013",
                sep=""))
lines(gr1190Pred$cDate, pred1190_multReg,type="l",col="red")
lines(gr1190Pred$cDate, drnArea1190/drnArea1160 * gr1190Pred$o4116000,
      type="l",col="green")
legend("bottomleft",legend=c("Measured","Multiple Regression","Ratio"),
       col=c("blue","red","green"),lty=c("solid","solid","solid"),
       pch=c(NA,NA,NA),horiz=TRUE,cex =0.8)

plot(ecdf(gr1190Pred$o4119000),col="blue",pch=".",
     xlab="Flow, in CFS",ylab="Cumulative frequency",
     main=paste("Cumulative Frequency Distribution of measured and estimated flows at 04119000 \n",
                "Grand River at Grand Rapids, MI for the period Apr 1982 to Sep 2013",
                sep=""))
lines(ecdf(pred1190_multReg),col="red",pch=".")
lines(ecdf(drnArea1190/drnArea1160 * gr1190Pred$o4116000),col="green",pch=".")
legend("bottomright",legend=c("Measured","Multiple Regression","Ratio"),
       col=c("blue","red","green"),lty=c("solid","solid","solid"),
       pch=c(NA,NA,NA),horiz=FALSE,cex =0.8)


plot(gr1190Pred$cDate,model1165_given1160$fit[1,],type="l",col="blue")

# Smearing error correction as the mean antilog of the residuals
meanExpResid <- mean(exp(lm1185_1160$residuals))
# Compute prediction in arithmetic space corrected for transformation bias
gr1190Pred$pred1185_regres1160 <- exp(model1185_given1160$fit[,1]) * meanExpResid
#
## Add flows from all tribs and adjustment for drainage area
gr1190Pred$pred1190_multRegRatio <- drnArea1190/drnAreaGaged * 
  ( gr1190Pred$o4116000 + gr1190Pred$pred1165_regres1160 +
      gr1190Pred$pred1180_regres1160 + gr1190Pred$pred1185_regres1160)
# Compute residuals of predicted values
gr1190Pred$resid1190_multRegRatio <- gr1190Pred$o4119000 - gr1190Pred$pred1190_multRegRatio  
#
paste("The bias of the multiple regression estimator is ",
       format(mean(gr1190Pred$resid1190_multRegRatio),digits=4),", in cfs",sep="")
paste("The variance of the simple regression estimator is ",
      format(var(gr1190Pred$resid1190_multRegRatio),digits=4),", in cfs",sep="")
# RMSE is the square root of the bias squared plus the variance
rmse1190_multRegRatio <- sqrt(mean(gr1190Pred$resid1190_multRegRatio)^2 + 
                              var(gr1190Pred$resid1190_multRegRatio))
# Root mean square error of the multiple regression estimator
paste("The RMSE of the 1190_multRegRatio regression estimator for the prediction interval is ",
      format(rmse1190_multRegRatio,digits=4),", in cfs",sep="")
#
# Boxplot of Prediction errors by month
par(cex.lab=0.8,cex.main=0.9)
boxplot(gr1190Pred$resid1190_multRegRatio ~ 
          gr1190Pred$month_nu, xlab="Month",ylab="Flow Residuals, cfs",
        axes=FALSE,main=paste0("Monthly Distribution of Flow Residuals for Grand River at Grand Rapids, MI based on a Simple Linear\n",
                               "Regression of Multiple Monthly Flows at Grand River at Ionia, MI for Prediction Period 1982-04 to 2013-09"),
        sub=paste("The RMSE for the prediction interval is ",
                  format(rmse1190_multRegRatio, digits=4),", in cfs",sep=""),
        cex.sub=0.7,notch=TRUE)
axis(side=1,at=1:12,labels=month.abb)
axis(side=2)
abline(h=0,col="red",lty="solid")
#
## Add flows from all tribs and adjustment for drainage area
GrandRiver$pred1190_meaRatio <- drnArea1190/drnAreaGaged *
  (GrandRiver$o4116000 + GrandRiver$o4116500 + GrandRiver$o4118000 + GrandRiver$o4118500)

GrandRiver$resid1190_meaRatio <- log10(GrandRiver$o4119000) - 
                                 log10(GrandRiver$pred1190_meaRatio) 
#
paste("The bias of the multiple regression estimator is ",
      format(mean(GrandRiver$resid1190_meaRatio),digits=4),", in cfs",sep="")
paste("The variance of the simple regression estimator is ",
      format(var(GrandRiver$resid1190_meaRatio),digits=4),", in cfs",sep="")
# RMSE is the square root of the bias squared plus the variance
rmse1190_meaRatio <- sqrt(mean(GrandRiver$resid1190_meaRatio)^2 + 
                                var(GrandRiver$resid1190_meaRatio))
# Root mean square error of the multiple regression estimator
paste("The RMSE of the 1190_multRegRatio simple regression estimator for the prediction interval is ",
      format(rmse1190_meaRatio,digits=4),", in cfs",sep="")
#
# Boxplot of Estimation errors by month
par(cex.lab=0.8,cex.main=0.9)
boxplot(GrandRiver$resid1190_meaRatio ~ 
          GrandRiver$month_nu, xlab="Month",ylab="Flow Residuals, log10 cfs",
        axes=FALSE,main=paste0("Monthly Distribution of Flow Residuals for Grand River at Grand Rapids, MI based on Area Adjusted\n",
                               "Flows at Upstream Gages 04116000, 04116500, 04118000, and 04118500 from Feb 1952 to Mar 1982"),
        sub=paste("The bias and RMSE for the estimation interval are ",
                  format(mean(GrandRiver$resid1190_meaRatio), digits=4),", and ",
                  format(rmse1190_meaRatio, digits=4),", in log10 cfs, respectively.",sep=""),
        cex.sub=0.7,notch=TRUE)
axis(side=1,at=1:12,labels=month.abb)
axis(side=2)
abline(h=0,col="red",lty="dashed")
#











source("Superior/src/work/plotModelCompare.R")
plotModelCompare(GrandRiver,"o4119000","o4119000_Ratio_o4116000",name1190,name1160,
                 titleMod <- paste("Relation between measured flows at ",name1190,
                                   "\nand drainage-area adjusted water yields at ",name1160))
#
# Boxplot errors by month
GrandRiver$pred1190_1160ratio  <- drnArea1190/drnArea1160 * GrandRiver$o4116000
GrandRiver$resid1190_1160ratio <- log10(GrandRiver$o4119000) - 
                                  log10(GrandRiver$pred1190_1160ratio)
#
bias1190_1160ratio <- mean(GrandRiver$resid1190_1160ratio)
var1190_1160ratio  <- var(GrandRiver$resid1190_1160ratio)
rmse1190_1160ratio <- sqrt(bias1190_1160ratio^2 + var1190_1160ratio)
par(cex.lab=0.8,cex.main=0.9)
boxplot(GrandRiver$resid1190_1160ratio ~ GrandRiver$month_nu, 
        xlab="Month",ylab="Flow Residuals, in log10 cfs", axes=FALSE,
        main=paste0("Monthly Distribution of Flow Residuals for Grand River at Grand Rapids, MI based on\n",
        "Drainage Area Adjusted Monthly Flows at Grand River at Ionia, MI from Feb 1952 to Mar 1983"),
        sub=paste("The bias and RMSE for the estimation interval are ",
                  format(bias1190_1160ratio,digits=4)," and ",
                  format(rmse1190_1160ratio,digits=4),", in cfs, respectively.",
                  sep=""),cex.sub=0.7,notch=TRUE)
axis(side=1,at=1:12,labels=month.abb)
axis(side=2)
abline(h=0,col="red",lty="dashed")
# 
# Compare with estimates using multiple streamgages
GrandRiver$o4119000_Ratio_60658085 <- drnArea1190/drnAreaGaged *
  (GrandRiver$o4116000 + GrandRiver$o4116500 + GrandRiver$o4118000 +
     GrandRiver$o4118500)
#
nameComposite <- "60658085 Grand River Streamgages Upstream from Grand Rapids, MI"
#
# Plot multiple station estimated based on drainage area ration
plotModelCompare(GrandRiver,'o4119000','o4119000_Ratio_60658085',
                 name1190,nameComposite,
                 titleMod <- paste("Relation between measured flows at ",name1190,
                                   "\nand drainage-area adjusted water yields at ",
                                   nameComposite))
# 
# 
# par(cex.lab=0.8,cex.main=0.9)
# plot(GrandRiver$o4119000,GrandRiver$o4119000_60658085,
#      xlab="Measured Monthly Flow at 04119000 Grand River at Ionia, MI in ft3/s",
#      ylab="Estimated Monthly Flow at 04119000 based on 0411[60,65,80,85]00, ft3/s",
#      main="Relation between measured monthly flow at 04119000 Grand River at Grand Rapids, ft3/s
#      and measured monthly flows at Upstream Gages Adjusted for Drainage Area",
#      pch=20, cex=0.6, col="blue",log="xy")
# abline(a=0,b=1,col="red",lty="solid")
#
#
# Boxplot residuals by Month of multiple station estimate
par(cex.lab=0.8,cex.main=0.9)
boxplot(GrandRiver$o4119000-GrandRiver$o4119000_Ratio_60658085 ~ 
          GrandRiver$month_nu, xlab="Month",ylab="Flow Residuals, cfs",
        axes=FALSE,main="Monthly Distribution of Flow Residuals for Grand River at Grand Rapids, MI based on
        a Drainage Area Adjustment Monthly Flows at 0411[60,65,80,85]00",notch=TRUE)
axis(side=1,at=1:12,labels=month.abb)
axis(side=2)
abline(h=0,col="red",lty="solid")
#
# Find relation between 04116000 and other streamgages
# Work in a yield rather than a flow metric
y04116000  <- GrandRiver$o4116000/drnArea1160
y04116500  <- GrandRiver$o4116500/drnArea1165
y04118000  <- GrandRiver$o4118000/drnArea1180
y04118500  <- GrandRiver$o4118500/drnArea1185
y04119000  <- GrandRiver$o4119000/drnArea1190
# # Distribution of water yields among gages
boxplot(cbind(y04116000,y04116500,y04118000,y04118500),notch=TRUE)
# Natural log transform log transform flows for linear modeling
ly04116000  <- log10(y04116000)
ly04116500  <- log10(y04116500)
ly04118000  <- log10(y04118000)
ly04118500  <- log10(y04118500)
ly04119000  <- log10(y04119000)
# Distribution of log water yields among gages
boxplot(cbind(ly04116000,ly04116500,ly04118000,ly04118500,ly04119000),
        notch=TRUE,col="lightblue",cex.main=0.9,
        main ="Distribution of Water Yields for Selected Streamgages in the Grand River Basin",
        sub  ="February 1952 to March 1982",cex.sub=0.9,
        ylab =expression(paste("Water Yields, log10 ft3/(s-mi^2)",sep="")),
        xlab ="Selected USGS Streamgages in Grand River Basin",
        names = c("04116000","04116500","04118000","04118500","04119000"))
abline(h=median(ly04119000),col="red",lty="dashed")
#
# Natural log transform flows for linear modeling
lq04116000  <- log(GrandRiver$o4116000)
lq04116500  <- log(GrandRiver$o4116500)
lq04118000  <- log(GrandRiver$o4118000)
lq04118500  <- log(GrandRiver$o4118500)
lq04119000  <- log(GrandRiver$o4119000)
#
#
source("Superior/src/work/selectModel.R")
selModel <- selectModel(lq04119000,lq04116000,as.factor(GrandRiver$month_nu))
#
if (selModel$rank>12){
  # If the selected model has monthly factor, then plot the monthly parameters
  plot(c(0,coefficients(selModel)[3:13]),xaxt="n",col="blue",pch=20,las=1,
       ylab="Regression Parameter Estimate",xlab="Month",
       main="Monthly Parameter Estimates for 04116500 based on 04116000")
  axis(side=1, at = 1:12, labels=month.abb)
  abline(h=0,col="red",lty="dotted")
  #
}
## Compute streamflow estimates and prediction errors
conf1190 <- predict(selModel,se.fit = TRUE,interval = 'conf',level=0.95)
pred1190 <- predict(selModel,se.fit = TRUE,interval = 'pred',level=0.95)
ordPred  <- order(selModel$fit)
plot(selModel$fit[ordPred],col="black",type="l",
     main="Monthly Flow at 04119000 Grand River at Grand Rapids, MI
     Given Monthly Flow at 04116000 Grand River at Ionia, MI and Month",
     xlab="Flow Magnitude Ordered by Expected Value",
     ylab="ln(Flow) at 04119000, in ft3/s",ylim=c(6.5,10))
points(conf1190$fit[ordPred,2],col="red",type="l",cex=0.5)
points(conf1190$fit[ordPred,3],col="red",type="l",cex=0.5)
points(pred1190$fit[ordPred,2],col="blue",type="l",cex=0.5)
points(pred1190$fit[ordPred,3],col="blue",type="l",cex=0.5)
points(lq04116000[ordPred] + log(drnArea1190) - log(drnArea1160),
       col="darkgrey",pch=20,cex=0.5)
legend("topleft",legend=c("Expected","95% Confidence Interval",
                          "95% Prediction Interval","Data Pairs"),
       col=c("black","red","blue","darkgrey"),cex=0.8,
       pch=c(NA,NA,NA,20),lty=c("solid","solid","solid","blank"))
#
# 
## Regression log(Q_04116500)|log(Q_04116000)
selModel <- selectModel(lq04116500,lq04116000,as.factor(GrandRiver$month_nu))
#
if (selModel$rank>12){
  # If the selected model has monthly factor, then plot the monthly parameters
  plot(c(0,coefficients(selModel)[3:13]),xaxt="n",col="blue",pch=20,las=1,
       ylab="Regression Parameter Estimate",xlab="Month",
       main="Monthly Parameter Estimates for 04116500 based on 04116000")
  axis(side=1, at = 1:12, labels=month.abb)
  abline(h=0,col="red",lty="dotted")
  #
}
## Compute streamflow estimates and prediction errors
conf1165 <- predict(selModel,se.fit = TRUE,interval = 'conf',level=0.95)
pred1165 <- predict(selModel,se.fit = TRUE,interval = 'pred',level=0.95)
ordPred  <- order(selModel$fit)
plot(selModel$fit[ordPred],col="black",type="l",
     main=paste("Monthly Flow at ",name1165, 
     "Given Monthly Flow at ",name1160," and Month"),
     xlab="Flow Magnitude Ordered by Expected Value",
     ylab="ln(Flow) at 04116500, in ft3/s",ylim=c(4.5,7.5))
points(conf1165$fit[ordPred,2],col="red",type="l",cex=0.5)
points(conf1165$fit[ordPred,3],col="red",type="l",cex=0.5)
points(pred1165$fit[ordPred,2],col="blue",type="l",cex=0.5)
points(pred1165$fit[ordPred,3],col="blue",type="l",cex=0.5)
points(lq04116500[ordPred],
       col="darkgrey",pch=20,cex=0.5)
legend("topleft",legend=c("Expected","95% Confidence Interval",
                          "95% Prediction Interval","Data Pairs"),
       col=c("black","red","blue","darkgrey"),cex=0.8,
       pch=c(NA,NA,NA,20),lty=c("solid","solid","solid","blank"))
#
par(las=1,mar=c(5,5,4,2)+0.1,cex.main=0.9)
plot(lq04116500,selModel$fit,pch=20,col="blue",cex.lab=0.8,cex.axis=0.8,
     xlab=paste0("Measured Ln of Monthly Flow at ",name1165),
     ylab=paste0("Predicted Ln Monthly Flow at \n",name1165),
     main=paste0("Relation between Estimated Flows at",name1165,
                 "\ngiven Ln Monthly Flow at ",name1160," and month"))
abline(0,1,col="black",lty="dashed")
legend("bottomright",legend=c("Data pairs","Line of agreement"),
       col=c("blue","black"),pch=c(20,NA),lty=c(NA,"dashed"),cex=0.8)
#
#   
plot(selModel$fit,selModel$se.fit,pch=20,col="red",cex=0.5)
lines(c(min(selModel$fit)))
# Compute the standard error of prediction 
sePred116500model <-  (pred1165$fit[3,]-pred1165$fit[2,])/(2*qt(0.975,pred1165$df))


plot(pred1165$fit[ordPred,2]/conf1165$fit[ordPred,2])

print(summary(lm04116500mon))
# Results show that the monthly factors neither AR[1] component is not sigificant
#
lm04116500 <- lm(lq04116500 ~ lq04116000)
print(summary(lm04116500))
# Compute the percent error
cv <- (exp(1)^summary(lm04116500)$sigma - 1)*100
print(paste("The regression error for logQ at 04116500|04116000 expressed as a percent is: ",
            format(cv,digits=4)))
#
lm04116500mon <- lm(lq04116500 ~ lq04116000 + as.factor(GrandRiver$month_nu))
print(summary(lm04116500mon))
# Compute the percent error
cvmon <- (exp(1)^summary(lm04116500mon)$sigma - 1)*100
print(paste("The regression error for logQ at 04116500|04116000 + I(month) expressed as a percent is: ",
            format(cvmon,digits=4)))
#
# The month indicators explain a significant amount of variance
anova(lm04116500,lm04116500mon)
#
# Compute the percent error
cv <- (exp(1)^summary(lm04116500)$sigma - 1)*100
print(paste("The regression error for logQ at 04116500|04116000 expressed as a percent is: ",
            format(cv,digits=6)))
print(paste("The regression mean for logQ at 04116500|04116000 will likely vary from",
            format(1/(1+cv/100)*100,digits=3),"to",format((1+cv/100)*100,digits=3),
            "percent of its true value.",sep=" "))
plot(lq04116000,lq04116500,pch=20,col="blue",cex=0.6,
     main="Relation Between Monthly Yields at 04116000 and 04116500 on Grand River",
     xlab="Log Monthly Flow at 04116000, in ft3/s",
     ylab="Estimated Log Monthly Flow at 04116500, in ft3/s")
abline(reg=lm04116500mon,col="red",lty="solid")
legend("topleft",legend=c("Data pairs","Regression Line with monthly factors",
                          cex.legend=0.7,
       col=c("blue","red"),pch=c(20,NA),lty=c(NA,"solid"))
#
# Analyze error structure of model residuals
pacfResid65  <- pacf(residuals(lm04116500mon),
                     main="Partial ACF of log Flow residuals at 04116500")
# Note: linear model with months as factors are used
arResMod65 <- arima(x = residuals(lm04116500mon),order=c(1,0,0))
print(arResMod65)
#
# Plot time series of log flow at 04116500 
plot(GrandRiver$cDate,GrandRiver$o4116500,type="l",col="blue",log="y",
     ylim=c(50,4000),xlab="Year",ylab="Flow at 04116500, in cfs",
     main=paste("Time Series of Flow at",name1165,sep=" "))
#
# Initialize matrix for nSim stochastic simulations
nSim       <- 1000
stochMat65 <- matrix(NA,length(GrandRiver$o4116500),nSim)
stochCI65  <- matrix(NA,length(GrandRiver$o4116500),2) 
for (i in 1:nSim){
  arResSim65 <- arima.sim(n=length(GrandRiver$o4116500), 
                          list(order = c(1,0,0), ar = arResMod65$coef[1]),
                          sd= sqrt(arResMod65$sigma2))
  stochMat65[,i] <- exp(1)^(fitted.values(lm04116500mon) + arResSim65)  
  lines(GrandRiver$cDate,stochMat65[,i],col="grey")
  
}
for (j in 1:length(GrandRiver$o4116500)){
  stochCI65[j,]  <- quantile(stochMat65[j,],c(0.025,0.975),na.rm=TRUE)
}
colnames(stochCI65) <- c("climit02_5","climit97_5")
#
# nGood is the number of measurements that fall within the 95 stochastic CI
nGood <- 0
for (i in 1:nrow(stochCI65)){
  if (stochCI65[i,1]<= GrandRiver$o4116500[i] & GrandRiver$o4116500[i] < stochCI65[i,2]){
  nGood = nGood + 1 
  }
}
print(paste0("The percent of cases within the nominal 95-% interval is ",
             format(nGood/nrow(stochCI65)*100,digits=4)))
#
# Develop a relation between standard deviation and yyyy-mon
stochStd65  <- apply(stochMat65,1,sd)
stochMean65 <- rowMeans(stochMat65) 
plot(stochMean65,stochStd65,col="blue",pch=20,cex=0.7,ylim=c(0,250),xlim=c(0,1200))
# Measured uncertainty: Excellent => +/- 5%, Good => +/- 10%, Fair => +/- 15%, Poor => >15%
points(GrandRiver$o4116500,GrandRiver$o4116500 * 0.10,col="red",pch=20,cex=0.7)
points(GrandRiver$o4116500,(GrandRiver$o4116500 * 0.10)/sqrt(30),col="green",pch=20,cex=0.7)
legend('topleft',legend=c("Regression Uncertainty","Measured Uncertainty with perfect correlation",
                          "Measurement Uncertainty with no autocorrelation"),
       col=c("blue","red","green"),pch=c(20,20,20),cex=0.6)


# Blue line shows measured values
lines(GrandRiver$cDate,GrandRiver$o4116500,type="l",col="blue")
#
stochMean65 <- rowMeans(stochMat65)
# Red line shows mean of stochastic simulation
lines(GrandRiver$cDate,stochMean65,col="red")
# Green lines show the upper and lower 2.5% confidence limits
lines(GrandRiver$cDate,stochCI65[,1],col="green")
lines(GrandRiver$cDate,stochCI65[,2],col="green")
legend('topleft',legend=c("Stochastic Realization","Measured",
                          "Stochastic Mean","Stochastic 95% Interval"),
       col=c("grey","blue","red","green"),
       lty=c(rep("solid",4)),pch=c(rep(NA,4)),cex=0.6)

# 
# Look at distribution of simulated flows in a particular month
monRndSel     <- sample(seq(1:nrow(stochMat65)),1)  # Randomly select a yr-month
stoRndSel65   <- stochMat65[monRndSel,]             # Store the stochastic realization
plot(density(stoRndSel65),
     main=paste("Density of Simulated Monthly Flows for on month",monRndSel,
                "in",format(GrandRiver$cDate[monRndSel],"%b-%Y"),sep=" "),
     xlab="Flow, in cubic feet per second", ylab="Probability density")
lines()
# Plot 
abline(v=GrandRiver$o4116500[monRndSel],col="red")

abline(v=exp(1)^mean(log(stoRndSel65)),col="green",lty="dashed")
abline(v=median(stoRndSel65),col="orange",lty="dotted")
#
### Station 04118000

# Compute regression in terms of flow
lm04118000mon <- lm(log(GrandRiver$o4118000) ~ log(GrandRiver$o4116000) + 
                    as.factor(GrandRiver$month_nu))
print(summary(lm04118000mon))
#
lm04118000    <- lm(log(GrandRiver$o4118000) ~ log(GrandRiver$o4116000))
print(summary(lm04118000))
#
# ANOVA indicates that month as.factor does not significantly improve the model 
anova(lm04118000,lm04118000mon)
# Analyze error structure of model residuals
pacfResid80  <- pacf(residuals(lm04118000),
                     main="Partial ACF of log Flow residuals at 04118000")
# 
arResMod80 <- arima(x = residuals(lm04118000),order=c(2,0,0))
print(arResMod80)
#
# Plot time series of log flow at 04118000 
plot(GrandRiver$cDate,GrandRiver$o4118000,type="l",col="blue",log="y",
     ylim=c(50,6000),xlab="Year",ylab="Flow at 04118000, in cfs",
     main=paste("Time Series of Flow at",name1180,sep=" "))
# Initialize matrix for nSim stochastic simulations
nSim       <- 1000
stochMat80 <- matrix(NA,length(GrandRiver$o4118000),nSim)
stochCI80  <- matrix(NA,length(GrandRiver$o4118000),2) 
for (i in 1:nSim){
  arResSim80 <- arima.sim(n=length(GrandRiver$o4118000), 
                          list(order = c(2,0,0), ar = arResMod80$coef[1:2]),
                          sd= sqrt(arResMod80$sigma2))
  #
  stochMat80[,i] <- exp(1)^(fitted.values(lm04118000) + arResSim80)  
  lines(GrandRiver$cDate,stochMat80[,i],col="grey")
  
}
for (j in 1:length(GrandRiver$o4118000)){
  stochCI80[j,]  <- quantile(stochMat80[j,],c(0.025,0.975),na.rm=TRUE)
}

lines(GrandRiver$cDate,GrandRiver$o4118000,type="l",col="blue")
#
stochMean80 <- rowMeans(stochMat80)
lines(GrandRiver$cDate,stochMean80,col="red")
# plot 2.5% Confidence Limit
lines(GrandRiver$cDate,stochCI80[,1],col="green")
lines(GrandRiver$cDate,stochCI80[,2],col="green")
legend('topleft',legend=c("Stochastic Realization","Measured",
                          "Stochastic Mean","Stochastic 95% Interval"),
       col=c("grey","blue","red","green"),
       lty=c(rep("solid",4)),pch=c(rep(NA,4)),cex=0.6)

#
###
# Test an AR term 
lm04118500    <- lm(lq04118500 ~ lq04116000 )
print(summary(lm04118500))
# Test an AR term and seasonal factor in the model
lm04118500mon <- lm(lq04118500 ~ lq04116000 + as.factor(GrandRiver$month_nu))
print(summary(lm04118500mon))
# Results indicate that monthly factors significantly improve the model
anova(lm04118500,lm04118500mon)
#
# Results show that neither AR[1] nor seasonal terms are sigificant
#
# Compute the percent error
cv <- (exp(1)^summary(lm04118500mon)$sigma - 1)*100
print(paste("The regression error for logQ at 04118500|04116000 expressed as a percent is: ",
            format(cv,digits=6)))
print(paste("The regression mean for logQ at 04118500|04116000 will likely vary from",
            format(1/(1+cv/100)*100,digits=3),"to",format((1+cv/100)*100,digits=3),
            "percent of its true value.",sep=" "))
plot(lq04116000,lq04118500,pch=20,col="blue",cex=0.6,
     main="Relation Between Monthly Yields at 04116000 and 04118500 on Grand River",
     xlab="Log Monthly Flow at 04116000, in ft3/s",
     ylab="Estimated Log Monthly Flow at 04118500, in ft3/s")
abline(reg=lm04118500,col="red",lty="solid")
legend("topleft",legend=c("Data pairs","Regression Line"),
       col=c("blue","red"),pch=c(20,NA),lty=c(NA,"solid"))
#
# Analyze error structure of model residuals
pacfResid85  <- pacf(residuals(lm04118500mon),main="Partial ACF of log Flow residuals at 04118500")
# 
arResMod85 <- arima(x = residuals(lm04118500mon),order=c(1,0,0))
print(arResMod85)
#
# Plot time series of log flow at 04118500 
plot(GrandRiver$cDate,GrandRiver$o4118500,type="l",col="blue",log="y",
     ylim=c(50,2000),xlab="Year",ylab="Flow at 04118500, in cfs",
     main=paste("Time Series of Flow at",name1185,sep=" "))
#
# Initialize matrix for nSim stochastic simulations
nSim       <- 1000
stochMat85 <- matrix(NA,length(GrandRiver$o4118500),nSim)
stochCI85  <- matrix(NA,length(GrandRiver$o4118500),2) 
for (i in 1:nSim){
  arResSim85 <- arima.sim(n=length(GrandRiver$o4118500), 
                          list(order = c(1,0,0), ar = arResMod85$coef[1]),
                          sd= sqrt(arResMod85$sigma2))
  stochMat85[,i] <- exp(1)^(fitted.values(lm04118500) + arResSim85)  
  lines(GrandRiver$cDate,stochMat85[,i],col="grey")
  
}
for (j in 1:length(GrandRiver$o4118500)){
  stochCI85[j,]  <- quantile(stochMat85[j,],c(0.025,0.975),na.rm=TRUE)
}

lines(GrandRiver$cDate,GrandRiver$o4118500,type="l",col="blue")
#
stochMean85 <- rowMeans(stochMat85)
lines(GrandRiver$cDate,stochMean85,col="red")
# plot 2.5% Confidence Limit
lines(GrandRiver$cDate,stochCI85[,1],col="green")
lines(GrandRiver$cDate,stochCI85[,2],col="green")
legend('topleft',legend=c("Stochastic Realization","Measured",
                          "Stochastic Mean","Stochastic 95% Interval"),
       col=c("grey","blue","red","green"),
       lty=c(rep("solid",4)),pch=c(rep(NA,4)),cex=0.6)
#
### 
# 
# Plot time series of log flow at 04118500 
plot(GrandRiver$cDate,GrandRiver$o4116000,type="l",col="blue",log="y",
     ylim=c(100,20000),xlab="Year",ylab="Flow at 04116000, in cfs",
     main=paste("Time Series of Flow at",name1160,sep=" "))
#
# Initialize matrix for nSim stochastic simulations
nSim       <- 1000
stochMat60 <- matrix(NA,length(GrandRiver$o4116000),nSim)
stochCI60  <- matrix(NA,length(GrandRiver$o4116000),2) 
for (i in 1:nSim){
  measNoise      <- rnorm(n=length(GrandRiver$o4116000),mean=0, sd=0.05)
  stochMat60[,i] <- exp(1)^(log(GrandRiver$o4116000) + measNoise)  
  lines(GrandRiver$cDate,stochMat60[,i],col="grey")
  
}
for (j in 1:length(GrandRiver$o4116000)){
  stochCI60[j,]  <- quantile(stochMat60[j,],c(0.025,0.975),na.rm=TRUE)
}

lines(GrandRiver$cDate,GrandRiver$o4116000,type="l",col="blue")
#
stochMean60 <- rowMeans(stochMat60)
lines(GrandRiver$cDate,stochMean60,col="red")
# plot 2.5% Confidence Limit
lines(GrandRiver$cDate,stochCI60[,1],col="green")
lines(GrandRiver$cDate,stochCI60[,2],col="green")
legend('topleft',legend=c("Stochastic Realization","Measured",
                          "Stochastic Mean","Stochastic 95% Interval"),
       col=c("grey","blue","red","green"),
       lty=c(rep("solid",4)),pch=c(rep(NA,4)),cex=0.6)

# 
y    <- rep(NA,4)  # Yield
w    <- rep(NA,4)  # weight
ndxYrMon <- sample(1:nrow(GrandRiver),1)
y[1] <- mean(stochMat60[ndxYrMon,]/drnArea1160)
y[2] <- mean(stochMat65[ndxYrMon,]/drnArea1165)
y[3] <- mean(stochMat80[ndxYrMon,]/drnArea1180)
y[4] <- mean(stochMat85[ndxYrMon,]/drnArea1185)
#
w[1] <- log(drnArea1160)/var(stochMat60[ndxYrMon,])
w[2] <- log(drnArea1165)/var(stochMat65[ndxYrMon,])
w[3] <- log(drnArea1180)/var(stochMat80[ndxYrMon,])
w[4] <- log(drnArea1185)/var(stochMat85[ndxYrMon,])

# Compute the tot gaged flow from the stochMat
# Allocate vector for total Gaged Flow
totGagedFlow <- rep(NA,nrow(GrandRiver))
# Allocate vector for variance of Gaged Flow
varGagedFlow <- rep(NA,nrow(GrandRiver))
#
for (i in 1:nrow(GrandRiver)){
  totGagedFlow[i] <- sum(       c(stochMean60[i],stochMean65[i],stochMean80[i],stochMean85[i]))
  varGagedFlow[i] <- sum(cov(cbind(stochMat60[i,],stochMat65[i,],stochMat80[i,],stochMat85[i,])))
}
plot(GrandRiver$cDate,totGagedFlow,type="l",col="blue",log="y")
lines(GrandRiver$cDate,GrandRiver$o4119000,type="l",col="red")
stdGagedFlow      <- sqrt(varGagedFlow)
ci975             <- totGagedFlow + qnorm(0.975) * stdGagedFlow
lines(GrandRiver$cDate,ci975,type="l",col="green")
ci025             <- totGagedFlow + qnorm(0.025) * stdGagedFlow
lines(GrandRiver$cDate,ci025,type="l",col="green")
#
# Compute a wgt mean yield
yldStocMean    <- matrix(NA,nrow(GrandRiver),4)  # mean yield of stochastic series
wgtStocMean    <- matrix(NA,nrow(GrandRiver),4)  # weight 
yldWghtMean    <- rep(NA,nrow(GrandRiver))    # weighted mean
yldWghtVar     <- rep(NA,nrow(GrandRiver))    # weighted variance
# 
for (i in 1:nrow(GrandRiver)){ 
  #
  yldStocMean[i,1]  <- mean(stochMat60[i,]/drnArea1160)
  yldStocMean[i,2]  <- mean(stochMat65[i,]/drnArea1165)
  yldStocMean[i,3]  <- mean(stochMat80[i,]/drnArea1180)
  yldStocMean[i,4]  <- mean(stochMat85[i,]/drnArea1185)
  #
  wgtStocMean[i,1]  <- log(drnArea1160)/var(stochMat60[i,]/drnArea1160)
  wgtStocMean[i,2]  <- log(drnArea1165)/var(stochMat65[i,]/drnArea1165)
  wgtStocMean[i,3]  <- log(drnArea1180)/var(stochMat80[i,]/drnArea1180)
  wgtStocMean[i,4]  <- log(drnArea1185)/var(stochMat85[i,]/drnArea1185)
  #
  yldWghtMean[i  ]  <- weighted.mean(yldStocMean[i,],wgtStocMean[i,]/sum(wgtStocMean[i,]))
  yldWghtVar[ i  ]  <- weighted.var( yldStocMean[i,],wgtStocMean[i,]/sum(wgtStocMean[i,]))
}
totUnGagedFlow      <- yldWghtMean * (drnArea04119000 - drnAreaGaged)
varUnGagedFlow      <- yldWghtVar  * (drnArea04119000 - drnAreaGaged)^2
#
####



totFlow             <- totGagedFlow + totUnGagedFlow
varFlow             <- varGagedFlow + varUnGagedFlow + # 2*cov(totGagedFlow,totUnGagedFlow)
stdFlow             <- sqrt(varFlow)
ci975TotFlow        <- totFlow + qnorm(0.975) * stdFlow
ci025TotFlow        <- totFlow + qnorm(0.025) * stdFlow
#
plot(GrandRiver$cDate,totFlow,log="y",type="l",col="blue",
     ylim=c(500,20000))
lines(GrandRiver$cDate,GrandRiver$o4119000,type="l",col="red")
lines(GrandRiver$cDate,ci975TotFlow,type="l",col="green")
lines(GrandRiver$cDate,ci025TotFlow,type="l",col="green")

ndxLo <- which(ci025TotFlow        > GrandRiver$o4119000)
ndxHi <- which(GrandRiver$o4119000 > ci975TotFlow)
(length(ndxLo)+length(ndxHi))/nrow(GrandRiver)





plot(GrandRiver$o4119000,totFlow,log="xy",col="blue",pch=20,cex=0.5,
     xlab="Measured Flow at 04119000, in cfs",
     ylab="Estimated Flow at 04119000, in cfs",
     main="Relation between measured and estimated flow at 
     04119000 Grand River at Grand Rapids,MI")
abline(0,1,col="red",lty="solid")

cov(cbind(totUnGagedFlow,totGagedFlow))


# Check autocorrelation of log flow residuals
pacfResid    <- pacf(residuals(lQm04118000))
# An AR(2) looks appropriate
arResidModel <- arima(x = residuals(lQm0411800

fName        <- "/Superior/data/Monthly/Streamgage/04118000.txt"
fullName     <- paste(pName,fName,sep="")
Q04118000    <- read.table(file=fullName,header=TRUE,sep="\t",
                         stringsAsFactors = FALSE, comment.char="#")
Q04118000$cDate <- as.Date(paste(Q04118000$year_nu,Q04118000$month_nu,'1',sep="/"),
                           "%Y/%m/%d")
keepNames  <- c("cDate","mean_va")
Q04118000  <- Q04118000[,keepNames]
0),order = c(2,0,0))
#
# plot the measured time series
plot(as.Date(paste(GrandRiver$year_nu,GrandRiver$month_nu,'1',sep="/"),"%Y/%m/%d"),
             exp(1)^log(GrandRiver$o4118000),col="blue",log="y",ylim=c(50,5000),
     type="l")








plot(ly04116000,ly04118000,pch=20,col="blue",cex=0.6,
     main="Relation Between Monthly Yields at 04116000 and 04118000 on Grand River",
     xlab="Log10 Monthly Yield at 04116000, in ft3/s",
     ylab="Log10 Monthly Yield at 04118000, in ft3/s")
lines(ly04116000,fitted.values(lm04118000),type="l",col="black")
abline(0,1,col="red",lty="solid")
legend("topleft",legend=c("Data pairs","Regression Line","Line of agreement"),
       col=c("blue","black","red"),pch=c(20,NA,NA),lty=c(NA,"solid","solid"))
#
lm04118500 <- lm(ly04118500 ~ ly04116000)
print(summary(lm04118500))
cv <- (exp(1)^summary(lm04118500)$sigma - 1)*100
print(paste("The regression error expressed as a percent is: ",
            format(cv,digits=4)))
print(paste("The regression mean will likely vary from",
            format(1/(1+cv/100)*100,digits=3),"to",format((1+cv/100)*100,digits=3),
            "percent of its true value.",sep=" "))
plot(ly04116000,ly04118500,pch=20,col="blue",cex=0.6,
     main="Relation Between Monthly Yields at 04116000 and 04118500 on Grand River",
     xlab="Log10 Monthly Yield at 04116000, in ft3/s",
     ylab="Log10 Monthly Yield at 04118500, in ft3/s")
lines(ly04116000,fitted.values(lm04118500),type="l",col="black")
abline(0,1,col="red",lty="solid")
legend("topleft",legend=c("Data pairs","Regression Line","Line of agreement"),
       col=c("blue","black","red"),pch=c(20,NA,NA),lty=c(NA,"solid","solid"))
#
newData          <- data.frame(log10(Q04116000$mean_va/drnArea1160))
colnames(newData) <- "ly04116000" 
pQ04116500 <- (10^predict(lm04116500,newdata=newData))*drnArea1165
pQ04118000 <- (10^predict(lm04118000,newdata=newData))*drnArea1180
qQ04118500 <- (10^predict(lm04118500,newdata=newData))*drnArea1185
#
# Compare with estimates using multiple streamgages
Q04116000$o4119000_p60658085 <- drnArea04119000/drnAreaGaged *
  (Q04116000$mean_va + pQ04116500 + pQ04118000 + qQ04118500)
#
# Create a data set here with 04119000 (length 1054) and 04116000-based data (749) 


#
par(cex.lab=0.8,cex.main=0.9)
plot(Q04119000$mean_va,Q04116000$o4119000_p60658085,
     xlab="Measured Monthly Flow at 04119000 Grand River at Ionia, MI in ft3/s",
     ylab="Estimated Monthly Flow at 04119000 based on 0411[60,65,80,85]00, ft3/s",
     main="Relation between measured monthly flow at 04119000 Grand River at Grand Rapids, ft3/s
     and measured monthly flows at Upstream Gages Adjusted for Drainage Area",
     pch=20, cex=0.6, col="blue",log="xy")
#
#
par(cex.lab=0.8,cex.main=0.9)
boxplot(GrandRiver$o4119000-GrandRiver$o4119000_60658085 ~ 
          GrandRiver$month_nu, xlab="Month",ylab="Flow Residuals, cfs",
        axes=FALSE,main="Monthly Distribution of Flow Residuals for Grand River at Grand Rapids, MI based on
        a Drainage Area Adjustment Monthly Flows at 0411[60,65,80,85]00",notch=TRUE)
axis(side=1,at=1:12,labels=month.abb)
axis(side=2)
abline(h=0,col="red",lty="solid")


