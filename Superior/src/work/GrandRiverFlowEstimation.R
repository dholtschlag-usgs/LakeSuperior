# This script analyzes the relation between a set of downstream flow gages on
# the Grand River to potentially improve estimates of unmeasured monthly flow to
# Lake Michigan. In this analysis, 04119000 Grand River at Grand Rapids, MI
# represents the unmeasured flow to Lake Michigan. 04119000 is the most
# downstream gage in a large network, but there are four unnested gages further
# upstream that can be used for estimation.  These gages include: 04113000 Grand 
# River at Ionia, MI, 04116500 Flat River at Smyrna, MI, 04118000 Thornapple River
# near Caledonia, MI, and 04118500 Rogue River near Rockford, MI.
# 
# All of the upstream gages have periods of record that started after 04119000. 
# Furthermore, two streamgages have been discontinued: 04116500 (in 1986), and 
# 04118000 (in 1994).  For this analysis, it is preferred that the estimator for
# 04119000 be consistent over the period from 1930 to present, which diminishes
# the potential utility of streamgages that have shorter periods of record. Not
# including information from discontinued streamgages, however, can potentially
# increase the bias and uncertainty of monthly flow estimates at 04119000. In 
# addition, an estimator based on only one or two index gages is not robust with
# respect to the possible discontinuation of a key streamgage.  
#
# This analysis explores the feasibility of improving estimates of monthly flows
# at 04119000 by using relations among monthly flows at upstream gages defined 
# during extended periods of contemporaneous operation.  This utility of this 
# relation will be evaluated for the potential to reduce the bias and
# uncertainty of flow estimates, to maintain consistency, and to improve the 
# robustness of the estimator.
#
# Although the immediate motivation for this analysis involves the water budget
# of Lake Superior, the strategy developed for the Grand River is expected to be
# applicable to all the basins in the Great Lakes.
#
# Clear workspace
rm(list=ls())
# Get current working directory to set path name
pName      <- getwd()
#
# Define streamgage numbers and names

name1190    <- "04119000 Grand River at Grand Rapids, MI"
name1160    <- "04116000 Grand River at Ionia, MI"
name1165    <- "04116500 Flat River near Smyrna, MI"
name1180    <- "04118000 Thornapple River near Caledonia, MI"
name1185    <- "04118500 Rouge River near Rockford, MI"
#
name1088    <- "04108801 MACATAWA RIVER NEAR ZEELAND, MI"
#
# Define drainage areas at streamgages, in sq. miles
drnArea1190 <- 4900
drnArea1160 <- 2840
drnArea1165 <-  528
drnArea1180 <-  773
drnArea1185 <-  234
drnArea1088 <-   68.5
#
# The gaged drainage area upstream from 04119000 is
drnAreaGaged    <- drnArea1160 + drnArea1165 + drnArea1180 + drnArea1185
print(paste("The gaged drainage area upstream from 04119000 is",
            format(drnAreaGaged,dig=4),"square miles.",sep=" "))
# 
print(paste("The drainage area at 04119000 is",format(drnArea1190,di=4),
            "square miles.",sep=" "))
#
# Source function to read monthly flows in ascii files as: readMonthlyFlow
source("Superior/src/work/readMonthlyFlows.R")
# Create data frames of monthly flows at streamgages 
#   using assign for dynamic assignment of variable names
assign(paste0("Q",substr(name1190,1,8)),readMonthlyFlows(substr(name1190,1,8)))
assign(paste0("Q",substr(name1160,1,8)),readMonthlyFlows(substr(name1160,1,8)))
assign(paste0("Q",substr(name1165,1,8)),readMonthlyFlows(substr(name1165,1,8)))
assign(paste0("Q",substr(name1180,1,8)),readMonthlyFlows(substr(name1180,1,8)))
assign(paste0("Q",substr(name1185,1,8)),readMonthlyFlows(substr(name1185,1,8)))
assign(paste0("Q",substr(name1088,1,8)),readMonthlyFlows(substr(name1088,1,8)))
#
# Setup Grand River prediction data set 
#   Merge data frames on Year/Month for estimation data
GrandRiverEsti   <- merge(Q04119000, Q04116000,by="cDate")
GrandRiverEsti   <- merge(GrandRiverEsti,Q04116500,by="cDate")
GrandRiverEsti   <- merge(GrandRiverEsti,Q04118000,by="cDate")
GrandRiverEsti   <- merge(GrandRiverEsti,Q04118500,by="cDate")
# GrandRiverTest   <- merge(GrandRiverEsti,Q04108801,by="cDate")
# 
#   Retrieve interval for estimation 
intervalEsti     <- paste0(format(GrandRiverEsti$cDate[1],'%b-%Y')," to ",
                           format(tail(GrandRiverEsti$cDate,1),'%b-%Y'))
#
# Create field in GrandRiverEsti containing month number
GrandRiverEsti$month_no <- as.factor(as.numeric(format(GrandRiverEsti$cDate,"%m")))
#
#   Merge data frames with continuous data for prediction interval
GrandRiverPred   <- Q04119000[677:nrow(Q04119000),]
GrandRiverPred   <- merge(GrandRiverPred,Q04116000,by="cDate")
#
# Create field in GrandRiverPred containing month number
GrandRiverPred$month_no <- as.factor(as.numeric(format(GrandRiverPred$cDate,"%m")))
#
#   Retrieve interval for prediction 
intervalPred     <- paste0(format(GrandRiverPred$cDate[1],'%b-%Y')," to ",
                           format(tail(GrandRiverPred$cDate,1),'%b-%Y'))
#
# Plot time series of monthly flows for all streamgages
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
# Compare yields among streamgageseamgages
# Work in a yield rather than a flow metric
y04116000  <- GrandRiverEsti$o4116000/drnArea1160
y04116500  <- GrandRiverEsti$o4116500/drnArea1165
y04118000  <- GrandRiverEsti$o4118000/drnArea1180
y04118500  <- GrandRiverEsti$o4118500/drnArea1185
y04119000  <- GrandRiverEsti$o4119000/drnArea1190
# y04108801  <- GrandRiverTest$o4108801/drnArea1088
# # Distribution of water yields among gages
boxplot(cbind(y04116000,y04116500,y04118000,y04118500),notch=TRUE)
# Natural log transform log transform flows for linear modeling
ly04116000  <- log10(y04116000)
ly04116500  <- log10(y04116500)
ly04118000  <- log10(y04118000)
ly04118500  <- log10(y04118500)
ly04119000  <- log10(y04119000)
# ly04108801  <- log10(y04108801)
#
# Distribution of log10 water yields among gages
par(las=1)  # axis labels are horizontal with las=1
boxplot(cbind(ly04116000,ly04116500,ly04118000,ly04118500,ly04119000),
        notch=TRUE,col="lightblue",
        cex.main=0.9,cex.axis=0.9,cex.lab=0.9,cex.sub=0.9,
        main  = "Distribution of Water Yields for Selected Streamgages in the Grand River Basin",
        sub   = paste0("Data interval is from ",intervalEsti),
        ylab  = expression(paste("Water Yields, in log10 ft3/(s-mi^2)",sep="")),
        xlab  = "Selected USGS Streamgages in Grand River Basin",
        names = c("04116000","04116500","04118000","04118500","04119000"))
abline(h=median(ly04119000),col="red",lty="dashed")
#
# Compute a flow prediction for Grand River at Grand Rapids, MI by adjusting 
#   Grand River at Ionia, MI by the drainage area ratio
GrandRiverPred$pred1190_ratio1160  <- GrandRiverPred$o4116000 * drnArea1190/drnArea1160
#
# Plot the ratio flow estimate with 04116000 for measured values at 04119000
par(mar=c(5,5,4,2)+0.1)
plot(GrandRiverEsti$o4119000,GrandRiverEsti$o4116000* drnArea1190/drnArea1160,
     pch=20,col="blue",cex=0.65,cex.main=0.9,cex.lab=0.8,cex.axis=0.8,
     log="xy",xlab=paste0("Measured flow at ",name1190,", in cfs"),
     ylab=paste0("Estimated flow based on Grand River at Ionia, MI\n",
                 "adjusted for drainage area at Grand Rapids, in cfs"),
     main=paste("Relation between measured and estimated monthly at Grand River at Grand Rapids, MI\n",
                "based on drainage area adjusted monthly flow at Grand River at Ionia, MI"))
points(GrandRiverPred$o4119000,GrandRiverPred$pred1190_ratio1160,
       col="green",pch=20,cex=0.65)
abline(0,1,col="red",lty="dashed")
legend("topleft",legend=c(paste0("Estimation interval ",intervalEsti),
                          paste0("Prediction interval ",intervalPred),
                          "Line of agreement"),col=c("blue","green","red"),
       pch=c(20,20,NA),lty=c(NA,NA,"dashed"),cex=0.8)
#
# Plot distribution of flow residuals by month
boxplot(GrandRiverEsti$o4119000 - GrandRiverEsti$o4116000* drnArea1190/drnArea1160 ~
          GrandRiverEsti$month_no, xlab="Month",ylab="Flow Residuals, cfs",
        axes=FALSE,main=paste0("Distribution of Flow Residuals for ",name1190," based on a Drainage Area\n",
                               "Adjustment of Flows at ",name1160," for Estimation Period ",intervalEsti),
        cex.main=0.8,notch=TRUE)
axis(side=1,at=1:12,labels=month.abb)
axis(side=2)
abline(h=0,col="red",lty="dashed")
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Develop linear model for pred1165_regres1160
# 
log1160        <- log10(GrandRiverEsti$o4116000)
# Simple linear regression model on log flows
lm1165_1160    <- lm(log10(GrandRiverEsti$o4116500) ~ log1160 )
# Summary of simple linear regression
summary(lm1165_1160)
# 
month_no       <- GrandRiverEsti$month_no
lm1165_1160mon <- lm(log10(GrandRiverEsti$o4116500) ~ log1160 + month_no)
# The following lm contains a lagged flow term.
lm1165_1160lag <- lm(log10(GrandRiverEsti$o4116500) ~ log1160 + month_no + 
                       c(log1160[-1], NA))
# The summary shows that the lag term in not significant
summary(lm1165_1160lag)
# Summary of the flow/month model
summary(lm1165_1160mon)
# Compare simple and monthly model
anova(lm1165_1160,lm1165_1160mon)
# Accesses (partial) autocorrelation of the monthly residuals
pacfResid1165       <- pacf(residuals(lm1165_1160mon),
                            main="Partial ACF of log Flow residuals at 04116500")
#
#
# Predict flows at 1165 with regression on 1160 with newdata
log1160             <- log10(GrandRiverPred$o4116000)
month_no            <- GrandRiverPred$month_no
newData             <- as.data.frame(cbind(log1160,month_no))
newData$month_no    <- as.factor(newData$month_no)
model1165_given1160 <- predict(lm1165_1160mon,newdata=newData,se.fit=TRUE,
                               interval="prediction")
#
# Smearing bias correction factor as the mean antilog of the residuals
smearBCF1165        <- mean(10^(lm1165_1160mon$residuals))
#
# Apply the Bias Correction Factor to predicted values in log space
GrandRiverEsti$pred1165_regres1160 <- 10^fitted.values(lm1165_1160mon) * smearBCF1165
GrandRiverPred$pred1165_regres1160 <- 10^model1165_given1160$fit[,1]*    smearBCF1165
#
plot(GrandRiverEsti$o4116500,GrandRiverEsti$pred1165_regres1160,
     col="blue",pch=20,cex=0.80,log="xy",cex.lab=0.8,
     xlab=paste0("Measured flow at ",name1165,", in CFS"),
     ylab=paste0("Regression Estimates Given 04116000 and month, in CFS"),
     main=paste0("Relation between measured and bias-corrected regression estimates of flow \n",
                 "at ",name1165," during ",intervalEsti),cex.main=0.95)
abline(0,1,col="red",lty="dashed")
legend("bottomright",legend=c("Data pairs","Line of agreement"),
       col=c("blue","red"),pch=c(20,NA),lty=c(NA,"dashed"))
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Develop linear model for pred1180_regres1160
log1160        <- log10(GrandRiverEsti$o4116000)
# Simple linear regression model on log flows
lm1180_1160    <- lm(log10(GrandRiverEsti$o4118000) ~ log1160 )
# Summary of simple linear regression
summary(lm1180_1160)
# 
month_no       <- GrandRiverEsti$month_no
lm1180_1160mon <- lm(log10(GrandRiverEsti$o4118000) ~ log1160 + month_no)
summary(lm1180_1160mon)
# Compare simple and monthly model
anova(lm1180_1160,lm1180_1160mon)
# Accesses (partial) autocorrelation of the monthly residuals
pacfResid1180  <- pacf(residuals(lm1180_1160mon),
                     main="Partial ACF of log Flow residuals at 04118000")
#
#
# Predict flows at 1180 with regression on 1160 with newdata
log1160             <- log10(GrandRiverPred$o4116000)
newData             <- as.data.frame(log1160)
model1180_given1160 <- predict(lm1180_1160,newdata=newData,se.fit=TRUE,
                               interval="prediction")
#
# Smearing bias correction factor as the mean antilog of the residuals
smearBCF1180        <- mean(10^(lm1180_1160$residuals))
#
# Apply the Bias Correction Factor to predicted values in log space
GrandRiverEsti$pred1180_regres1160 <- 10^fitted.values(lm1180_1160) * smearBCF1180
GrandRiverPred$pred1180_regres1160 <- 10^model1180_given1160$fit[,1]* smearBCF1180
#
plot(GrandRiverEsti$o4118000,GrandRiverEsti$pred1180_regres1160,
     col="blue",pch=20,cex=0.80,log="xy",cex.lab=0.8,
     xlab=paste0("Measured flow at ",name1180,", in CFS"),
     ylab=paste0("Regression Estimates Given 04116000 and month, in CFS"),
     main=paste0("Relation between measured and bias-corrected regression estimates of flow \n",
                 "at ",name1180," during ",intervalEsti),cex.main=0.95)
abline(0,1,col="red",lty="dashed")
legend("bottomright",legend=c("Data pairs","Line of agreement"),
       col=c("blue","red"),pch=c(20,NA),lty=c(NA,"dashed"))
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Develop linear model for pred1185_regres1160
log1160        <- log10(GrandRiverEsti$o4116000)
# Simple linear regression model on log flows
lm1185_1160    <- lm(log10(GrandRiverEsti$o4118500) ~ log1160 )
# Summary of simple linear regression
summary(lm1185_1160)
# 
month_no       <- GrandRiverEsti$month_no
lm1185_1160mon <- lm(log10(GrandRiverEsti$o4118500) ~ log1160 + month_no)
summary(lm1185_1160mon)
# Compare simple and monthly model
anova(lm1185_1160,lm1185_1160mon)
# Accesses (partial) autocorrelation of the monthly residuals
pacfResid1185  <- pacf(residuals(lm1185_1160mon),
                       main="Partial ACF of log Flow residuals at 04118500")
#
#
# Predict flows at 1180 with regression on 1160 with newdata
log1160             <- log10(GrandRiverPred$o4116000)
month_no            <- GrandRiverPred$month_no
newData             <- as.data.frame(cbind(log1160,month_no))
newData$month_no    <- as.factor(newData$month_no)
#
model1185_given1160 <- predict(lm1185_1160mon,newdata=newData,se.fit=TRUE,
                               interval="prediction")
#
# Smearing bias correction factor as the mean antilog of the residuals
smearBCF1185        <- mean(10^(lm1185_1160mon$residuals))
#
# Apply the Bias Correction Factor to predicted values in log space
GrandRiverEsti$pred1185_regres1160 <- 10^fitted.values(lm1185_1160) * smearBCF1185
GrandRiverPred$pred1185_regres1160 <- 10^model1185_given1160$fit[,1]* smearBCF1185
#
plot(GrandRiverEsti$o4118500,GrandRiverEsti$pred1185_regres1160,
     col="blue",pch=20,cex=0.80,log="xy",cex.lab=0.8,
     xlab=paste0("Measured flow at ",name1185,", in CFS"),
     ylab=paste0("Regression Estimates Given 04116000 and month, in CFS"),
     main=paste0("Relation between measured and bias-corrected regression estimates of flow \n",
                 "at ",name1185," during ",intervalEsti),cex.main=0.95)
abline(0,1,col="red",lty="dashed")
legend("bottomright",legend=c("Data pairs","Line of agreement"),
       col=c("blue","red"),pch=c(20,NA),lty=c(NA,"dashed"))
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Compare regression and ratio estimators
sumEstiMeasFlows_1160 <- rowSums(cbind(GrandRiverEsti$pred1165_regres1160,
                             GrandRiverEsti$pred1180_regres1160,
                             GrandRiverEsti$pred1185_regres1160,
                             GrandRiverEsti$o4116000)) * 
  drnArea1190 / drnAreaGaged
#
plot(GrandRiverEsti$o4119000,sumEstiMeasFlows_1160,col="blue",pch=20,cex=0.75,
     log="xy",cex.main=0.9,cex.axis=0.9,cex.lab=0.9,
     xlab=paste0("Measured flow at ",name1190,", in CFS"),
     ylab=paste0("Estimated Flows, in CFS"),
     main=paste0("Relation between measured flows at ",name1190," and measured\n",
                 "and regressed flows using ",name1160," from ",intervalEsti))
#
abline(0,1,col="red",lty="dashed")
#
# Determine the bias and mse of the estimated and measured flows
biasSumEstiMeasFlow_1160   <- mean(GrandRiverEsti$o4119000 - sumEstiMeasFlows_1160)
errVarSumEstiMeasFlow_1160 <- var(GrandRiverEsti$o4119000 - sumEstiMeasFlows_1160)
rmseSumEstiMeasFlow_1160   <- sqrt(biasSumEstiMeasFlow_1160^2 + errVarSumEstiMeasFlow_1160)
print(paste0("The bias and rmse of 04116000-based flows for 04119000 are ",
             format(biasSumEstiMeasFlow_1160,digits=4),", and ",
             format(rmseSumEstiMeasFlow_1160,digits=4)," in CFS"))
#
# Sum of the measured flows, adjusted for drainage area, during the estimation interval
sumMeasFlowsEstiIntr <- rowSums(cbind(GrandRiverEsti$o4116000,
                                  GrandRiverEsti$o4116500,
                                  GrandRiverEsti$o4118000,
                                  GrandRiverEsti$o4118500)) * drnArea1190 / drnAreaGaged
#
# Error characteristics of 04119000 flows based on upstream measured flows
biasMeasFlowsEstiIntr   <- mean(GrandRiverEsti$o4119000 - sumMeasFlowsEstiIntr)
errVarMeasFlowsEstiIntr <-  var(GrandRiverEsti$o4119000 - sumMeasFlowsEstiIntr)
rmseMeasFlowsEstiIntr   <- sqrt(biasMeasFlowsEstiIntr^2 + errVarMeasFlowsEstiIntr)
print(paste0("The bias and rmse of upstream measured flows, adjusted for change in ",
             "drainage area, and measured flow during the estimation interval are ",
             format(biasMeasFlowsEstiIntr,digits=4),", and ",
             format(rmseMeasFlowsEstiIntr,digits=4)," in CFS"))
#
# Bias and rmse of upstream measured flows could not be computed during the prediction interval
#   because the flows were not consistently available
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Evaluate bias and rmse of ratio estimator during estimation and prediction intervals 
biasRatioEstiFlow   <- mean(GrandRiverEsti$o4119000 - drnArea1190/drnArea1160 * GrandRiverEsti$o4116000)
errVarRatioEstiFlow <-  var(GrandRiverEsti$o4119000 - drnArea1190/drnArea1160 * GrandRiverEsti$o4116000)
rmseRatioEstiFlow   <- sqrt(biasRatioEstiFlow^2     + errVarRatioEstiFlow)
print(paste0("The bias and rmse of ratio estimates of flow during the estimation interval are ",
             format(biasRatioEstiFlow,digits=4),", and ",
             format(rmseRatioEstiFlow,digits=4)," CFS, respectively"))
#
biasRatioPredFlow   <- mean(GrandRiverPred$o4119000 - drnArea1190/drnArea1160 * GrandRiverPred$o4116000)
errVarRatioPredFlow <-  var(GrandRiverPred$o4119000 - drnArea1190/drnArea1160 * GrandRiverPred$o4116000)
rmseRatioPredFlow   <- sqrt(biasRatioPredFlow^2     + errVarRatioPredFlow)
print(paste0("The bias and rmse of ratio estimates of flow during the prediction interval are ",
             format(biasRatioPredFlow,digits=4),", and ",
             format(rmseRatioPredFlow,digits=4)," CFS, respectively"))
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Evaluate bias and rmse of regression estimator during estimation and prediction intervals 
sumRegresEstiFlows <- rowSums(cbind(GrandRiverEsti$pred1165_regres1160,
                                    GrandRiverEsti$pred1180_regres1160,
                                    GrandRiverEsti$pred1185_regres1160,
                                    GrandRiverEsti$o4116000)) * drnArea1190 / drnAreaGaged
#
biasRegresEstiFlow   <- mean(GrandRiverEsti$o4119000 - sumRegresEstiFlows)
errVarRegresEstiFlow <-  var(GrandRiverEsti$o4119000 - sumRegresEstiFlows)
rmseRegresEstiFlow   <- sqrt(biasRegresEstiFlow^2    + errVarRegresEstiFlow)
print(paste0("The bias and rmse of regression estimates of flow during the estimation interval are ",
             format(biasRegresEstiFlow,digits=4),", and ",
             format(rmseRegresEstiFlow,digits=4)," CFS, respectively"))
#
#
sumRegresPredFlows <- rowSums(cbind(GrandRiverPred$pred1165_regres1160,
                                    GrandRiverPred$pred1180_regres1160,
                                    GrandRiverPred$pred1185_regres1160,
                                    GrandRiverPred$o4116000)) * drnArea1190 / drnAreaGaged
#
biasRegresPredFlow   <- mean(GrandRiverPred$o4119000 - sumRegresPredFlows)
errVarRegresPredFlow <-  var(GrandRiverPred$o4119000 - sumRegresPredFlows)
rmseRegresPredFlow   <- sqrt(biasRegresPredFlow^2    + errVarRegresPredFlow)
print(paste0("The bias and rmse of regression estimates of flow during the prediction interval are ",
             format(biasRegresPredFlow,digits=4),", and ",
             format(rmseRegresPredFlow,digits=4)," CFS, respectively"))
#
# Plot Estimated and Predicted Flows at 04119000
par(mar=c(5,5,4,2))
plot(GrandRiverEsti$o4119000,sumRegresEstiFlows,pch=20,col="blue",cex=0.75,
     log="xy",xlab=paste0("Measured Flow at ",name1190,", in CFS"),cex.main=0.95,
     ylab=paste0("Estimated flow based on four main tributaries to\n",
                 name1190,", in CFS"),
     main=paste0("Relation between measured and estimated flows at ",name1190,"\n",
                 "based on area adjusted regression estimates from four main tribuaries"),
)
points(GrandRiverPred$o4119000,sumRegresPredFlows,pch=20,col="green",cex=0.75)
abline(0,1,col="red",lty="dashed")
legend("topleft",legend=c(paste0("Estimation interval ",intervalEsti),
                          paste0("Prediction interval ",intervalPred),
                          "Line of agreement"),col=c("blue","green","red"),
       pch=c(20,20,NA),lty=c(NA,NA,"dashed"),cex=0.8)
#
# Is there a pattern in the residuals
err1190sumRegresEstiFlows <- GrandRiverEsti$o4119000 - sumRegresEstiFlows
plot(GrandRiverEsti$cDate,err1190sumRegresEstiFlows,col="blue",pch=20,cex=0.75,
     xlab="Year",ylab="Error, in CFS",cex.main=0.99,
     xlim=c(as.Date("1950/01/10","%Y/%m/%d"),as.Date("2014/01/01","%Y/%m/%d")),
     main=paste0("Time Series of Model Residuals using Adjusted Sum of Regression\n",
                 "Flows for Predicting ",name1190))
#
err1190sumRegresPredFlows <- GrandRiverPred$o4119000 - sumRegresPredFlows
points(GrandRiverPred$cDate,err1190sumRegresPredFlows,col="green",pch=20,cex=0.75)
abline(h=0,col="red",lty="dashed")
#
legend("bottomright",legend=c("Estimated","Predicted","Line of agreement"),
       col=c("blue","green","red"),pch=c(20,20,NA),cex=0.85,
       lty=c(NA,NA,"dashed"))

#
pacf(err1190sumRegresPredFlows)
#             
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 
# Var[A*(q1 + q2)] = A^2 * [Var(q1) + Var(q2) + 2 * Cov(q1,q2)]


myColors <- c(colors()[20*as.numeric(GrandRiverPred$month_no)+10])

plot(GrandRiverPred$pred1165_regres1160,
     GrandRiverPred$pred1180_regres1160,
     pch=20,log="xy",cex=1,col=c(myColors))
# abline(0,1,col="red",lty="dashed")
# myColors <- c(colors()[as.numeric(GrandRiverPred$month_no)+c(553-1,617-2,569-3,549-4,457-5,509-6,
#                                                393-7, 33-8, 53-9,69-10, 9-11,45-12)])
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Multivariate regression with r
y1 <- log10(GrandRiverEsti$o4116500)
y2 <- log10(GrandRiverEsti$o4118000)
y3 <- log10(GrandRiverEsti$o4118500)
Y  <- cbind(y1,y2,y3)
x  <- log10(GrandRiverEsti$o4116000)
#
multLm <- lm(Y ~ x)
summary(multLm)
print(multLm)

#
predEstiLogQ1165_1160 <- predict(lm1165_1160mon,se.fit=TRUE,interval="confidence")

plot(ecdf(predEstiLogQ1165_1160$fit[,1]),lty="solid",col="red",cex=0)
lines(ecdf(predEstiLogQ1165_1160$fit[,2]),lty="solid",col="salmon",cex=0)
lines(ecdf(predEstiLogQ1165_1160$fit[,3]),lty="solid",col="salmon",cex=0)
lines(ecdf(log10(GrandRiverEsti$o4116500)),lty="solid",col="blue",cex=0)

predEstiLogQ1165_1160 <- predict(lm1165_1160mon,se.fit=TRUE,interval="prediction")
plot(GrandRiverEsti$cDate,log10(GrandRiverEsti$o4116500),type="l",col="blue")
lines(GrandRiverEsti$cDate,predEstiLogQ1165_1160$fit[,1],type="l",col="red")
lines(GrandRiverEsti$cDate,predEstiLogQ1165_1160$fit[,2],type="l",col="salmon")
lines(GrandRiverEsti$cDate,predEstiLogQ1165_1160$fit[,3],type="l",col="salmon")

ndx95 <- which(log10(GrandRiverEsti$o4116500) > predEstiLogQ1165_1160$fit[,2] &
        log10(GrandRiverEsti$o4116500) <= predEstiLogQ1165_1160$fit[,3])
print(paste0("The percentage of 04116500 flows in the nominal 95% interval is ",
             format(length(ndx95)/nrow(GrandRiverEsti)*100,digit=4)))
# Prediction variance of logQ 04116500 during the estimation interval
pVarEstiLogQ1165_1160 <- ((predEstiLogQ1165_1160$fit[,1]-predEstiLogQ1165_1160$fit[,2])/
  qnorm(0.975))^2
plot(GrandRiverEsti$cDate,sqrt(pVarEstiLogQ1165_1160)*log(10)*100,type="l",col="red",
     xlab="Year",ylab="Variance, in Percent",
     main=paste0("Variance of estimates of log streamflow during the estimation period at\n"))


# lines(ecdf(predEstiLogQ1165_1160$fit[,2]),lty="solid",col="salmon",cex=0)
# lines(ecdf(predEstiLogQ1165_1160$fit[,3]),lty="solid",col="salmon",cex=0)
# lines(ecdf(log10(GrandRiverEsti$o4116500)),lty="solid",col="blue",cex=0)







