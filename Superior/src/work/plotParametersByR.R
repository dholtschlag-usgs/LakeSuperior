# R diag components coded with upper Neff & Nicholas limits
modelFile1 <- s5o5SeaARQ_NeffHi
# R diag components coded with lower Neff & Nicholas limits
# modelFile2 <- s5o5SeaARQ_NeffLo
modelFile2 <- s5o5SeaARQ_NeffLo5Half
# R diag components coded with estimated values
modelFile3 <- s5o5SeaAR.bfgs1

# Estimated coefficient names
coefNames1 <- names(modelFile1$coef)
# Estimated coefficient values
coefValue1 <- modelFile1$coef
# Extract C matrix seasonal parameters
ndxStrmSea1 <- grep("C.\\(X.stmrCMS,",coefNames1)
ndxPrecSea1 <- grep("C.\\(X.precCMS,",coefNames1)
ndxEvapSea1 <- grep("C.\\(X.evapCMS,",coefNames1)
ndxrOffSea1 <- grep("C.\\(X.rOffCMS,",coefNames1)
ndxdStoSea1 <- grep("C.\\(X.dStoCMS,",coefNames1)

# Estimated coefficient names
coefNames2 <- names(modelFile2$coef)
# Estimated coefficient values
coefValue2 <- modelFile2$coef
# Extract C matrix seasonal parameters
ndxStrmSea2 <- grep("C.\\(X.stmrCMS,",coefNames2)
ndxPrecSea2 <- grep("C.\\(X.precCMS,",coefNames2)
ndxEvapSea2 <- grep("C.\\(X.evapCMS,",coefNames2)
ndxrOffSea2 <- grep("C.\\(X.rOffCMS,",coefNames2)
ndxdStoSea2 <- grep("C.\\(X.dStoCMS,",coefNames2)

# Estimated coefficient names
coefNames3 <- names(modelFile3$coef)
# Estimated coefficient values
coefValue3 <- modelFile3$coef
# Extract C matrix seasonal parameters
ndxStrmSea3 <- grep("C.\\(X.stmrCMS,",coefNames3)
ndxPrecSea3 <- grep("C.\\(X.precCMS,",coefNames3)
ndxEvapSea3 <- grep("C.\\(X.evapCMS,",coefNames3)
ndxrOffSea3 <- grep("C.\\(X.rOffCMS,",coefNames3)
ndxdStoSea3 <- grep("C.\\(X.dStoCMS,",coefNames3)

# Plot seasonal parameters for St. Marys River
par(mfrow=c(1,1))
plot(coefValue1[ndxStrmSea1],col="blue",pch=20,xaxt="n",cex=1.5,
     ylab="Streamflow, CMS", xlab="Month",
     main="Fixed Factors of Monthly Effects on St. Marys River\n Outflow as a Function of Measurement Variance R")
points(coefValue2[ndxStrmSea2],col="red",pch=20,cex=1.5)
points(coefValue3[ndxStrmSea3],col="green",pch=20,cex=1.5)
abline(h=0, col="orange", lty="dashed")
axis(side=1, at=1:12, labels=month.abb)
legend("topright",legend=c("High R","Low R","Estimated R"),
       col=c("blue","red","green"),pch=c(20,20,20))

# Plot seasonal parameters for Precipitation
par(mfrow=c(1,1))
plot(coefValue1[ndxPrecSea1],col="blue",pch=20,xaxt="n",cex=2,
     ylim = c(-800,600),
     ylab="Precipitation, CMS",main="Fixed Monthly Parameters for Precipitation")
points(coefValue2[ndxPrecSea2],col="red",pch=20,cex=2)
points(coefValue3[ndxPrecSea3],col="green",pch=20,cex=2)
abline(h=0, col="orange", lty="dashed")
axis(side=1, at=1:12, labels=month.abb)
legend("topright",legend=c("High R","Low R","Estimated R"),
       col=c("blue","red","green"),pch=c(20,20,20))

# Plot seasonal parameters for Evaporation
par(mfrow=c(1,1))
plot(coefValue1[ndxEvapSea1],col="blue",pch=20,xaxt="n",cex=2,
     ylim = c(-1500,1500),
     ylab="Evaporation, CMS",
     main="Fixed Monthly Parameters for Precipitation")
points(coefValue2[ndxEvapSea2],col="red",pch=20,cex=2)
points(coefValue3[ndxEvapSea3],col="green",pch=20,cex=2)
abline(h=0, col="orange", lty="dashed")
axis(side=1, at=1:12, labels=month.abb)

# Plot seasonal parameters for Evaporation
par(mfrow=c(1,1))
plot(coefValue1[ndxEvapSea1],col="blue",pch=20,xaxt="n",cex=2,
     ylim = c(-1500,1500),
     ylab="Evaporation, CMS",
     main="Fixed Monthly Parameters for Precipitation")
points(coefValue2[ndxEvapSea2],col="red",pch=20,cex=2)
points(coefValue3[ndxEvapSea3],col="green",pch=20,cex=2)
abline(h=0, col="orange", lty="dashed")
axis(side=1, at=1:12, labels=month.abb)

# Plot seasonal parameters for Runoff
par(mfrow=c(1,1))
plot(coefValue1[ndxrOffSea1],col="blue",pch=1,xaxt="n",cex=1,
     ylim = c(-1500,2000),
     ylab="Runoff, CMS",
     main="Fixed Monthly Parameters for Basin Runoff")
points(coefValue2[ndxrOffSea2],col="red",pch=1,cex=1)
points(coefValue3[ndxrOffSea3],col="green",pch=1,cex=1)
abline(h=0, col="orange", lty="dashed")
axis(side=1, at=1:12, labels=month.abb)


par(mfrow=c(1,1))
plot(coefValue1[ndxdStoSea1],col="blue",pch=1,xaxt="n",cex=1,
     ylim = c(-2000,2500),
     ylab="Runoff, CMS",
     main="Fixed Monthly Parameters for Basin Runoff")
points(coefValue2[ndxdStoSea2],col="red",pch=1,cex=1)
points(coefValue3[ndxdStoSea3],col="green",pch=1,cex=1)
abline(h=0, col="orange", lty="dashed")
axis(side=1, at=1:12, labels=month.abb)


# Get indices of all C matrix elements 
ndxSea     <- c(ndxStrmSea,ndxPrecSea,ndxEvapSea,ndxrOffSea,ndxdStoSea)
print(paste0("ndxSea= ",ndxSea))


plot(coefValue[ndxStrmSea],col="blue",pch=20,xaxt="n",cex=2,
     ylab="Streamflow",main="Monthly Parameters for St. Marys River Outflow")
abline(h=0, col="red", lty="dashed")
plot(coefValue[ndxPrecSea],col="blue",pch=20,xaxt="n",cex=2,
     ylab="Precipitation",main="Monthly Parameters for Overlake Precipitation")
abline(h=0, col="red", lty="dashed")
plot(coefValue[ndxEvapSea],col="blue",pch=20,xaxt="n",cex=2,
     ylab="Evaporation",main="Monthly Parameters for Overlake Evaporation")
abline(h=0, col="red", lty="dashed")
axis(side=1, at=1:12, labels=month.abb)
# New plot
par(mfrow=c(2,1),mar=c(2,4,1,2)+0.1)
plot(coefValue[ndxrOffSea],col="blue",pch=20,xaxt="n",
     ylab="Runoff",main="Monthly Parameters for Runoff from Contributing Basins")
abline(h=0, col="red", lty="dashed")
plot(coefValue[ndxdStoSea],col="blue",pch=20,xaxt="n",
     ylab="Change in Storage",main="Monthly Parameters for Change in Lake Storage")
abline(h=0, col="red", lty="dashed")
axis(side=1, at=1:12, labels=month.abb)

# 
