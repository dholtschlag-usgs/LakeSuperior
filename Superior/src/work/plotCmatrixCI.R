# Script to plot parameter MARSS parameter estimates C matrix
#   The C matrix is a fixed seasonal component that has 60 elements
#   for 12 months of 
plotCmatrixCI <- function(modelFile){
  print(modelFile$model)
  
  # Estimated coefficient names
  coefNames <- names(modelFile$coef)
  # Estimated coefficient values
  coefValue <- modelFile$coef
  # Extract C matrix seasonal parameters
  ndxStrmSea <- grep("C.\\(X.stmrCMS,",coefNames)
  ndxPrecSea <- grep("C.\\(X.precCMS,",coefNames)
  ndxEvapSea <- grep("C.\\(X.evapCMS,",coefNames)
  ndxrOffSea <- grep("C.\\(X.rOffCMS,",coefNames)
  ndxdStoSea <- grep("C.\\(X.dStoCMS,",coefNames)
  # Get indices of all C matrix elements 
  ndxSea     <- c(ndxStrmSea,ndxPrecSea,ndxEvapSea,ndxrOffSea,ndxdStoSea)
  print(paste0("ndxSea= ",ndxSea))
  minParm    <- min(coefValue[ndxSea])
  print(paste0("minParm= ",minParm))
  maxParm    <- max(coefValue[ndxSea])
  print(paste0("maxParm= ",maxParm))
  
  # If there is a C matrix plot the parameters 
  if (length(ndxSea)==60){
    # Plot seasonal parameters for St. Marys River
    par(mfrow=c(3,1),mar=c(2,4,1,2)+0.1)
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
  }
  
  print(modelFile$model)
  
  # modelFile <- s5o5SeaAR.ci
  library(ggplot2)
  
  dftmp <- as.data.frame(cbind(modelFile$par$U[1:60],modelFile$par.lowCI$U[1:60],
                               tmp$par.upCI$U[1:60]))
  row.names(dftmp) <- row.names(modelFile$par$U)[1:60]
  colnames(dftmp)  <- c("parExpect", "parLoCI", "parUpCI")
  
  dftmp$mnth <- ordered(factor(substr(row.names(dftmp),12,14),month.abb))                       
  
  dftmp$comp <- ordered(factor(substr(row.names(dftmp), 4, 7)),
                        c("stmr","rOff","prec","evap","dSto"))
  levels(dftmp$comp)[levels(dftmp$comp)=="stmr"] <- "St Marys River"
  levels(dftmp$comp)[levels(dftmp$comp)=="rOff"] <- "Runoff"
  levels(dftmp$comp)[levels(dftmp$comp)=="prec"] <- "Precipitation"
  levels(dftmp$comp)[levels(dftmp$comp)=="evap"] <- "Evaporation"
  levels(dftmp$comp)[levels(dftmp$comp)=="dSto"] <- "Change Lake Storage"
  
  
  mnth <- ordered(factor(substr(coefNames[ndxSea],14,16),month.abb))
  
  comp <- ordered(factor(substr(coefNames[ndxSea], 4, 7)),
                  c("stmr","rOff","prec","evap","dSto"))
  
  # Create datafile in format that the ggplot2 command is expecting
  
  dfParm_C <- data.frame(
    cbind(as.numeric(rep(1,length(ndxSea))),
          as.numeric(modelFile$coef[ndxSea]),
          as.numeric(modelFile$par.se$U[ndxSea]),
          as.numeric(modelFile$par.se$U[ndxSea]),
          1.96*as.numeric(modelFile$par.se$U[ndxSea])))
  
  summary(dfParm_C)
  
  # horizontally append column to data frame
  dfParm_C_fac <- data.frame(mnth,comp)
  # 
  # 
  dfParm_C      <- cbind(dfParm_C,dfParm_C_fac)
  dfParm_C$mnth <- ordered(factor(dfParm_C$mnth,month.abb))
  dfParm_C$comp <- ordered(factor(dfParm_C$comp,
                                  c("stmr","rOff","prec","evap","dSto")))
  str(dfParm_C)
  
  
  colnames(dfParm_C) <- c("N","mean","sd","se","ci","mnth","component")
  
  # p <- ggplot(dfParm_C, aes(x=mnth, y=mean )) + 
  #   geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
  #   geom_line() +
  #   geom_point() +
  #   facet_grid(component ~ ., scales = "free")
  # 
  # p + geom_hline(aes(yintercept = 0, colour = "red"), linetype = 2)
  
  # p <- ggplot(dfParm_C, aes(x=mnth, y=mean)) + geom_point(shape = 1)
  # p + facet_grid(component ~ ., scales = "free")
  
  # Improve labels
  dfParm_C2 <- dfParm_C
  levels(dfParm_C2$component)[levels(dfParm_C2$component)=="stmr"] <- "St Marys River"
  levels(dfParm_C2$component)[levels(dfParm_C2$component)=="rOff"] <- "Runoff"
  levels(dfParm_C2$component)[levels(dfParm_C2$component)=="prec"] <- "Precipitation"
  levels(dfParm_C2$component)[levels(dfParm_C2$component)=="evap"] <- "Evaporation"
  levels(dfParm_C2$component)[levels(dfParm_C2$component)=="dSto"] <- "Change Lake Storage"
  
  
#   p <- ggplot(dfParm_C2, aes(x=mnth, y=mean )) + 
#     geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
#     geom_line() +
#     geom_point() +
#     facet_grid(component ~ ., scales = "free")
  

  p <- ggplot(dftmp, aes(x=mnth, y=parExpect )) + 
    geom_errorbar(aes(ymin=parLoCI, ymax=parUpCI), width=.1) +
    geom_line() +
    geom_point() +
    facet_grid(comp ~ ., scales = "free")

  # p augmented with better labels
  p_aug <- p + geom_hline(aes(yintercept = 0, colour = "red"), linetype = 2) +
    xlab("Month") + ylab("Flow, in cubic meters per second") +
    ggtitle("Expected Magnitude and 95% Confidence Intervals for Seasonal Paramters by Component")
  
  print(p_aug)
}




