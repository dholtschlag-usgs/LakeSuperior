# Script to plot parameter MARSS parameter estimates B matrix
#   The B matrix is a fixed autoregressive component that has 5 elements

plotUmatrixCI <- function(modelFile){
  # Estimated coefficient names
  coefNames <- names(modelFile$coef)
  # Estimated coefficient values
  coefValue <- modelFile$coef
  # Extract C matrix seasonal parameters
  ndxStrmSea <- grep("U.X.stmrCMS",coefNames)
  ndxPrecSea <- grep("U.X.precCMS",coefNames)
  ndxEvapSea <- grep("U.X.evapCMS",coefNames)
  ndxrOffSea <- grep("U.X.rOffCMS",coefNames)
  ndxdStoSea <- grep("U.X.dStoCMS",coefNames)
  # Get indices of all C matrix elements 
  ndxAr1     <- c(ndxStrmSea,ndxPrecSea,ndxEvapSea,ndxrOffSea,ndxdStoSea)
  print(paste0("ndxAr1= ",ndxAr1))
  
  library(ggplot2)
  
  comp <- ordered(factor(substr(coefNames[ndxAr1], 5, 8)),
                  c("stmr","rOff","prec","evap","dSto"))
  
  # Create datafile in format that the ggplot2 command is expecting
  dfParm_B <- data.frame(
    cbind(as.numeric(rep(1,5)),
          as.numeric(modelFile$coef[ndxAr1]),
          as.numeric(modelFile$par.se$U[61:65]),
          as.numeric(modelFile$par.se$U[61:65]),
          1.96*as.numeric(modelFile$par.se$U[61:65])))
  
  # horizontally append column to data frame
  dfParm_B_fac <- data.frame(comp)
  
  
  dfParm_B      <- cbind(dfParm_B,dfParm_B_fac)
  dfParm_B$comp <- ordered(factor(dfParm_B$comp,
                                  c("stmr","rOff","prec","evap","dSto")))
  str(dfParm_B)
  
  colnames(dfParm_B) <- c("N","mean","sd","se","ci","component")
  
  # Improve labels
  dfParm_B2 <- dfParm_B
  levels(dfParm_B2$component)[levels(dfParm_B2$component)=="stmr"] <- "St Marys River"
  levels(dfParm_B2$component)[levels(dfParm_B2$component)=="rOff"] <- "Runoff"
  levels(dfParm_B2$component)[levels(dfParm_B2$component)=="prec"] <- "Precipitation"
  levels(dfParm_B2$component)[levels(dfParm_B2$component)=="evap"] <- "Evaporation"
  levels(dfParm_B2$component)[levels(dfParm_B2$component)=="dSto"] <- "Change in Lake Storage"
  
  p <- ggplot(dfParm_B2, aes(x=component, y=mean )) + 
    geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
    geom_line() +
    geom_point()
  
  # p augmented with better labels
  p_aug <- p +
    xlab("Component") + ylab("Flow, in cubic meters per second") +
    ggtitle("Expected Magnitude and 95% Confidence Intervals for the Contant U vector")
  
  print(p_aug)
}
