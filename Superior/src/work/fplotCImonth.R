ggPlotCI <- function(marssModel,marssParCI,paramSel)
  # fPlotCI is a function to plot confidence intervals from a MARSSparamCIs list
  # 
  # Load ggplot2 library
  library(ggplot2)
# 
# marssModel <- seas.mod.1
# marssParCI <- parmCIinfo
# paramSel   <- "Season"
# paramSel   <- "arMatrix"
#
if (paramSel == "Season"){
  # Seasonal components
  CIlo_Seas  <- marssParCI$par.lowCI$U
  CIhi_Seas  <- marssParCI$par.upCI$U
  mean_Seas  <- marssParCI$par$U
  # 
  rowNames   <- row.names(mean_Seas)
  # Find the start of the variable name location after the period in the name
  begVarList <- gregexpr(pattern="\\.",rowNames)
  begVarVec  <- unlist(begVarList) + 1  
  # Find the end of the variable name
  endVarList <- gregexpr(pattern="CMS",rowNames)
  endVarVec  <- unlist(endVarList) - 1
  varName    <- substr(rowNames, begVarVec, endVarVec)
  # Find the beginning of the month name
  begMonList <- gregexpr(pattern=",",rowNames)
  begMonVec  <- unlist(begMonList) + 1  
  # Find the end of the month name
  endMonList <- gregexpr(pattern=")",rowNames)
  endMonVec  <- unlist(endMonList) - 1
  
  monName    <- substr(rowNames,begMonVec,endMonVec)
  monName    <- factor(monName, levels = month.abb, ordered = TRUE)
  #
  # Create a dataframe to contain the data
  df_Sea     <- data.frame(monName,varName,mean_Seas,CIlo_Seas,CIhi_Seas)
  # 
  pd <- position_dodge(.8) # move them .05 to the left and right
  ggplot(df_Sea, aes(x=monName, y=mean_Seas, colour=varName, group=varName)) +
    geom_errorbar(aes(ymin=CIlo_Seas, ymax=CIhi_Seas), width=0.12, position = pd) +
    xlab("Month") + 
    ylab("95% Confidence Interval and Expected Values of \nZ-Scores of Water Budget Components") + 
    ggtitle("Monthly Variation of Lake Superior Water Budget Components") +
    # theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
    geom_line(position=pd) +
    geom_point(position=pd, aes(ymin=CIlo_Seas, ymax=CIhi_Seas))
  #
} else if (paramSel == "AR"){
  # Autoregressive components
  CIlo_AR    <- marssParCI$par.lowCI$B
  CIhi_AR    <- marssParCI$par.upCI$B
  mean_AR    <- marssParCI$par$B
  # Create a dataframe to contain the data
  compNames  <- row.names(marssModel$marss$data)
  compNames  <- factor(compNames, levels=compNames, ordered = TRUE)
  df_AR      <- data.frame(compNames,mean_AR,CIlo_AR,CIhi_AR)
  #
  ggplot(df_AR, aes(x=compNames,
                    y=mean_AR, colour=compNames, group=varName)) +
    geom_errorbar(aes(ymin=CIlo_Seas, ymax=CIhi_Seas), width=0.12, position = pd) +
    xlab("Month") + 
    ylab("95% Confidence Interval and Expected Values of \nZ-Scores of Water Budget Components") + 
    ggtitle("Monthly Variation of Lake Superior Water Budget Components") +
    # theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
    geom_line(position=pd) +
    geom_point(position=pd, aes(ymin=CIlo_Seas, ymax=CIhi_Seas))
  #
  ggplot(df_AR, aes(x=compNames, y=mean_AR, group=1)) + 
    geom_line(linetype = 2, color="grey") +
    geom_errorbar(width=-.15, aes(ymin = CIlo_AR, ymax = CIhi_AR), color="red") +
    geom_point(shapte=21, size=3, fill = "black", color="black") + ylim(0,1) +
    ylab("Autocorrelation Coefficient") + xlab("Water Budget Component") + 
    ggtitle("Diagonal Components of the State Auto Regression Matrix")
} else{
  print("Variable set not recognized as Season or AR")
}
