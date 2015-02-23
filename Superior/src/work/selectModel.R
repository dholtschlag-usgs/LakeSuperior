selectModel <- function(lqResponse,lqExplain,monthFactor){
  # Test an AR term and seasonal factor in the model
  #  
  # Simple regression model
  lm_reg         <- lm(lqResponse ~ lqExplain)
  # Regression with monthly factor indicator
  lm_regMonth    <- lm(lqResponse ~ lqExplain + as.factor(monthFactor))
  # Regression with monthly factor and AR term
  #   The c(tail(lq04116000,-1),NA) is a lag1 term
  lm_regMonthAR  <- lm(lqResponse ~ lqExplain + as.factor(monthFactor) + 
                         c(tail(lqExplain,-1),tail(lqExplain,1)))
  #
  # Comparison of models estimated
  anovaModels <- anova(lm_reg,lm_regMonth,lm_regMonthAR)
  print(anovaModels)
  
  if (any(anovaModels$Pr[2:3] < 0.001)){
    ndxModel <- max(which(anovaModels$Pr[2:3]<0.001)) + 1
  }  else {
    ndxModel <- 1
  }
  
  if (ndxModel == 1) {
    return(lm_reg)
  } else if (ndxModel == 2){
    return(lm_regMonth)
  } else {
    return(lm_regMonthAR)
  }
}
