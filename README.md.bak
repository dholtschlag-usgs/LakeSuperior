LakeSuperior
============

Lake Superior Water Budget Model for Evaluation of a State-Space Approach

Work supported by the International Joint Commission

Investigators: Dave Holtschlag, USGS, and Vincent Fortin, Environment Canada, 

Monthly data for the residual net basin supply analysis was provided by Jacob Bruxer, EC, as: "Lake Superior data through 2013.xls" on Oct. 21, 2014.
Monthly data for the component net basin supply analysis was provided by Tim Hunter, NOAA GLERL, on Oct. 22, 2014.

   R-Script                                   Function
----------------   -------------------------------------------------------------------
checkCompNBS.R     Reads text files of data to confirm estimated and computed NBS for components method.  Provides probability density plots for elements of NBS components.

checkResidNBS.R    Reads text files of data to confirm estimated and computed NBS for residuals methods.  Provides probability density plots for elements of residual components.

setupMonthlyDataFrame.R   Merges residual and component dataframes on date. 

simpleRegressionModel_Compare_LM_MARSS  Develops multiple-linear regression equations using both linear regression and MARSS output-only equation.  Shows the magnitude and uncertaintiy of parameters are consistent for the two methods.  Shows acf of model residuals and apparent trend in residuals with flow magnitude on St. Marys River.

monthlyWaterBudgetModels.R   Develops monthly multiple regression and AR[1] multiple regression models for Lake Superior Water Budget.  Show monthly parameter estimates and 95% confidence intervals and relation between measured and estimated flows.  A plot compares R2 values for from annual and monthly (AR[1]) multiple regression models.  AR[1] parameters for November and December models are greater than 1.  Note .Rmd and .html files are included from script. 

readPlotDiversions.R     Read and plot monthly flow diversions to Lake Superior from Long Lac and Ogoki River.
