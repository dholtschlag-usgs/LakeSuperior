# MARSS model with time-varying parameters
#
# Component NBS Eqn:
# NBS = Precipitation + Runoff - Evaporation
# Residual NBS Eqn:
# NBS = ChangeInStorage - Inflow + Outflow
# Water budget for Lake Superior
# Outflow = Prec + Runoff - Evap - ChangeInStorage + Div
#    stmr = prec + rOff   - evap - dSto            + divr  
TT   <- 696                   # 58 years of monthly data
stmr <- NBSrcDf$stmrCMS[1:TT] 
prec <- NBSrcDf$precCMS[1:TT] 
rOff <- NBSrcDf$rOffCMS[1:TT] 
evap <- NBSrcDf$evapCMS[1:TT] 
dSto <- NBSrcDf$dStoCMS[1:TT] 
divr <- NBSrcDf$divrCMS[1:TT] 
#
# Load MARSS library
library(MARSS)
#
# Set up seasonal model with fixed factors (MARSSv3.9, p. 157)
# Create the c matrix
# number of "seasons" (e.g., 12 months per year)
period = 12
# first "season" (e.g., Jan = 1, July = 7)
per.1st = 1
# create factors for seasons
c.in = diag(period)
for(i in 2:(ceiling(TT/period))) {c.in = cbind(c.in,diag(period))}
# trim c.in to correct start & length
c.in = c.in[,(1:TT)+(per.1st-1)]
# better row names
rownames(c.in) = month.abb



