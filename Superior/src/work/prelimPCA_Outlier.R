# PCA analyis of multivariate data to identify outliers

stmr       <- NBSrcDf$stmrCMS[1:TT] 
prec       <- NBSrcDf$precCMS[1:TT] 
rOff       <- NBSrcDf$rOffCMS[1:TT] 
evap       <- NBSrcDf$evapCMS[1:TT] 
dSto       <- NBSrcDf$dStoCMS[1:TT] 
divr       <- NBSrcDf$divrCMS[1:TT]
# dTim       <- as.numeric(NBSrcDf$DateSeq[1:TT])
# Evaluate the effectiveness of lag1 flows
ar01Stmr    <- NBSrcDf$stmrCMS[ 2:(TT+ 1)]
ar02Stmr    <- NBSrcDf$stmrCMS[ 3:(TT+ 2)]
#
# Standardize variables before PCA analysis
zStmr       <- (stmr-mean(stmr))/sd(stmr)
x   <- as.matrix(cbind(stmr,ar01Stmr,ar02Stmr,prec,rOff,evap,dSto,divr))
z   <- matrix(NA,744,8)
for (i in 1:8){
  z[,i] <- (x[,i]-mean(x[,i]))/sd(x[,i])
}
#
pcaX <- princomp(x,scale=TRUE)
print(summary(pcaX))
plot(pcaX$scores[,1],pcaX$scores[,2],pch=c(0:5),col=colorPnt,cex=0.8)
abline(v=0,col="blue",lty="dotdash")
abline(h=0,col="blue",lty="dotdash")
legend("topleft",legend=month.abb,col=colorPnt,pch=c(0:5),cex=0.9)
abline(v=0)
