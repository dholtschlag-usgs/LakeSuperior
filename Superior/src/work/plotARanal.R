# Plot results of simulation

# Probability level
alpha = as.numeric(readline("Enter p-value of confidence interval[0.05]: "))
if (length(alpha)==0 | alpha <= 0 | alpha>1) alpha <- 0.05
print(paste0("alpha= ",alpha))


# allocate memory to contain quantiles for each
qQ      <- matrix(NA,dim(Qest)[2],2)
qR      <- matrix(NA,dim(Rest)[2],2)
qBdiag  <- matrix(NA,dim(Bdiag)[2],2) 

# Populate selected quantiles of Q
for (i in 1:dim(Qest)[2]) {
  qQ[i,] <- quantile(Qest[,i], prob=c(alpha/2,1-alpha/2),na.rm=TRUE) 
}

# Populate selected quantiles of R
for (i in 1:dim(Rest)[2]) {
  qR[i,] <- quantile(Rest[,i], prob=c(alpha/2,1-alpha/2),na.rm=TRUE) 
}

# Extract the diagonal components from the B matrix
Bdiag  <- parmB[,seq(from=1, to=25, by=6)]

# Populate selected quantiles of Bdiag
for (i in 1:dim(Bdiag)[2]) {
  qBdiag[i,] <- quantile(Bdiag[,i], prob=c(alpha/2,1-alpha/2),na.rm=TRUE) 
}

# Find the minimum AICc in which all Qs, Rs, and Bdiags are within the specified interval
# Find the indices of sorted AICc values
ordNdx <- order(AICc)
for (i in 1:length(AICc)) {
  #
  if ( all( qQ[,1] <=  Qest[ordNdx[i],] &  Qest[ordNdx[i],] <     qQ[,2] &
            qR[,1] <=  Rest[ordNdx[i],] &  Rest[ordNdx[i],] <     qR[,2] &
        qBdiag[,1] <= Bdiag[ordNdx[i],] & Bdiag[ordNdx[i],] < qBdiag[,2] ))
    {
    print(paste0("i= ",i, ", ordNdx= ", ordNdx[i]))
    # mininum index that satisfies the contraint of being within the confidence interval
    tarNdx <- i
    break
  }
}

par(mfrow=c(4,4),mar=c(2,4,1,2)+0.1)
for (j in 1:dim(Qest)[2]) {
  dens <- density(Qest[,j],na.rm=TRUE)
  plot(dens, main=paste0("Q: ",colnames(Qest)[j],": alpha= ",alpha),
       ylab="",cex.main=0.95)
      
  x1 <- min(which(dens$x >= qQ[j,1]))
  x2 <- max(which(dens$x <  qQ[j,2]))
  
  with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray"))
  abline(v=Qest[ordNdx[tarNdx],j], col="red", lty="dashed")
}
#
plot(AICc, pch=20, col="blue", 
     main=paste0("AICc Index for Simulation: ",ordNdx[tarNdx]), cex.main=0.95,
     xlab=paste0("AICc for Simulation: ",format(AICc[ordNdx[tarNdx]],digits=4)),
     ylab="AICc")
abline(v=ordNdx[tarNdx], col="red", lty="dashed")
abline(h=AICc[ordNdx[tarNdx]], col="red", lty="solid")

############################################################################
# Analysis of measurement variance R
############################################################################

# 
par(mfrow=c(2,3),mar=c(2,4,1,2)+0.1)
for (j in 1:dim(Rest)[2]) {
  dens <- density(Rest[,j],na.rm=TRUE)
  plot(dens, main=paste0("R: ",colnames(Rest)[j],": alpha= ",alpha),
       ylab="",cex.main=0.95)
    
  x1 <- min(which(dens$x >= qR[j,1]))
  x2 <- max(which(dens$x <  qR[j,2]))
  
  with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray"))
  abline(v=Rest[ordNdx[tarNdx],j], col="red", lty="dashed")
}
#
plot(AICc, pch=20, col="blue", 
     main=paste0("AICc Index for Simulation: ",ordNdx[tarNdx]), cex.main=0.95,
     xlab=paste0("AICc for Simulation: ",format(AICc[ordNdx[tarNdx]],digits=4)),
     ylab="AICc")
abline(v=ordNdx[tarNdx], col="red", lty="dashed")
abline(h=AICc[ordNdx[tarNdx]], col="red", lty="solid")


############################################################################
# Analysis of Diagonal Components of Autoregressive Matrix 
############################################################################

# Plot density of diag B components
par(mfrow=c(2,3),mar=c(2,4,1,2)+0.1)
for (j in 1:dim(Bdiag)[2]) {
  dens <- density(Bdiag[,j],na.rm=TRUE)
  plot(dens, main=paste0("Bdiag: ",colnames(Bdiag)[j],": alpha= ",alpha),
       ylab="",cex.main=0.95)
    
  x1 <- min(which(dens$x >= qBdiag[j,1]))
  x2 <- max(which(dens$x <  qBdiag[j,2]))
  
  with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray"))
  abline(v=Bdiag[ordNdx[tarNdx],j], col="red", lty="dashed")
}
#
plot(AICc, pch=20, col="blue", 
     main=paste0("AICc Index for Simulation: ",ordNdx[tarNdx]), cex.main=0.95,
     xlab=paste0("AICc for Simulation: ",format(AICc[ordNdx[tarNdx]],digits=4)),
     ylab="AICc")
abline(v=ordNdx[tarNdx], col="red", lty="dashed")
abline(h=AICc[ordNdx[tarNdx]], col="red", lty="solid")


########################################################################

# Store the matrix of 
BoffDiag <- tmpMat

ndxSel   <- colnames(parmB) %in% BoffDiag[ordNdx[tarNdx],]

Boff     <- parmB[,ndxSel]

qBoff  <- matrix(NA,dim(Boff)[2],2) 

# Populate selected quantiles of Q
for (i in 1:dim(Boff)[2]) {
  qBoff[i,] <- quantile(Boff[,i], prob=c(alpha/2,1-alpha/2),
                        na.rm=TRUE) 
}

par(mfrow=c(2,3),mar=c(4,4,3,2))

for (j in 1:dim(Boff)[2]) {
  dens <- density(Boff[,j],na.rm=TRUE)
  plot(dens, main=paste0("Boff: ",colnames(Boff)[j],": alpha= ",alpha),
       ylab="",cex.main=0.95)
  
  x1 <- min(which(dens$x >= qBoff[j,1]),na.rm=TRUE)
  x2 <- max(which(dens$x <  qBoff[j,2]),na.rm=TRUE)
  
  with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray"))
  abline(v=Boff[ordNdx[tarNdx],j], col="red", lty="dashed")
}
#
plot(AICc, pch=20, col="blue", 
     main=paste0("AICc Index for Simulation: ",ordNdx[tarNdx]), cex.main=0.95,
     xlab=paste0("AICc for Simulation: ",format(AICc[ordNdx[tarNdx]],digits=4)),
     ylab="AICc")
abline(v=ordNdx[tarNdx], col="red", lty="dashed")
abline(h=AICc[ordNdx[tarNdx]], col="red", lty="solid")


