#
# Set path to target directory
setwd("C:/Home/Projects/GLWaterBudget/Analysis/Superior")

# source script to set up data files
source("Superior/src/work/setupMonthlyDataFrame.R")

# Source scripts to setup innovations
source("Superior/src/work/trendAnnualStmr.R")
source("Superior/src/work/trendAnnualPrec.R")
source("Superior/src/work/trendAnnualEvap.R")
source("Superior/src/work/trendAnnualRoff.R")
source("Superior/src/work/trendAnnualdSto.R")

# Create observation matrix
dtrndMatrix <- t(rbind(stmrInno,precInno,evapInno,rOffInno,dStoInno))
colnames(dtrndMatrix) <- c('strmInno',"precInno","evapInno","rOffInno","dStoInno")


library(MARSS)

# Systematically replace individual components
U   <- "zero"
x0  <- "zero"
Q   <- "unconstrained"
Z   <- "identity"
A   <- "zero"
R   <- "diagonal and unequal"

Bfull   <- matrix(c("b11","b12","b13","b14","b15",
                    "b21","b22","b23","b24","b25",
                    "b31","b32","b33","b34","b35",
                    "b41","b42","b43","b44","b45",
                    "b51","b52","b53","b54","b55"),5,5)

# Off diag components on the B matrix
Boff <- matrix(c("b12","b13","b14","b15",
                 "b21","b23","b24","b25",
                 "b31","b32","b34","b35",
                 "b41","b42","b43","b45",
                 "b51","b52","b53","b54"),5,4)

# Diag components of the B matrix
Bdiag <- matrix(list("b11",    0,    0,    0,    0,
                     0,"b22",    0,    0,    0,
                     0,    0,"b33",    0,    0,
                     0,    0,    0,"b44",    0,
                     0,    0,    0,    0,"b55"),5,5)

# Specify the number of samples to generate
nGenerate <- 10000; nParm <- 5;
# Allocate matrices to store results
tmpMat <- matrix(NA,nGenerate,nParm); tmpVec <- matrix(NA,nGenerate,1)

library(stringr)  # function str_c

# Set random number seed for reproducible results with sample
set.seed(12345)

# Conduct sampling of off diag AR terms
for (i in 1:nGenerate) {
  # tmpMat[i,] <- sort(unlist(sample(Boff, nParm, replace=FALSE)))
  junk         <- sample(1:1000, 30, replace=FALSE)
  # tmpVec[i]  <- str_c( tmpMat[i,1:nParm], collapse="")
}

# The number of unique samples may be less than the sample size specified
tmp <- unique(tmpVec)

# set the number of samples (nSample) less than nGenerate for dups after sort
nSample <-  1000

# 

# Select sample 
ndxSel <- sample(1:length(tmp), nSample, replace=FALSE) 

# Allocate matrix to contain all B parameters
parmB <- matrix(NA,nSample,25)
# Specify column names for B parameters
colnames(parmB) <- Bfull

# Allocate matrix to contain statistics
AICc     <- matrix(NA,nSample, 1)
logLik   <- matrix(NA,nSample, 1)
numIter  <- matrix(NA,nSample, 1)
converge <- matrix(NA,nSample, 1)
Qest     <- matrix(NA,nSample,15) 
Rest     <- matrix(NA,nSample, 5)
# Initialize counter
k    <- 0
#
for (i in 1:nSample){
  start_time <- Sys.time()
  # Populate diag elements of AR matrix (always estimated)
  B1 <- Bdiag
  # Populate off diag elements based on randomly selected terms
  for (j in 1:nParm) {
    ndx <- match(substr(tmp[ndxSel[i]],1+(3*(j-1)),3+(3*(j-1))),Bfull)
    B1[ndx] <- Bfull[ndx]
  }
  # Increment model counter
  k       <- k + 1
  
  # Specify model
  model.lst <- list(B=B1, U=U, x0=x0, Q=Q, 
                    Z=Z, A=A,        R=R)
  
  ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix),
                                          model = model.lst)
  
  ssAnnDtrnds5Bds1U0x0QduZiA0Rdu <- MARSS(t(dtrndMatrix), method="BFGS",
                                          model = model.lst,
                                          inits = ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par)
  
  AICc[k] <- ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$AICc
  # Find indices of estimated parameter
  ndxB    <- Bfull %in% rownames(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par$B)
  # Store parameter estimates for trial model in parmB matrix
  parmB[k,ndxB] <- ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par$B
  # Store to indicate convergence
  converge[k]   <- ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$convergence
  # Store logLik
  logLik[k]     <- ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$logLik
  # Store number of iterations
  numIter[k]    <- ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$numIter
  # Store process variance info
  Qest[k,]      <- ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par$Q
  # Store measurement variance info
  Rest[k,]      <- ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par$R
  #
  print(Sys.time() - start_time, digits=4)
  print(paste0("Model ",k))
}
# Add column names to process variance matrix
colnames(Qest) <- rownames(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par$Q)
colnames(Rest) <- rownames(ssAnnDtrnds5Bds1U0x0QduZiA0Rdu$par$R)

