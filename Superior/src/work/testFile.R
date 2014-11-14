# B = "diagonal and unequal"
B = "identity"
# Assume independent process errors
Q = "diagonal and unequal"
# We have demeaned the data & are fitting a mean-reverting model
# by estimating a diagonal B, thus
# U = matrix(c(rowMeans(dat)),5,1)
# U = "unconstrained"
U = "zero"
# Each obs time series is associated with only one process
Z = "unconstrained" 
# The data are demeaned & fluctuate around a mean
A = "zero" 
# We assume observation errors are independent, but they
# have similar variance due to similar collection methods
R = "diagonal and equal"
# We are not including covariate effects in the obs equation
D = "zero"
d = "zero"
# Compute model
model.list = list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,C=C,c=c.in,D=D,d=d)
seas.mod.7 = MARSS(dat,model=model.list,control=list(maxit=1500),
                   inits=list(x0=0))

### RESULTS ###
Success! abstol and log-log tests passed at 185 iterations.
Alert: conv.test.slope.tol is 0.5.
Test with smaller values (<0.1) to ensure convergence.

MARSS fit is
Estimation method: kem 
Convergence test: conv.test.slope.tol = 0.5, abstol = 0.001
Estimation converged in 185 iterations. 
Log-likelihood: -28829.74 
AIC: 57851.48   AICc: 57856.62   
