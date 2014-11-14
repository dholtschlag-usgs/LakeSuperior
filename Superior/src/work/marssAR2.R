
zeroMat  <- matrix(dat[,1],5,1)
datDelay <- cbind(zeroMat,dat[,1:743]) 
dat2     <- rbind(dat,datDelay)
rownames(dat2) <- c("stmrCMS","precCMS","evapCMS","rOffCMS","dStoCMS","stmrCMS_","precCMS_","evapCMS_","rOffCMS_","dStoCMS_")
# B = "diagonal and unequal"

# Z = "identity" 
Z        <- cbind(diag(5),zero5)
B1       <- matrix(list(0),5,5)
diag(B1) <- list("b11","b12","b13","b14","b15")
B2       <- matrix(list(0),5,5)
diag(B2) <- list("b21","b22","b23","b24","b25")
iden5    <- diag(5)
zero5    <- matrix(list(0),5,5)
B        <- rbind( cbind(B1,B2), cbind(iden5,zero5))
# U vector
U        <- matrix(0,10,1)
# Assume independent process errors
# Q = "diagonal and unequal"
Q1       <- matrix(list(0),5,5)
diag(Q1) <- list("q1","q2","q3","q4","q5")
Q        <- rbind( cbind(Q1,zero5), cbind(zero5,zero5))
# We have demeaned the data & are fitting a mean-reverting model
# by estimating a diagonal B, thus
# U = matrix(c(rowMeans(dat)),5,1)
# U = "unconstrained"
# U = "zero"
# Each obs time series is associated with only one process

# The data are demeaned & fluctuate around a mean
A        <- matrix(0,5,1)
# We assume observation errors are independent, but they
# have similar variance due to similar collection methods
R        <- matrix(list(0),5,5)
diag(R)  <- list("q11","q12","q13","q14","q15")
# pi -> pie
pie      <- matrix(rowMeans(dat),10,1);
V        <- matrix(0,10,10)
# 
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
#
C = matrix(month.abb,5,12,byrow=TRUE)
C

###################################################
### code chunk number 14: Covar_sec6_04_C-constrained2
###################################################
C = "unconstrained"

# Specification problem
# Estimate model
model.list = list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,x0=pie,V0=V,tinitx=0)
#
marssAR2 = MARSS(dat,model=model.list,control=list(maxit=1500,safe=TRUE))
#
# Subtract monthly mean from dat and re-estimate
# Extract components from dat
stmrCMS  <- dat[1,] 
precCMS  <- dat[2,]
evapCMS  <- dat[3,]
rOffCMS  <- dat[4,]
dStoCMS  <- dat[5,]
#
# Reform 
strmMonth <- matrix(stmrCMS,62,12,byrow=TRUE)
boxplot(strmMonth,xaxt="n");
axis(side=1,labels=month.abb,at=seq(1,12))
abline(h=0,col="red",lty="dashed")

strmMonth0 <- strmMonth - matrix(rep(c(colMeans(strmMonth)),62),62,12,byrow=TRUE)
boxplot(strmMonth0,xaxt="n");
axis(side=1,labels=month.abb,at=seq(1,12))
abline(h=0,col="red",lty="dashed")
stmrMon    <- matrix(strmMonth0,1,744)
#
precCMS    <- dat[2,]
precMonth  <- matrix(precCMS,62,12,byrow=TRUE)
precMonth0 <- precMonth - matrix(rep(c(colMeans(precMonth)),62),62,12,byrow=TRUE)
boxplot(precMonth0,xaxt="n");
axis(side=1,labels=month.abb,at=seq(1,12))
abline(h=0,col="red",lty="dashed")
precMon    <- matrix(precMonth0,1,744)
#
evapCMS    <- dat[3,]
evapMonth  <- matrix(evapCMS,62,12,byrow=TRUE)
evapMonth0 <- evapMonth - matrix(rep(c(colMeans(evapMonth)),62),62,12,byrow=TRUE)
boxplot(evapMonth0,xaxt="n");
axis(side=1,labels=month.abb,at=seq(1,12))
abline(h=0,col="red",lty="dashed")
evapMon    <- matrix(evapMonth0,1,744)
#
rOffCMS    <- dat[4,]
rOffMonth  <- matrix(rOffCMS,62,12,byrow=TRUE)
rOffMonth0 <- rOffMonth - matrix(rep(c(colMeans(rOffMonth)),62),62,12,byrow=TRUE)
boxplot(rOffMonth0,xaxt="n");
axis(side=1,labels=month.abb,at=seq(1,12))
abline(h=0,col="red",lty="dashed")
rOffMon    <- matrix(rOffMonth0,1,744)
#
dStoCMS    <- dat[5,]
dStoMonth  <- matrix(dStoCMS,62,12,byrow=TRUE)
dStoMonth0 <- dStoMonth - matrix(rep(c(colMeans(dStoMonth)),62),62,12,byrow=TRUE)
boxplot(dStoMonth0,xaxt="n");
axis(side=1,labels=month.abb,at=seq(1,12))
abline(h=0,col="red",lty="dashed")
dStoMon    <- matrix(dStoMonth0,1,744)
#
datMon     <- rbind(stmrMon,precMon,evapMon,rOffMon,dStoMon)

model.list = list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,x0=pie,V0=V,tinitx=0)
marssMonAR2 = MARSS(datMon,model=model.list,control=list(maxit=1500,safe=TRUE))
#
Z[1,1] <- 'z1'; Z[2,2] <- 'z2'; Z[3,3] <- 'z3'; Z[4,4] <- 'z4'; Z[5,5] <- 'Z5'
model.list = list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,x0=pie,V0=V,tinitx=0)
marssMonAR2z = MARSS(datMon,model=model.list,control=list(maxit=1500,safe=TRUE))


Z[1,1] <- 1; Z[2,2] <- 1; Z[3,3] <- 1; Z[4,4] <- 1; Z[5,5] <- 1
c2.in <- rbind(c.in,c.in)
Cm = matrix(c("c101","c102","c103","c104","c105","c106","c107","c108","c109","c110","c111","c112",
                  "c201","c202","c203","c204","c205","c206","c207","c208","c209","c210","c211","c212",
                  "c301","c302","c303","c304","c305","c306","c307","c308","c309","c310","c311","c312",
                  "c401","c402","c403","c404","c405","c406","c407","c408","c409","c410","c411","c412",
                  "c501","c502","c503","c504","c505","c506","c507","c508","c509","c510","c511","c512"),
         5,12,byrow=TRUE)
Cm <- rbind(Cm,Cm)
model.list = list(B=B,U=U,Q=Q,Z=Z,A=A,R=R,x0=pie,V0=V,tinitx=0)
marssAR2B <- MARSS(dat,model=model.list,control=list(maxit=1500,safe=TRUE))



# Compute parameter uncertainities
marssParCI <- MARSSparamCIs(seas.mod.10)


tt <- 50
true.2 <- c(r=0,b1=-1.5,b2=-0.75,q=1)
temp   <- arima.sim(n=tt,list(ar=true.2[2:3]),sd=sqrt(true.2[4]))
sim.ar2<- matrix(temp,nrow=1)
#
Z <- matrix(c(1,0),1,2)
B <- matrix(list("b1",1,"b2",0),2,2)
U <- matrix(0,2,1)
Q <- matrix(list("q",0,0,0),2,2)
A <- matrix(0,1,1)
R <- matrix(0,1,1)
pi<- matrix(sim.ar2[2:1],2,1)
V <- matrix(0,2,2)
model.list.2<-list(Z=Z,B=B,U=U,Q=Q,A=A,R=R,x0=pi,V0=V,tinitx=1)
ar2 <- MARSS(sim.ar2[2:tt],model=model.list.2)
