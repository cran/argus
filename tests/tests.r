library(argus)
# pdf, cdf and quantile function
dargus((0:10)/10,.6)
qargus(0.9,2)
pargus(qargus(0.9,2),2)

y<-rargus(n=1.e5,chi=0.3,method="inversion")
hist(y,breaks=100,main="Argus pdf with chi=0.3",freq=F)
lines(xv<-seq(0,1,0.001),dargus(xv,chi=0.3),lwd=0.2,col=2)

n=1.e5
chiv <- 1. +runif(n)*5 # chi-values in (1,5)
system.time({set.seed(123);y<-rargus(chi=chiv)})
set.seed(123);u<- runif(n)
mean(abs(pargus(y,chiv)-u))

# random variate generation with the inversion method
y<-rargus(n=1.e5,chi=0.3,method="inversion")
hist(y,breaks=100,main="Argus pdf with chi=0.3",freq=F)
lines(xv<-seq(0,1,0.001),dargus(xv,chi=0.3),lwd=0.2,col=2)
# using Ratio of Uniforms
y<-rargus(n=1.e5,chi=3,method="RoU")
hist(y,breaks=100,main="Argus pdf with chi=3",freq=F)
lines(xv<-seq(0,1,0.001),dargus(xv,chi=3),lwd=0.2,col=2)


#########################################################
#########################################################
# checking the u-error of the inversion method
n=1.e6
chiv <- 1. +runif(n)*5 # chi-values in (1,5)
system.time({set.seed(123);y<-rargus(chi=chiv)})
set.seed(123);u<- runif(n)
mean(abs(pargus(y,chiv)-u))
max(abs(pargus(y,chiv)-u))


chiv <- 0.1 + runif(n)*0.9 # chi-values in (0.1,1)
system.time({set.seed(123);y<-rargus(chi=chiv)})
set.seed(123);u<- runif(n)
mean(abs(pargus(y,chiv)-u))
max(abs(pargus(y,chiv)-u))

chiv <- 0.01 + runif(n)*0.09 # chi-values in (0.01,0.1)
system.time({set.seed(123);y<-rargus(chi=chiv)})
set.seed(123);u<- runif(n)
mean(abs(pargus(y,chiv)-u))
max(abs(pargus(y,chiv)-u))

chiv <- runif(n)*0.01 # chi-values in (0,0.01)
system.time({set.seed(123);y<-rargus(chi=chiv)})
set.seed(123);u<- runif(n)
mean(abs(pargus(y,chiv)-u))
max(abs(pargus(y,chiv)-u))

#####################
# checking the CDF using the pdf and numeric integration

integrate(f=dargus,chi=1,lower=0,upper=0.3,rel.tol=1.e-14)$val
#[1] 0.1094953
integrate(f=dargus,chi=1,lower=0,upper=0.3,rel.tol=1.e-14)$val-pargus(0.3,1)
#[1] 2.775558e-16
integrate(f=dargus,chi=1,lower=0,upper=0.03,rel.tol=1.e-14)$val-pargus(0.03,1)
#[1] -1.15576e-16
integrate(f=dargus,chi=6,lower=0,upper=0.3,rel.tol=1.e-14)$val-pargus(0.3,6)
#[1] 1.588187e-22
pargus(0.3,6)
#[1] 2.86997e-07
integrate(f=dargus,chi=6,lower=0.9,upper=1,rel.tol=1.e-12)$val-pargus(0.9,6,lower=FALSE)
#[1] 1.665335e-15
pargus(0.9,6,lower=FALSE)
#[1] 0.9228239


###################
# comparing CDF and quantile function
u <- runif(1.e6)
chiv <- runif(1.e6)*.01
max(abs(pargus(qargus(u,chiv),chiv)-u))
#[1] 5.091674e-14
chiv <- runif(1.e6)*1
max(abs(pargus(qargus(u,chiv),chiv)-u))
#[1] 1.64313e-14
chiv <- runif(1.e6)*10
max(abs(pargus(qargus(u,chiv),chiv)-u))
#[1] 7.993606e-15

###############
pargus(0.9,6,lower=FALSE)
#[1] 0.9228239
1-exp(pargus(0.9,6,log.p=T))
#[1] 0.9228239

qargus(0.9,2)
#[1] 0.9398552
qargus(log(0.1),2,lower=F,log.p=T)
#[1] 0.9398552


