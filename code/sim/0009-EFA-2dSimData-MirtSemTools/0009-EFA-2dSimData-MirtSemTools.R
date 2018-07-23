
rm(list=ls())

library(MASS)
library(rockchalk)
library(GPArotation)

library(mirt)


###Generate person parameters
N <- 1000
theta <- mvrnorm(n=N, mu=c(0,0), Sigma = lazyCor(0.4, 2) )
head(theta)
cor(theta)
summary(theta)
var(theta)

###Generate item parameters
n <- 7 #number of itens
item.par <- list()

a1 <- runif(n,0.5,2)
a2 <- runif(n,0.5,2)
d <- rnorm(n,0,2)
item.par <- cbind(a1,a2,d)

###Create arrays for prob (p) and simulated data (x)
p<-matrix(0,N,n)
x<-matrix(0,N,n)
colnames(x)<-c("i1","i2","i3","i4","i5","i6","i7")
###Use a stochastic process to generate model-fitting data
for (i in 1:N) {
  for (j in 1:n){
    p[i,j] <- 1/(1+exp(-1*(a1[j]*theta[i,1]+a2[j]*theta[i,2]+d[j])))
    r <- runif(1,0,1)
    if (p[i,j]>=r) {
      x[i,j] <- 1 #individual answers correctly if p> 0.5
    }
  }
}
  


model.2d <- mirt.model('
              F1 = 1-7
              F2 = 1-7
              (F1*F2) =1-7
                       ')

mirt.fit = mirt(x, model=model.2d, itemtype = '2PL', method = 'EM')  # 
PAR=coef(mirt.fit,simplify=TRUE)
PAR




head(x)
### run EFA with WLSMV 
ef2_irt <- efaUnrotate(data=x,
                       estimator="wlsmv", 
                       nf=2,
                       start=T,
                       ordered =colnames(x),
                       parameterization="theta")
summary(ef2_irt, std = TRUE)
str(inspect(ef2_irt, "std"))

#getting factors
factors <-inspect(ef2_irt, "std")[["lambda"]]
factors
factors <-as.matrix(factors)
tau <-inspect(ef2_irt, "std")[["tau"]]
tau

d= tau[1]/sqrt(1-factors[1,]%*%cor(theta)%*%factors[1,])

dim(factors[1,])
dim(cor(theta))
dim(factors[1,])

## use oblique rotation
ef2_ob <- oblqRotate(ef2_irt)
summary(ef2_ob,suppress=.001)

ef2_ob



tau <-ef2_ob @
