
rm(list=ls())

library(MASS)
library(mirt)
library(rockchalk)


###Generate person parameters
N <- 10000

theta <- mvrnorm(n=N, mu=c(0,0), Sigma = lazyCor(0.4, 2) )
head(theta)
cor(theta)
summary(theta)
var(theta)

###Generate item parameters
n <- 7 #number of itens

a1 <- runif(n,0.1,1.2)
a2 <- runif(n,0.1,1.2)
d <- rnorm(n,0,1)
item.par.sim <- cbind(a1,a2,d)

###Create arrays for prob (p) and simulated data (x)
p<-matrix(0,N,n)
x<-matrix(0,N,n)
#colnames(x)<-c("i1","i2","i3","i4","i5","i6","i7","i8","i9","i10","i11","i12","i13","i14","i15","i16","i17","i18","i19","i20")
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

# model.2d <- mirt.model('
#               F1 = 1-7
#               F2 = 1-6
#                      ')
# 
# mirt.fit = mirt(x, model=model.2d, itemtype = '2PL', method = 'EM', SE=TRUE)  # 

mirt.fit = mirt(x, model=2, itemtype = '2PL', method = 'EM', SE=TRUE)
#getting coefficients
item.par.est=coef(mirt.fit,
                  rotate="oblimin",
                  simplify=TRUE)
#checking parameter recovery
var(theta)
item.par.est$items
item.par.sim

cbind(item.par.est$items[,1],item.par.est$items[,2],item.par.est$items[,3])-item.par.sim

summary(mirt.fit) 







