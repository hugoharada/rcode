library(MASS)
library(rockchalk)
library(GPArotation)

###Generate person parameters
## subjects
N <- 1000
#theta <- rnorm(N,0,1)
theta <- mvrnorm(n=N, mu=c(0,0,0,0), Sigma = lazyCor(0.4, 4) )
head(theta)
cor(theta)
summary(theta)
var(theta)

###Generate item parameters
item.par <- list()
p <- list()
x <- list()
for(k in 1:4){ # k dimensions
  n <- 7 # n items
  b <- rnorm(n,0,1)
  a <- runif(n,0.5,3)
  c <- runif(n,0,0.3)#rep(0,n)#
  item.par[[k]] <- cbind(a,b,c)
  
  ###Create arrays for prob (p) and simulated data (x)
  p[[k]] <- matrix(0,N,n)
  x[[k]] <- matrix(0,N,n)
  
  ###Use a stochastic process to generate model-fitting data
  for (i in 1:N) {
    for (j in 1:n){
      p[[k]][i,j] <- c[j] + (1-c[j])/(1+exp(-a[j]*(theta[i,k]-b[j])))
      r <- runif(1,0,1)
      if (r <= p[[k]][i,j]) {
        x[[k]][i,j] <- 1
      }
    }
  }
  
}

head(x[[2]])

## combine items from the 4 factors
x2 <- data.frame(x[[1]], x[[2]], x[[3]], x[[4]])
head(x2)



### run EFA with WLSMV 
ef2_irt <- efaUnrotate(data=x2,
                       estimator="wlsmv", 
                       nf=4,
                       start=T,
                       ordered =colnames(x2),
                       parameterization="delta")

summary(ef2_irt, std = TRUE)
inspect(ef2_irt, "std")

## use oblique rotation
ef2_ob <- oblqRotate(ef2_irt)
summary(ef2_ob,suppress=.001)


### run EFA with WLSMV: Theta 
ef2_irt_theta <- efaUnrotate(data=x2,
                             estimator="wlsmv", 
                             nf=2,
                             start=F,
                             ordered =colnames(x2),
                             parameterization="theta")

summary(ef2_irt_theta, std = TRUE)
inspect(ef2_irt_theta, "std")

## use oblique rotation
ef2_ob_theta <- oblqRotate(ef2_irt_theta)
summary(ef2_ob_theta,suppress=.001)