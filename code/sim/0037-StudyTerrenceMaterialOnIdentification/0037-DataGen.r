rm(list=ls())

if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(mirt)) install.packages("mirt"); library(mirt)


thresholdCalc <- function(){}
# Threshold calculation 1

# generate ‘ordered’ data with 4 categories
Y <- sample(1:4, size = 100, replace = TRUE)
Y
prop <- table(Y)/sum(table(Y))
cprop <- c(0, cumsum(prop))
th <- qnorm(cprop)
th
# Threshold calculation 2 - quando há uma dependência de variáveis exógenas.

library(MASS)

X1 <- rnorm(100); X2 <- rnorm(100); X3 <- rnorm(100)
fit <- polr(ordered(Y) ~ X1 + X2 + X3, method = "probit")
fit$zeta
cbind(X1,X2,X3)


s2 <- sem("Y ~ X1 + X2 + X3", data=data.frame(Y,X1,X2,X3), ordered=c("Y","X1","X2","X3"))
summary(s2)

rm(dat,X1,Y,prop, cprop,th,theta_mean,theta_sd)
theta_mean<-0
theta_sd<-1
N <- 100000
dat<- matrix(NA,nrow=N,ncol=4)
X1 <- rnorm(N,mean=theta_mean,sd=theta_sd);
Y <- as.numeric(X1 > -0.5)
prop <- table(Y)/sum(table(Y))
cprop <- c(0, cumsum(prop))
th <- qnorm(cprop,mean=theta_mean,sd=theta_sd)
th

dat[,1] <- Y
dat[,2] <- as.numeric(X1 > 0)
dat[,3] <- as.numeric(X1 > 0.5)
dat[,4] <- as.numeric(X1 > 1)

colnames(dat)<-c("y2w1","y2w2","y2w3","y2w4")


# model with 1 latent variable and three indicators
# numeric case
# + without means structure modn_ind3_lv1_nomeans
# + with means structure modn_ind3_lv1_means
# binary case
# + without means structure mod2_ind3_lv1_nomeans
# + with means structure mod2_ind3_lv1_means

# Specify Model Parameters for numerical itens
# 3 indicators with latent variables. 

# data - 6 - ov (co)variances
# varibles - 3 lambdas
#          - 3 residual variances
#          - 3 residual covariances
#          - 1 lv variance
# Identified if
#          - residual covariances = 0
#          - lv variance fixed =1
# or
# Identified if
#          - residual covariances = 0
#          - one lambda fixed =1



mod2_ind3_lv1_means<- function(){}
#with means format 1
mod2_ind3_lv1_means <- '

lv1 =~ l1*y2w1 + l2*y2w2 + l3*y2w3 
lv1 ~~ lv1var*lv1

## LIR means
y2w1 ~ mean1*1 + 0*1
y2w2 ~ mean2*1 + 0*1
y2w3 ~ mean3*1 + 0*1
#mean1==0;mean2==0;mean3==0;
#0 == mean1 + mean2 +mean3
#lv1mean==0

## thresholds link LIRs to observed items
y2w1 | thr1*t1
y2w2 | thr2*t1
y2w3 | thr3*t1

## LIR (co)variances
y2w1 ~~ var1*y2w1 + 1*y2w1 + 0*y2w2 + 0*y2w3
y2w2 ~~ var2*y2w2 + 1*y2w2 + 0*y2w3
y2w3 ~~ var3*y2w3 + 1*y2w3 
'


rm(mod2_ind3_lv1_means.fit)
mod2_ind3_lv1_means.fit <- lavaan(mod2_ind3_lv1_means, 
                                    data = dat, 
                                    std.lv = TRUE,
                                    meanstructure =TRUE,
                                    ordered = c("y2w1","y2w2","y2w3"),
                                    parameterization = "delta")

lavParTable(mod2_ind3_lv1_means,
            std.lv = TRUE,
            meanstructure =TRUE,
            parameterization = "delta")

summary(mod2_ind3_lv1_means.fit)
fitMeasures(mod2_ind3_lv1_means.fit)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]
inspect(mod2_ind3_lv1_means.fit) #free
inspect(mod2_ind3_lv1_means.fit,"est") #estimated


