rm(list=ls())

if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(mirt)) install.packages("mirt"); library(mirt)

set.seed(123)

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


X1 <- rnorm(100); X2 <- rnorm(100); X3 <- rnorm(100)
fit <- polr(ordered(Y) ~ X1 + X2 + X3, method = "probit")
fit$zeta
cbind(X1,X2,X3)

s2 <- sem("Y ~ X1 + X2 + X3", data=data.frame(Y,X1,X2,X3), ordered=c("Y","X1","X2","X3"))
summary(s2)

# Generate Longitudinal Items from a Bivariate LIR
data_creation <-function(){}

N <- 1000
# mean difference = 0.8
mu <- c(wave1 = 0, wave2 = 0.5, wave3 = 1.0, wave4 =1.5)
# autocorrelation = 0.7 / sqrt(2) = 0.495
Sigma <- matrix(c(1, .7, .49, .34,.7, 1,.7,.49,.49, .7, 1,.7,.34, .49,.7, 1 ), nrow = 4,byrow = TRUE)

dat <- data.frame(MASS::mvrnorm(N, mu, Sigma))
# binary (1 threshold)
dat$y2w1 <- as.numeric(dat$wave1 > -0.5)
dat$y2w2 <- as.numeric(dat$wave2 > 0.0)
dat$y2w3 <- as.numeric(dat$wave3 > 0.5)
dat$y2w4 <- as.numeric(dat$wave4 > 1.0)
# ternary (2 thresholds)
dat$y3w1 <- dat$y2w1 + (dat$wave1 > 0.5)
dat$y3w2 <- dat$y2w2 + (dat$wave2 > 0.5)
dat$y3w3 <- dat$y2w3 + (dat$wave3 > 0.5)
# polytomous (3 thresholds)
dat$y4w1 <- dat$y3w1 + (dat$wave1 > 1)
dat$y4w2 <- dat$y3w2 + (dat$wave2 > 1)
dat$y4w3 <- dat$y3w3 + (dat$wave3 > 1)


#dat binary
#datn numeric

datn <- dat
names(dat)
datn <- datn[,c("wave1","wave2","wave3","wave4")]
colnames(datn)<-c("y2w1","y2w2","y2w3","y2w4")
datn 


# model with 1 latent variable and three indicators
# numeric case
# + without means structure modn_ind3_lv1_nomeans
# + with means structure modn_ind3_lv1_means
# binary case
# + without means structure mod2_ind3_lv1_nomeans
# + with means structure mod2_ind3_lv1_means

# Specify Model Parameters for numerical itens
# 3 indicators with latent variables. 
modn_ind3_lv1_nomeans<- function(){}

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

modn_ind3_lv1_nomeans <- '

lv1 =~ l1*y2w1 + l2*y2w2 + l3*y2w3 
#1 == l1 + l2 + l3
lv1var==1
lv1 ~~ lv1var*lv1

## LIR (co)variances
y2w1 ~~ var1*y2w1 + 0*y2w2 + 0*y2w3
y2w2 ~~ var2*y2w2 + 0*y2w3
y2w3 ~~ var3*y2w3
'
rm(modn_ind3_lv1_nomeans.fit)
modn_ind3_lv1_nomeans.fit <- lavaan(modn_ind3_lv1_nomeans, 
                          data = datn, 
                          parameterization = "delta")

varTable(modn_ind3_lv1_nomeans.fit)
lavParTable(modn_ind3_lv1_nomeans,
            parameterization = "theta")
inspect(modn_ind3_lv1_nomeans.fit) #free
inspect(modn_ind3_lv1_nomeans.fit,"est") #estimated
summary(modn_ind3_lv1_nomeans.fit)
fitMeasures(modn_ind3_lv1_nomeans.fit)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]
vcov <- lavInspect(modn_ind3_lv1_nomeans.fit,what = "vcov")
eigen(vcov)

lavOptions() # check options

modn_ind3_lv1_means<- function(){}

#with means format 1

modn_ind3_lv1_means <- '

lv1 =~ l1*y2w1 + l2*y2w2 + l3*y2w3 

lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1

## LIR means
y2w1 ~ mean1*1
y2w2 ~ mean2*1
y2w3 ~ mean3*1

0 == mean1 + mean2 +mean3
#lv1mean==0

## LIR (co)variances
y2w1 ~~ var1*y2w1 + 0*y2w2 + 0*y2w3
y2w2 ~~ var2*y2w2 + 0*y2w3
y2w3 ~~ var3*y2w3
'

rm(modn_ind3_lv1_means.fit)
modn_ind3_lv1_means.fit <- lavaan(modn_ind3_lv1_means, 
                          data = datn, 
                          std.lv = TRUE,
                          meanstructure =TRUE,
                          parameterization = "theta")

lavParTable(modn_ind3_lv1_means,
            std.lv = TRUE,
            meanstructure =TRUE,
            parameterization = "theta")

summary(modn_ind3_lv1_means.fit)
fitMeasures(modn_ind3_lv1_means.fit)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]
inspect(modn_ind3_lv1_means.fit,"free")
inspect(modn_ind3_lv1_means.fit,"partable")
inspect(modn_ind3_lv1_means.fit,"est")

mod2_ind3_lv1_nomeans<- function(){}

#with means format 1

mod2_ind3_lv1_nomeans <- '

lv1 =~ l1*y2w1 + l2*y2w2 + l3*y2w3 
lv1 ~~ lv1var*lv1

## LIR means
y2w1 ~ mean1*1
y2w2 ~ mean2*1
y2w3 ~ mean3*1
mean1==0;mean2==0;mean3==0;
#0 == mean1 + mean2 +mean3
#lv1mean==0

## thresholds link LIRs to observed items
y2w1 | thr1*t1
y2w2 | thr2*t1
y2w3 | thr3*t1

## LIR (co)variances
y2w1 ~~ var1*y2w1 + 0*y2w2 + 0*y2w3
y2w2 ~~ var2*y2w2 + 0*y2w3
y2w3 ~~ var3*y2w3
var1==1;var2==1;var3==1;
'


rm(mod2_ind3_lv1_nomeans.fit)
mod2_ind3_lv1_nomeans.fit <- lavaan(mod2_ind3_lv1_nomeans, 
                                  data = dat, 
                                  std.lv = TRUE,
                                  meanstructure =TRUE,
                                  parameterization = "delta")

lavParTable(mod2_ind3_lv1_nomeans,
            std.lv = TRUE,
            meanstructure =TRUE,
            parameterization = "delta")

summary(mod2_ind3_lv1_nomeans.fit)
fitMeasures(mod2_ind3_lv1_nomeans.fit)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]
inspect(mod2_ind3_lv1_nomeans.fit) #free
inspect(mod2_ind3_lv1_nomeans.fit,"est") #estimated


mod2_ind3_lv1_means<- function(){}
#with means format 1
mod2_ind3_lv1_means <- '

lv1 =~ l1*y2w1 + l2*y2w2 + l3*y2w3 
lv1 ~~ lv1var*lv1

## LIR means
y2w1 ~ mean1*1
y2w2 ~ mean2*1
y2w3 ~ mean3*1
mean1==0;mean2==0;mean3==0;
#0 == mean1 + mean2 +mean3
#lv1mean==0

## thresholds link LIRs to observed items
y2w1 | thr1*t1
y2w2 | thr2*t1
y2w3 | thr3*t1

## LIR (co)variances
y2w1 ~~ var1*y2w1 + 0*y2w2 + 0*y2w3
y2w2 ~~ var2*y2w2 + 0*y2w3
y2w3 ~~ var3*y2w3
var1==1;var2==1;var3==1;
'


rm(mod2_ind3_lv1_means.fit)
mod2_ind3_lv1_means.fit <- lavaan(mod2_ind3_lv1_means, 
                                    data = dat, 
                                    std.lv = TRUE,
                                    meanstructure =TRUE,
                                    parameterization = "delta")

lavParTable(mod2_ind3_lv1_means,
            std.lv = TRUE,
            meanstructure =TRUE,
            parameterization = "delta")

summary(mod2_ind3_lv1_means.fit)
fitMeasures(mod2_ind3_lv1_means.fit)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]
inspect(mod2_ind3_lv1_means.fit) #free
inspect(mod2_ind3_lv1_means.fit,"est") #estimated


