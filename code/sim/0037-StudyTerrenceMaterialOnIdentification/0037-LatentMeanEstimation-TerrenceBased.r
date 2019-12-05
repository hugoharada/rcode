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



# Generate Longitudinal Items from a Bivariate LIR
data_creation <-function(){}

N <- 1000
# mean difference = 0.8
mu <- c(wave1 = 0, wave2 = 0.5, wave3 = 1.0, wave4 =1.5)
# autocorrelation = 0.7 / sqrt(2) = 0.495
Sigma <- matrix(c(1, .7, .49, .34,.7, 1,.7,.49,.49, .7, 1,.7,.34, .49,.7, 1 ), nrow = 4,byrow = TRUE)
set.seed(123)
dat <- data.frame(MASS::mvrnorm(N, mu, Sigma))
# binary (1 threshold)
dat$y2w1 <- as.numeric(dat$wave1 > -0.5)
dat$y2w2 <- as.numeric(dat$wave2 > -0.5)
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



# Specify Model Parameters for Binary Items
# 2 indicators
mod2ind2 <- function(){}

mod2 <- '

y_star =~ l1*y2w1 + l2*y2w2
y_star ~ y_star_mean*1
y_star ~~ y_star_var*y_star

## LIR means
y2w1 ~ mean1*1
y2w2 ~ mean2*1
## LIR (co)variances
y2w1 ~~ var1*y2w1 +0*y2w2
y2w2 ~~ var2*y2w2
## thresholds link LIRs to observed items
y2w1 | thr1*t1
y2w2 | thr2*t1
'
constr2z <- '
## Wave 1
mean1 == 0 ; var1 == 1
## Wave 2
mean2 == 0 ; var2 == 1
'
Y <- cbind(dat$y2w1,dat$y2w2)
prop <- colSums(Y)/nrow(Y)
th <- qnorm(1-prop)
th

fit2z <- lavaan(mod2, data = dat, constraints = constr2z,
                int.ov.free =TRUE, int.lv.free=TRUE,
                ordered = c("y2w1","y2w2"),
                parameterization = "theta")

qnorm(1-prop[1],mean = lavInspect(fit2z,what = "est")$nu[1], sd = sqrt(lavInspect(fit2z,what = "est")$theta[1,1]))
qnorm(1-prop[1],mean = lavInspect(fit2z,what = "est")$lambda[1]*lavInspect(fit2z,what = "est")$alpha, 
      sd = sqrt(lavInspect(fit2z,what = "est")$lambda[1]*lavInspect(fit2z,what = "est")$lambda[1]*lavInspect(fit2z,what = "est")$psi +lavInspect(fit2z,what = "est")$theta[1,1]))


qnorm(1-prop[2],mean = lavInspect(fit2z,what = "est")$nu[1], sd = sqrt(lavInspect(fit2z,what = "est")$theta[1,1]))

qnorm(1-prop[2],mean = lavInspect(fit2z,what = "est")$lambda[2]*lavInspect(fit2z,what = "est")$alpha, 
      sd = sqrt(lavInspect(fit2z,what = "est")$lambda[2]*lavInspect(fit2z,what = "est")$lambda[2]*lavInspect(fit2z,what = "est")$psi +lavInspect(fit2z,what = "est")$theta[2,2]))

summary(fit2z)
fitMeasures(fit2z)[c('tli',"cfi","rmsea")]
lavInspect(fit2z,what = "vcov")
varTable(fit2z)
lavParTable(mod2,
            parameterization = "theta")
inspect(fit2z) #free
inspect(fit2z,"est") #estimated


# Specify Model Parameters for Binary Items
# 3 indicators mean zero
mod2ind3meanzero <- function(){}
mod2ind3 <- '
## LIR means
y2w1 ~ mean1*1
y2w2 ~ mean2*1
y2w3 ~ mean3*1
## LIR (co)variances
y2w1 ~~ var1*y2w1 + 0*y2w2 + 0*y2w3 
y2w2 ~~ var2*y2w2 + 0*y2w3
y2w3 ~~ var3*y2w3
## thresholds link LIRs to observed items
y2w1 | thr1*t1
y2w2 | thr2*t1
y2w3 | thr3*t1
'
constr2z <- '
## Wave 1
mean1 == 0 ; var1 == 1
## Wave 2
mean2 == 0 ; var2 == 1
## Wave 3
mean3 == 0 ; var3 == 1
'
mod2ind3.fit <- lavaan(mod2ind3, data = dat, constraints = constr2z,
                ordered = c("y2w1","y2w2","y2w3"),
                parameterization = "theta")

lavTables(mod2ind3.fit) 
summary(mod2ind3.fit)
fitMeasures(mod2ind3.fit)[c('tli',"cfi","rmsea")]
lavInspect(mod2ind3.fit,what = "vcov")
lavInspect(mod2ind3.fit,what = "est")

# Specify Model Parameters for numerical itens
# 3 indicators with latent variables. 
mod2ind3lv1<- function(){}

mod2ind3lv1 <- '

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

datn <- dat
names(dat)
datn <- datn[,c("wave1","wave2","wave3","wave4")]
colnames(datn)<-c("y2w1","y2w2","y2w3","y2w4")
datn 
rm(mod2ind3lv1.fit)
mod2ind3lv1.fit <- lavaan(mod2ind3lv1, 
                          data = datn, 
                          std.lv = TRUE,
                          meanstructure =TRUE,
                          parameterization = "theta")

lavParTable(mod2ind3lv1,
            std.lv = TRUE,
            meanstructure =TRUE,
            parameterization = "theta")


summary(mod2ind3lv1.fit)
fitMeasures(mod2ind3lv1.fit)[c("df",'tli',"cfi","rmsea")]
vcov <- lavInspect(mod2ind3lv1.fit,what = "vcov")
lavInspect(mod2ind3lv1.fit,what = "est")
eigen(vcov)



# Specify Model Parameters for Binary Items
# 3 indicators. Tau set to zero
mod2ind3tauzero <- function(){}

mod2ind3 <- '
## LIR means
y2w1 ~ mean1*1
y2w2 ~ mean2*1
y2w3 ~ mean3*1
## LIR (co)variances
y2w1 ~~ var1*y2w1 
y2w2 ~~ var2*y2w2
y2w3 ~~ var3*y2w3
## thresholds link LIRs to observed items
y2w1 | thr1*t1
y2w2 | thr2*t1
y2w3 | thr3*t1
'
constr2z <- '
## Wave 1
thr1 == 0 ; var1 == 1
## Wave 2
thr2 == 0 ; var2 == 1
## Wave 3
thr3 == 0 ; var3 == 1
'
mod2ind3.fit <- lavaan(mod2ind3, 
                       data = dat, 
                       constraints = constr2z,
                       ordered = c("y2w1","y2w2","y2w3"),
                       parameterization = "theta")

summary(mod2ind3.fit)
fitMeasures(mod2ind3.fit)[c('tli',"cfi","rmsea")]
lavInspect(mod2ind3.fit,what = "vcov")

mod2ind4 <- function(){}

# Specify Model Parameters for Binary Items
# 4 indicators. Tau set as simulated

mod2ind4 <- '
## LIR means
y2w1 ~ mean1*1
y2w2 ~ mean2*1
y2w3 ~ mean3*1
y2w4 ~ mean4*1
## LIR (co)variances
y2w1 ~~ var1*y2w1 
y2w2 ~~ var2*y2w2
y2w3 ~~ var3*y2w3
y2w4 ~~ var4*y2w4
## thresholds link LIRs to observed items
y2w1 | thr1*t1
y2w2 | thr2*t1
y2w3 | thr3*t1
y2w4 | thr4*t1
'
constr2z <- '
## Wave 1
thr1 == -0.5 ; var1 == 1
## Wave 2
thr2 == -0.5 ; var2 == 1
## Wave 3
thr3 == 0.5 ; var3 == 1
## Wave 4
thr4 == 1 ; var4 == 1
'
mod2ind4.fit <- lavaan(mod2ind4, 
                       data = dat, 
                       constraints = constr2z,
                       ordered = c("y2w1","y2w2","y2w3",'y2w4'),
                       parameterization = "theta")

summary(mod2ind4.fit)
fitMeasures(mod2ind4.fit)[c('tli',"cfi","rmsea")]
lavInspect(mod2ind4.fit,what = "vcov")

# Specify Model Parameters for Binary Items
# 3 indicators with latent variables. DF =-2 
mod2ind3lv1<- function(){}

mod2ind3lv1 <- '

lv1 =~ l1*y2w1 + l2*y2w2 + l3*y2w3 

lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1

## LIR means
y2w1 ~ mean1*1
y2w2 ~ mean2*1
y2w3 ~ mean3*1
## LIR (co)variances
y2w1 ~~ var1*y2w1 + 0*y2w2 + 0*y2w3
y2w2 ~~ var2*y2w2 + 0*y2w3
y2w3 ~~ var3*y2w3
## thresholds link LIRs to observed items
y2w1 | thr1*t1
y2w2 | thr2*t1
y2w3 | thr3*t1
'
constr2z <- '
## Wave 1
thr1 == 0.5 ; var1 == 1
## Wave 2
thr2 == 0.5 ; var2 == 1
## Wave 3
thr3 == 0.5 ; var3 == 1

'

datn <- dat
names(dat)
datn <- datn[,c("wave1","wave2","wave3","wave4")]
colnames(datn)<-c("y2w1","y2w2","y2w3","y2w4")
datn 
rm(mod2ind3lv1.fit)
mod2ind3lv1.fit <- lavaan(mod2ind3lv1, 
                           data = dat, 
                           constraints = constr2z,
                           ordered = c("y2w1","y2w2","y2w3"),
                          meanstructure =TRUE,
                          parameterization = "theta")

lavParTable(mod2ind3lv1,
            constraints = constr2z,
            meanstructure =TRUE,
            parameterization = "theta")


summary(mod2ind3lv1.fit)
fitMeasures(mod2ind3lv1.fit)['tli']
fitMeasures(mod2ind3lv1.fit)['cfi']
fitMeasures(mod2ind3lv1.fit)['rmsea']
vcov <- lavInspect(mod2ind3lv1.fit,what = "vcov")
eigen(vcov)

mod2ind4lv1 <- function(){}
# Specify Model Parameters for Binary Items
# 4 indicators with latent variables. DF =1 

mod2ind4lv1 <- '

lv1 =~ lmbd1*y2w1 + lmbd2*y2w2 + lmbd3*y2w3 +lmbd4*y2w4

lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1

## LIR means
y2w1 ~ mean1*1
y2w2 ~ mean2*1
y2w3 ~ mean3*1
y2w4 ~ mean4*1
## LIR (co)variances
y2w1 ~~ var1*y2w1 
y2w2 ~~ var2*y2w2
y2w3 ~~ var3*y2w3
y2w4 ~~ var4*y2w4
## thresholds link LIRs to observed items
y2w1 | thr1*t1
y2w2 | thr2*t1
y2w3 | thr3*t1
y2w4 | thr4*t1
'
constr2z <- '
## Wave 1
thr1 == -0.5 ; var1 == 1
## Wave 2
thr2 == -0.5 ; var2 == 1
## Wave 3
thr3 == 0.5 ; var3 == 1
## Wave 4
thr4 == 1.0 ; var4 == 1
'

mod2ind4lv1.fit <- lavaan(mod2ind4lv1, 
                          data = dat, 
                          constraints = constr2z,
                          ordered = c("y2w1","y2w2","y2w3","y2w4"),
                          parameterization = "theta")

lavParTable(mod2ind4lv1, 
            constraints = constr2z,
            parameterization = "theta")

summary(mod2ind4lv1.fit)
fitMeasures(mod2ind4lv1.fit)['tli']
fitMeasures(mod2ind4lv1.fit)['cfi']
fitMeasures(mod2ind4lv1.fit)['rmsea']
vcov <- lavInspect(mod2ind4lv1.fit,what = "vcov")
eigen(vcov)$values
resid(mod2ind4lv1.fit, type = "cor")
View(resid(mod2ind4lv1.fit, type = "cor")$cov)

mod2ind4lv1_parametros <- function(){}
mod2ind4lv1.fit <- lavaan(mod2ind4lv1, 
                          data = dat, 
                          int.ov.free = TRUE,
                          int.lv.free = TRUE,
                          meanstructure = TRUE,
                          std.lv =FALSE,
                          constraints = constr2z,
                          ordered = c("y2w1","y2w2","y2w3"),
                          parameterization = "theta")

lavParTable(mod2ind4lv1, 
            int.ov.free = TRUE,
            int.lv.free = TRUE,
            meanstructure = TRUE,
            std.lv =TRUE,
            constraints = constr2z,
            parameterization = "theta")
parTable(mod2ind4lv1.fit)

lavop <- lavInspect(mod2ind4lv1.fit, "options")
summary(mod2ind4lv1.fit)
fitMeasures(mod2ind4lv1.fit)['tli']
fitMeasures(mod2ind4lv1.fit)['cfi']
fitMeasures(mod2ind4lv1.fit)['rmsea']

# Fix Thresholds to ZERO = 0 Instead
# Notice the estimated means are the same magnitude 
# (but opposite sign) as the previously estimated thresholds

constr2t <- '
## Wave 1
thr1 == 0 ; var1 == 1
## Wave 2
thr2 == 0 ; var2 == 1
'
fit2t <- lavaan(mod2, data = dat, constraints = constr2t,
                ordered = c("y2w1","y2w2"),
                parameterization = "theta")
summary(fit2t)

lavInspect(fit2t,what = "partable")
parameterTable(fit2t)

