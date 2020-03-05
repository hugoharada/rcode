rm(list=ls())

if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 
if(!require(ltm)) install.packages("ltm"); library(ltm)
if(!require(pryr)) install.packages("pryr"); library(pryr)


thresholdCalc <- function(){}
set.seed(1) # Resetando a semente

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

N <- 2000 ## subjects
I= 4  # Number of Items
PL=2 # Logistic Model (1,2,3 parameters)
SigmaType <- 3 # 0 = Covariance Uniform, 1 = Covariancia AR1, 2 =  Covariancia de bandas 3 = Covariancia Nula
rho<-0.7

coefs <- matrix(ncol=6,nrow=I)
colnames(coefs)=c("a1","b1","c1","a2","b2","c2")

if (PL==1) {a = rep(1,I)} else {a = runif(I,0.5, 2.5)}    # U(0.5 , 2.5)
b = runif(I,-2, 2.0)     # U(-2 , 2)
if (PL<=2) {c = rep(0,I)} else{c = runif(I,0.0, 0.3) } # U(0 , 0.3)

d=-a*b # MIRT trabalha com o intercepto (d=-ab) e nao com a dificuldade (b)

if(SigmaType==0){
  Sigma <- lazyCor(c(rho,rho,rho)) #Matriz de Covariancia Uniforme
}else if(SigmaType==1){
  Sigma <- lazyCor(c(rho,rho*rho,rho)) #Matriz de Covariancia AR(1)
}else if(SigmaType==2){
  Sigma <- lazyCor(c(rho,0,0)) #Matriz de Covariancia de bandas
}else if(SigmaType==3){
  Sigma <- lazyCor(c(0,0,0)) #Matriz de Covariancia nula
}else{
  Sigma <- NULL
}
Sigma

# mirt                              <-	"mirt" 
# fixed_factor,		delta_marginal 		<-	"ff_dm" 
# fixed_factor, 		theta_conditional	<- 	"ff_tc"
# indicator_marker, 	delta_marginal 		<- 	"im_dm"
# indicator_marker, 	theta_conditional 	<-  "im_tc"
# indicator_effects, 	delta_marginal   	<- 	"ie_dm"
# indicator_effects, 	theta_conditional 	<-	"ie_tc"
# indicator_effects, 	delta_marginal, long   	<- 	"ie_dm_lo"
experiments <- c("sim", "mirt", "ff_dm" ,"ff_tc","im_dm","im_tc","ie_dm","ie_tc")
  

acomp <- matrix(data=rep(NA,length(experiments)*I),ncol = length(experiments),nrow = I)
colnames(acomp) <- experiments
acomp[,"sim"]<-a

dcomp <- matrix(data=rep(NA,length(experiments)*I),ncol = length(experiments),nrow = I)
colnames(dcomp) <- experiments
dcomp[,"sim"]<-d


Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )

head(Theta)
cor(Theta)
var(Theta)
dim(Theta)

mean(Theta[,1])
mean(Theta[,2])
mean(Theta[,3])

var(Theta[,1])
var(Theta[,2])
var(Theta[,3])


dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
dat.0p5 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,2],ncol=1,nrow = length(Theta[,2])))
dat.1p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,3],ncol=1,nrow = length(Theta[,3])))

colnames(dat) <-     paste("item",1:I,sep="")
colnames(dat.0p0) <- paste("item",1:I,sep="")
colnames(dat.0p5) <- paste("item",1:I,sep="")
colnames(dat.1p0) <- paste("item",1:I,sep="")

#data selection
dat <- dat.0p0
#dat <- dat.0p5
#dat <- dat.1p0
alpha <- ltm::cronbach.alpha(dat)
print(alpha)


#expected thresholds
prop <- colSums(dat)/nrow(dat)
prop
th <- qnorm(1-prop)
th

loopn <-200
coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
  dat <- dat.0p0
  colnames(dat) <- paste("item",1:I,sep="")
  mod<-mirt(data=dat,model = 1,itemtype = "2PL")
  M2(mod)
  itemfit(mod)
  coef(mod, simplify=TRUE)
  coef_a[i,] <- coef(mod, simplify=TRUE)$item[,"a1"]
  coef_d[i,] <- coef(mod, simplify=TRUE)$item[,"d"]
}

acomp[,"mirt"]<-colMeans(coef_a)
dcomp[,"mirt"]<-colMeans(coef_d)

acomp
dcomp

# Specify Model Parameters for numerical itens
# 3 indicators with latent variables. 

mod2ind3lv1.fixed_factor.delta_marginal.simple<- function(){}

mod2ind3lv1.fixed_factor.delta_marginal <- '

lv1 =~ l1*item1 + l2*item2 + l3*item3 + l4*item4

'

sem.model <- mod2ind3lv1.fixed_factor.delta_marginal

rm(mod2ind3lv1.fit)

mod2ind3lv1.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = TRUE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =FALSE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = c("item1","item2","item3","item4"),
                          parameterization = "delta")


summary(mod2ind3lv1.fit)
fitMeasures(mod2ind3lv1.fit)[c("npar","df",'tli',"cfi","rmsea")]

parTable(mod2ind3lv1.fit)
lavInspect(mod2ind3lv1.fit,what = "partable")
lavInspect(mod2ind3lv1.fit,what = "est")
lavInspect(mod2ind3lv1.fit,what = "start")
vcov(mod2ind3lv1.fit)

#check y* stats
lavInspect(mod2ind3lv1.fit,what = "mu")
lavInspect(mod2ind3lv1.fit,what = "vy")



loopn <-200
coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
  dat <- dat.0p0
  colnames(dat) <- paste("item",1:I,sep="")
  
  mod2ind3lv1.fixed_factor.delta_marginal <- '
    lv1 =~ l1*item1 + l2*item2 + l3*item3 + l4*item4
    '
  sem.model <- mod2ind3lv1.fixed_factor.delta_marginal
  
  rm(mod2ind3lv1.fit)
  
  mod2ind3lv1.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = TRUE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =FALSE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = c("item1","item2","item3","item4"),
                            parameterization = "delta")
  

  for(j in (1:I)){
    lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi
    tau <- lavInspect(mod2ind3lv1.fit,what = "est")$tau[j]
    mu <- lavInspect(mod2ind3lv1.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda/sqrt(1-lambda^2)*1.7
    coef_d[i,j] <-  -tau/sqrt(1-lambda^2)*1.7
  }
  
}
acomp[,"ff_dm"]<-colMeans(coef_a)
dcomp[,"ff_dm"]<-colMeans(coef_d)
acomp
dcomp


mod2ind3lv1.fixed_factor.delta_marginal<- function(){}

mod2ind3lv1.fixed_factor.delta_marginal <- '

lv1 =~ l1*item1 + l2*item2 + l3*item3
lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1
lv1mean==0

## LIR means
item1 ~ int1*1
item2 ~ int2*1
item3 ~ int3*1

int1+l1*lv1mean==0
int2+l2*lv1mean==0
int3+l3*lv1mean==0

## thresholds link LIRs to observed items
item1 | thr1*t1
item2 | thr2*t1
item3 | thr3*t1
#thr1+thr2+thr3 ==0

## LIR (co)variances
item1 ~~ var1*item1 + 0*item2 + 0*item3
item2 ~~ var2*item2 + 0*item3
item3 ~~ var3*item3
lv1var*l1^2 + var1 == 1
lv1var*l2^2 + var2 == 1
lv1var*l3^2 + var3 == 1

'

sem.model <- mod2ind3lv1.fixed_factor.delta_marginal

rm(mod2ind3lv1.fit)


mod2ind3lv1.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = TRUE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = c("item1","item2","item3"),
                          parameterization = "delta")


summary(mod2ind3lv1.fit)
fitMeasures(mod2ind3lv1.fit)[c("npar","df",'tli',"cfi","rmsea")]

parTable(mod2ind3lv1.fit)
lavInspect(mod2ind3lv1.fit,what = "free")
lavInspect(mod2ind3lv1.fit,what = "partable")
lavInspect(mod2ind3lv1.fit,what = "est")
lavInspect(mod2ind3lv1.fit,what = "start")
vcov(mod2ind3lv1.fit)

#check y* stats
lavInspect(mod2ind3lv1.fit,what = "mu")
lavInspect(mod2ind3lv1.fit,what = "vy")



loopn <-200
coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
  dat <- dat.0p0
  colnames(dat) <- paste("item",1:I,sep="")
  mod2ind3lv1.fixed_factor.delta_marginal <- '
  
  lv1 =~ l1*item1 + l2*item2 + l3*item3 + l4*item4
  lv1 ~ lv1mean*1
  lv1 ~~ lv1var*lv1
  lv1mean==0
  
  ## LIR means
  item1 ~ int1*1
  item2 ~ int2*1
  item3 ~ int3*1
  item4 ~ int4*1
  
  int1+l1*lv1mean==0
  int2+l2*lv1mean==0
  int3+l3*lv1mean==0
  int4+l4*lv1mean==0
  
  ## thresholds link LIRs to observed items
  item1 | thr1*t1
  item2 | thr2*t1
  item3 | thr3*t1
  item4 | thr4*t1
  #thr1+thr2+thr3 ==0
  
  ## LIR (co)variances
  item1 ~~ var1*item1 + 0*item2 + 0*item3 + 0*item4
  item2 ~~ var2*item2 + 0*item3 + 0*item4
  item3 ~~ var3*item3 + 0*item4
  item4 ~~ var4*item4
  lv1var*l1^2 + var1 == 1
  lv1var*l2^2 + var2 == 1
  lv1var*l3^2 + var3 == 1
  lv1var*l3^2 + var4 == 1
  
  '
  
  sem.model <- mod2ind3lv1.fixed_factor.delta_marginal
  
  rm(mod2ind3lv1.fit)
  
  mod2ind3lv1.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = TRUE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = c("item1","item2","item3","item4"),
                            parameterization = "delta")
  
  
  for(j in (1:I)){
    lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi
    tau <- lavInspect(mod2ind3lv1.fit,what = "est")$tau[j]
    mu <- lavInspect(mod2ind3lv1.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda/sqrt(1-lambda^2)*1.7
    coef_d[i,j] <-  -tau/sqrt(1-lambda^2)*1.7
  }
  
}
acomp[,"ff_dm"]<-colMeans(coef_a)
dcomp[,"ff_dm"]<-colMeans(coef_d)
acomp
dcomp

mod2ind3lv1.fixed_factor.theta_conditional<- function(){}

mod2ind3lv1.fixed_factor.theta_conditional <- '

  lv1 =~ l1*item1 + l2*item2 + l3*item3
  lv1 ~ lv1mean*1
  lv1 ~~ lv1var*lv1
  lv1mean==0
  
  ## LIR means
  item1 ~ int1*1
  item2 ~ int2*1
  item3 ~ int3*1
  
  int1+l1*lv1mean==0
  int2+l2*lv1mean==0
  int3+l3*lv1mean==0
  
  ## thresholds link LIRs to observed items
  item1 | thr1*t1
  item2 | thr2*t1
  item3 | thr3*t1
  #thr1+thr2+thr3 ==0
  
  ## LIR (co)variances
  item1 ~~ var1*item1 + 0*item2 + 0*item3
  item2 ~~ var2*item2 + 0*item3
  item3 ~~ var3*item3
  var1 == 1
  var2 == 1
  var3 == 1

'

sem.model <- mod2ind3lv1.fixed_factor.theta_conditional
rm(mod2ind3lv1.fit)
mod2ind3lv1.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = TRUE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = c("item1","item2","item3"),
                          parameterization = "theta")

summary(mod2ind3lv1.fit)
fitMeasures(mod2ind3lv1.fit)[c("df",'tli',"cfi","rmsea")]


loopn <-200
coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
  dat <- dat.0p0
  colnames(dat) <- paste("item",1:I,sep="")
  mod2ind3lv1.fixed_factor.delta_marginal <- '
  
  lv1 =~ l1*item1 + l2*item2 + l3*item3 + l4*item4
  lv1 ~ lv1mean*1
  lv1 ~~ lv1var*lv1
  lv1mean==0
  
  ## LIR means
  item1 ~ int1*1
  item2 ~ int2*1
  item3 ~ int3*1
  item4 ~ int4*1
  
  int1+l1*lv1mean==0
  int2+l2*lv1mean==0
  int3+l3*lv1mean==0
  int4+l4*lv1mean==0
  
  ## thresholds link LIRs to observed items
  item1 | thr1*t1
  item2 | thr2*t1
  item3 | thr3*t1
  item4 | thr4*t1
  #thr1+thr2+thr3 ==0
  
  ## LIR (co)variances
  item1 ~~ var1*item1 + 0*item2 + 0*item3 + 0*item4
  item2 ~~ var2*item2 + 0*item3 + 0*item4
  item3 ~~ var3*item3 + 0*item4
  item4 ~~ var4*item4
  var1 == 1
  var2 == 1
  var3 == 1
  var4 == 1
  
  '
  
  sem.model <- mod2ind3lv1.fixed_factor.delta_marginal
  
  rm(mod2ind3lv1.fit)
  
  mod2ind3lv1.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = TRUE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = c("item1","item2","item3","item4"),
                            parameterization = "theta")
  
  
  for(j in (1:I)){
    lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi
    tau <- lavInspect(mod2ind3lv1.fit,what = "est")$tau[j]
    mu <- lavInspect(mod2ind3lv1.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*1.7
    coef_d[i,j] <-  -(tau)*1.7
  }
  
}
acomp[,"ff_tc"]<-colMeans(coef_a)
dcomp[,"ff_tc"]<-colMeans(coef_d)
acomp
dcomp


mod2ind3lv1.indicator_effects.delta_marginal<- function(){}

mod2ind3lv1.indicator_effects.delta_marginal <- '

lv1 =~ l1*item1 + l2*item2 + l3*item3 
l1 + l2 + l3 == 3 

lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1


## LIR means
item1 ~ int1*1
item2 ~ int2*1
item3 ~ int3*1
int1+int2+int3==0
int1+l1*lv1mean==0
int2+l2*lv1mean==0
int3+l3*lv1mean==0

## thresholds link LIRs to observed items
item1 | thr1*t1
item2 | thr2*t1
item3 | thr3*t1
#thr1 + thr2 +thr3 ==0

## LIR (co)variances
item1 ~~ var1*item1 + 0*item2 + 0*item3
item2 ~~ var2*item2 + 0*item3
item3 ~~ var3*item3
lv1var*l1^2 + var1 == 1
lv1var*l2^2 + var2 == 1
lv1var*l3^2 + var3 == 1
'

sem.model <- mod2ind3lv1.indicator_effects.delta_marginal
rm(mod2ind3lv1.fit)
mod2ind3lv1.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = FALSE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = c("item1","item2","item3"),
                          parameterization = "delta")


summary(mod2ind3lv1.fit)
fitMeasures(mod2ind3lv1.fit)[c("df",'tli',"cfi","rmsea")]

parTable(mod2ind3lv1.fit)
lavInspect(mod2ind3lv1.fit,what = "free")
lavInspect(mod2ind3lv1.fit,what = "partable")
lavInspect(mod2ind3lv1.fit,what = "est")
lavInspect(mod2ind3lv1.fit,what = "start")
vcov(mod2ind3lv1.fit)

#check y* stats
lavInspect(mod2ind3lv1.fit,what = "mu")
lavInspect(mod2ind3lv1.fit,what = "vy")

loopn <-200
coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
  dat <- dat.0p0
  colnames(dat) <- paste("item",1:I,sep="")
  mod2ind3lv1.indicator_effects.delta_marginal <- '
  
  lv1 =~ l1*item1 + l2*item2 + l3*item3 + l4*item4
  lv1 ~ lv1mean*1
  lv1 ~~ lv1var*lv1

  ## LIR means
  item1 ~ int1*1
  item2 ~ int2*1
  item3 ~ int3*1
  item4 ~ int4*1
  int1+int2+int3+int4==0
  int1+l1*lv1mean==0
  int2+l2*lv1mean==0
  int3+l3*lv1mean==0
  int4+l4*lv1mean==0
  
  ## thresholds link LIRs to observed items
  item1 | thr1*t1
  item2 | thr2*t1
  item3 | thr3*t1
  item4 | thr4*t1
  #thr1+thr2+thr3 ==0
  
  ## LIR (co)variances
  item1 ~~ var1*item1 + 0*item2 + 0*item3 + 0*item4
  item2 ~~ var2*item2 + 0*item3 + 0*item4
  item3 ~~ var3*item3 + 0*item4
  item4 ~~ var4*item4
  lv1var*l1^2 + var1 == 1
  lv1var*l2^2 + var2 == 1
  lv1var*l3^2 + var3 == 1
  lv1var*l3^2 + var4 == 1
  
  '
  
  sem.model <- mod2ind3lv1.indicator_effects.delta_marginal
  
  rm(mod2ind3lv1.fit)
  
  mod2ind3lv1.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = TRUE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = c("item1","item2","item3","item4"),
                            parameterization = "delta")
  
  
  for(j in (1:I)){
    lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi
    tau <- lavInspect(mod2ind3lv1.fit,what = "est")$tau[j]
    mu <- lavInspect(mod2ind3lv1.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*sqrt(psi)*1.7 / sqrt( 1 - psi*lambda^2)
    coef_d[i,j] <-   -(tau - lambda*mu)*1.7 / sqrt( 1 - psi*lambda^2)
  }
  
}
acomp[,"ie_dm"]<-colMeans(coef_a)
dcomp[,"ie_dm"]<-colMeans(coef_d)
acomp
dcomp


mod2ind3lv1.indicator_effects.theta_conditional<- function(){}

mod2ind3lv1.indicator_effects.theta_conditional <- '

lv1 =~ l1*item1 + l2*item2 + l3*item3 
l1 + l2 + l3 == 3 

lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1


## LIR means
item1 ~ int1*1
item2 ~ int2*1
item3 ~ int3*1
int1+int2+int3==0
int1+l1*lv1mean==0
int2+l2*lv1mean==0
int3+l3*lv1mean==0

## thresholds link LIRs to observed items
item1 | thr1*t1
item2 | thr2*t1
item3 | thr3*t1
#thr1 + thr2 +thr3 ==0

## LIR (co)variances
item1 ~~ var1*item1 + 0*item2 + 0*item3
item2 ~~ var2*item2 + 0*item3
item3 ~~ var3*item3
var1 == 1
var2 == 1
var3 == 1
'

sem.model <- mod2ind3lv1.indicator_effects.theta_conditional
rm(mod2ind3lv1.fit)
mod2ind3lv1.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = FALSE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = c("item1","item2","item3"),
                          parameterization = "theta")


summary(mod2ind3lv1.fit)
fitMeasures(mod2ind3lv1.fit)[c("df",'tli',"cfi","rmsea")]

summary(mod2ind3lv1.fit)
fitMeasures(mod2ind3lv1.fit)[c("df",'tli',"cfi","rmsea")]

parTable(mod2ind3lv1.fit)
lavInspect(mod2ind3lv1.fit,what = "free")
lavInspect(mod2ind3lv1.fit,what = "partable")
lavInspect(mod2ind3lv1.fit,what = "est")
lavInspect(mod2ind3lv1.fit,what = "start")
vcov(mod2ind3lv1.fit)

#check y* stats
lavInspect(mod2ind3lv1.fit,what = "mu")
lavInspect(mod2ind3lv1.fit,what = "vy")

loopn <-200
coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
  dat <- dat.0p0
  colnames(dat) <- paste("item",1:I,sep="")
  mod2ind3lv1.indicator_effects.theta_conditional <- '
  
  lv1 =~ l1*item1 + l2*item2 + l3*item3 + l4*item4
  l1 + l2 + l3 +l4 == 4 

  lv1 ~ lv1mean*1
  lv1 ~~ lv1var*lv1

  ## LIR means
  item1 ~ int1*1
  item2 ~ int2*1
  item3 ~ int3*1
  item4 ~ int4*1
  int1+int2+int3+int4==0
  int1+l1*lv1mean==0
  int2+l2*lv1mean==0
  int3+l3*lv1mean==0
  int4+l4*lv1mean==0
  
  ## thresholds link LIRs to observed items
  item1 | thr1*t1
  item2 | thr2*t1
  item3 | thr3*t1
  item4 | thr4*t1
  #thr1+thr2+thr3 ==0
  
  ## LIR (co)variances
  item1 ~~ var1*item1 + 0*item2 + 0*item3 + 0*item4
  item2 ~~ var2*item2 + 0*item3 + 0*item4
  item3 ~~ var3*item3 + 0*item4
  item4 ~~ var4*item4
  var1 == 1
  var2 == 1
  var3 == 1
  var4 == 1  
  '
  
  sem.model <- mod2ind3lv1.indicator_effects.theta_conditional
  
  rm(mod2ind3lv1.fit)
  
  mod2ind3lv1.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = FALSE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = c("item1","item2","item3","item4"),
                            parameterization = "theta")
  
  
  for(j in (1:I)){
    lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi
    tau <- lavInspect(mod2ind3lv1.fit,what = "est")$tau[j]
    mu <- lavInspect(mod2ind3lv1.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*sqrt(psi)*1.7
    coef_d[i,j] <- -(tau - lambda*mu)*1.7
  }
  
}
acomp[,"ie_tc"]<-colMeans(coef_a)
dcomp[,"ie_tc"]<-colMeans(coef_d)
acomp
dcomp

mod2ind3lv1.long.indicator_effects.delta_marginal<- function(){}

mod2ind3lv1.long.indicator_effects.delta_marginal <- '

lv1 =~ l1*item1 + l2*item2 + l3*item3 
lv2 =~ l4*item4 + l5*item5 + l6*item6 
l1 + l2 + l3 == 3 


lv1 ~ lv1mean*1
lv2 ~ lv2mean*1

lv1 ~~ lv1var*lv1
lv2 ~~ lv2var*lv2

lv2~lv1


## LIR means
item1 ~ int1*1
item2 ~ int2*1
item3 ~ int3*1

item4 ~ int4*1
item5 ~ int5*1
item6 ~ int6*1

#int1+int2+int3==0

int1+l1*lv1mean==0
int2+l2*lv1mean==0
int3+l3*lv1mean==0

int4+l4*lv2mean==0
int5+l5*lv2mean==0
int6+l6*lv2mean==0



## thresholds link LIRs to observed items
item1 | thr1*t1
item2 | thr2*t1
item3 | thr3*t1

item4 | thr4*t1
item5 | thr5*t1
item6 | thr6*t1


## LIR (co)variances
item1 ~~ var1*item1 +    0*item2 +    0*item3 + var14*item4 +     0*item5 +     0*item6
item2 ~~              var2*item2 +    0*item3 +     0*item4 + var25*item5 +     0*item6
item3 ~~                           var3*item3 +     0*item4 +     0*item5 + var36*item6
item4 ~~                                         var4*item4 +     0*item5 +     0*item6
item5 ~~                                                       var5*item5 +     0*item6
item6 ~~                                                                     var6*item6

lv1var*l1^2 + var1 == 1
lv1var*l2^2 + var2 == 1
lv1var*l3^2 + var3 == 1

lv2var*l4^2 + var4 == 1
lv2var*l5^2 + var5 == 1
lv2var*l6^2 + var6 == 1


l1==l4
l2==l5
l3==l6

int1==int4
int2==int5
int3==int6

thr1==thr4
thr2==thr5
thr3==thr6

item1mean := int1+l1*lv1mean
item2mean := int2+l2*lv1mean
item3mean := int3+l3*lv1mean

item4mean := int1+l4*lv2mean
item5mean := int2+l5*lv2mean
item6mean := int3+l6*lv2mean


item1var := lv1var*l1^2 + var1 
item2var := lv1var*l2^2 + var2 
item3var := lv1var*l3^2 + var3 

item4var := lv2var*l4^2 + var4 
item5var := lv2var*l5^2 + var5 
item6var := lv2var*l6^2 + var6 



'

dat<- cbind(dat.0p0[,1:3],dat.0p5[,1:3])
head(dat)
colnames(dat)<- paste("item",1:6,sep="")
itemnames <- paste("item",1:6,sep="")

sem.model <- mod2ind3lv1.long.indicator_effects.delta_marginal
rm(mod2ind3lv1.fit)
mod2ind3lv1.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = FALSE,
                          int.ov.free = TRUE,
                          int.lv.free = TRUE, #<- set to TRUE for longitudinal models
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = itemnames,
                          parameterization = "delta")


summary(mod2ind3lv1.fit)
fitMeasures(mod2ind3lv1.fit)[c("df",'tli',"cfi","rmsea")]

parTable(mod2ind3lv1.fit)
lavInspect(mod2ind3lv1.fit,what = "free")
lavInspect(mod2ind3lv1.fit,what = "partable")
lavInspect(mod2ind3lv1.fit,what = "est")
lavInspect(mod2ind3lv1.fit,what = "start")
vcov(mod2ind3lv1.fit)

#check y* stats
lavInspect(mod2ind3lv1.fit,what = "mu")
lavInspect(mod2ind3lv1.fit,what = "vy")

#check eta stats
lavInspect(mod2ind3lv1.fit,what = "est")$alpha
lavInspect(mod2ind3lv1.fit,what = "est")$psi


loopn <-200
coef_a <- matrix(data=NA, nrow = loopn, ncol = 6)
colnames(coef_a) <- paste("item",1:6,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = 6)
colnames(coef_d) <- paste("item",1:6,sep="")

for(i in 1:loopn){
  Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
  dat.0p5 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,2],ncol=1,nrow = length(Theta[,2])))
  dat<- cbind(dat.0p0[,1:3],dat.0p5[,1:3])
  head(dat)
  colnames(dat)<- paste("item",1:6,sep="")
  itemnames <- paste("item",1:6,sep="")
  rm(mod2ind3lv1.fit)
  mod2ind3lv1.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = FALSE,
                            int.ov.free = TRUE,
                            int.lv.free = TRUE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = itemnames,
                            parameterization = "delta")
  
  for(j in (1:6)){
    if(j<4){
      lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j,1]
      psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi[1,1]
    }else{
      lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j,2]
      psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi[2,2]
    }
    tau <- lavInspect(mod2ind3lv1.fit,what = "est")$tau[j]
    mu <- lavInspect(mod2ind3lv1.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*sqrt(psi)*1.7 / sqrt( 1 - psi*lambda^2)
    coef_d[i,j] <-   -(tau - lambda*mu)*1.7 / sqrt( 1 - psi*lambda^2)
  }
  
}

along <-colMeans(coef_a)
dlong <-colMeans(coef_d)
along
c(a[1:3],a[1:3])
dlong
c(d[1:3],d[1:3])

