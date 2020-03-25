rm(list=ls())

if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 
if(!require(ltm)) install.packages("ltm"); library(ltm)
if(!require(pryr)) install.packages("pryr"); library(pryr)
if(!require(cowplot)) install.packages("cowplot"); library(cowplot)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(directlabels)) install.packages("directlabels"); library(directlabels)
if(!require(gridExtra)) install.packages("gridExtra"); library(gridExtra)
if(!require(gtable)) install.packages("gtable"); library(gtable)
if(!require(ltm)) install.packages("ltm"); library(ltm)
if(!require(readxl)) install.packages("readxl"); library(readxl)
if(!require(tidyr)) install.packages("tidyr"); library(tidyr)
if(!require(stringr)) install.packages("stringr"); library(stringr)

if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

get_time<-function(){
  return(Sys.time())
}
get_delta_time<-function(start_time){
  return(Sys.time()-start_time)
}


thresholdCalc <- function(){}
set.seed(12) # Resetando a semente

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
loopn <-10

# N <- 10000 ## subjects
# loopn <-1000


I= 15  # Number of Items
PL=2 # Logistic Model (1,2,3 parameters)
SigmaType <- 1 # 0 = Covariance Uniform, 1 = Covariancia AR1, 2 =  Covariancia de bandas 3 = Covariancia Nula
rho<-0.7

coefs <- matrix(ncol=6,nrow=I)
colnames(coefs)=c("a1","b1","c1","a2","b2","c2")


#par_type <- 1 #random
#par_type <- 2 #a and b varying.
par_type <- 3 #a=1, b varying
if(par_type==1){
  if (PL==1) {a = rep(1,I)} else {a = runif(I,0.5, 2.5)}    # U(0.5 , 2.5)
  b = runif(I,-2, 2.0)     # U(-2 , 2)
  if (PL<=2) {c = rep(0,I)} else{c = runif(I,0.0, 0.3) } # U(0 , 0.3)
}else if(par_type==2){
  a <- rep(1,13)
  b <- seq(-3.0,3.0,by=0.5)
}else if(par_type==3){
  a <- c(rep(seq(0.75,1.5,by=0.25),3),0.75)
  b <- seq(-3,3,by=0.5)
}
d=-a*b # MIRT trabalha com o intercepto (d=-ab) e nao com a dificuldade (b)
pars <- cbind(a,b,d)

plot_data <- cbind(sample_p(a=pars[1,"a"],b=pars[1,"b"],c=0),1)
for(i in 1:nrow(pars)){
  plot_data <- rbind(plot_data, cbind(sample_p(a=pars[i,"a"],b=pars[i,"b"],c=0),i))
}
colnames(plot_data)[4]<-"item"
ggplot(data=data.frame(plot_data),aes(x=theta_n,y=p,group=item,color=item))+geom_line()
ggplot(data=data.frame(plot_data),aes(x=theta_n,y=i,group=item,color=item))+geom_line()


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
  

coef_a_mean <- matrix(data=rep(NA,length(experiments)*I),ncol = length(experiments),nrow = I)
colnames(coef_a_mean) <- experiments
coef_a_mean[,"sim"]<-a
coef_a_sd <-  matrix(data=rep(NA,length(experiments)*I),ncol = length(experiments),nrow = I)
colnames(coef_a_sd) <- experiments


coef_b_mean <- matrix(data=rep(NA,length(experiments)*I),ncol = length(experiments),nrow = I)
colnames(coef_b_mean) <- experiments
coef_b_mean[,"sim"]<-b
coef_b_sd <-  matrix(data=rep(NA,length(experiments)*I),ncol = length(experiments),nrow = I)
colnames(coef_b_sd) <- experiments


coef_d_mean <- matrix(data=rep(NA,length(experiments)*I),ncol = length(experiments),nrow = I)
colnames(coef_d_mean) <- experiments
coef_d_mean[,"sim"]<-d
coef_d_sd <-  matrix(data=rep(NA,length(experiments)*I),ncol = length(experiments),nrow = I)
colnames(coef_d_sd) <- experiments


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

#data selection
dat <- dat.0p0
#dat <- dat.0p5
#dat <- dat.1p0
itemnames <- paste("item",1:I,"_",time_n,sep="")
colnames(dat) <-     itemnames
colnames(dat.0p0) <- itemnames
colnames(dat.0p5) <- itemnames
colnames(dat.1p0) <- itemnames
str(dat)


#expected thresholds
prop <- colSums(dat)/nrow(dat)
prop
th <- qnorm(1-prop)
th

coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
coef_b <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_b) <- paste("item",1:I,sep="")

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
  coef_b[i,] <- -coef_d[i,]/coef_a[i,]
}

coef_a_mean[,"mirt"]<-colMeans(coef_a,na.rm=TRUE)
coef_b_mean[,"mirt"]<-colMeans(coef_b,na.rm=TRUE)
coef_d_mean[,"mirt"]<-colMeans(coef_d,na.rm=TRUE)

coef_a_sd[,"mirt"]<- apply(coef_a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_b_sd[,"mirt"]<- apply(coef_b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_d_sd[,"mirt"]<- apply(coef_d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})

coef_a_mean
coef_b_mean
coef_d_mean

# Specify Model Parameters for numerical itens

# mod2ind3lv1.fixed_factor.delta_marginal.simple<- function(){}
# 
# 
# mod2ind3lv1.fixed_factor.delta_marginal <- '
# 
# eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1
# 
# '
# itemnames <- c( "item1_1","item2_1","item3_1","item4_1","item5_1","item6_1","item7_1","item8_1","item9_1","item10_1")
# colnames(dat)<- itemnames
# 
# sem.model <- mod2ind3lv1.fixed_factor.delta_marginal
# 
# rm(mod2ind3lv1.fit)
# 
# start_time <- get_time()
# mod2ind3lv1.fit <- lavaan(model = sem.model, 
#                           data = dat, 
#                           std.lv = TRUE,
#                           int.ov.free = TRUE,
#                           int.lv.free = FALSE,
#                           meanstructure =FALSE,
#                           auto.fix.first = FALSE,
#                           auto.var = TRUE,
#                           auto.th = TRUE,
#                           auto.delta = TRUE,
#                           auto.cov.y = TRUE,
#                           ordered = itemnames,
#                           parameterization = "delta")
# 
# print(get_delta_time(start_time))
# 
# summary(mod2ind3lv1.fit)
# fitMeasures(mod2ind3lv1.fit)[c("npar","df",'tli',"cfi","rmsea")]
# 
# parTable(mod2ind3lv1.fit)
# lavInspect(mod2ind3lv1.fit,what = "partable")
# lavInspect(mod2ind3lv1.fit,what = "est")
# lavInspect(mod2ind3lv1.fit,what = "start")
# vcov(mod2ind3lv1.fit)
# 
# #check y* stats
# lavInspect(mod2ind3lv1.fit,what = "mu")
# lavInspect(mod2ind3lv1.fit,what = "vy")
# 
# 
# 
# loopn <-200
# coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
# colnames(coef_a) <- paste("item",1:I,sep="")
# coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
# colnames(coef_d) <- paste("item",1:I,sep="")
# for(i in 1:loopn){
#   Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
#   dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
#   dat <- dat.0p0
#   colnames(dat)<- itemnames
#   
#   mod2ind3lv1.fixed_factor.delta_marginal <- '
# 
#   eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1
# 
#   '
#   sem.model <- mod2ind3lv1.fixed_factor.delta_marginal
#   
#   rm(mod2ind3lv1.fit)
#   
#   mod2ind3lv1.fit <- lavaan(model = sem.model, 
#                             data = dat, 
#                             std.lv = TRUE,
#                             int.ov.free = TRUE,
#                             int.lv.free = FALSE,
#                             meanstructure =FALSE,
#                             auto.fix.first = FALSE,
#                             auto.var = TRUE,
#                             auto.th = TRUE,
#                             auto.delta = TRUE,
#                             auto.cov.y = TRUE,
#                             ordered = itemnames,
#                             parameterization = "delta")
#   
# 
#   for(j in (1:I)){
#     lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j]
#     psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi
#     tau <- lavInspect(mod2ind3lv1.fit,what = "est")$tau[j]
#     mu <- lavInspect(mod2ind3lv1.fit,what = "mu")[j]
#     
#     coef_a[i,j] <- lambda/sqrt(1-lambda^2)*1.7
#     coef_d[i,j] <-  -tau/sqrt(1-lambda^2)*1.7
#   }
#   
# }
# coef_a_mean[,"ff_dm"]<-colMeans(coef_a,na.rm=TRUE)
# coef_d_mean[,"ff_dm"]<-colMeans(coef_d,na.rm=TRUE)
# coef_a_mean
# coef_d_mean


mod2ind3lv1.fixed_factor.delta_marginal<- function(){}

mod2ind3lv1.fixed_factor.delta_marginal <- '

eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
#latent var means and var
eta1_1 ~ eta1_1_mean*1
eta1_1 ~~ eta1_1_var*eta1_1

## Latent response variable (LRV) intercepts
item1_1 ~ int1_1*1
item2_1 ~ int2_1*1
item3_1 ~ int3_1*1
item4_1 ~ int4_1*1
item5_1 ~ int5_1*1
item6_1 ~ int6_1*1
item7_1 ~ int7_1*1
item8_1 ~ int8_1*1
item9_1 ~ int9_1*1
item10_1 ~ int10_1*1
item11_1 ~ int11_1*1
item12_1 ~ int12_1*1
item13_1 ~ int13_1*1

int1_1 + int2_1 + int3_1 + int4_1 + int5_1 + int6_1 + int7_1 + int8_1 + int9_1 + int10_1 + int11_1 + int12_1 + int13_1==0
#Latent response variable (LRV) mean constraint

int1_1 + l1_1*eta1_1_mean == 0
int2_1 + l2_1*eta1_1_mean == 0
int3_1 + l3_1*eta1_1_mean == 0
int4_1 + l4_1*eta1_1_mean == 0
int5_1 + l5_1*eta1_1_mean == 0
int6_1 + l6_1*eta1_1_mean == 0
int7_1 + l7_1*eta1_1_mean == 0
int8_1 + l8_1*eta1_1_mean == 0
int9_1 + l9_1*eta1_1_mean == 0
int10_1 + l10_1*eta1_1_mean == 0
int11_1 + l11_1*eta1_1_mean == 0
int12_1 + l12_1*eta1_1_mean == 0
int13_1 + l13_1*eta1_1_mean == 0

## thresholds link  LRVs to observed items
item1_1 | thr1_1*t1
item2_1 | thr2_1*t1
item3_1 | thr3_1*t1
item4_1 | thr4_1*t1
item5_1 | thr5_1*t1
item6_1 | thr6_1*t1
item7_1 | thr7_1*t1
item8_1 | thr8_1*t1
item9_1 | thr9_1*t1
item10_1 | thr10_1*t1
item11_1 | thr11_1*t1
item12_1 | thr12_1*t1
item13_1 | thr13_1*t1

#thr1_1 + thr2_1 + thr3_1 + thr4_1 + thr5_1 + thr6_1 + thr7_1 + thr8_1 + thr9_1 + thr10_1 + thr11_1 + thr12_1 + thr13_1==0

## LRVs (co)variances
item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
item12_1 ~~ var12_1*item12_1+ 0*item13_1
item13_1 ~~ var13_1*item13_1


## LRVs variances constraints
eta1_1_var*l1_1^2 + var1_1 == 1
eta1_1_var*l2_1^2 + var2_1 == 1
eta1_1_var*l3_1^2 + var3_1 == 1
eta1_1_var*l4_1^2 + var4_1 == 1
eta1_1_var*l5_1^2 + var5_1 == 1
eta1_1_var*l6_1^2 + var6_1 == 1
eta1_1_var*l7_1^2 + var7_1 == 1
eta1_1_var*l8_1^2 + var8_1 == 1
eta1_1_var*l9_1^2 + var9_1 == 1
eta1_1_var*l10_1^2 + var10_1 == 1
eta1_1_var*l11_1^2 + var11_1 == 1
eta1_1_var*l12_1^2 + var12_1 == 1
eta1_1_var*l13_1^2 + var13_1 == 1

'


sem.model <- mod2ind3lv1.fixed_factor.delta_marginal

rm(mod2ind3lv1.fit)
colnames(dat)<- itemnames
start_time <- get_time()
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
                          ordered = itemnames,
                          parameterization = "delta")
print(get_delta_time(start_time))


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



coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_b <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_b) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
  dat <- dat.0p0
  colnames(dat)<- itemnames
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
                            ordered = itemnames,
                            parameterization = "delta")
  
  
  for(j in (1:I)){
    lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi
    tau <- lavInspect(mod2ind3lv1.fit,what = "est")$tau[j]
    mu <- lavInspect(mod2ind3lv1.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda/sqrt(1-lambda^2)*1.7
    coef_d[i,j] <-  -tau/sqrt(1-lambda^2)*1.7
    coef_b[i,j] <- -coef_d[i,j]/coef_a[i,j]
  }
  
}

coef_a_mean[,"ff_dm"]<-colMeans(coef_a,na.rm=TRUE)
coef_b_mean[,"ff_dm"]<-colMeans(coef_b,na.rm=TRUE)
coef_d_mean[,"ff_dm"]<-colMeans(coef_d,na.rm=TRUE)

coef_a_sd[,"ff_dm"]<- apply(coef_a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_b_sd[,"ff_dm"]<- apply(coef_b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_d_sd[,"ff_dm"]<- apply(coef_d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})

coef_a_mean
coef_b_mean
coef_d_mean

write.table(coef_a_mean, 'coef_a_mean_ff_dm.txt')
write.table(coef_b_mean, 'coef_b_mean_ff_dm.txt')
write.table(coef_d_mean, 'coef_d_mean_ff_dm.txt')

write.table(coef_a_sd, 'coef_a_sd_ff_dm.txt')
write.table(coef_b_sd, 'coef_b_sd_ff_dm.txt')
write.table(coef_d_sd, 'coef_d_sd_ff_dm.txt')


mod2ind3lv1.fixed_factor.theta_conditional<- function(){}

mod2ind3lv1.fixed_factor.theta_conditional <- '
  eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+l11_1*item11_1+l12_1*item12_1+l13_1*item13_1
  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  item11_1 ~ int11_1*1
  item12_1 ~ int12_1*1
  item13_1 ~ int13_1*1
  int1_1 + int2_1 + int3_1 + int4_1 + int5_1 + int6_1 + int7_1 + int8_1 + int9_1 + int10_1 + int11_1 + int12_1 + int13_1 ==0
    
  #Latent response variable (LRV) mean constraint
    
  int1_1 + l1_1*eta1_1_mean == 0
  int2_1 + l2_1*eta1_1_mean == 0
  int3_1 + l3_1*eta1_1_mean == 0
  int4_1 + l4_1*eta1_1_mean == 0
  int5_1 + l5_1*eta1_1_mean == 0
  int6_1 + l6_1*eta1_1_mean == 0
  int7_1 + l7_1*eta1_1_mean == 0
  int8_1 + l8_1*eta1_1_mean == 0
  int9_1 + l9_1*eta1_1_mean == 0
  int10_1 + l10_1*eta1_1_mean == 0
  int11_1 + l11_1*eta1_1_mean == 0
  int12_1 + l12_1*eta1_1_mean == 0
  int13_1 + l13_1*eta1_1_mean == 0
    
    ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1
  item11_1 | thr11_1*t1
  item12_1 | thr12_1*t1
  item13_1 | thr13_1*t1
  
  ## LRVs (co)variances
item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item2_1 ~~ var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item3_1 ~~ var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item4_1 ~~ var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item5_1 ~~ var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item6_1 ~~ var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item7_1 ~~ var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item8_1 ~~ var8_1*item8_1+ 0*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item9_1 ~~ var9_1*item9_1+ 0*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item10_1 ~~ var10_1*item10_1+ 0*item11_1+ 0*item12_1+ 0*item13_1
item11_1 ~~ var11_1*item11_1+ 0*item12_1+ 0*item13_1
item12_1 ~~ var12_1*item12_1+ 0*item13_1
item13_1 ~~ var13_1*item13_1
  
  var1_1 ==1
  var2_1 ==1
  var3_1 ==1
  var4_1 ==1
  var5_1 ==1
  var6_1 ==1
  var7_1 ==1
  var8_1 ==1
  var9_1 ==1
  var10_1 ==1
  var11_1 ==1
  var12_1 ==1
  var13_1 ==1
'

sem.model <- mod2ind3lv1.fixed_factor.theta_conditional
rm(mod2ind3lv1.fit)
colnames(dat)<- itemnames
start_time <- get_time()
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
                          ordered = itemnames,
                          parameterization = "theta")
print(get_delta_time(start_time))

summary(mod2ind3lv1.fit)
fitMeasures(mod2ind3lv1.fit)[c("df",'tli',"cfi","rmsea")]


coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_b <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_b) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
  dat <- dat.0p0
  colnames(dat) <- itemnames
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
                            ordered = itemnames,
                            parameterization = "theta")
  
  
  for(j in (1:I)){
    lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi
    tau <- lavInspect(mod2ind3lv1.fit,what = "est")$tau[j]
    mu <- lavInspect(mod2ind3lv1.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*1.7
    coef_d[i,j] <-  -(tau)*1.7
    coef_b[i,j] <- -coef_d[i,j]/coef_a[i,j]
  }
  
}

coef_a_mean[,"ff_tc"]<-colMeans(coef_a,na.rm=TRUE)
coef_b_mean[,"ff_tc"]<-colMeans(coef_b,na.rm=TRUE)
coef_d_mean[,"ff_tc"]<-colMeans(coef_d,na.rm=TRUE)

coef_a_sd[,"ff_tc"]<- apply(coef_a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_b_sd[,"ff_tc"]<- apply(coef_b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_d_sd[,"ff_tc"]<- apply(coef_d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})

coef_a_mean
coef_b_mean
coef_d_mean

write.table(coef_a_mean, 'coef_a_mean_ff_tc.txt')
write.table(coef_b_mean, 'coef_b_mean_ff_tc.txt')
write.table(coef_d_mean, 'coef_d_mean_ff_tc.txt')

write.table(coef_a_sd, 'coef_a_sd_ff_tc.txt')
write.table(coef_b_sd, 'coef_b_sd_ff_tc.txt')
write.table(coef_d_sd, 'coef_d_sd_ff_tc.txt')


mod2ind3lv1.indicator_effects.delta_marginal<- function(){}

mod2ind3lv1.indicator_effects.delta_marginal <- '

eta1_1 =~ l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1
10==l1_1+l2_1+l3_1+l4_1+l5_1+l6_1+l7_1+l8_1+l9_1+l10_1

#latent var means and var
eta1_1 ~ eta1_1_mean*1
eta1_1 ~~ eta1_1_var*eta1_1

## Latent response variable (LRV) intercepts
item1_1 ~ int1_1*1
item2_1 ~ int2_1*1
item3_1 ~ int3_1*1
item4_1 ~ int4_1*1
item5_1 ~ int5_1*1
item6_1 ~ int6_1*1
item7_1 ~ int7_1*1
item8_1 ~ int8_1*1
item9_1 ~ int9_1*1
item10_1 ~ int10_1*1
int1_1 + int2_1 + int3_1 + int4_1 + int5_1 + int6_1 + int7_1 + int8_1 + int9_1 + int10_1==0

#Latent response variable (LRV) mean constraint

int1_1 + l1_1*eta1_1_mean == 0
int2_1 + l2_1*eta1_1_mean == 0
int3_1 + l3_1*eta1_1_mean == 0
int4_1 + l4_1*eta1_1_mean == 0
int5_1 + l5_1*eta1_1_mean == 0
int6_1 + l6_1*eta1_1_mean == 0
int7_1 + l7_1*eta1_1_mean == 0
int8_1 + l8_1*eta1_1_mean == 0
int9_1 + l9_1*eta1_1_mean == 0
int10_1 + l10_1*eta1_1_mean == 0

## thresholds link  LRVs to observed items
item1_1 | thr1_1*t1
item2_1 | thr2_1*t1
item3_1 | thr3_1*t1
item4_1 | thr4_1*t1
item5_1 | thr5_1*t1
item6_1 | thr6_1*t1
item7_1 | thr7_1*t1
item8_1 | thr8_1*t1
item9_1 | thr9_1*t1
item10_1 | thr10_1*t1

## LRVs (co)variances
item1_1  ~~  var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
item2_1  ~~  var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
item3_1  ~~  var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
item4_1  ~~  var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
item5_1  ~~  var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
item6_1  ~~  var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
item7_1  ~~  var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
item8_1  ~~  var8_1*item8_1+ 0*item9_1+ 0*item10_1
item9_1  ~~  var9_1*item9_1+ 0*item10_1
item10_1 ~~ var10_1*item10_1


## LRVs variances constraints
eta1_1_var*l1_1^2 + var1_1 == 1
eta1_1_var*l2_1^2 + var2_1 == 1
eta1_1_var*l3_1^2 + var3_1 == 1
eta1_1_var*l4_1^2 + var4_1 == 1
eta1_1_var*l5_1^2 + var5_1 == 1
eta1_1_var*l6_1^2 + var6_1 == 1
eta1_1_var*l7_1^2 + var7_1 == 1
eta1_1_var*l8_1^2 + var8_1 == 1
eta1_1_var*l9_1^2 + var9_1 == 1
eta1_1_var*l10_1^2 + var10_1 == 1
'

sem.model <- mod2ind3lv1.indicator_effects.delta_marginal
rm(mod2ind3lv1.fit)
start_time <- get_time()
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
                          ordered = itemnames,
                          parameterization = "delta")
print(get_delta_time(start_time))


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
  colnames(dat) <- itemnames
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
                            ordered = itemnames,
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
coef_a_mean[,"ie_dm"]<-colMeans(coef_a,na.rm=TRUE)
coef_d_mean[,"ie_dm"]<-colMeans(coef_d,na.rm=TRUE)
coef_a_mean
coef_d_mean


mod2ind3lv1.indicator_effects.theta_conditional<- function(){}

mod2ind3lv1.indicator_effects.theta_conditional <- '

  eta1_1 =~ l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1
  10==l1_1+l2_1+l3_1+l4_1+l5_1+l6_1+l7_1+l8_1+l9_1+l10_1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  int1_1 + int2_1 + int3_1 + int4_1 + int5_1 + int6_1 + int7_1 + int8_1 + int9_1 + int10_1==0
  
  #Latent response variable (LRV) mean constraint
  
  int1_1 + l1_1*eta1_1_mean == 0
  int2_1 + l2_1*eta1_1_mean == 0
  int3_1 + l3_1*eta1_1_mean == 0
  int4_1 + l4_1*eta1_1_mean == 0
  int5_1 + l5_1*eta1_1_mean == 0
  int6_1 + l6_1*eta1_1_mean == 0
  int7_1 + l7_1*eta1_1_mean == 0
  int8_1 + l8_1*eta1_1_mean == 0
  int9_1 + l9_1*eta1_1_mean == 0
  int10_1 + l10_1*eta1_1_mean == 0
  
  ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1

  ## LRVs (co)variances
  item1_1  ~~  var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item2_1  ~~  var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item3_1  ~~  var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item4_1  ~~  var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item5_1  ~~  var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item6_1  ~~  var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item7_1  ~~  var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item8_1  ~~  var8_1*item8_1+ 0*item9_1+ 0*item10_1
  item9_1  ~~  var9_1*item9_1+ 0*item10_1
  item10_1 ~~ var10_1*item10_1

  var1_1==1
  var2_1==1
  var3_1==1
  var4_1==1
  var5_1==1
  var6_1==1
  var7_1==1
  var8_1==1
  var9_1==1
  var10_1==1
'

sem.model <- mod2ind3lv1.indicator_effects.theta_conditional
rm(mod2ind3lv1.fit)
start_time <- get_time()
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
                          ordered = itemnames,
                          parameterization = "theta")
print(get_delta_time(start_time))

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
  colnames(dat) <- itemnames
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
                            ordered = itemnames,
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
coef_a_mean[,"ie_tc"]<-colMeans(coef_a,na.rm=TRUE)
coef_d_mean[,"ie_tc"]<-colMeans(coef_d,na.rm=TRUE)
coef_a_mean
coef_d_mean




mod2ind3lv1.indicator_marker.delta_marginal<- function(){}

mod2ind3lv1.indicator_marker.delta_marginal <- '

  eta1_1 =~ l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1
  l5_1==1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  int1_1 + int2_1 + int3_1 + int4_1 + int5_1 + int6_1 + int7_1 + int8_1 + int9_1 + int10_1==0
  
  #Latent response variable (LRV) mean constraint
  
  int1_1 + l1_1*eta1_1_mean == 0
  int2_1 + l2_1*eta1_1_mean == 0
  int3_1 + l3_1*eta1_1_mean == 0
  int4_1 + l4_1*eta1_1_mean == 0
  int5_1 + l5_1*eta1_1_mean == 0
  int6_1 + l6_1*eta1_1_mean == 0
  int7_1 + l7_1*eta1_1_mean == 0
  int8_1 + l8_1*eta1_1_mean == 0
  int9_1 + l9_1*eta1_1_mean == 0
  int10_1 + l10_1*eta1_1_mean == 0
  
  ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1

  ## LRVs (co)variances
  item1_1  ~~  var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item2_1  ~~  var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item3_1  ~~  var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item4_1  ~~  var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item5_1  ~~  var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item6_1  ~~  var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item7_1  ~~  var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item8_1  ~~  var8_1*item8_1+ 0*item9_1+ 0*item10_1
  item9_1  ~~  var9_1*item9_1+ 0*item10_1
  item10_1 ~~ var10_1*item10_1

  var1_1==1
  var2_1==1
  var3_1==1
  var4_1==1
  var5_1==1
  var6_1==1
  var7_1==1
  var8_1==1
  var9_1==1
  var10_1==1
'

sem.model <- mod2ind3lv1.indicator_marker.delta_marginal
rm(mod2ind3lv1.fit)
start_time <- get_time()
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
                          ordered = itemnames,
                          parameterization = "delta")
print(get_delta_time(start_time))

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
  colnames(dat) <- itemnames
  sem.model <- mod2ind3lv1.indicator_marker.delta_marginal
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
                            ordered = itemnames,
                            parameterization = "delta")
  for(j in (1:I)){
    lambda <- lavInspect(mod2ind3lv1.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod2ind3lv1.fit,what = "est")$psi
    tau <- lavInspect(mod2ind3lv1.fit,what = "est")$tau[j]
    mu <- lavInspect(mod2ind3lv1.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*sqrt(psi)*1.7
    coef_d[i,j] <- -(tau - lambda*mu)*1.7
  }
  
}
coef_a_mean[,"im_dm"]<-colMeans(coef_a,na.rm=TRUE)
coef_d_mean[,"im_dm"]<-colMeans(coef_d,na.rm=TRUE)
coef_a_mean
coef_d_mean

mod2ind3lv1.indicator_marker.theta_conditional<- function(){}

mod2ind3lv1.indicator_marker.theta_conditional <- '

  eta1_1 =~ l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1
  l5_1==1

  #latent var means and var
  eta1_1 ~ eta1_1_mean*1
  eta1_1 ~~ eta1_1_var*eta1_1
  
  ## Latent response variable (LRV) intercepts
  item1_1 ~ int1_1*1
  item2_1 ~ int2_1*1
  item3_1 ~ int3_1*1
  item4_1 ~ int4_1*1
  item5_1 ~ int5_1*1
  item6_1 ~ int6_1*1
  item7_1 ~ int7_1*1
  item8_1 ~ int8_1*1
  item9_1 ~ int9_1*1
  item10_1 ~ int10_1*1
  int1_1 + int2_1 + int3_1 + int4_1 + int5_1 + int6_1 + int7_1 + int8_1 + int9_1 + int10_1==0
  
  #Latent response variable (LRV) mean constraint
  
  int1_1 + l1_1*eta1_1_mean == 0
  int2_1 + l2_1*eta1_1_mean == 0
  int3_1 + l3_1*eta1_1_mean == 0
  int4_1 + l4_1*eta1_1_mean == 0
  int5_1 + l5_1*eta1_1_mean == 0
  int6_1 + l6_1*eta1_1_mean == 0
  int7_1 + l7_1*eta1_1_mean == 0
  int8_1 + l8_1*eta1_1_mean == 0
  int9_1 + l9_1*eta1_1_mean == 0
  int10_1 + l10_1*eta1_1_mean == 0
  
  ## thresholds link  LRVs to observed items
  item1_1 | thr1_1*t1
  item2_1 | thr2_1*t1
  item3_1 | thr3_1*t1
  item4_1 | thr4_1*t1
  item5_1 | thr5_1*t1
  item6_1 | thr6_1*t1
  item7_1 | thr7_1*t1
  item8_1 | thr8_1*t1
  item9_1 | thr9_1*t1
  item10_1 | thr10_1*t1

  ## LRVs (co)variances
  item1_1  ~~  var1_1*item1_1+ 0*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item2_1  ~~  var2_1*item2_1+ 0*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item3_1  ~~  var3_1*item3_1+ 0*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item4_1  ~~  var4_1*item4_1+ 0*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item5_1  ~~  var5_1*item5_1+ 0*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item6_1  ~~  var6_1*item6_1+ 0*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item7_1  ~~  var7_1*item7_1+ 0*item8_1+ 0*item9_1+ 0*item10_1
  item8_1  ~~  var8_1*item8_1+ 0*item9_1+ 0*item10_1
  item9_1  ~~  var9_1*item9_1+ 0*item10_1
  item10_1 ~~ var10_1*item10_1

  var1_1==1
  var2_1==1
  var3_1==1
  var4_1==1
  var5_1==1
  var6_1==1
  var7_1==1
  var8_1==1
  var9_1==1
  var10_1==1
'

sem.model <- mod2ind3lv1.indicator_marker.theta_conditional
rm(mod2ind3lv1.fit)
start_time <- get_time()
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
                          ordered = itemnames,
                          parameterization = "theta")
print(get_delta_time(start_time))

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
  colnames(dat) <- itemnames
  sem.model <- mod2ind3lv1.indicator_marker.theta_conditional
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
                            ordered = itemnames,
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
coef_a_mean[,"im_tc"]<-colMeans(coef_a,na.rm=TRUE)
coef_d_mean[,"im_tc"]<-colMeans(coef_d,na.rm=TRUE)
coef_a_mean
coef_d_mean


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


loopn <-100
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

along <-colMeans(coef_a,na.rm=TRUE)
dlong <-colMeans(coef_d,na.rm=TRUE)
along
c(a[1:3],a[1:3])
dlong
c(d[1:3],d[1:3])



p_calc<- function(a=1,b,c=0,D=1, theta){
  p <- c + (1-c)/(1+exp(-a*D*(theta-b)))
}

i_calc<- function(a=1,b,c=0,D=1, theta){
  p <- c + (1-c)/(1+exp(-a*(theta-b)))
  i <- D^2*a^2*(1-p)/p*((p-c)/(1-c))^2
}

sample_p <- function(upper_limit=6, lower_limit=-6, a,b,c,n=200){
  
  theta_n <- runif(n=n,min=lower_limit,max=upper_limit)
  p <- p_calc(a=a,b=b,c=c,theta=theta_n)
  i <- i_calc(a=a,b=b,c=c,theta=theta_n)
  return <-cbind(theta_n,p,i)   
}

# plot_cci(a=1,b=-2,c=.18)
# plot_cci(a=1,b=-2,c=.18,plot_info=TRUE)
# plot_cci(a=.45,b=-1,c=.15,500)
# plot_cci(a=1,b=-1,c=0,500,item_id = "CH34")
# plot_cci(a=1,b=-1,c=0,500,item_id = "CN76",plot_info = TRUE)
# setwd("C:\\Users\\hugo\\Downloads")
# plot_cci(a=1,b=-1,c=0,500,item_id = "CN76",plot_info = TRUE,filename="cci_cii.pdf")
# if filename is passed. image is saved in working dir.
plot_cci <- function(a=1,b,c=0,n=200,filename=NULL,item_id="", plot_info=FALSE){
  
  data <- sample_p(a=a,b=b,c=c,n=n)
  title <- sprintf("Curva Característica do Item %s", item_id)
  subtitle <- sprintf("a = %.2f; b = %.2f; c = %.2f",a,b,c)
  pfinal<-ggplot(data.frame(data),aes(x=theta_n,y=p))+geom_line(colour="blue")+
    ylim(0,1)+geom_hline(yintercept=0.5,linetype="dotted")+
    labs(x="Proficiência", y="Probabilidade de acerto",title =title, subtitle = subtitle )
  if(plot_info==TRUE){
    title <- sprintf("Curva de Informação do Item %s", item_id)
    h<-ggplot(data.frame(data),aes(x=theta_n,y=i))+geom_line(colour="red")+
      ylim(0,NA)+geom_hline(yintercept=0.5,linetype="dotted")+
      labs(x="Proficiência", y="Informação",title =title, subtitle = subtitle )
    require(cowplot)
    title <- sprintf("Item %s", item_id)
    title <- ggdraw() + draw_label(title, fontface='bold')
    pfinal <- cowplot::plot_grid(title, pfinal,h,nrow=3,rel_heights=c(0.1, 1,1))
  }
  print(pfinal)
  if(!is.null(filename)){
    ggsave(filename)
  }
  return(pfinal)
}



