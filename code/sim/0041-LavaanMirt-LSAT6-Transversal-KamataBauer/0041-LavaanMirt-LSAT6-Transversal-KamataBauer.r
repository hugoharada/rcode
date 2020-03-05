rm(list=ls())

if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(cowplot)) install.packages("cowplot"); library(cowplot)


lsat6_parametros <- function(){}

dat <- expand.table(LSAT6)
head(dat)

#calculating parameters.
item.a <- matrix(0,nrow=5,ncol=6)
colnames(item.a) <- c("mirt","lavI","lavII","lavIII","lavIV","label")
item.a[,"label"]=1:5
item.d <- matrix(0,nrow=5,ncol=6)
colnames(item.d) <- c("mirt","lavI","lavII","lavIII","lavIV","label")
item.d[,"label"]=1:5

lambda <- matrix(NA,nrow=5,ncol=6)
colnames(lambda) <- c("mirt","lavI","lavII","lavIII","lavIV","label")
lambda[,"label"]=1:5
tau <- matrix(NA,nrow=5,ncol=6)
colnames(tau) <- c("mirt","lavI","lavII","lavIII","lavIV","label")
tau[,"label"]=1:5

mirt_bookmark <- function(){}

mod<-mirt(data=dat,model = 1,itemtype = "2PL")
M2(mod)
itemfit(mod)
coef(mod, simplify=TRUE)
item.a[,"mirt"]<-coef(mod, simplify=TRUE)$item[,"a1"]
item.d[,"mirt"]<-coef(mod, simplify=TRUE)$item[,"d"]


lavaan_models <- function(){}

mod2ind4lv1 <- '
lv1 =~ lmbd1*Item_1 + lmbd2*Item_2 + lmbd3*Item_3 +lmbd4*Item_4 + lmbd5*Item_5
lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1 

## LIR means
Item_1 ~ mean1*1
Item_2 ~ mean2*1
Item_3 ~ mean3*1
Item_4 ~ mean4*1
Item_5 ~ mean5*1
0 == mean1+mean2+mean3+mean4+mean5

## LIR means
# Item_1 ~ mean1*1 + 0*1
# Item_2 ~ mean2*1 + 0*1
# Item_3 ~ mean3*1 + 0*1
# Item_4 ~ mean4*1 + 0*1
# Item_5 ~ mean5*1 + 0*1


## LIR (co)variances
Item_1 ~~ var1*Item_1 + 0*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
Item_2 ~~ var2*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
Item_3 ~~ var3*Item_3 + 0*Item_4 + 0*Item_5 
Item_4 ~~ var4*Item_4 + 0*Item_5 
Item_5 ~~ var5*Item_5 

# Item_1 ~~ var1*Item_1 + 1*Item_1 + 0*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
# Item_2 ~~ var2*Item_2 + 1*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
# Item_3 ~~ var3*Item_3 + 1*Item_3 + 0*Item_4 + 0*Item_5 
# Item_4 ~~ var4*Item_4 + 1*Item_4 + 0*Item_5 
# Item_5 ~~ var5*Item_5 + 1*Item_5

## thresholds link LIRs to observed items
Item_1 | thr1*t1 
Item_2 | thr2*t1
Item_3 | thr3*t1
Item_4 | thr4*t1
Item_5 | thr5*t1
#thr1 + thr2 + thr3 + thr4 + thr5 ==0
'
mod2ind4lv1 <- '
lv1 =~ lmbd1*Item_1 + lmbd2*Item_2 + lmbd3*Item_3 +lmbd4*Item_4 + lmbd5*Item_5
lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1 

## LIR means
Item_1 ~ mean1*1
Item_2 ~ mean2*1
Item_3 ~ mean3*1
Item_4 ~ mean4*1
Item_5 ~ mean5*1

mean1+lmbd1*lv1mean==0
mean2+lmbd2*lv1mean==0
mean3+lmbd3*lv1mean==0
mean4+lmbd4*lv1mean==0
mean5+lmbd5*lv1mean==0

## LIR (co)variances
Item_1 ~~ var1*Item_1 + 0*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
Item_2 ~~ var2*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
Item_3 ~~ var3*Item_3 + 0*Item_4 + 0*Item_5 
Item_4 ~~ var4*Item_4 + 0*Item_5 
Item_5 ~~ var5*Item_5 

# Item_1 ~~ var1*Item_1 + 1*Item_1 + 0*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
# Item_2 ~~ var2*Item_2 + 1*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
# Item_3 ~~ var3*Item_3 + 1*Item_3 + 0*Item_4 + 0*Item_5 
# Item_4 ~~ var4*Item_4 + 1*Item_4 + 0*Item_5 
# Item_5 ~~ var5*Item_5 + 1*Item_5

## thresholds link LIRs to observed items
Item_1 | thr1*t1 
Item_2 | thr2*t1
Item_3 | thr3*t1
Item_4 | thr4*t1
Item_5 | thr5*t1
thr1 + thr2 + thr3 + thr4 + thr5 ==0
'


lavaan_constraints <- function(){}

constraint.m0.v1.tf <- '
## Wave 1
mean1 == 0 ; var1 == 1
## Wave 2
mean2 == 0 ; var2 == 1
## Wave 3
mean3 == 0 ; var3 == 1
## Wave 4
mean4 == 0 ; var4 == 1
## Wave 5
mean5 == 0 ; var5 == 1
'
constraint.mf.v1.t0 <- '
## Wave 1
thr1 == 0 ; var1 == 1
## Wave 2
thr2 == 0 ; var2 == 1
## Wave 3
thr3 == 0 ; var3 == 1
## Wave 4
thr4 == 0 ; var4 == 1
## Wave 5
thr5 == 0 ; var5 == 1
'
constraint.mf.v1.t1_0 <- '
## Wave 1
thr1 == 0 ; var1 == 1
## Wave 2
var2 == 1
## Wave 3
var3 == 1
## Wave 4
var4 == 1
## Wave 5
var5 == 1
'

#does not work
#df becomes -5
constraint.mf.vf.t1_0 <- '
## Wave 1
thr1 == 0 
'


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
mod2ind4lv1_parametros_I <- function(){}
rm(mod2ind4lv1.fit)
mod2ind4lv1.fit <- lavaan(mod2ind4lv1, 
                          data = dat, 
                          int.ov.free = TRUE, # intercepts
                          int.lv.free = TRUE,
                          std.lv =TRUE,
                          fixed.x = FALSE,
                          meanstructure = TRUE,
                          auto.fix.first = FALSE,
                          auto.fix.single = TRUE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.cov.lv.x = TRUE,
                          auto.cov.y = TRUE,
                          auto.delta = TRUE,
                          ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                          parameterization = "delta")

lavCor(dat,
       ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),output = "cor")
lavCor(dat,
       ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),output = "cov")
lavCor(dat,
       ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),output = "th")
lavParTable(mod2ind4lv1, 
            int.ov.free = TRUE,
            int.lv.free = FALSE,
            meanstructure = TRUE,
            std.lv =TRUE,
            auto.fix.first = FALSE,
            auto.var = TRUE,
            parameterization = "delta")
parTable(mod2ind4lv1.fit)

lavop <- lavInspect(mod2ind4lv1.fit, "options")
summary(mod2ind4lv1.fit)
inspect(mod2ind4lv1.fit,what="free")
inspect(mod2ind4lv1.fit,"est")
inspect(mod2ind4lv1.fit,what="partable")
fitMeasures(mod2ind4lv1.fit)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]
vcov <- lavInspect(mod2ind4lv1.fit,what = "vcov")
eigen(vcov)$values
resid(mod2ind4lv1.fit, type = "cor")
#View(resid(mod2ind4lv1.fit, type = "cor")$cov)

#getting lambda values
lavInspect(mod2ind4lv1.fit,what = 'est')$lambda
lambda[,"lavI"] <- lavInspect(mod2ind4lv1.fit,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod2ind4lv1.fit,what = 'est')$tau
tau[,"lavI"] <- lavInspect(mod2ind4lv1.fit,what = 'est')$tau
tau

#getting mean values
lavInspect(mod2ind4lv1.fit,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
mu <- lavInspect(mod2ind4lv1.fit,what = 'est')$alpha
mu
#getting cov values
lavInspect(mod2ind4lv1.fit,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
thetvar<- diag(lavInspect(mod2ind4lv1.fit,what = 'est')$psi)
thetvar

for(i in seq(1,5,1)){# i items
  item.a[i,"lavI"] <- lambda[i,"lavI"]/sqrt(1-lambda[i,"lavI"]*lambda[i,"lavI"])*1.702
  item.d[i,"lavI"] <- -tau[i,"lavI"]/sqrt(1-lambda[i,"lavI"]*lambda[i,"lavI"])*1.702
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavI))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavI))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
mod2ind4lv1_parametros_II <- function(){}


rm(mod2ind4lv1.fit)

mod2ind4lv1.fit <- lavaan(mod2ind4lv1, 
                          data = dat, 
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          fixed.x = FALSE,
                          meanstructure = TRUE,
                          std.lv =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                          parameterization = "theta")

lavParTable(mod2ind4lv1, 
            int.ov.free = TRUE,
            int.lv.free = FALSE,
            meanstructure = TRUE,
            std.lv =TRUE,
            auto.fix.first = FALSE,
            auto.var = TRUE,
            constraints = constraint.m0.v1.tf,
            parameterization = "theta")

parTable(mod2ind4lv1.fit)

lavop <- lavInspect(mod2ind4lv1.fit, "options")
summary(mod2ind4lv1.fit)
fitMeasures(mod2ind4lv1.fit)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]
vcov <- lavInspect(mod2ind4lv1.fit,what = "vcov")
eigen(vcov)$values
resid(mod2ind4lv1.fit, type = "cor")
#View(resid(mod2ind4lv1.fit, type = "cor")$cov)

#getting lambda values
lavInspect(mod2ind4lv1.fit,what = 'est')$lambda
lambda[,"lavII"] <- lavInspect(mod2ind4lv1.fit,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod2ind4lv1.fit,what = 'est')$tau
tau[,"lavII"] <- lavInspect(mod2ind4lv1.fit,what = 'est')$tau
tau

#getting mean values
lavInspect(mod2ind4lv1.fit,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
mu <- lavInspect(mod2ind4lv1.fit,what = 'est')$alpha
mu
#getting cov values
lavInspect(mod2ind4lv1.fit,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
thetvar<- diag(lavInspect(mod2ind4lv1.fit,what = 'est')$psi)
thetvar

for(i in seq(1,5,1)){# i items
  item.a[i,"lavII"] <- lambda[i,"lavII"]*1.702
  item.d[i,"lavII"] <- -tau[i,"lavII"]*1.702
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavII))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavII))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
mod2ind4lv1_parametros_III <- function(){}

mod2ind4lv1 <- '
lv1 =~ lmbd1*Item_1 + 1*Item_1 + lmbd2*Item_2 + lmbd3*Item_3 +lmbd4*Item_4 + lmbd5*Item_5
lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1 

## LIR means
Item_1 ~ mean1*1
Item_2 ~ mean2*1
Item_3 ~ mean3*1
Item_4 ~ mean4*1
Item_5 ~ mean5*1
0 == mean1+mean2+mean3+mean4+mean5

## LIR means
# Item_1 ~ mean1*1 + 0*1
# Item_2 ~ mean2*1 + 0*1
# Item_3 ~ mean3*1 + 0*1
# Item_4 ~ mean4*1 + 0*1
# Item_5 ~ mean5*1 + 0*1


## LIR (co)variances
Item_1 ~~ var1*Item_1 + 1*Item_1 + 0*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
Item_2 ~~ var2*Item_2 + 1*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
Item_3 ~~ var3*Item_3 + 1*Item_3 + 0*Item_4 + 0*Item_5 
Item_4 ~~ var4*Item_4 + 1*Item_4 + 0*Item_5 
Item_5 ~~ var5*Item_5 + 1*Item_5

## thresholds link LIRs to observed items
Item_1 | thr1*t1
#Item_1 | thr1*t1
Item_2 | thr2*t1
Item_3 | thr3*t1
Item_4 | thr4*t1
Item_5 | thr5*t1
'

rm(mod2ind4lv1.fit)

mod2ind4lv1.fit <- lavaan(mod2ind4lv1, 
                          data = dat, 
                          int.ov.free = TRUE,
                          int.lv.free = TRUE,
                          meanstructure = TRUE,
                          std.lv =FALSE,
                          auto.fix.first = TRUE,
                          auto.var = TRUE,
                          ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                          parameterization = "delta")

lavParTable(mod2ind4lv1, 
            int.ov.free = TRUE,
            int.lv.free = TRUE,
            meanstructure = TRUE,
            std.lv =FALSE,
            auto.fix.first = TRUE,
            auto.var = TRUE,
#            constraints = constraint.mf.v1.t0 ,
            parameterization = "delta")

parTable(mod2ind4lv1.fit)

lavop <- lavInspect(mod2ind4lv1.fit, "options")
summary(mod2ind4lv1.fit)

lavInspect(mod2ind4lv1.fit,what="free")
lavInspect(mod2ind4lv1.fit,what="start")

fitMeasures(mod2ind4lv1.fit)['tli']
fitMeasures(mod2ind4lv1.fit)['cfi']
fitMeasures(mod2ind4lv1.fit)['rmsea']
vcov <- lavInspect(mod2ind4lv1.fit,what = "vcov")
eigen(vcov)$values
resid(mod2ind4lv1.fit, type = "cor")
#View(resid(mod2ind4lv1.fit, type = "cor")$cov)

#getting lambda values
lavInspect(mod2ind4lv1.fit,what = 'est')$lambda0..0..9,,,,uiuiuiiuiiuiuiiuiuiuiuuunmN     










































lambda[,"lavIII"] <- lavInspect(mod2ind4lv1.fit,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod2ind4lv1.fit,what = 'est')$tau
tau[,"lavIII"] <- lavInspect(mod2ind4lv1.fit,what = 'est')$tau
tau

#getting mean values
lavInspect(mod2ind4lv1.fit,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
mu <- lavInspect(mod2ind4lv1.fit,what = 'est')$alpha
mu
#getting cov values
lavInspect(mod2ind4lv1.fit,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
theta_var<- diag(lavInspect(mod2ind4lv1.fit,what = 'est')$psi)
theta_var


for(i in seq(1,5,1)){# i items
  item.a[i,"lavIII"] <- lambda[i,"lavIII"]*sqrt(theta_var)/sqrt(1-lambda[i,"lavIII"]*lambda[i,"lavIII"]*theta_var)*1.7
  item.d[i,"lavIII"] <- (-tau[i,"lavIII"]+lambda[i,"lavIII"]*mu)/sqrt(1-lambda[i,"lavIII"]*lambda[i,"lavIII"]*theta_var)*1.7
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavIII))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavIII))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
mod2ind4lv1_parametros_IV <- function(){}


mod2ind4lv1 <- '
lv1 =~ lmbd1*Item_1 + 1*Item_1 + lmbd2*Item_2 + lmbd3*Item_3 +lmbd4*Item_4 + lmbd5*Item_5
lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1 

## LIR means
Item_1 ~ mean1*1
Item_2 ~ mean2*1
Item_3 ~ mean3*1
Item_4 ~ mean4*1
Item_5 ~ mean5*1
0 == mean1+mean2+mean3+mean4+mean5

## LIR means
# Item_1 ~ mean1*1 + 0*1
# Item_2 ~ mean2*1 + 0*1
# Item_3 ~ mean3*1 + 0*1
# Item_4 ~ mean4*1 + 0*1
# Item_5 ~ mean5*1 + 0*1


## LIR (co)variances
Item_1 ~~ var1*Item_1 + 1*Item_1 + 0*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
Item_2 ~~ var2*Item_2 + 1*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5 
Item_3 ~~ var3*Item_3 + 1*Item_3 + 0*Item_4 + 0*Item_5 
Item_4 ~~ var4*Item_4 + 1*Item_4 + 0*Item_5 
Item_5 ~~ var5*Item_5 + 1*Item_5

## thresholds link LIRs to observed items
Item_1 | thr1*t1
Item_2 | thr2*t1
Item_3 | thr3*t1
Item_4 | thr4*t1
Item_5 | thr5*t1
'

rm(mod2ind4lv1.fit)

mod2ind4lv1.fit <- lavaan(mod2ind4lv1, 
                          data = dat, 
                          int.ov.free = TRUE,
                          int.lv.free = TRUE,
                          meanstructure = TRUE,
                          std.lv =FALSE,
                          auto.fix.first = TRUE,
                          auto.var = TRUE,
                          ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                          parameterization = "theta")

lavParTable(mod2ind4lv1, 
            int.ov.free = TRUE,
            int.lv.free = TRUE,
            meanstructure = TRUE,
            std.lv =FALSE,
            auto.fix.first = TRUE,
            auto.var = TRUE,
            #            constraints = constraint.mf.v1.t0 ,
            parameterization = "delta")

parTable(mod2ind4lv1.fit)

lavop <- lavInspect(mod2ind4lv1.fit, "options")
summary(mod2ind4lv1.fit)
fitMeasures(mod2ind4lv1.fit)['tli']
fitMeasures(mod2ind4lv1.fit)['cfi']
fitMeasures(mod2ind4lv1.fit)['rmsea']
vcov <- lavInspect(mod2ind4lv1.fit,what = "vcov")
eigen(vcov)$values
resid(mod2ind4lv1.fit, type = "cor")
#View(resid(mod2ind4lv1.fit, type = "cor")$cov)

#getting lambda values
lavInspect(mod2ind4lv1.fit,what = 'est')$lambda
lambda[,"lavIV"] <- lavInspect(mod2ind4lv1.fit,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod2ind4lv1.fit,what = 'est')$tau
tau[,"lavIV"] <- lavInspect(mod2ind4lv1.fit,what = 'est')$tau
tau

#getting mean values
lavInspect(mod2ind4lv1.fit,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
mu <- lavInspect(mod2ind4lv1.fit,what = 'est')$alpha
mu
#getting cov values
lavInspect(mod2ind4lv1.fit,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
theta_var<- diag(lavInspect(mod2ind4lv1.fit,what = 'est')$psi)
theta_var


lavInspect(mod2ind4lv1.fit,what = "est")$theta[1,1]+lavInspect(mod2ind4lv1.fit,what = "est")$lambda[1]*lavInspect(mod2ind4lv1.fit,what = "est")$lambda[1]*lavInspect(mod2ind4lv1.fit,what = "est")$psi



for(i in seq(1,5,1)){# i items
  item.a[i,"lavIV"] <- lambda[i,"lavIV"]*sqrt(theta_var)*1.702
  item.d[i,"lavIV"] <- (-tau[i,"lavIV"]+lambda[i,"lavIV"]*mu)*1.702
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavIII))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavIII))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)

