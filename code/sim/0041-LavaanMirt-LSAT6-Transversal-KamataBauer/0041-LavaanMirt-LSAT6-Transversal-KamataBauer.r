rm(list=ls())

if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(cowplot)) install.packages("cowplot"); library(cowplot)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)


lsat6_parametros <- function(){}

dat <- expand.table(LSAT6)
head(dat)


thresholdCalc <- function(vector){
  prop <- table(vector)/sum(table(vector))
  cprop <- c(0, cumsum(prop))
  th <- qnorm(cprop)
  return (list(th=th, prop=prop, cprop=cprop))
}

thresholdCalc(dat[,1])
thresholdCalc(dat[,2])
thresholdCalc(dat[,3])
thresholdCalc(dat[,4])
thresholdCalc(dat[,5])



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
histogram(fscores(mod))
item.a[,"mirt"]<-coef(mod, simplify=TRUE)$item[,"a1"]
item.d[,"mirt"]<-coef(mod, simplify=TRUE)$item[,"d"]


cbind(item = 1:5, b= -item.d[,"mirt"]/item.a[,"mirt"])

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


mod_noconstraints <- '
lv1 =~ lmbd1*Item_1 + lmbd2*Item_2 + lmbd3*Item_3 +lmbd4*Item_4 + lmbd5*Item_5
lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1 

## LRV/LIR intercept
Item_1 ~ int1*1
Item_2 ~ int2*1
Item_3 ~ int3*1
Item_4 ~ int4*1
Item_5 ~ int5*1

# LRV/LIR vars
Item_1 ~~ var1*Item_1 
Item_2 ~~ var2*Item_2
Item_3 ~~ var3*Item_3
Item_4 ~~ var4*Item_4
Item_5 ~~ var5*Item_5 

## thresholds link LIRs to observed items
Item_1 | thr1*t1 
Item_2 | thr2*t1
Item_3 | thr3*t1
Item_4 | thr4*t1
Item_5 | thr5*t1

'

constraint.I <- '
lv1mean==0

int1==0
int2==0
int3==0
int4==0
int5==0

1== lmbd1*lmbd1*lv1var + var1
1== lmbd2*lmbd2*lv1var + var2
1== lmbd3*lmbd3*lv1var + var3
1== lmbd4*lmbd4*lv1var + var4
1== lmbd5*lmbd5*lv1var + var5

'


mod2ind4lv1 <- '
lv1 =~ lmbd1*Item_1 + lmbd2*Item_2 + lmbd3*Item_3 +lmbd4*Item_4 + lmbd5*Item_5
lv1 ~ lv1mean*1
lv1 ~~ lv1var*lv1 

lv1mean==0

## LIR means
Item_1 ~ mean1*1 + 0*1
Item_2 ~ mean2*1 + 0*1
Item_3 ~ mean3*1 + 0*1
Item_4 ~ mean4*1 + 0*1
Item_5 ~ mean5*1 + 0*1

Item_1 ~~ var1*Item_1 + 0*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5
Item_2 ~~ var2*Item_2 + 0*Item_3 + 0*Item_4 + 0*Item_5
Item_3 ~~ var3*Item_3 + 0*Item_4 + 0*Item_5
Item_4 ~~ var4*Item_4 + 0*Item_5
Item_5 ~~ var5*Item_5 

## thresholds link LIRs to observed items
Item_1 | thr1*t1 
Item_2 | thr2*t1
Item_3 | thr3*t1
Item_4 | thr4*t1
Item_5 | thr5*t1

1== lmbd1*lmbd1*lv1var + var1
1== lmbd2*lmbd2*lv1var + var2
1== lmbd3*lmbd3*lv1var + var3
1== lmbd4*lmbd4*lv1var + var4
1== lmbd5*lmbd5*lv1var + var5

'



rm(mod2ind4lv1.fit)
mod2ind4lv1.fit <- lavaan(mod2ind4lv1, 
                          data = dat, 
                          int.ov.free = TRUE, # intercepts
                          int.lv.free = FALSE,
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

mod2ind4lv1.fit.noconst <- lavaan(mod_noconstraints, 
                          data = dat, 
                          int.ov.free = TRUE, # intercepts
                          int.lv.free = FALSE,
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
                          parameterization = "delta",
                          constraints = constraint.I)

summary(mod2ind4lv1.fit.noconst)


summary(mod2ind4lv1.fit)
fitmeasures(mod2ind4lv1.fit)[c("tli","cfi","rmsea")]
fitMeasures(mod2ind4lv1.fit)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

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
alpha <- lavInspect(mod2ind4lv1.fit,what = 'est')$alpha
alpha

#LRV mean and var
lavInspect(mod2ind4lv1.fit,what = 'mu')
lavInspect(mod2ind4lv1.fit,what = 'vy')
th


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


loopn<-500
tmp <- matrix(NA,nrow=loopn,ncol=10)
mod2ind4lv1 <- '
lv1 =~ lmbd1*Item_1 + lmbd2*Item_2 + lmbd3*Item_3 +lmbd4*Item_4 + lmbd5*Item_5
'
for(i in 1:loopn){
  
  boot_data <- dplyr::sample_n(dat,size=500,replace=TRUE)
  
  mod2ind4lv1.fit <- cfa(mod2ind4lv1,
                         data = boot_data, 
                         std.lv =TRUE,
                         ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                         parameterization = "delta")
  
  if(fitmeasures(mod2ind4lv1.fit)["tli"] > 0.9){
    tmp[i,1:5]<- lavInspect(mod2ind4lv1.fit,what = 'est')$lambda
    tmp[i,6:10]<- lavInspect(mod2ind4lv1.fit,what = 'est')$tau
    
  }
}
tmp

tmp2 <- tmp[complete.cases(tmp),]

quantile(tmp2[,1], prob=c(.25))
quantile(tmp2[,1], prob=c(.75))

histogram(tmp[,1])
histogram(tmp2[,1])
histogram(tmp3[,1])
summary(tmp3)
qqnorm(tmp2[,1])
qqline(tmp2[,1])
boxplot(tmp2[,1])

iqr_values_only <- function(vector){
  quantis_25_75 <-quantile(vector, prob=c(.25,.75),na.rm = TRUE)
  vector<- ifelse( (vector < quantis_25_75[1])|(vector > quantis_25_75[2]),
                   NA,
                   vector)
  return(vector)
}

iqr_values_only(tmp2[,1])

tmp3 <- apply(tmp2, MARGIN=2, FUN=iqr_values_only)


mean_tmp <-colMeans(tmp2,na.rm = TRUE)
mean_tmp <-colMeans(tmp3,na.rm = TRUE)

lambda[,"lavI"] <- mean_tmp[1:5]
tau[,"lavI"] <-  mean_tmp[6:10]


for(i in seq(1,5,1)){# i items
  item.a[i,"lavI"] <- lambda[i,"lavI"]/sqrt(1-lambda[i,"lavI"]*lambda[i,"lavI"])*1.702
  item.d[i,"lavI"] <- -tau[i,"lavI"]/sqrt(1-lambda[i,"lavI"]*lambda[i,"lavI"])*1.702
}

item.a
item.d

lambda
tau

-item.a[,"lavI"]*item.d[,"lavI"]



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

fitmeasures(mod2ind4lv1.fit)["tli"]


# twoP.model<-'
# # loadings
# Theta =~ l1*Q1 + l2*Q2 + l3*Q3 + l4*Q4 + l5*Q5
# # thresholds
# Q1 | th1*t1
# Q2 | th2*t1
# Q3 | th3*t1
# Q4 | th4*t1
# Q5 | th5*t1
# # convert loadings to slopes (normal)
# alpha1.N := (l1)/sqrt(1-l1^2)
# alpha2.N := (l2)/sqrt(1-l2^2)
# alpha3.N := (l3)/sqrt(1-l3^2)
# alpha4.N := (l4)/sqrt(1-l4^2)
# alpha5.N := (l5)/sqrt(1-l5^2)
# # convert thresholds to intercepts (normal)
# beta1.N := (-th1)/sqrt(1-l1^2)
# beta2.N := (-th2)/sqrt(1-l2^2)
# beta3.N := (-th3)/sqrt(1-l3^2)
# beta4.N := (-th4)/sqrt(1-l4^2)
# beta5.N := (-th5)/sqrt(1-l5^2)
# # convert intercepts to locations (normal)
# loc1 := -beta1.N/alpha1.N
# loc2 := -beta2.N/alpha2.N
# loc3 := -beta3.N/alpha3.N
# loc4 := -beta4.N/alpha4.N
# loc5 := -beta5.N/alpha5.N
# # convert loadings to slopes (logistic)
# alpha1.L := (l1)/sqrt(1-l1^2)*1.7
# alpha2.L := (l2)/sqrt(1-l2^2)*1.7
# alpha3.L := (l3)/sqrt(1-l3^2)*1.7
# alpha4.L := (l4)/sqrt(1-l4^2)*1.7
# alpha5.L := (l5)/sqrt(1-l5^2)*1.7
# # convert thresholds to locations (logistic)
# loc1.L := th1/l1
# loc2.L := th2/l2
# loc3.L := th3/l3
# loc4.L := th4/l4
# loc5.L := th5/l5
# # convert locations to intercepts (logistic)
# beta1.L := (-alpha1.L)*loc1.L
# beta2.L := (-alpha2.L)*loc2.L
# beta3.L := (-alpha3.L)*loc3.L
# beta4.L := (-alpha4.L)*loc4.L
# beta5.L := (-alpha5.L)*loc5.L
# '
# 
# dat2 <- dat
# colnames(dat2)<- c("Q1","Q2","Q3","Q4", "Q5")
# 
# twoP.fit <- cfa(twoP.model, data=dat2,  std.lv=TRUE, ordered=c("Q1","Q2","Q3","Q4", "Q5"))
# summary(twoP.fit, standardized=TRUE)
# 

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

lavInspect(mod2ind4lv1.fit,what = 'sampstat')
lavInspect(mod2ind4lv1.fit,what = 'mu')



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

