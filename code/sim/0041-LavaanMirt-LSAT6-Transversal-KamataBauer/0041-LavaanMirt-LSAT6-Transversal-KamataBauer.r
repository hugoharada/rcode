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
table(dat[,1])


thresholdCalc <- function(vector){
  prop <- table(vector)/sum(table(vector))
  cprop <- c(0, cumsum(prop))
  th <- qnorm(cprop)
  return (list(th=th, prop=prop, cprop=cprop))
}
th <- list(
  thresholdCalc(dat[,1]),
  thresholdCalc(dat[,2]),
  thresholdCalc(dat[,3]),
  thresholdCalc(dat[,4]),
  thresholdCalc(dat[,5])
)

th

colnames_lav <- c("mirt","lavI","lavII","lavIIIa","lavIIIb","lavIVa","lavIVb","lavVa","lavVb","lavVIa","lavVIb","label")

#calculating parameters.
item.a <- matrix(0,nrow=5,ncol=length(colnames_lav))
colnames(item.a) <- colnames_lav
item.a[,"label"]=1:5
item.d <- matrix(0,nrow=5,ncol=length(colnames_lav))
colnames(item.d) <- colnames_lav
item.d[,"label"]=1:5

lambda <- matrix(NA,nrow=5,ncol=length(colnames_lav))
colnames(lambda) <- colnames_lav
lambda[,"label"]=1:5
tau <- matrix(NA,nrow=5,ncol=length(colnames_lav))
colnames(tau) <- colnames_lav
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





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
mod2ind4lv1_parametros_I <- function(){}



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




mod.fit.I <- lavaan(mod_noconstraints, 
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

summary(mod.fit.I)
fitmeasures(mod.fit.I)[c("tli","cfi","rmsea")]
fitMeasures(mod.fit.I)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

#getting lambda values
lavInspect(mod.fit.I,what = 'est')$lambda
lambda[,"lavI"] <- lavInspect(mod.fit.I,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod.fit.I,what = 'est')$tau
tau[,"lavI"] <- lavInspect(mod.fit.I,what = 'est')$tau
tau

#LRV mean and var
lavInspect(mod.fit.I,what = 'mu')
lavInspect(mod.fit.I,what = 'vy')
th


#getting mean values
lavInspect(mod.fit.I,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
alpha <- lavInspect(mod.fit.I,what = 'est')$alpha
alpha


#getting cov values
lavInspect(mod.fit.I,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
thetvar<- diag(lavInspect(mod.fit.I,what = 'est')$psi)
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

constraint.II <- '
lv1mean==0

int1==0
int2==0
int3==0
int4==0
int5==0

1== var1
1== var2
1== var3
1== var4
1== var5
'

rm(mod.fit.II)

mod.fit.II <- lavaan(mod_noconstraints, 
                          data = dat, 
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          fixed.x = FALSE,
                          meanstructure = TRUE,
                          std.lv =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                          parameterization = "theta",
                          constraints = constraint.II)

summary(mod.fit.II)
fitMeasures(mod.fit.II)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

#getting lambda values
lavInspect(mod.fit.II,what = 'est')$lambda
lambda[,"lavII"] <- lavInspect(mod.fit.II,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod.fit.II,what = 'est')$tau
tau[,"lavII"] <- lavInspect(mod.fit.II,what = 'est')$tau
tau
lavInspect(mod.fit.II,what = 'th')


#LRV mean and var
mu <- lavInspect(mod.fit.II,what = 'mu')
vy <- lavInspect(mod.fit.II,what = 'vy')
vy_exp <- lambda[,"lavII"]*lambda[,"lavII"] +1 #expected values of vy
vy - vy_exp #should be zero


# checking if accumulated probabilities are accurate for  estimated/standardized LIR/LRV
pnorm(tau[1,"lavII"],mean = mu[1],sd =sqrt(vy[1])) 
th[[1]]$cprop[2]

pnorm(tau[2,"lavII"],mean = mu[2],sd =sqrt(vy[2])) 
th[[2]]$cprop[2]


#getting mean values
lavInspect(mod.fit.II,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
alpha <- lavInspect(mod.fit.II,what = 'est')$alpha
alpha


#getting cov values
lavInspect(mod.fit.II,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
thetvar<- diag(lavInspect(mod.fit.II,what = 'est')$psi)
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
mod2ind4lv1_parametros_IIIa <- function(){}


constraint.IIIa <- '

lmbd1==1
thr1 ==0

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

rm(mod.fit.IIIa)

mod.fit.IIIa <- lavaan(mod_noconstraints, 
                       data = dat, 
                       int.ov.free = TRUE,
                       int.lv.free = TRUE,
                       meanstructure = TRUE,
                       std.lv =FALSE,
                       auto.fix.first = TRUE,
                       auto.var = TRUE,
                       ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                       parameterization = "delta",
                       constraints = constraint.IIIa
)


summary(mod.fit.IIIa)
fitMeasures(mod.fit.IIIa)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

lambda[,"lavIIIa"] <- lavInspect(mod.fit.IIIa,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod.fit.IIIa,what = 'est')$tau
tau[,"lavIIIa"] <- lavInspect(mod.fit.IIIa,what = 'est')$tau
tau

#LRV mean and var
mu <- lavInspect(mod.fit.IIIa,what = 'mu')
vy <- lavInspect(mod.fit.IIIa,what = 'vy')


# checking if accumulated probabilities are accurate for  estimated/standardized LIR/LRV
pnorm(tau[1,"lavIIIa"],mean = mu[1],sd =sqrt(vy[1])) 
th[[1]]$cprop[2]

pnorm(tau[2,"lavIIIa"],mean = mu[2],sd =sqrt(vy[2])) 
th[[2]]$cprop[2]


#getting mean values
lavInspect(mod.fit.IIIa,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
alpha <- lavInspect(mod.fit.IIIa,what = 'est')$alpha
alpha


#getting cov values
lavInspect(mod.fit.IIIa,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
theta_var<- diag(lavInspect(mod.fit.IIIa,what = 'est')$psi)
theta_var





for(i in seq(1,5,1)){# i items
  item.a[i,"lavIIIa"] <- lambda[i,"lavIIIa"]*sqrt(theta_var)/sqrt(1-lambda[i,"lavIIIa"]*lambda[i,"lavIIIa"]*theta_var)*1.7
  item.d[i,"lavIIIa"] <- (-tau[i,"lavIIIa"]+lambda[i,"lavIIIa"]*alpha)/sqrt(1-lambda[i,"lavIIIa"]*lambda[i,"lavIIIa"]*theta_var)*1.7
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavIIIa))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavIIIa))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
mod2ind4lv1_parametros_IIIb <- function(){}


constraint.IIIb <- '

lmbd1==1
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

rm(mod.fit.IIIb)

mod.fit.IIIb <- lavaan(mod_noconstraints, 
                       data = dat, 
                       int.ov.free = TRUE,
                       int.lv.free = TRUE,
                       meanstructure = TRUE,
                       std.lv =FALSE,
                       auto.fix.first = TRUE,
                       auto.var = TRUE,
                       ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                       parameterization = "delta",
                       constraints = constraint.IIIb
)


summary(mod.fit.IIIb)
fitMeasures(mod.fit.IIIb)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

lambda[,"lavIIIb"] <- lavInspect(mod.fit.IIIb,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod.fit.IIIb,what = 'est')$tau
tau[,"lavIIIb"] <- lavInspect(mod.fit.IIIb,what = 'est')$tau
tau

#LRV mean and var
mu <- lavInspect(mod.fit.IIIb,what = 'mu')
vy <- lavInspect(mod.fit.IIIb,what = 'vy')


# checking if accumulated probabilities are accurate for  estimated/standardized LIR/LRV
pnorm(tau[1,"lavIIIb"],mean = mu[1],sd =sqrt(vy[1])) 
th[[1]]$cprop[2]

pnorm(tau[2,"lavIIIb"],mean = mu[2],sd =sqrt(vy[2])) 
th[[2]]$cprop[2]


#getting mean values
lavInspect(mod.fit.IIIb,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
alpha <- lavInspect(mod.fit.IIIb,what = 'est')$alpha
alpha


#getting cov values
lavInspect(mod.fit.IIIb,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
theta_var<- diag(lavInspect(mod.fit.IIIb,what = 'est')$psi)
theta_var



for(i in seq(1,5,1)){# i items
  item.a[i,"lavIIIb"] <- lambda[i,"lavIIIb"]*sqrt(theta_var)/sqrt(1-lambda[i,"lavIIIb"]*lambda[i,"lavIIIb"]*theta_var)*1.7
  item.d[i,"lavIIIb"] <- (-tau[i,"lavIIIb"]+lambda[i,"lavIIIb"]*alpha)/sqrt(1-lambda[i,"lavIIIb"]*lambda[i,"lavIIIb"]*theta_var)*1.7
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavIIIb))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN IIIb")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavIIIb))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN IIIb")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
mod2ind4lv1_parametros_IVa <- function(){}


constraint.IVa <- '

lmbd1==1
thr1 ==0

int1==0
int2==0
int3==0
int4==0
int5==0

1== var1
1== var2
1== var3
1== var4
1== var5
'

rm(mod.fit.IVa)

mod.fit.IVa <- lavaan(mod_noconstraints, 
                       data = dat, 
                       int.ov.free = TRUE,
                       int.lv.free = TRUE,
                       meanstructure = TRUE,
                       std.lv =FALSE,
                       auto.fix.first = TRUE,
                       auto.var = TRUE,
                       ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                       parameterization = "theta",
                       constraints = constraint.IVa
)
summary(mod.fit.IVa)
fitMeasures(mod.fit.IVa)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

#getting lambda values
lavInspect(mod.fit.IVa,what = 'est')$lambda
lambda[,"lavIVa"] <- lavInspect(mod.fit.IVa,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod.fit.IVa,what = 'est')$tau
tau[,"lavIVa"] <- lavInspect(mod.fit.IVa,what = 'est')$tau
tau

#LRV mean and var
mu <- lavInspect(mod.fit.IVa,what = 'mu')
vy <- lavInspect(mod.fit.IVa,what = 'vy')


# checking if accumulated probabilities are accurate for  estimated/standardized LIR/LRV
pnorm(tau[1,"lavIVa"],mean = mu[1],sd =sqrt(vy[1])) 
th[[1]]$cprop[2]

pnorm(tau[2,"lavIVa"],mean = mu[2],sd =sqrt(vy[2])) 
th[[2]]$cprop[2]


#getting mean values
lavInspect(mod.fit.IVa,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
alpha <- lavInspect(mod.fit.IVa,what = 'est')$alpha
alpha


#getting cov values
lavInspect(mod.fit.IVa,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
theta_var<- diag(lavInspect(mod.fit.IVa,what = 'est')$psi)
theta_var

for(i in seq(1,5,1)){# i items
  item.a[i,"lavIVa"] <- lambda[i,"lavIVa"]*sqrt(theta_var)*1.702
  item.d[i,"lavIVa"] <- (-tau[i,"lavIVa"]+lambda[i,"lavIVa"]*alpha)*1.702
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavIVa))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN IVa")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavIVa))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN IVa")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)




#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
mod2ind4lv1_parametros_IVb <- function(){}


constraint.IVb <- '

lmbd1==1
lv1mean==0

int1==0
int2==0
int3==0
int4==0
int5==0

1== var1
1== var2
1== var3
1== var4
1== var5
'

rm(mod.fit.IVb)

mod.fit.IVb <- lavaan(mod_noconstraints, 
                      data = dat, 
                      int.ov.free = TRUE,
                      int.lv.free = TRUE,
                      meanstructure = TRUE,
                      std.lv =FALSE,
                      auto.fix.first = TRUE,
                      auto.var = TRUE,
                      ordered = c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                      parameterization = "theta",
                      constraints = constraint.IVb
)
summary(mod.fit.IVb)
fitMeasures(mod.fit.IVb)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

#getting lambda values
lavInspect(mod.fit.IVb,what = 'est')$lambda
lambda[,"lavIVb"] <- lavInspect(mod.fit.IVb,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod.fit.IVb,what = 'est')$tau
tau[,"lavIVb"] <- lavInspect(mod.fit.IVb,what = 'est')$tau
tau

#LRV mean and var
mu <- lavInspect(mod.fit.IVb,what = 'mu')
vy <- lavInspect(mod.fit.IVb,what = 'vy')


# checking if accumulated probabilities are accurate for  estimated/standardized LIR/LRV
pnorm(tau[1,"lavIVb"],mean = mu[1],sd =sqrt(vy[1])) 
th[[1]]$cprop[2]

pnorm(tau[2,"lavIVb"],mean = mu[2],sd =sqrt(vy[2])) 
th[[2]]$cprop[2]


#getting mean values
lavInspect(mod.fit.IVb,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
alpha <- lavInspect(mod.fit.IVb,what = 'est')$alpha
alpha


#getting cov values
lavInspect(mod.fit.IVb,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
theta_var<- diag(lavInspect(mod.fit.IVb,what = 'est')$psi)
theta_var

for(i in seq(1,5,1)){# i items
  item.a[i,"lavIVb"] <- lambda[i,"lavIVb"]*sqrt(theta_var)*1.702
  item.d[i,"lavIVb"] <- (-tau[i,"lavIVb"]+lambda[i,"lavIVb"]*alpha)*1.702
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavIVb))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("EstimatIVbs de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN IVb")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavIVb))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN IVb")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# offset coding
# std.lv = FALSE

mod2ind4lv1_parametros_Va <- function(){}

constraint.Va <- '

lmbd1+lmbd2+lmbd3+lmbd4+lmbd5==5
thr1==0

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

cat(mod_noconstraints)


mod.fit.Va <- lavaan(mod_noconstraints, 
                    data = dat, 
                    int.ov.free = TRUE, # intercepts
                    int.lv.free = FALSE,
                    std.lv =FALSE,
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
                    constraints = constraint.Va)

summary(mod.fit.Va)
fitmeasures(mod.fit.Va)[c("tli","cfi","rmsea")]
fitMeasures(mod.fit.Va)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

lambda[,"lavVa"] <- lavInspect(mod.fit.Va,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod.fit.Va,what = 'est')$tau
tau[,"lavVa"] <- lavInspect(mod.fit.Va,what = 'est')$tau
tau

#LRV mean and var
mu <- lavInspect(mod.fit.Va,what = 'mu')
vy <- lavInspect(mod.fit.Va,what = 'vy')




#getting mean values
lavInspect(mod.fit.IIIa,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
alpha <- lavInspect(mod.fit.Va,what = 'est')$alpha
alpha


#getting cov values
lavInspect(mod.fit.Va,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
theta_var<- diag(lavInspect(mod.fit.Va,what = 'est')$psi)
theta_var





for(i in seq(1,5,1)){# i items
  item.a[i,"lavVa"] <- lambda[i,"lavVa"]*sqrt(theta_var)/sqrt(1-lambda[i,"lavVa"]*lambda[i,"lavVa"]*theta_var)*1.7
  item.d[i,"lavVa"] <- (-tau[i,"lavVa"]+lambda[i,"lavVa"]*alpha)/sqrt(1-lambda[i,"lavVa"]*lambda[i,"lavVa"]*theta_var)*1.7
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavVa))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavVa))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# offset coding
# std.lv = FALSE

mod2ind4lv1_parametros_Vb <- function(){}

constraint.Vb <- '

lmbd1+lmbd2+lmbd3+lmbd4+lmbd5 ==5
thr1+thr2+thr3+thr4+thr5 ==0

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

cat(mod_noconstraints)


mod.fit.Vb <- lavaan(mod_noconstraints, 
                     data = dat, 
                     int.ov.free = TRUE, # intercepts
                     int.lv.free = FALSE,
                     std.lv =FALSE,
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
                     constraints = constraint.Vb)

summary(mod.fit.Vb)
fitmeasures(mod.fit.Vb)[c("tli","cfi","rmsea")]
fitMeasures(mod.fit.Vb)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

lambda[,"lavVb"] <- lavInspect(mod.fit.Vb,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod.fit.Vb,what = 'est')$tau
tau[,"lavVb"] <- lavInspect(mod.fit.Vb,what = 'est')$tau
tau

#LRV mean and var
mu <- lavInspect(mod.fit.Vb,what = 'mu')
vy <- lavInspect(mod.fit.Vb,what = 'vy')


#getting mean values
lavInspect(mod.fit.Vb,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
alpha <- lavInspect(mod.fit.Vb,what = 'est')$alpha
alpha


#getting cov values
lavInspect(mod.fit.Vb,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
theta_var<- diag(lavInspect(mod.fit.Vb,what = 'est')$psi)
theta_var





for(i in seq(1,5,1)){# i items
  item.a[i,"lavVb"] <- lambda[i,"lavVb"]*sqrt(theta_var)/sqrt(1-lambda[i,"lavVb"]*lambda[i,"lavVb"]*theta_var)*1.7
  item.d[i,"lavVb"] <- (-tau[i,"lavVb"]+lambda[i,"lavVb"]*alpha)/sqrt(1-lambda[i,"lavVb"]*lambda[i,"lavVb"]*theta_var)*1.7
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavVb))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavVb))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)


mod2ind4lv1_parametros_VIa <- function(){}

constraint.VIa <- '

lmbd1+lmbd2+lmbd3+lmbd4+lmbd5==5
thr1==0

int1==0
int2==0
int3==0
int4==0
int5==0

1== var1
1== var2
1== var3
1== var4
1== var5
'

cat(mod_noconstraints)


mod.fit.VIa <- lavaan(mod_noconstraints, 
                     data = dat, 
                     int.ov.free = TRUE, # intercepts
                     int.lv.free = FALSE,
                     std.lv =FALSE,
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
                     parameterization = "theta",
                     constraints = constraint.VIa)

summary(mod.fit.VIa)
fitmeasures(mod.fit.VIa)[c("tli","cfi","rmsea")]
fitMeasures(mod.fit.VIa)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

lambda[,"lavVIa"] <- lavInspect(mod.fit.VIa,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod.fit.IIIa,what = 'est')$tau
tau[,"lavVIa"] <- lavInspect(mod.fit.VIa,what = 'est')$tau
tau

#LRV mean and var
mu <- lavInspect(mod.fit.VIa,what = 'mu')
vy <- lavInspect(mod.fit.VIa,what = 'vy')



#getting mean values
lavInspect(mod.fit.VIa,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
alpha <- lavInspect(mod.fit.VIa,what = 'est')$alpha
alpha


#getting cov values
lavInspect(mod.fit.VIa,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
theta_var<- diag(lavInspect(mod.fit.VIa,what = 'est')$psi)
theta_var





for(i in seq(1,5,1)){# i items
  item.a[i,"lavVIa"] <- lambda[i,"lavVIa"]*sqrt(theta_var)/sqrt(1-lambda[i,"lavVIa"]*lambda[i,"lavVIa"]*theta_var)*1.7
  item.d[i,"lavVIa"] <- (-tau[i,"lavVIa"]+lambda[i,"lavVIa"]*alpha)/sqrt(1-lambda[i,"lavVIa"]*lambda[i,"lavVIa"]*theta_var)*1.7
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavVIa))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavVIa))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# offset coding
# std.lv = FALSE

mod2ind4lv1_parametros_VIb <- function(){}

constraint.VIb <- '

lmbd1+lmbd2+lmbd3+lmbd4+lmbd5 ==5
thr1+thr2+thr3+thr4+thr5 ==0

int1==0
int2==0
int3==0
int4==0
int5==0

1== var1
1== var2
1== var3
1== var4
1== var5
'

cat(mod_noconstraints)


mod.fit.VIb <- lavaan(mod_noconstraints, 
                     data = dat, 
                     int.ov.free = TRUE, # intercepts
                     int.lv.free = FALSE,
                     std.lv =FALSE,
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
                     parameterization = "theta",
                     constraints = constraint.VIb)

summary(mod.fit.VIb)
fitmeasures(mod.fit.VIb)[c("tli","cfi","rmsea")]
fitMeasures(mod.fit.VIb)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

lambda[,"lavVIb"] <- lavInspect(mod.fit.VIb,what = 'est')$lambda
lambda

#getting tau values
lavInspect(mod.fit.VIb,what = 'est')$tau
tau[,"lavVIb"] <- lavInspect(mod.fit.VIb,what = 'est')$tau
tau

#LRV mean and var
mu <- lavInspect(mod.fit.VIb,what = 'mu')
vy <- lavInspect(mod.fit.VIb,what = 'vy')


#getting mean values
lavInspect(mod.fit.VIb,what = 'est')$alpha # same as lavInspect(mod2ind4lv1.fit,what = "mean.lv")
alpha <- lavInspect(mod.fit.VIb,what = 'est')$alpha
alpha


#getting cov values
lavInspect(mod.fit.VIb,what = 'est')$psi # same as lavInspect(mod2ind4lv1.fit,what = "cov.lv")
theta_var<- diag(lavInspect(mod.fit.VIb,what = 'est')$psi)
theta_var





for(i in seq(1,5,1)){# i items
  item.a[i,"lavVIb"] <- lambda[i,"lavVIb"]*sqrt(theta_var)/sqrt(1-lambda[i,"lavVIb"]*lambda[i,"lavVIb"]*theta_var)*1.7
  item.d[i,"lavVIb"] <- (-tau[i,"lavVIb"]+lambda[i,"lavVIb"]*alpha)/sqrt(1-lambda[i,"lavVIb"]*lambda[i,"lavVIb"]*theta_var)*1.7
}

item.a
item.d
lambda
tau

a <- ggplot(data=data.frame(item.a), aes(x=mirt,y=lavVIb))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de a")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

b<- ggplot(data=data.frame(item.d), aes(x=mirt,y=lavVIb))+
  geom_point()+
  geom_abline(slope=1,intercept = 0)+geom_text(aes(label=label, hjust = 0.5,  vjust = -1))+
  ggtitle("Estimativas de d")+ 
  xlab("MIRT")+
  ylab("LAVAAN")+ coord_fixed()+theme_bw()

cowplot::plot_grid(a,b,nrow=1)

lavInspect(mod.fit.I,what = 'Sigma')
lavInspect(mod.fit.II,what = 'Sigma')
lavInspect(mod.fit.IIIa,what = 'Sigma')
lavInspect(mod.fit.IIIb,what = 'Sigma')
lavInspect(mod.fit.IVa,what = 'Sigma')
lavInspect(mod.fit.IVb,what = 'Sigma')
lavInspect(mod.fit.Va,what = 'Sigma')
lavInspect(mod.fit.Vb,what = 'Sigma')
lavInspect(mod.fit.VIa,what = 'Sigma')
lavInspect(mod.fit.VIb,what = 'Sigma')

