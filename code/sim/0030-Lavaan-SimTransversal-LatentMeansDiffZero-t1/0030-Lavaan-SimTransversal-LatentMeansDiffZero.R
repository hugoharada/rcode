
rm(list=ls())
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 
if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(GPArotation)) install.packages("GPArotation"); library(GPArotation)
if(!require(pryr)) install.packages("pryr"); library(pryr)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)

###Generate person parameters
set.seed(1) # Resetando a semente
N <- 10000 ## subjects
I= 70  # Number of Items
PL=2 # Logistic Model (1,2,3 parameters)
SigmaType <- 1 # 0 = Covariance Uniform, 1 = Covariância AR1, 2 =  Covariância de bandas
rho<-0.7


coefs <- matrix(ncol=6,nrow=I)
colnames(coefs)=c("a1","b1","c1","a2","b2","c2")

if (PL==1) {a = rep(1,I)} else {a = runif(I,0.5, 2.5)}    # U(0.5 , 2.5)
b = runif(I,-2, 2.0)     # U(-2 , 2)
if (PL<=2) {c = rep(0,I)} else{c = runif(I,0.0, 0.3) } # U(0 , 0.3)
  
d=-a*b # MIRT trabalha com o intercepto (d=-ab) e não com a dificuldade (b)

Theta = matrix(0,N,1)

Theta[,1] <- mvrnorm(n=N, mu=0.5,1 )

mean(Theta)
cov(Theta)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# momento 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eta  = Theta[,1]%*% t(a[1:30]) +  matrix(d[1:30],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[1:30] + (1-c[1:30])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U1 = 1*(X<P)  ; U1=as.data.frame(U1) # AQUI TEMOS OS DADOS DICOTÔMICOS
colnames(U1)=paste0("Item",1:30)

rm(P,X,eta)


head(U1)

p1<-paste("lambda",1:30,"*Item",1:30,sep="",collapse = " + ")
p1<-paste("d1 =~",p1,sep="",collapse = "")
p1
cat(paste("Item",1:30," | tlambda",1:30,"*t1",sep="",collapse = "\n"))




lavaan.model.std.lv.false <-'

d1 =~lambda1*Item1 + lambda2*Item2 + lambda3*Item3 + lambda4*Item4 + lambda5*Item5 + lambda6*Item6 + lambda7*Item7 + lambda8*Item8 + lambda9*Item9 + lambda10*Item10 + lambda11*Item11 + lambda12*Item12 + lambda13*Item13 + lambda14*Item14 + lambda15*Item15 + lambda16*Item16 + lambda17*Item17 + lambda18*Item18 + lambda19*Item19 + lambda20*Item20 + lambda21*Item21 + lambda22*Item22 + lambda23*Item23 + lambda24*Item24 + lambda25*Item25 + lambda26*Item26 + lambda27*Item27 + lambda28*Item28 + lambda29*Item29 + lambda30*Item30

Item1 | tlambda1*t1
Item2 | tlambda2*t1
Item3 | tlambda3*t1
Item4 | tlambda4*t1
Item5 | tlambda5*t1
Item6 | tlambda6*t1
Item7 | tlambda7*t1
Item8 | tlambda8*t1
Item9 | tlambda9*t1
Item10 | tlambda10*t1
Item11 | tlambda11*t1
Item12 | tlambda12*t1
Item13 | tlambda13*t1
Item14 | tlambda14*t1
Item15 | tlambda15*t1
Item16 | tlambda16*t1
Item17 | tlambda17*t1
Item18 | tlambda18*t1
Item19 | tlambda19*t1
Item20 | tlambda20*t1
Item21 | tlambda21*t1
Item22 | tlambda22*t1
Item23 | tlambda23*t1
Item24 | tlambda24*t1
Item25 | tlambda25*t1
Item26 | tlambda26*t1
Item27 | tlambda27*t1
Item28 | tlambda28*t1
Item29 | tlambda29*t1
Item30 | tlambda30*t1

'


lavaan.model.fit <- lavaan(lavaan.model.std.lv.false, data = U1 , 
                        std.lv=FALSE,
                        parameterization="delta",
                        meanstructure = TRUE,
                        int.lv.free	=TRUE)
lavOptions()


summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)
fitMeasures(lavaan.model.fit)['tli']
fitMeasures(lavaan.model.fit)['cfi']
fitMeasures(lavaan.model.fit)['rmsea']

lavInspect(lavaan.model.fit,what = "cov.lv")
lavInspect(lavaan.model.fit,what = "mean.lv")
lavInspect(lavaan.model.fit,what = "partable")
lavInspect(lavaan.model.fit,what = "est")
parameterTable(lavaan.model.fit)
View(parameterTable(lavaan.model.fit))
lavInspect(lavaan.model.fit,what = "cov.ov")
lavInspect(lavaan.model.fit,what = "mean.ov")


#getting lambda values
lavInspect(lavaan.model.fit,what = 'est')$lambda
lambda2 <- lavInspect(lavaan.model.fit,what = 'est')$lambda
colnames(lambda2) <- "lambda1"

#getting tau values
lavInspect(lavaan.model.fit,what = 'est')$tau
tau <- lavInspect(lavaan.model.fit,what = 'est')$tau
colnames(tau) <- c("tau")

#getting mean values
lavInspect(lavaan.model.fit,what = 'est')$alpha # same as lavInspect(lavaan.model.fit,what = "mean.lv")
mu <- lavInspect(lavaan.model.fit,what = 'est')$alpha
mu
#getting cov values
lavInspect(lavaan.model.fit,what = 'est')$psi # same as lavInspect(lavaan.model.fit,what = "cov.lv")
theta_var<- diag(lavInspect(lavaan.model.fit,what = 'est')$psi)
theta_var

#calculating parameters.
item.par.sim <- matrix(0,90,4)
colnames(item.par.sim) <- c("aj_t1_lav","aj_t2_lav","aj_t3_lav","dj_lav")



#lavaan.model.fit <- cfa(lavaan.model, data = U , 
#                        std.lv=TRUE,
#                        parameterization="delta")
for(i in seq(1,90,1)){# i items
  for(j in c(1,2,3)){ # j moments
    item.par.sim[i,j] <- lambda2[i,j]/sqrt(1-t(lambda2[i,])%*%lambda2[i,])*1.7
  }
  item.par.sim[i,4] <- tau[i]/sqrt(1-t(lambda2[i,])%*%lambda2[i,])*1.7
}

item.par.sim

plot(a[1:30],item.par.sim[1:30,1], 
     main = "Estimativas de a - momento 1 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(d[1:30],-item.par.sim[1:30,4], 
     main = "Estimativas de d - momento 1 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

data<-cbind(d[1:30],-item.par.sim[1:30,4])
colnames(data)=c("real","est")

reg1 <- lm(est~real,data=data.frame(data) )
summary(reg1)
abline(reg1)



plot(a[21:50],item.par.sim[31:60,2], 
     main = "Estimativas de a - momento 2 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(d[21:50],-item.par.sim[31:60,4], 
     main = "Estimativas de d - momento 2 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)


data<-cbind(d[21:50],-item.par.sim[31:60,4])
colnames(data)=c("real","est")

reg1 <- lm(est~real,data=data.frame(data) )
summary(reg1)
abline(reg1)


plot(a[41:70],item.par.sim[61:90,3], 
     main = "Estimativas de a - momento 3 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(d[41:70],-item.par.sim[61:90,4], 
     main = "Estimativas de d  - momento 3 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

data<-cbind(d[41:70],-item.par.sim[61:90,4])
colnames(data)=c("real","est")

reg1 <- lm(est~real,data=data.frame(data) )
summary(reg1)
abline(reg1)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%  REPRODUÇÃO DAS PROFICIÊNCIAS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# use pars="values" to get the parameters table to set the items parameters values starting values. 
# in column "value" set the starting value
# in column "est" set to FALSE to disable estimation. If set to TRUE the parameter will be estimated.

# momento 1
parameters = mirt(U1, 1, itemtype = '2PL', pars = "values")
str(parameters)

parameters$value[parameters$name=="a1"] #list all lines labed "a1"
parameters$value[parameters$name=="a1"] <- item.par.sim[1:30,1]
parameters$est[parameters$name=="a1"] <- FALSE
parameters$value[parameters$name=="d"] #list all lines labed "d"
parameters$value[parameters$name=="d"] <- item.par.sim[1:30,4]
parameters$est[parameters$name=="d"] <- FALSE

mirt.2PL = mirt(U1, 1, itemtype = '2PL', pars = parameters)
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
profic1 = fscores(mirt.2PL) #estimativas das proficiências individuais
mean(profic1)
sd(profic1)
plot(Theta[,1],profic1, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")


# momento 2
parameters = mirt(U2, 1, itemtype = '2PL', pars = "values")
str(parameters)

parameters$value[parameters$name=="a1"] #list all lines labed "a1"
parameters$value[parameters$name=="a1"] <- item.par.sim[31:60,2]
parameters$est[parameters$name=="a1"] <- FALSE
parameters$value[parameters$name=="d"] #list all lines labed "d"
parameters$value[parameters$name=="d"] <- item.par.sim[31:60,4]
parameters$est[parameters$name=="d"] <- FALSE

mirt.2PL = mirt(U2, 1, itemtype = '2PL', pars = parameters)
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
profic2 = fscores(mirt.2PL) #estimativas das proficiências individuais
mean(profic2)
sd(profic2)
plot(Theta[,2],profic2, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

# momento 3
parameters = mirt(U3, 1, itemtype = '2PL', pars = "values")
str(parameters)

parameters$value[parameters$name=="a1"] #list all lines labed "a1"
parameters$value[parameters$name=="a1"] <- item.par.sim[61:90,3]
parameters$est[parameters$name=="a1"] <- FALSE
parameters$value[parameters$name=="d"] #list all lines labed "d"
parameters$value[parameters$name=="d"] <- item.par.sim[61:90,4]
parameters$est[parameters$name=="d"] <- FALSE

mirt.2PL = mirt(U3, 1, itemtype = '2PL', pars = parameters)
#PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
profic3 = fscores(mirt.2PL) #estimativas das proficiências individuais
mean(profic3)
sd(profic3)
plot(Theta[,3],profic3, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

