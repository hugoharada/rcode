
rm(list=ls())
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 
if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(GPArotation)) install.packages("GPArotation"); library(GPArotation)
if(!require(pryr)) install.packages("pryr"); library(pryr)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)


niter<-3 # número de iterações


###Generate person parameters
## subjects
N <- 1000
#theta <- rnorm(N,0,1)


set.seed(1) # Resetando a semente

I= 70  # Number of Items
N = 30*334

PL=2 # Logistic Model (1,2,3 parameters)
coefs <- matrix(ncol=6,nrow=I)
colnames(coefs)=c("a1","b1","c1","a2","b2","c2")

if (PL==1) {a = rep(1,I)} else {a = runif(I,0.5, 2.5)}    # U(0.5 , 2.5)
b = runif(I,-2, 2.0)     # U(-2 , 2)
if (PL<=2) {c = rep(0,I)} else{c = runif(I,0.0, 0.3) } # U(0 , 0.3)
  
d=-a*b # MIRT trabalha com o intercepto (d=-ab) e não com a dificuldade (b)

rho<-0.7


#Matriz de Covariância Uniforme
Sigma <- lazyCor(c(rho,rho,rho))

#Matriz de Covariância AR(1)
Sigma = lazyCor(c(rho,rho*rho,rho))

#Matriz de Covariância de bandas
Sigma = lazyCor(c(rho,0,0))


Sigma


Theta <- mvrnorm(n=N, mu=c(0,0,0), Sigma )
head(Theta)
cor(Theta)
summary(Theta)
var(Theta)
dim(Theta)


for(iter in 1:niter)
{ 
  
}#for(iter in 1:niter){ 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# momento 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eta  = Theta[,1]%*% t(a[1:30]) +  matrix(d[1:30],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[1:30] + (1-c[1:30])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U1 = 1*(X<P)  ; U1=as.data.frame(U1) # AQUI TEMOS OS DADOS DICOTÔMICOS
colnames(U1)=paste0("Item.",1:30,".t1")

rm(P,X,eta)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ESTIMAÇÃO PELO MIRT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mirt.2PL = mirt(U1, 1, itemtype = '2PL')  # 
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
profic = fscores(mirt.2PL) #estimativas das proficiências individuais
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%  TRANSFORMAÇÃO PARA OBTER A DIFICULDADE (b) %%%%%%%%%%%%%%%%%%%%%%%%
PAR=cbind(PAR, -PAR[,2]/PAR[,1]) # Coloquei o "b"na última coluna
colnames(PAR)=c("a","d","c","b")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%% REPRODUÇÃO DOS PARÂMETROS DOS ITENS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(a[1:30],PAR[,1], main="Recuperação dos parâmetros de discriminação", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(b[1:30],PAR[,4], main="Recuperação dos parâmetros de dificuldade",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%  REPRODUÇÃO DAS PROFICIÊNCIAS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(Theta[,1],profic, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
hist(profic, main="Recuperação das proficiências", xlab="Estimativas",ylab="Frequência")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# momento 2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


eta  = Theta[,2]%*% t(a[21:50]) +  matrix(d[21:50],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[21:50] + (1-c[21:50])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U2 = 1*(X<P)  ; U2=as.data.frame(U2) # AQUI TEMOS OS DADOS DICOTÔMICOS
colnames(U2)=paste0("Item.",21:50,".t2")

rm(P,X,eta)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ESTIMAÇÃO PELO MIRT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mirt.2PL = mirt(U2, 1, itemtype = '2PL')  # 
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
profic = fscores(mirt.2PL) #estimativas das proficiências individuais
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%  TRANSFORMAÇÃO PARA OBTER A DIFICULDADE (b) %%%%%%%%%%%%%%%%%%%%%%%%
PAR=cbind(PAR, -PAR[,2]/PAR[,1]) # Coloquei o "b"na última coluna
colnames(PAR)=c("a","d","c","b")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%% REPRODUÇÃO DOS PARÂMETROS DOS ITENS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(a[21:50],PAR[,1], main="Recuperação dos parâmetros de discriminação", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(b[21:50],PAR[,4], main="Recuperação dos parâmetros de dificuldade",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%  REPRODUÇÃO DAS PROFICIÊNCIAS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(Theta[,2],profic, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
hist(profic, main="Recuperação das proficiências", xlab="Estimativas",ylab="Frequência")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# momento 3
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


eta  = Theta[,3]%*% t(a[41:70]) +  matrix(d[41:70],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[41:70] + (1-c[41:70])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U3 = 1*(X<P)  ; U3=as.data.frame(U3) # AQUI TEMOS OS DADOS DICOTÔMICOS
colnames(U3)=paste0("Item.",41:70,".t3")

rm(P,X,eta)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ESTIMAÇÃO PELO MIRT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mirt.2PL = mirt(U3, 1, itemtype = '2PL')  # 
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
profic = fscores(mirt.2PL) #estimativas das proficiências individuais
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%  TRANSFORMAÇÃO PARA OBTER A DIFICULDADE (b) %%%%%%%%%%%%%%%%%%%%%%%%
PAR=cbind(PAR, -PAR[,2]/PAR[,1]) # Coloquei o "b"na última coluna
colnames(PAR)=c("a","d","c","b")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%% REPRODUÇÃO DOS PARÂMETROS DOS ITENS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(a[41:70],PAR[,1], main="Recuperação dos parâmetros de discriminação", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(b[41:70],PAR[,4], main="Recuperação dos parâmetros de dificuldade",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%  REPRODUÇÃO DAS PROFICIÊNCIAS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(Theta[,3],profic, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
hist(profic, main="Recuperação das proficiências", xlab="Estimativas",ylab="Frequência")




head(U1)
head(U2)
head(U3)
U<-cbind(U1,U2,U3)
head(U)


p1<-paste("lambda",1:30,".t1*Item.",1:30,".t1",sep="",collapse = " + ")
p1<-paste("d1 =~",p1,sep="",collapse = "")
p2<-paste("lambda",21:50,".t2*Item.",21:50,".t2",sep="",collapse = " + ")
p2<-paste("d2 =~",p2,sep="",collapse = "")
p3<-paste("lambda",41:70,".t3*Item.",41:70,".t3",sep="",collapse = " + ")
p3<-paste("d3 =~",p3,sep="",collapse = "")

cat(paste("Item.",1:30,".t1 | tlambda",1:30,".t1*t1",sep="",collapse = "\n"))
cat(paste("Item.",21:50,".t2 | tlambda",21:50,".t2*t1",sep="",collapse = "\n"))
cat(paste("Item.",41:70,".t3 | tlambda",41:70,".t3*t1",sep="",collapse = "\n"))

cat(paste("Item.",21:30,".t1 ~~ Item.",21:30,".t2" ,sep="",collapse = "\n"))
cat(paste("Item.",41:50,".t2 ~~ Item.",41:50,".t3" ,sep="",collapse = "\n"))

lavaan.model <-'

d1 =~ lambda1.t1*Item.1.t1+ lambda2.t1*Item.2.t1+ lambda3.t1*Item.3.t1+ lambda4.t1*Item.4.t1+ lambda5.t1*Item.5.t1+ lambda6.t1*Item.6.t1+ lambda7.t1*Item.7.t1+ lambda8.t1*Item.8.t1+ lambda9.t1*Item.9.t1+ lambda10.t1*Item.10.t1+ lambda11.t1*Item.11.t1+ lambda12.t1*Item.12.t1+ lambda13.t1*Item.13.t1+ lambda14.t1*Item.14.t1+ lambda15.t1*Item.15.t1+ lambda16.t1*Item.16.t1+ lambda17.t1*Item.17.t1+ lambda18.t1*Item.18.t1+ lambda19.t1*Item.19.t1+ lambda20.t1*Item.20.t1+ lambda21.t1*Item.21.t1+ lambda22.t1*Item.22.t1+ lambda23.t1*Item.23.t1+ lambda24.t1*Item.24.t1+ lambda25.t1*Item.25.t1+ lambda26.t1*Item.26.t1+ lambda27.t1*Item.27.t1+ lambda28.t1*Item.28.t1+ lambda29.t1*Item.29.t1+ lambda30.t1*Item.30.t1
d2 =~ lambda21.t2*Item.21.t2+ lambda22.t2*Item.22.t2+ lambda23.t2*Item.23.t2+ lambda24.t2*Item.24.t2+ lambda25.t2*Item.25.t2+ lambda26.t2*Item.26.t2+ lambda27.t2*Item.27.t2+ lambda28.t2*Item.28.t2+ lambda29.t2*Item.29.t2+ lambda30.t2*Item.30.t2+ lambda31.t2*Item.31.t2+ lambda32.t2*Item.32.t2+ lambda33.t2*Item.33.t2+ lambda34.t2*Item.34.t2+ lambda35.t2*Item.35.t2+ lambda36.t2*Item.36.t2+ lambda37.t2*Item.37.t2+ lambda38.t2*Item.38.t2+ lambda39.t2*Item.39.t2+ lambda40.t2*Item.40.t2+ lambda41.t2*Item.41.t2+ lambda42.t2*Item.42.t2+ lambda43.t2*Item.43.t2+ lambda44.t2*Item.44.t2+ lambda45.t2*Item.45.t2+ lambda46.t2*Item.46.t2+ lambda47.t2*Item.47.t2+ lambda48.t2*Item.48.t2+ lambda49.t2*Item.49.t2+ lambda50.t2*Item.50.t2
d3 =~ lambda41.t3*Item.41.t3 + lambda42.t3*Item.42.t3 + lambda43.t3*Item.43.t3 + lambda44.t3*Item.44.t3 + lambda45.t3*Item.45.t3 + lambda46.t3*Item.46.t3 + lambda47.t3*Item.47.t3 + lambda48.t3*Item.48.t3 + lambda49.t3*Item.49.t3 + lambda50.t3*Item.50.t3 + lambda51.t3*Item.51.t3 + lambda52.t3*Item.52.t3 + lambda53.t3*Item.53.t3 + lambda54.t3*Item.54.t3 + lambda55.t3*Item.55.t3 + lambda56.t3*Item.56.t3 + lambda57.t3*Item.57.t3 + lambda58.t3*Item.58.t3 + lambda59.t3*Item.59.t3 + lambda60.t3*Item.60.t3 + lambda61.t3*Item.61.t3 + lambda62.t3*Item.62.t3 + lambda63.t3*Item.63.t3 + lambda64.t3*Item.64.t3 + lambda65.t3*Item.65.t3 + lambda66.t3*Item.66.t3 + lambda67.t3*Item.67.t3 + lambda68.t3*Item.68.t3 + lambda69.t3*Item.69.t3 + lambda70.t3*Item.70.t3

d1~~d2
d2~~d3
d1~~d3

Item.21.t1 ~~ Item.21.t2
Item.22.t1 ~~ Item.22.t2
Item.23.t1 ~~ Item.23.t2
Item.24.t1 ~~ Item.24.t2
Item.25.t1 ~~ Item.25.t2
Item.26.t1 ~~ Item.26.t2
Item.27.t1 ~~ Item.27.t2
Item.28.t1 ~~ Item.28.t2
Item.29.t1 ~~ Item.29.t2
Item.30.t1 ~~ Item.30.t2

Item.41.t2 ~~ Item.41.t3
Item.42.t2 ~~ Item.42.t3
Item.43.t2 ~~ Item.43.t3
Item.44.t2 ~~ Item.44.t3
Item.45.t2 ~~ Item.45.t3
Item.46.t2 ~~ Item.46.t3
Item.47.t2 ~~ Item.47.t3
Item.48.t2 ~~ Item.48.t3
Item.49.t2 ~~ Item.49.t3
Item.50.t2 ~~ Item.50.t3


Item.1.t1 | tlambda1.t1*t1
Item.2.t1 | tlambda2.t1*t1
Item.3.t1 | tlambda3.t1*t1
Item.4.t1 | tlambda4.t1*t1
Item.5.t1 | tlambda5.t1*t1
Item.6.t1 | tlambda6.t1*t1
Item.7.t1 | tlambda7.t1*t1
Item.8.t1 | tlambda8.t1*t1
Item.9.t1 | tlambda9.t1*t1
Item.10.t1 | tlambda10.t1*t1
Item.11.t1 | tlambda11.t1*t1
Item.12.t1 | tlambda12.t1*t1
Item.13.t1 | tlambda13.t1*t1
Item.14.t1 | tlambda14.t1*t1
Item.15.t1 | tlambda15.t1*t1
Item.16.t1 | tlambda16.t1*t1
Item.17.t1 | tlambda17.t1*t1
Item.18.t1 | tlambda18.t1*t1
Item.19.t1 | tlambda19.t1*t1
Item.20.t1 | tlambda20.t1*t1
Item.21.t1 | tlambda21.t1*t1
Item.22.t1 | tlambda22.t1*t1
Item.23.t1 | tlambda23.t1*t1
Item.24.t1 | tlambda24.t1*t1
Item.25.t1 | tlambda25.t1*t1
Item.26.t1 | tlambda26.t1*t1
Item.27.t1 | tlambda27.t1*t1
Item.28.t1 | tlambda28.t1*t1
Item.29.t1 | tlambda29.t1*t1
Item.30.t1 | tlambda30.t1*t1

Item.21.t2 | tlambda21.t2*t1
Item.22.t2 | tlambda22.t2*t1
Item.23.t2 | tlambda23.t2*t1
Item.24.t2 | tlambda24.t2*t1
Item.25.t2 | tlambda25.t2*t1
Item.26.t2 | tlambda26.t2*t1
Item.27.t2 | tlambda27.t2*t1
Item.28.t2 | tlambda28.t2*t1
Item.29.t2 | tlambda29.t2*t1
Item.30.t2 | tlambda30.t2*t1
Item.31.t2 | tlambda31.t2*t1
Item.32.t2 | tlambda32.t2*t1
Item.33.t2 | tlambda33.t2*t1
Item.34.t2 | tlambda34.t2*t1
Item.35.t2 | tlambda35.t2*t1
Item.36.t2 | tlambda36.t2*t1
Item.37.t2 | tlambda37.t2*t1
Item.38.t2 | tlambda38.t2*t1
Item.39.t2 | tlambda39.t2*t1
Item.40.t2 | tlambda40.t2*t1
Item.41.t2 | tlambda41.t2*t1
Item.42.t2 | tlambda42.t2*t1
Item.43.t2 | tlambda43.t2*t1
Item.44.t2 | tlambda44.t2*t1
Item.45.t2 | tlambda45.t2*t1
Item.46.t2 | tlambda46.t2*t1
Item.47.t2 | tlambda47.t2*t1
Item.48.t2 | tlambda48.t2*t1
Item.49.t2 | tlambda49.t2*t1
Item.50.t2 | tlambda50.t2*t1

Item.41.t3 | tlambda41.t3*t1
Item.42.t3 | tlambda42.t3*t1
Item.43.t3 | tlambda43.t3*t1
Item.44.t3 | tlambda44.t3*t1
Item.45.t3 | tlambda45.t3*t1
Item.46.t3 | tlambda46.t3*t1
Item.47.t3 | tlambda47.t3*t1
Item.48.t3 | tlambda48.t3*t1
Item.49.t3 | tlambda49.t3*t1
Item.50.t3 | tlambda50.t3*t1
Item.51.t3 | tlambda51.t3*t1
Item.52.t3 | tlambda52.t3*t1
Item.53.t3 | tlambda53.t3*t1
Item.54.t3 | tlambda54.t3*t1
Item.55.t3 | tlambda55.t3*t1
Item.56.t3 | tlambda56.t3*t1
Item.57.t3 | tlambda57.t3*t1
Item.58.t3 | tlambda58.t3*t1
Item.59.t3 | tlambda59.t3*t1
Item.60.t3 | tlambda60.t3*t1
Item.61.t3 | tlambda61.t3*t1
Item.62.t3 | tlambda62.t3*t1
Item.63.t3 | tlambda63.t3*t1
Item.64.t3 | tlambda64.t3*t1
Item.65.t3 | tlambda65.t3*t1
Item.66.t3 | tlambda66.t3*t1
Item.67.t3 | tlambda67.t3*t1
Item.68.t3 | tlambda68.t3*t1
Item.69.t3 | tlambda69.t3*t1
Item.70.t3 | tlambda70.t3*t1

'

lavaan.model.fit <- cfa(lavaan.model, data = U , std.lv=TRUE )
summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)

#library("semPlot")
#semPaths(lavaan.model.fit,title=FALSE, curvePivot = TRUE)
#semPaths(lavaan.model.fit,"std",edge.label.cex=0.5, curvePivot = TRUE)
#semPaths(lavaan.model.fit,"std",edge.label.cex=0.5, curvePivot = TRUE, intercepts=FALSE)


#getting lambda values
lavInspect(lavaan.model.fit,what = 'est')$lambda
lambda2 <- lavInspect(lavaan.model.fit,what = 'est')$lambda
colnames(lambda2) <- c("lambda1","lambda2")

#getting tau values
lavInspect(lavaan.model.fit,what = 'est')$tau
tau <- lavInspect(lavaan.model.fit,what = 'est')$tau
colnames(tau) <- c("tau")


item.par.sim <- matrix(0,90,4)
colnames(item.par.sim) <- c("aj_t1_lav","aj_t2_lav","aj_t3_lav","dj_lav")

for(i in seq(1,90,1)){
  for(j in c(1,2,3)){
    item.par.sim[i,j] <- lambda2[i,j]/sqrt(1-t(lambda2[i,])%*%lambda2[i,])*1.7
  }
  item.par.sim[i,4] <- tau[i]/sqrt(1-t(lambda2[i,])%*%lambda2[i,])*1.7
}


item.par.sim

plot(a[1:30],item.par.sim[1:30,1], 
     main = "Estimativas de a - momento 1 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(d[1:30],-item.par.sim[1:30,4], 
     main = "Estimativas de d - momento 1 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)


plot(a[21:50],item.par.sim[31:60,2], 
     main = "Estimativas de a - momento 2 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(d[21:50],-item.par.sim[31:60,4], 
     main = "Estimativas de d - momento 2 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(a[41:70],item.par.sim[61:90,3], 
     main = "Estimativas de a - momento 3 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(d[41:70],-item.par.sim[61:90,4], 
     main = "Estimativas de d  - momento 3 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)


# model <- mirt.model(' T1 = 1-30
#   T2 = 31-60
#   T3 = 61-90
#   CONSTRAIN = (21-30, a1), (31-40, a1)
#   CONSTRAIN = (21-30, d), (31-40, d)
#   CONSTRAIN = (51-60, a1), (61-70, a1)
#   CONSTRAIN = (51-60, d), (61-70, d)
#   COV = T1*T2*T3'
# )
# 
# mirt.2PL = mirt(U, model, itemtype = '2PL')  # 
# PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
# profic = fscores(mirt.2PL) #estimativas das proficiências individuais


#%%%%%%%%%%%%%%%%%%%%%%%%%%%  REPRODUÇÃO DAS PROFICIÊNCIAS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# use pars="values" to get the parameters table to set the items parameters values starting values. 
# in column "value" set the starting value
# in column "est" set to FALSE to disable estimation. If set to TRUE the parameter will be estimated.
parameters = mirt(U1, 1, itemtype = '2PL', pars = "values")
str(parameters)

parameters$value[parameters$name=="a1"] #list all lines labed "a1"
parameters$value[parameters$name=="a1"] <- item.par.sim[1:30,1]
parameters$est[parameters$name=="a1"] <- FALSE
parameters$value[parameters$name=="d"] #list all lines labed "a1"
parameters$value[parameters$name=="d"] <- item.par.sim[1:30,4]
parameters$est[parameters$name=="d"] <- FALSE

mirt.2PL = mirt(U1, 1, itemtype = '2PL', pars = parameters)
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
profic = fscores(mirt.2PL) #estimativas das proficiências individuais

plot(Theta[,1],profic, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
hist(profic, main="Recuperação das proficiências", xlab="Estimativas",ylab="Frequência")


# # código abaixo checa se a programação esta correta.
# # grafico final deve ser uma linha y=x 
# mirt.2PL = mirt(U1, 1, itemtype = '2PL')  # 
# PAR1=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
# profic1 = fscores(mirt.2PL) #estimativas das proficiências individuais
# parameters = mirt(U1, 1, itemtype = '2PL', pars = "values")
# parameters$value[parameters$name=="a1"] <- PAR1[,1]
# parameters$est[parameters$name=="a1"] <- FALSE
# parameters$value[parameters$name=="d"] #list all lines labed "a1"
# parameters$value[parameters$name=="d"] <- PAR1[,2]
# parameters$est[parameters$name=="d"] <- FALSE
# mirt.2PL = mirt(U1, 1, itemtype = '2PL', pars = parameters)
# PAR2=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
# profic = fscores(mirt.2PL) #estimativas das proficiências individuais
# plot(profic1,profic, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

