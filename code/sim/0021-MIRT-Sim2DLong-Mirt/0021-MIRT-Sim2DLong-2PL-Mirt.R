
rm(list=ls())
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 
if(!require(MASS)) install.packages("mirt"); library(MASS)
if(!require(rockchalk)) install.packages("mirt"); library(rockchalk)
if(!require(GPArotation)) install.packages("mirt"); library(GPArotation)
library(lavaan)


###Generate person parameters
## subjects
N <- 1000
#theta <- rnorm(N,0,1)


set.seed(1) # Resetando a semente

I= 50  # Number of Items
N = 30*334

PL=2 # Logistic Model (1,2,3 parameters)
coefs <- matrix(ncol=6,nrow=I)
colnames(coefs)=c("a1","b1","c1","a2","b2","c2")

if (PL==1) {a = rep(1,I)} else {a = runif(I,0.5, 2.5)}    # U(0.5 , 2.5)
b = runif(I,-2, 2.0)     # U(-2 , 2)
if (PL<=2) {c = rep(0,I)} else{c = runif(I,0.0, 0.3) } # U(0 , 0.3)
  
d=-a*b # MIRT trabalha com o intercepto (d=-ab) e não com a dificuldade (b)


Theta <- mvrnorm(n=N, mu=c(0,0), Sigma = lazyCor(0.7, 2) )
head(Theta)
cor(Theta)
summary(Theta)
var(Theta)
dim(Theta)

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


head(U1)
head(U2)
U<-cbind(U1,U2)
head(U)


p1<-paste("lambda",1:30,".t1*Item.",1:30,".t1",sep="",collapse = " + ")
p1<-paste("d1 =~",p1,sep="",collapse = "")
p2<-paste("lambda",21:50,".t2*Item.",21:50,".t2",sep="",collapse = " + ")
p2<-paste("d2 =~",p2,sep="",collapse = "")

cat(paste("Item.",1:30,".t1 | tlambda",1:30,".t1*t1",sep="",collapse = "\n"))

cat(paste("Item.",21:50,".t2 | tlambda",21:50,".t2*t1",sep="",collapse = "\n"))

cat(paste("lambda",21:30,".t1==lambda",21:30,".t2" ,sep="",collapse = "\n"))

lavaan.model <-'

d1 =~ lambda1.t1*Item.1.t1+ lambda2.t1*Item.2.t1+ lambda3.t1*Item.3.t1+ lambda4.t1*Item.4.t1+ lambda5.t1*Item.5.t1+ lambda6.t1*Item.6.t1+ lambda7.t1*Item.7.t1+ lambda8.t1*Item.8.t1+ lambda9.t1*Item.9.t1+ lambda10.t1*Item.10.t1+ lambda11.t1*Item.11.t1+ lambda12.t1*Item.12.t1+ lambda13.t1*Item.13.t1+ lambda14.t1*Item.14.t1+ lambda15.t1*Item.15.t1+ lambda16.t1*Item.16.t1+ lambda17.t1*Item.17.t1+ lambda18.t1*Item.18.t1+ lambda19.t1*Item.19.t1+ lambda20.t1*Item.20.t1+ lambda21.t1*Item.21.t1+ lambda22.t1*Item.22.t1+ lambda23.t1*Item.23.t1+ lambda24.t1*Item.24.t1+ lambda25.t1*Item.25.t1+ lambda26.t1*Item.26.t1+ lambda27.t1*Item.27.t1+ lambda28.t1*Item.28.t1+ lambda29.t1*Item.29.t1+ lambda30.t1*Item.30.t1
d2 =~ lambda21.t2*Item.21.t2+ lambda22.t2*Item.22.t2+ lambda23.t2*Item.23.t2+ lambda24.t2*Item.24.t2+ lambda25.t2*Item.25.t2+ lambda26.t2*Item.26.t2+ lambda27.t2*Item.27.t2+ lambda28.t2*Item.28.t2+ lambda29.t2*Item.29.t2+ lambda30.t2*Item.30.t2+ lambda31.t2*Item.31.t2+ lambda32.t2*Item.32.t2+ lambda33.t2*Item.33.t2+ lambda34.t2*Item.34.t2+ lambda35.t2*Item.35.t2+ lambda36.t2*Item.36.t2+ lambda37.t2*Item.37.t2+ lambda38.t2*Item.38.t2+ lambda39.t2*Item.39.t2+ lambda40.t2*Item.40.t2+ lambda41.t2*Item.41.t2+ lambda42.t2*Item.42.t2+ lambda43.t2*Item.43.t2+ lambda44.t2*Item.44.t2+ lambda45.t2*Item.45.t2+ lambda46.t2*Item.46.t2+ lambda47.t2*Item.47.t2+ lambda48.t2*Item.48.t2+ lambda49.t2*Item.49.t2+ lambda50.t2*Item.50.t2

d1~~d2

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


item.par.sim <- matrix(0,60,3)
colnames(item.par.sim) <- c("aj1_lav","aj2_lav","dj_lav")

for(i in seq(1,60,1)){
  for(j in c(1,2)){
    item.par.sim[i,j] <- lambda2[i,j]/sqrt(1-t(lambda2[i,])%*%lambda2[i,])*1.7
  }
  item.par.sim[i,3] <- tau[i]/sqrt(1-t(lambda2[i,])%*%lambda2[i,])*1.7
}


item.par.sim

plot(a[1:30],item.par.sim[1:30,1], 
     main = "Estimativas de a1 ", 
     xlab = "Est. MIRT", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(d[1:30],-item.par.sim[1:30,3], 
     main = "Estimativas de d1 ", 
     xlab = "Est. MIRT", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)


plot(a[21:50],item.par.sim[31:60,2], 
     main = "Estimativas de a2 ", 
     xlab = "Est. MIRT", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(d[21:50],-item.par.sim[31:60,3], 
     main = "Estimativas de d2 ", 
     xlab = "Est. MIRT", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

