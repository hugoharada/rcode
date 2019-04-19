
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


Theta = matrix(0,N,3)

Theta[,1] <- mvrnorm(n=N, mu=0,1 )
Theta[,2] <- mvrnorm(n=N, mu=0.5,1.5 )
Theta[,3] <- mvrnorm(n=N, mu=1, 0.7 )

cov(Theta)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# momento 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eta  = Theta[,1]%*% t(a[1:30]) +  matrix(d[1:30],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[1:30] + (1-c[1:30])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U1 = 1*(X<P)  ; U1=as.data.frame(U1) # AQUI TEMOS OS DADOS DICOTÔMICOS
colnames(U1)=paste0("Item.",1:30,".t1")

rm(P,X,eta)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# momento 2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


eta  = Theta[,2]%*% t(a[21:50]) +  matrix(d[21:50],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[21:50] + (1-c[21:50])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U2 = 1*(X<P)  ; U2=as.data.frame(U2) # AQUI TEMOS OS DADOS DICOTÔMICOS
colnames(U2)=paste0("Item.",21:50,".t2")

rm(P,X,eta)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# momento 3
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


eta  = Theta[,3]%*% t(a[41:70]) +  matrix(d[41:70],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[41:70] + (1-c[41:70])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U3 = 1*(X<P)  ; U3=as.data.frame(U3) # AQUI TEMOS OS DADOS DICOTÔMICOS
colnames(U3)=paste0("Item.",41:70,".t3")

rm(P,X,eta)


head(U1)
head(U2)
head(U3)
U<-cbind(U1,U2,U3)
head(U)

mNA = rep(NA,(N*20));  dim(mNA)=c(N,20)   # matriz n x I de U(0,1)
dim(mNA)

itemnames <- c(1:70)
U1t1 <- cbind(U1,mNA,mNA)
names(U1t1)<-itemnames
U2t2 <- cbind(mNA,U2,mNA)
names(U2t2)<-itemnames
U3t3 <- cbind(mNA,mNA,U3)
names(U3t3)<-itemnames

#group names
group_names= paste0("G",rep(1:3, each=N)) #G1...G1,G2...G2, G3..G3

group_names[0:10]
group_names[9990:10020]
group_names[19990:20020]

Ut <-rbind(U1t1,U2t2,U3t3)
str(Ut)
Ut[9990:10010,15:35]
dim(Ut)
Ut[19990:20010,35:55]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ESTIMAÇÃO PELO MIRT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#models <- 'F1 = 1-30
#           F2 = 21-50
#           F3 = 41-70'

models <- 'F1 = 1-70'
mod_long <- multipleGroup( data=Ut, 
                           model=models, 
                           group = group_names,
                           itemtype='2PL',
                           invariance = c(itemnames,'free_means','free_var','slopes','intercepts')) 

par<-coef(mod_long,simplify=TRUE,IRTpars = TRUE)
par
str(coef(mod_long,simplify=TRUE))
profic = fscores(mod_long) #estimativas das proficiÃªncias individuais
mean(profic)

par2<-mod2values(mod_long) # getting everything that was estimated. Like pars='values'

plot(a[1:30],par$G1[1][['items']][1:30,1], 
     main="Recuperação dos parâmetros de discriminação - a - t1", 
     xlab="Valores verdadeiros",
     ylab="Estimativas",
     asp = 1);
lines(c(-4,4),c(-4,4), col = "blue",)

plot(a[21:50],par$G1[1][['items']][21:50,1], 
     main="Recuperação dos parâmetros de discriminação - a - t2", 
     xlab="Valores verdadeiros",
     ylab="Estimativas",
     asp = 1);
lines(c(-4,4),c(-4,4), col = "blue",)

plot(a[41:70],par$G1[1][['items']][41:70,1], 
     main="Recuperação dos parâmetros de discriminação - a -t3", 
     xlab="Valores verdadeiros",
     ylab="Estimativas",
     asp = 1);
lines(c(-4,4),c(-4,4), col = "blue",)



plot(b[1:30],par$G1[1][['items']][1:30,2], 
     main="Recuperação dos parâmetros de dificuldade - b - t1", 
     xlab="Valores verdadeiros",
     ylab="Estimativas",
     asp = 1);
lines(c(-4,4),c(-4,4), col = "blue",)


plot(b[21:50],par$G1[1][['items']][21:50,2], 
     main="Recuperação dos parâmetros de dificuldade - b - t2", 
     xlab="Valores verdadeiros",
     ylab="Estimativas",
     asp = 1);
lines(c(-4,4),c(-4,4), col = "blue",)


plot(b[41:70],par$G1[1][['items']][41:70,2], 
     main="Recuperação dos parâmetros de dificuldade - b - t3", 
     xlab="Valores verdadeiros",
     ylab="Estimativas",
     asp = 1);
lines(c(-4,4),c(-4,4), col = "blue",)


plot(Theta[,1],profic[1:N], main="Recuperação das proficiências - t1", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
mean(profic[1:N])
sd(profic[1:N])

plot(Theta[,2],profic[(N+1):(2*N)], main="Recuperação das proficiências - t2", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
mean(profic[(N+1):(2*N)])
sd(profic[(N+1):(2*N)])


plot(Theta[,3],profic[(2*N+1):(3*N)], main="Recuperação das proficiências - t3", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
mean(profic[(2*N+1):(3*N)])
sd(profic[(2*N+1):(3*N)])

cov(cbind(profic[1:N],
      profic[(N+1):(2*N)],
      profic[(2*N+1):(3*N)] ))
cor(Theta)
