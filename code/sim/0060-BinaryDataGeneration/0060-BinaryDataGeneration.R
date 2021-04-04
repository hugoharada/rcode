
rm(list=ls())
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 
if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(GPArotation)) install.packages("GPArotation"); library(GPArotation)
if(!require(pryr)) install.packages("pryr"); library(pryr)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(ggrepel)) install.packages("ggrepel"); library(ggrepel)



parameter_set <- function (){}
set.seed(12) # Resetando a semente

N <- 10000 ## subjects
I= 70  # Number of Items
Ig = 30 #Items per group
Ic = 10 # Common Items

PL=2 # Logistic Model (1,2,3 parameters)
SigmaType <- 1 # 0 = Covariance Uniform, 1 = Covariância AR1, 2 =  Covari?ncia de bandas
rho<-0.7

#if (PL==1) {a = rep(1,I)} else {a = runif(I,0.5, 2.5)}    # U(0.5 , 2.5)
if (PL==1) {a = rep(1,I)} else {a = runif(I,0.65, 1.34)}    # moderate
#if (PL==1) {a = rep(1,I)} else {a = runif(I,1.35, 1.69)}    # high
b = runif(I,-2, 2.0)     # U(-2 , 2)
if (PL<=2) {c = rep(0,I)} else{c = runif(I,0.0, 0.3) } # U(0 , 0.3)
  
d=-a*b # MIRT trabalha com o intercepto (d=-ab) e não com a dificuldade (b)

if(SigmaType==0){
  Sigma <- rockchalk::lazyCov(Rho=c(rho,rho,rho),Sd=1) #Matriz de Covari?ncia Uniforme
}else if(SigmaType==1){
  Sigma <- rockchalk::lazyCov(Rho=c(rho,rho*rho,rho),Sd=1) #Matriz de Covari?ncia AR(1)
}else if(SigmaType==2){
  Sigma <- rockchalk::lazyCov(Rho=c(rho,0,0),Sd=1) #Matriz de Covari?ncia de bandas
}else{
  Sigma <- NULL
}
mu=c(0,0.7,1.3)
Theta <- mvrnorm(n=N, mu=mu , Sigma=Sigma )

head(Theta)
cor(Theta)
summary(Theta)
var(Theta)
dim(Theta)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# momento 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eta  = Theta[,1]%*% t(a[1:30]) +  matrix(d[1:30],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[1:30] + (1-c[1:30])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U1 = 1*(X<P)  ; U1=as.data.frame(U1) # AQUI TEMOS OS DADOS DICOT?MICOS
colnames(U1)=paste0("Item.",1:30,".t1")
head(U1)

eta  = Theta[,2]%*% t(a[21:50]) +  matrix(d[21:50],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[21:50] + (1-c[21:50])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U2 = 1*(X<P)  ; U2=as.data.frame(U2) # AQUI TEMOS OS DADOS DICOT?MICOS
colnames(U2)=paste0("Item.",21:50,".t2")


eta  = Theta[,3]%*% t(a[41:70]) +  matrix(d[41:70],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[41:70] + (1-c[41:70])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U3 = 1*(X<P)  ; U3=as.data.frame(U3) # AQUI TEMOS OS DADOS DICOT?MICOS
colnames(U3)=paste0("Item.",41:70,".t3")


data_gen <- function (a_vector,d_vector,c_vector=NULL,N,theta_vector,names_vector){
  if(length(a_vector)!= length(d_vector)){stop("parameters have different lengths.\n")}
  theta_matrix <- matrix(theta_vector,ncol = 1,nrow=length(theta_vector))
  eta  = theta_matrix[,1]%*% t(a_vector) +  matrix(d_vector,N,length(d_vector),byrow=TRUE);  
  eta=t(eta) # N x I (a_vector'theta+d_vector)
  if(is.null(c_vector)){ c_vector <- rep(0,length(a_vector))}
  P = c_vector + (1-c_vector)/(1+exp(-eta));  P=t(P) # n x I
  X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
  U1 = 1*(X<P)  ; U1=as.data.frame(U1) # AQUI TEMOS OS DADOS DICOT?MICOS
  if(is.null(names_vector)){ colnames(U1)=names_vector}
  return(U1)
}

head(
data_gen(a_vector = a[1:30],
         d_vector = d[1:30],
         N=N,
         theta_vector = Theta[,1],
         names_vector = paste0("i",1:30))
)


dat1 <- simdata(a=a[1:30],
                d=d[1:30],
                N=N,itemtype = '2PL', 
                Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
dat2 <- simdata(a=a[21:50],
                d=d[21:50],
                N=N,itemtype = '2PL', 
                Theta = matrix(Theta[,2],ncol=1,nrow = length(Theta[,3])))
dat3 <- simdata(a=a[41:70],
                d=d[41:70],
                N=N,itemtype = '2PL', 
                Theta = matrix(Theta[,3],ncol=1,nrow = length(Theta[,3])))


rm(P,X,eta)

mod <-mirt(data=U1,itemtype = "2PL",model=1)
par<-coef(mod,simplify=TRUE,IRTpars = TRUE)
str(par)
mean(a[1:30]-par$items[,"a"])

mod <-mirt(data=dat1,itemtype = "2PL",model=1)
par<-coef(mod,simplify=TRUE,IRTpars = TRUE)
str(par)
mean(a[1:30]-par$items[,"a"])
mean(b[1:30]-par$items[,"b"])


mod <-mirt(data=U2,itemtype = "2PL",model=1)
par<-coef(mod,simplify=TRUE,IRTpars = TRUE)
str(par)
mean(a[21:50]-par$items[,"a"])
mean(b[21:50]-par$items[,"b"])

mod <-mirt(data=dat2,itemtype = "2PL",model=1)
par<-coef(mod,simplify=TRUE,IRTpars = TRUE)
str(par)
mean(a[21:50]-par$items[,"a"])
mean(b[21:50]-par$items[,"b"])


mod <-mirt(data=U3,itemtype = "2PL",model=1)
par<-coef(mod,simplify=TRUE,IRTpars = TRUE)
str(par)
mean(a[41:70]-par$items[,"a"])
mean(b[41:70]-par$items[,"b"])

mod <-mirt(data=dat3,itemtype = "2PL",model=1)
par<-coef(mod,simplify=TRUE,IRTpars = TRUE)
str(par)
mean(a[41:70]-par$items[,"a"])
mean(b[41:70]-par$items[,"b"])


