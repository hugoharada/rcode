
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




###Generate person parameters
parameter_set <- function (){}
set.seed(1) # Resetando a semente

N <- 10000 ## subjects
I= 70  # Number of Items
Ig = 30 #Items per group
Ic = 10 # Common Items

PL=2 # Logistic Model (1,2,3 parameters)
SigmaType <- 1 # 0 = Covariance Uniform, 1 = Covari?ncia AR1, 2 =  Covari?ncia de bandas
rho<-0.7

#if (PL==1) {a = rep(1,I)} else {a = runif(I,0.5, 2.5)}    # U(0.5 , 2.5)
if (PL==1) {a = rep(1,I)} else {a = runif(I,0.65, 1.34)}    # moderate
#if (PL==1) {a = rep(1,I)} else {a = runif(I,1.35, 1.69)}    # high
b = runif(I,-2, 2.0)     # U(-2 , 2)
if (PL<=2) {c = rep(0,I)} else{c = runif(I,0.0, 0.3) } # U(0 , 0.3)
  
d=-a*b # MIRT trabalha com o intercepto (d=-ab) e n?o com a dificuldade (b)

if(SigmaType==0){
  Sigma <- lazyCor(c(rho,rho,rho)) #Matriz de Covari?ncia Uniforme
}else if(SigmaType==1){
  Sigma <- lazyCor(c(rho,rho*rho,rho)) #Matriz de Covari?ncia AR(1)
}else if(SigmaType==2){
  Sigma <- lazyCor(c(rho,0,0)) #Matriz de Covari?ncia de bandas
}else{
  Sigma <- NULL
}

Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )

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

rm(P,X,eta)

mod <-mirt(data=U1,itemtype = "2PL",model=1)
par<-coef(mod,simplify=TRUE,IRTpars = TRUE)
tmp1 <- cbind(a[1:30],par$items[,"a"],a[1:30]-par$items[,"a"] )
mean(tmp1[,3])
tmp2 <- cbind(b[1:30],par$items[,"b"],b[1:30]-par$items[,"b"] )
mean(tmp2[,3])
profic = fscores(mod) #estimativas das proficiências individuais
mean(profic)
sd(profic)

plot(cbind(a[1:30],par$items[,"a"]))
lines(c(-4,4),c(-4,4), col = "blue")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# momento 2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


eta  = Theta[,2]%*% t(a[21:50]) +  matrix(d[21:50],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[21:50] + (1-c[21:50])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U2 = 1*(X<P)  ; U2=as.data.frame(U2) # AQUI TEMOS OS DADOS DICOT?MICOS
colnames(U2)=paste0("Item.",21:50,".t2")

rm(P,X,eta)

 mod <-mirt(data=U2,itemtype = "2PL",model=1)
 par<-coef(mod,simplify=TRUE,IRTpars = TRUE)
 tmp1 <- cbind(a[21:50],par$items[,"a"],a[21:50]-par$items[,"a"] )
 mean(tmp1[,3])
 tmp2 <- cbind(b[21:50],par$items[,"b"],b[21:50]-par$items[,"b"] )
 mean(tmp2[,3])
profic = fscores(mod) #estimativas das proficiências individuais
mean(profic)
sd(profic)

plot(cbind(a[21:50],par$items[,"a"]))
lines(c(-4,4),c(-4,4), col = "blue")
plot(cbind(b[21:50],par$items[,"b"]))
lines(c(-4,4),c(-4,4), col = "blue")



# momento 2
parameters = mirt(U2, 1, itemtype = '2PL', pars = "values")
str(parameters)

parameters$value[parameters$name=="a1"] #list all lines labed "a1"
parameters$value[parameters$name=="a1"] <- a[21:50]
parameters$est[parameters$name=="a1"] <- FALSE
parameters$value[parameters$name=="d"] #list all lines labed "d"
parameters$value[parameters$name=="d"] <- d[21:50]
parameters$est[parameters$name=="d"] <- FALSE

mirt.2PL = mirt(U2, 1, itemtype = '2PL', pars = parameters)
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estima??o dos par?metros dos itens: Colunas a,d,c
profic2 = fscores(mirt.2PL) #estimativas das profici?ncias individuais
mean(profic2)
mean(Theta[,2])
sd(profic2)
sd(Theta[,2])
plot(Theta[,2],profic2, main="Recupera??o das profici?ncias", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# momento 3
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


eta  = Theta[,3]%*% t(a[41:70]) +  matrix(d[41:70],N,30,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c[41:70] + (1-c[41:70])/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*30);  dim(X)=c(N,30)   # matriz n x I de U(0,1)
U3 = 1*(X<P)  ; U3=as.data.frame(U3) # AQUI TEMOS OS DADOS DICOT?MICOS
colnames(U3)=paste0("Item.",41:70,".t3")

rm(P,X,eta)


 mod <-mirt(data=U3,itemtype = "2PL",model=1)
 summary(mod)
 par<-coef(mod,simplify=TRUE,IRTpars = TRUE)
 tmp1 <- cbind(a[41:70],par$items[,"a"],a[41:70]-par$items[,"a"])
 mean(tmp1[,3])
 tmp2 <- cbind(b[41:70],par$items[,"b"], b[41:70]-par$items[,"b"])
 mean(tmp2[,3])
 profic = fscores(mod) #estimativas das proficiências individuais
mean(profic)
sd(profic)

plot(cbind(a[41:70],par$items[,"a"]))
lines(c(-4,4),c(-4,4), col = "blue")
plot(cbind(b[41:70],par$items[,"b"]))
lines(c(-4,4),c(-4,4), col = "blue")


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


param.convertion.tag <- function(){}

#parametrização FI-EF-CP fixed Intercept - effects coding - conditional parameterization 
#
# d = -tau + lambda*alpha
# a = lambda*sqrt(var(eta)) 
# => lambda = a/sqrt(var(eta)) 
# => tau = lambda*alpha -d 

alpha = 0
var_eta =1
lambda_sim <- a/sqrt(var_eta)/1.702
tau_sim <- (lambda_sim*alpha -d)/1.702



p1<-paste("lambda",1:30,".t1*Item.",1:30,".t1",sep="",collapse = " + ")
p1<-paste("d1 =~",p1,sep="",collapse = "")
p2<-paste("lambda",21:50,".t2*Item.",21:50,".t2",sep="",collapse = " + ")
p2<-paste("d2 =~",p2,sep="",collapse = "")
p3<-paste("lambda",41:70,".t3*Item.",41:70,".t3",sep="",collapse = " + ")
paste("lambda",41:70,".t3",sep="",collapse = " + ")

p3<-paste("d3 =~",p3,sep="",collapse = "")

cat(paste("Item.",1:30,".t1 | tlambda",1:30,".t1*t1",sep="",collapse = "\n"))
cat(paste("Item.",21:50,".t2 | tlambda",21:50,".t2*t1",sep="",collapse = "\n"))
cat(paste("Item.",41:70,".t3 | tlambda",41:70,".t3*t1",sep="",collapse = "\n"))

parameters <-function(){}

cat(paste("lambda",1:30,".t1 ==",lambda_sim[1:30],sep="",collapse = "\n"))
cat(paste("tlambda",1:30,".t1 ==",tau_sim[1:30],sep="",collapse = "\n"))

cat(paste("lambda",21:50,".t2 ==",lambda_sim[21:50],sep="",collapse = "\n"))
cat(paste("tlambda",21:50,".t2 ==",tau_sim[21:50],sep="",collapse = "\n"))

cat(paste("lambda",1:30,".t1+",sep="",collapse = ""))
cat(paste("lambda",21:50,".t2+",sep="",collapse = ""))

cat(paste("Item.",1:20,".t1 ~~ Item.",1:20,".t1" ,sep="",collapse = "\n"))
cat(paste("Item.",21:30,".t1 ~~ Item.",21:30,".t1"," + Item.",21:30,".t2" ,sep="",collapse = "\n"))
cat(paste("Item.",21:40,".t2 ~~ Item.",21:40,".t2" ,sep="",collapse = "\n"))
cat(paste("Item.",41:50,".t2 ~~ Item.",41:50,".t2"," + Item.",41:50,".t3" ,sep="",collapse = "\n"))
cat(paste("Item.",41:70,".t3 ~~ Item.",41:70,".t3" ,sep="",collapse = "\n"))

cat(paste("Item.",21:30,".t1 ~~ Item.",21:30,".t2" ,sep="",collapse = "\n"))
cat(paste("Item.",41:50,".t2 ~~ Item.",41:50,".t3" ,sep="",collapse = "\n"))

get_item_index <- function(time,Ig,Ic){
  return( c( group_index_start = ((time-1)*Ig-(time-1)*Ic+1),  # grupo_index_start
             first_common_index_end = ((time-1)*Ig-(time-1)*Ic+Ic), # first_common_index_end
             second_common_index_start = (time*Ig-(time-1)*Ic-Ic+1),   # second_common_index_start
             group_index_end = (time*Ig-(time-1)*Ic))        # group_index_end
  )
}

str(get_item_index(1,Ig,Ic))



I= 70  # Number of Items
Ig = 30 #Items per group
Ic = 10 # Common Items
nt =2

lambda_est <- matrix(data = NA, nrow = 3,ncol = Ig)
tau_est<- matrix(data = NA, nrow = 3,ncol = Ig)
single_moment_item_indexes <-get_item_index(1,Ig,Ic)


model_gen <- function(Ig = 30, #Items per group
                    Ic = 10, # Common Items
                    n0 = 1, # Common Items
                    nt =2, # number of moments
                    lmb_values = NULL, # nt x Ig matrix 
                    tau_values = NULL # nt x Ig matrix 
){
  item_name <- matrix(NA,nrow = nt, ncol=Ig)
  lmb_name <- matrix(NA,nrow = nt, ncol=Ig)
  tau_name <- matrix(NA,nrow = nt, ncol=Ig)
  item_indexes <- matrix(NA,nrow = nt, ncol=4)
  single_moment_item_indexes <-get_item_index(1,Ig,Ic)
  for(i in n0:nt){
    item_indexes[i,] <-get_item_index(i,Ig,Ic)
    item_name[i,] <- paste0("Item.",item_indexes[i,1]:item_indexes[i,4],".t",i)
    lmb_name[i,] <- paste0("lmb.",item_indexes[i,1]:item_indexes[i,4],".t",i)
    tau_name[i,] <- paste0("thr.",item_indexes[i,1]:item_indexes[i,4],".t",i)
    
  }
  mod <- ""
  for(i in n0:nt){
    tmp <- paste0("eta",i,"=~", 
                  paste(lmb_name[i,],"*",item_name[i,],sep = "",collapse="+"))
    mod <- c(mod,tmp,"\n\n")
  }
  
  #common factor covariances
  if(nt==n0){
    tmp<-paste0("eta",n0,"~~",
                paste("eta",n0,
                      sep="",collapse = "+"))
    mod <- c(mod,tmp,"\n\n")
  }else{
    for(i in n0:(nt-1)){
      tmp<-paste0("eta",i,"~~",
                paste("eta",(i+1):nt,
                      sep="",collapse = "+"))
      mod <- c(mod,tmp,"\n\n")
    }
  }

  #effect coding restrictions
  for(i in n0:nt){
    tmp <- paste0(length(item_name[i,]),"==", paste(lmb_name[i,],sep = "",collapse="+"))
    mod <- c(mod,tmp,"\n\n")
  }
  #thresholds
  for(i in n0:nt){
    tmp <- paste(item_name[i,]," | ",tau_name[i,],"*t1",sep="",collapse = "\n")
    mod <- c(mod,tmp,"\n\n")
  }
  mod <- c(mod,"\n")
  
  #covariance matrix
  for(i in n0:nt){
    if(i<nt){
      tmp <- paste(item_name[i,single_moment_item_indexes[1]:(single_moment_item_indexes[3]-1)]," ~~ ",
                   item_name[i,single_moment_item_indexes[1]:(single_moment_item_indexes[3]-1)],sep="",collapse = "\n")
      mod <- c(mod,tmp,"\n")
      tmp <- paste(item_name[i,single_moment_item_indexes[3]:single_moment_item_indexes[4]]," ~~ ",
                   item_name[i,single_moment_item_indexes[3]:single_moment_item_indexes[4]],"+",
                   item_name[i+1,1:Ic],
                   sep="",collapse = "\n")
      mod <- c(mod,tmp,"\n\n")
    }else{
      tmp <- paste(item_name[i,single_moment_item_indexes[1]:single_moment_item_indexes[4]]," ~~ ",
                   item_name[i,single_moment_item_indexes[1]:single_moment_item_indexes[4]],sep="",collapse = "\n")
      mod <- c(mod,tmp,"\n\n")
    }
  }

  if( !is.null(lmb_values)){
    for(i in n0:nt){
      tmp <- paste(  lmb_name[i,], "==",lmb_values[i,single_moment_item_indexes[1]:single_moment_item_indexes[4]],sep="",collapse = "\n")  
      mod <- c(mod,tmp,"\n\n")
    }
  }
  
    
  if( !is.null(tau_values)){
    for(i in n0:nt){
      tmp <- paste(  tau_name[i,], "==",tau_values[i, single_moment_item_indexes[1]:single_moment_item_indexes[4]],sep="",collapse = "\n")  
      mod <- c(mod,tmp,"\n\n")
    }
  }
  
  # for(i in n0:nt){
  #   tmp <- paste(  lmb_name[i,], "==",lambda_sim[item_indexes[i,1]:(item_indexes[i,4])],sep="",collapse = "\n")
  #   mod <- c(mod,tmp,"\n\n")
  #   tmp <- paste(  tau_name[i,], "==",tau_sim[item_indexes[i,1]:(item_indexes[i,4])],sep="",collapse = "\n")  
  #   mod <- c(mod,tmp,"\n\n")
  # }
  return(mod)
} #model_gen <- function(I= 70,  # Number of Items

cat(model_gen(Ig = 30, Ic = 10, nt=2,lmb_values = NULL, tau_values = NULL ))

cat(model_gen(Ig = 15, Ic = 5, nt=3,lmb_values = NULL, tau_values = NULL ))

cat(model_gen(Ig = 10, Ic = 5, nt=2,lmb_values = NULL, tau_values = NULL ))

cat(model_gen(Ig = 10, Ic = 5, n0=2, nt=2,lmb_values = NULL, tau_values = NULL ))
cat(model_gen(Ig = 10, Ic = 5, n0=3, nt=3,lmb_values = NULL, tau_values = NULL ))

lmb_values_prep <- matrix(data = c(lambda_sim[item_indexes[1,1]:item_indexes[1,4]],
                                   lambda_sim[item_indexes[2,1]:item_indexes[2,4]],
                                   lambda_sim[item_indexes[3,1]:item_indexes[3,4]]), nrow=nt,ncol=Ig,byrow = TRUE)

tau_values_prep <- matrix(data = c(tau_sim[item_indexes[1,1]:item_indexes[1,4]],
                                   tau_sim[item_indexes[2,1]:item_indexes[2,4]],
                                   tau_sim[item_indexes[3,1]:item_indexes[3,4]]), nrow=nt,ncol=Ig,byrow = TRUE)

cat(model_gen(Ig = 10, Ic = 5, nt=1,lmb_values = NULL, tau_values = tau_values_prep))
cat(model_gen(Ig = 10, Ic = 5, nt=1,lmb_values = lmb_values_prep, tau_values = NULL))
cat(model_gen(Ig = 10, Ic = 5, nt=1,lmb_values = lmb_values_prep, tau_values = tau_values_prep))
cat(model_gen(Ig = 10, Ic = 5, nt=2,lmb_values = lmb_values_prep, tau_values = tau_values_prep))

working <- function(){}



get_lmb_tau_alpha_psi <- function( fitted.model){
  
  lambda <- lavInspect(fitted.model,what = 'est')$lambda
  colnames(lambda) <- paste0("lambda", 1:ncol(lambda))
  
  #getting tau values
  tau <- lavInspect(fitted.model,what = 'est')$tau
  colnames(tau) <- c("tau")
  
  alpha <- lavInspect(fitted.model,what = 'mean.lv')
  psi <- lavInspect(fitted.model,what = 'cov.lv')
  
  return(list(lambda=lambda,tau=tau,alpha=alpha,psi=psi))         
}


get_a_d_b <- function( tn.sem.param){
  
  n <- length(tn.sem.param$tau)
  a <- rep(NA,n)
  d <- rep(NA,n)
  
  for(i in seq(1,n,1)){
    a[i] <- tn.sem.param$lambda[i,1]*sqrt(tn.sem.param$psi[1,1])*1.7
    d[i] <- (-tn.sem.param$tau[i] +tn.sem.param$lambda[i,1]*tn.sem.param$alpha[1]) *1.7
  }
  b = -d/a
  
  return(list(a=a,d=d,b=b))         
}


lavaan.model.t1.free.tag <- function(){}
lavaan.model.t1.free <- model_gen(Ig = 30, Ic = 10, nt=1,lmb_values = NULL, tau_values = NULL )

lavaan.model.fit <- lavaan(lavaan.model.t1.free, 
                           data = U1, 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = colnames(U1),
                           parameterization = "theta")


lavaan.model.t1.free.fit <- lavaan.model.fit
summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
eta1.free<-predict(lavaan.model.fit)
mean(eta1.free)
sd(eta1.free)

t1.sem.param<-get_lmb_tau_alpha_psi(lavaan.model.fit)
t1.tri.param<-get_a_d_b(t1.sem.param)

lambda_est[1,] <- t1.sem.param$lambda
tau_est[1,] <- t1.sem.param$tau


plot(cbind(a[1:30],t1.tri.param$a[1:30]),xlim = c(0.5 , 2.5),ylim = c(0.5 , 2.5),asp=1)
lines(c(-4,4),c(-4,4), col = "blue")

plot(cbind(b[1:30],t1.tri.param$b[1:30]))
lines(c(-4,4),c(-4,4), col = "blue")

eta1.free<-predict(lavaan.model.fit)
mean(eta1.free)
sd(eta1.free)





lavaan.model.t2.free.tag <- function(){}

lavaan.model.t2.free <- model_gen(Ig = 30, Ic = 10,n0=2, nt=2,lmb_values = NULL, tau_values = NULL )

lavaan.model.fit <- lavaan(lavaan.model.t2.free, 
                           data = U2, 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = colnames(U2),
                           parameterization = "theta")

lavaan.model.t2.free.fit <- lavaan.model.fit

summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
t2.sem.param<-get_lmb_tau_alpha_psi(lavaan.model.fit)
t2.tri.param<-get_a_d_b(t2.sem.param)

plot(cbind(a[21:50],t2.tri.param$a[1:30])) 
lines(c(-4,4),c(-4,4), col = "blue")

plot(cbind(b[21:50],t2.tri.param$b[1:30])) 
lines(c(-4,4),c(-4,4), col = "blue")

eta2.free<-predict(lavaan.model.fit)
mean(eta2.free)
sd(eta2.free)

data <- data.frame(b1 = t1.tri.param$b[21:30],
                   b2 = t2.tri.param$b[1:10])
plot(data)

mod<-lm(formula = b1 ~ b2, data = data)
summary(mod)
str(mod)
A <- mod$coefficients[2]
B <- mod$coefficients[1]

a2_equalized <- t2.tri.param$a/A
b2_equalized <- t2.tri.param$b*A+B
d2_equalized = -a2_equalized*b2_equalized


plot(cbind(b[21:50],t2.tri.param$b))
lines(c(-4,4),c(-4,4), col = "blue")

plot(cbind(b[21:50],b2_equalized))
lines(c(-4,4),c(-4,4), col = "blue")

plot(cbind(a[21:50],a2_equalized))
lines(c(-4,4),c(-4,4), col = "blue")

plot(cbind(a[21:50],t2.tri.param$a))
lines(c(-4,4),c(-4,4), col = "blue")


alpha = 0
var_eta =1
lambda_est[2,] <- a2_equalized/sqrt(var_eta)/1.702
tau_est[2,] <- (lambda_est[2,]*alpha -d2_equalized)/1.702


lmb_values_prep <- matrix(data = c(lambda_est[1,single_moment_item_indexes[1]:single_moment_item_indexes[4]],
                                   lambda_est[2,single_moment_item_indexes[1]:single_moment_item_indexes[4]],
                                   lambda_est[3,single_moment_item_indexes[1]:single_moment_item_indexes[4]]), nrow=nt,ncol=Ig,byrow = TRUE)

tau_values_prep <- matrix(data = c(tau_est[1, single_moment_item_indexes[1]:single_moment_item_indexes[4]],
                                   tau_est[2, single_moment_item_indexes[1]:single_moment_item_indexes[4]],
                                   tau_est[3, single_moment_item_indexes[1]:single_moment_item_indexes[4]]), nrow=nt,ncol=Ig,byrow = TRUE)


lavaan.model.t12.t12fixed.tag <- function(){}

lavaan.model.t12.t12fixed <-  model_gen(Ig = 30, Ic = 10,n0=1, nt=2,lmb_values = lmb_values_prep, tau_values = tau_values_prep )

lavaan.model.fit <- lavaan(lavaan.model.t12.t12fixed, 
                           data = cbind(U1,U2), 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = c(colnames(U1),colnames(U2)),
                           parameterization = "theta")
lavaan.model.t12.t12fixed.fit <- lavaan.model.fit

summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
t12.sem.param<-get_lmb_tau_alpha_psi(lavaan.model.fit)
t12.tri.param<-get_a_d_b(t12.sem.param)


eta12.free<-predict(lavaan.model.fit)
mean(eta12.free[,1])
sd(eta12.free[,1])
mean(eta12.free[,2])
sd(eta12.free[,2])
cov(eta12.free)

lavaan.model.t12.t12fixed.nocov.tag <- function(){}

lavaan.model.t12.t12fixed.nocov <-'

d1 =~ lambda1.t1*Item.1.t1+ lambda2.t1*Item.2.t1+ lambda3.t1*Item.3.t1+ lambda4.t1*Item.4.t1+ lambda5.t1*Item.5.t1+ lambda6.t1*Item.6.t1+ lambda7.t1*Item.7.t1+ lambda8.t1*Item.8.t1+ lambda9.t1*Item.9.t1+ lambda10.t1*Item.10.t1+ lambda11.t1*Item.11.t1+ lambda12.t1*Item.12.t1+ lambda13.t1*Item.13.t1+ lambda14.t1*Item.14.t1+ lambda15.t1*Item.15.t1+ lambda16.t1*Item.16.t1+ lambda17.t1*Item.17.t1+ lambda18.t1*Item.18.t1+ lambda19.t1*Item.19.t1+ lambda20.t1*Item.20.t1+ lambda21.t1*Item.21.t1+ lambda22.t1*Item.22.t1+ lambda23.t1*Item.23.t1+ lambda24.t1*Item.24.t1+ lambda25.t1*Item.25.t1+ lambda26.t1*Item.26.t1+ lambda27.t1*Item.27.t1+ lambda28.t1*Item.28.t1+ lambda29.t1*Item.29.t1+ lambda30.t1*Item.30.t1
d2 =~ lambda21.t2*Item.21.t2+ lambda22.t2*Item.22.t2+ lambda23.t2*Item.23.t2+ lambda24.t2*Item.24.t2+ lambda25.t2*Item.25.t2+ lambda26.t2*Item.26.t2+ lambda27.t2*Item.27.t2+ lambda28.t2*Item.28.t2+ lambda29.t2*Item.29.t2+ lambda30.t2*Item.30.t2+ lambda31.t2*Item.31.t2+ lambda32.t2*Item.32.t2+ lambda33.t2*Item.33.t2+ lambda34.t2*Item.34.t2+ lambda35.t2*Item.35.t2+ lambda36.t2*Item.36.t2+ lambda37.t2*Item.37.t2+ lambda38.t2*Item.38.t2+ lambda39.t2*Item.39.t2+ lambda40.t2*Item.40.t2+ lambda41.t2*Item.41.t2+ lambda42.t2*Item.42.t2+ lambda43.t2*Item.43.t2+ lambda44.t2*Item.44.t2+ lambda45.t2*Item.45.t2+ lambda46.t2*Item.46.t2+ lambda47.t2*Item.47.t2+ lambda48.t2*Item.48.t2+ lambda49.t2*Item.49.t2+ lambda50.t2*Item.50.t2

d1~~d2


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





lambda1.t1 ==0.726371655117903
lambda2.t1 ==0.822808811591549
lambda3.t1 ==1.08304504781398
lambda4.t1 ==1.58820037269627
lambda5.t1 ==0.652574605506499
lambda6.t1 ==1.39740726424945
lambda7.t1 ==1.51160738087182
lambda8.t1 ==1.23840723760775
lambda9.t1 ==1.15320254443496
lambda10.t1 ==0.450649951316108
lambda11.t1 ==0.671195787900798
lambda12.t1 ==0.596315148347679
lambda13.t1 ==1.25483766885902
lambda14.t1 ==0.86535485050754
lambda15.t1 ==1.2799244228771
lambda16.t1 ==1.02720264922705
lambda17.t1 ==1.29048370810758
lambda18.t1 ==1.48774477200108
lambda19.t1 ==0.878366211073239
lambda20.t1 ==1.23429804529063
lambda21.t1 ==1.51345011147542
lambda22.t1 ==0.643134270220657
lambda23.t1 ==1.22144173571852
lambda24.t1 ==0.495112980463626
lambda25.t1 ==0.672711022194983
lambda26.t1 ==0.820184410676499
lambda27.t1 ==0.381742887400009
lambda28.t1 ==0.853327635480844
lambda29.t1 ==1.35872216510816
lambda30.t1 ==0.830174645863274

tlambda1.t1 ==-0.395546136939333
tlambda2.t1 ==0.959079672806876
tlambda3.t1 ==-0.588513642733792
tlambda4.t1 ==-0.925742433597419
tlambda5.t1 ==-0.0705454998911635
tlambda6.t1 ==1.96953696699368
tlambda7.t1 ==1.94015257701499
tlambda8.t1 ==-0.487561273101274
tlambda9.t1 ==1.11840823581658
tlambda10.t1 ==0.688737505421704
tlambda11.t1 ==-0.158414540595477
tlambda12.t1 ==0.452957536204564
tlambda13.t1 ==-0.418516267690346
tlambda14.t1 ==-0.541699731395222
tlambda15.t1 ==1.17103228010068
tlambda16.t1 ==-1.01984753714729
tlambda17.t1 ==0.961703175031416
tlambda18.t1 ==-1.99922676647371
tlambda19.t1 ==-0.785310801880931
tlambda20.t1 ==-1.59388797523404
tlambda21.t1 ==-1.37430488410851
tlambda22.t1 ==-0.984242105837807
tlambda23.t1 ==0.613178665334313
tlambda24.t1 ==0.659961558506827
tlambda25.t1 ==0.669675718400049
tlambda26.t1 ==0.887485195339564
tlambda27.t1 ==-0.0718487313154983
tlambda28.t1 ==-0.275037889439151
tlambda29.t1 ==1.53103139706903
tlambda30.t1 ==0.304775897761329



lambda21.t2 ==1.24865675112519
lambda22.t2 ==0.515599014600513
lambda23.t2 ==1.05145488196072
lambda24.t2 ==0.453964212371336
lambda25.t2 ==0.665997170349377
lambda26.t2 ==0.770344052021204
lambda27.t2 ==0.317495465956755
lambda28.t2 ==0.76607864230443
lambda29.t2 ==1.29065234352694
lambda30.t2 ==0.745288706316526
lambda31.t2 ==0.912844562700094
lambda32.t2 ==0.991692689326616
lambda33.t2 ==0.835813142483618
lambda34.t2 ==0.502224163738531
lambda35.t2 ==1.24889759071193
lambda36.t2 ==1.01090652901111
lambda37.t2 ==1.10708781059155
lambda38.t2 ==0.440527600332572
lambda39.t2 ==1.06183488177454
lambda40.t2 ==0.776734132791193
lambda41.t2 ==1.12355600081307
lambda42.t2 ==1.06158785348757
lambda43.t2 ==1.19948620166494
lambda44.t2 ==0.956462962617167
lambda45.t2 ==0.819960290219269
lambda46.t2 ==1.03585517480234
lambda47.t2 ==0.322652153248899
lambda48.t2 ==0.802286752805132
lambda49.t2 ==1.11803841197997
lambda50.t2 ==1.11531720122423

tlambda21.t2 ==-1.31480709232932
tlambda22.t2 ==-0.93320548507609
tlambda23.t2 ==0.633960376782047
tlambda24.t2 ==0.711031314271462
tlambda25.t2 ==0.743707481150658
tlambda26.t2 ==0.952547417648624
tlambda27.t2 ==-0.0562038392915776
tlambda28.t2 ==-0.265436814248522
tlambda29.t2 ==1.62421692172907
tlambda30.t2 ==0.335303345049908
tlambda31.t2 ==0.604055509052579
tlambda32.t2 ==-0.560640430193666
tlambda33.t2 ==-0.770683222667336
tlambda34.t2 ==1.0058309206772
tlambda35.t2 ==0.728934617019349
tlambda36.t2 ==-1.16098304177316
tlambda37.t2 ==-1.62497731719925
tlambda38.t2 ==-0.022502491767487
tlambda39.t2 ==1.8936278191813
tlambda40.t2 ==0.321785020739131
tlambda41.t2 ==2.20436439319413
tlambda42.t2 ==0.992264228311611
tlambda43.t2 ==-0.630859487814159
tlambda44.t2 ==-0.220636663848532
tlambda45.t2 ==-1.17162175329766
tlambda46.t2 ==-2.13143973278746
tlambda47.t2 ==0.321031549591306
tlambda48.t2 ==-1.2730213246044
tlambda49.t2 ==-0.219455302640744
tlambda50.t2 ==0.678606190168499

'

lavaan.model.fit <- lavaan(lavaan.model.t12.t12fixed.nocov, 
                           data = cbind(U1,U2), 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = c(colnames(U1),colnames(U2)),
                           parameterization = "theta")
lavaan.model.t12.t12fixed.nocov.fit <- lavaan.model.fit

summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
t12.nocov.sem.param<-get_lmb_tau_alpha_psi(lavaan.model.fit)
t12.nocov.tri.param<-get_a_d_b(t12.sem.param)


eta12.nocov.free<-predict(lavaan.model.fit)
mean(eta12.nocov.free[,1])
sd(eta12.nocov.free[,1])
mean(eta12.nocov.free[,2])
sd(eta12.nocov.free[,2])
cov(eta12.nocov.free)


lavaan.model.t3.free.tag <- function(){}

lavaan.model.t3.free <- model_gen(Ig = 30, Ic = 10,n0=3, nt=3,lmb_values = NULL, tau_values = NULL )


lavaan.model.fit <- lavaan(lavaan.model.t3.free, 
                           data = U3, 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = colnames(U3),
                           parameterization = "theta")
lavaan.model.t3.free.fit <- lavaan.model.fit

summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
t3.sem.param<-get_lmb_tau_alpha_psi(lavaan.model.fit)
t3.tri.param<-get_a_d_b(t3.sem.param)

plot(cbind(a[41:70],t3.tri.param$a[1:30])) 
lines(c(-4,4),c(-4,4), col = "blue")

plot(cbind(b[41:70],t3.tri.param$b[1:30])) 
lines(c(-4,4),c(-4,4), col = "blue")

eta3.free<-predict(lavaan.model.fit)
mean(eta3.free)
sd(eta3.free)

data <- data.frame(b2 = b2_equalized[21:30],
                   b3 = t3.tri.param$b[1:10])
plot(data)

mod<-lm(formula = b2 ~ b3, data = data)
summary(mod)
str(mod)
A <- mod$coefficients[2]
B <- mod$coefficients[1]

a3_equalized <- t3.tri.param$a/A
b3_equalized <- t3.tri.param$b*A+B
d3_equalized <- -a3_equalized*b3_equalized


plot(cbind(b[41:70],b3_equalized))
lines(c(-4,4),c(-4,4), col = "blue")

plot(cbind(a[41:70],a3_equalized))
lines(c(-4,4),c(-4,4), col = "blue")

plot(cbind(a[41:70],t3.tri.param$a))
lines(c(-4,4),c(-4,4), col = "blue")


alpha = 0
var_eta =1
#alpha = t3.sem.param$alpha
#var_eta =t3.sem.param$psi
lambda_est[3,] <- a3_equalized/sqrt(var_eta)/1.702
tau_est[3,] <- (lambda_est[3,]*alpha -d3_equalized)/1.702


lavaan.model.t123.t123fixed.tag <- function(){}

lavaan.model.t123.t123fixed <- model_gen(Ig = 30, Ic = 10,n0=1, nt=3,lmb_values = lmb_values_prep, tau_values = tau_values_prep )


lavaan.model.fit <- lavaan(lavaan.model.t123.t123fixed, 
                           data = cbind(U1,U2,U3), 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = c(colnames(U1),colnames(U2),colnames(U3)),
                           parameterization = "theta")

lavaan.model.t123.t123fixed.fit <- lavaan.model.fit

summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]

t123.sem.param<-get_lmb_tau_alpha_psi(lavaan.model.fit)
t123.tri.param<-get_a_d_b(t123.sem.param)

eta123.free <-predict(lavaan.model.fit)
mean(eta123.free[,1])
mean(eta123.free[,2])
mean(eta123.free[,3])
sd(eta123.free[,1])
sd(eta123.free[,2])
sd(eta123.free[,3])
cov(eta123.free)



lavaan.model.t123.t123fixed.nocov.tag <- function(){}

lavaan.model.t123.t123fixed.nocov <-'

d1 =~ lambda1.t1*Item.1.t1+ lambda2.t1*Item.2.t1+ lambda3.t1*Item.3.t1+ lambda4.t1*Item.4.t1+ lambda5.t1*Item.5.t1+ lambda6.t1*Item.6.t1+ lambda7.t1*Item.7.t1+ lambda8.t1*Item.8.t1+ lambda9.t1*Item.9.t1+ lambda10.t1*Item.10.t1+ lambda11.t1*Item.11.t1+ lambda12.t1*Item.12.t1+ lambda13.t1*Item.13.t1+ lambda14.t1*Item.14.t1+ lambda15.t1*Item.15.t1+ lambda16.t1*Item.16.t1+ lambda17.t1*Item.17.t1+ lambda18.t1*Item.18.t1+ lambda19.t1*Item.19.t1+ lambda20.t1*Item.20.t1+ lambda21.t1*Item.21.t1+ lambda22.t1*Item.22.t1+ lambda23.t1*Item.23.t1+ lambda24.t1*Item.24.t1+ lambda25.t1*Item.25.t1+ lambda26.t1*Item.26.t1+ lambda27.t1*Item.27.t1+ lambda28.t1*Item.28.t1+ lambda29.t1*Item.29.t1+ lambda30.t1*Item.30.t1
d2 =~ lambda21.t2*Item.21.t2+ lambda22.t2*Item.22.t2+ lambda23.t2*Item.23.t2+ lambda24.t2*Item.24.t2+ lambda25.t2*Item.25.t2+ lambda26.t2*Item.26.t2+ lambda27.t2*Item.27.t2+ lambda28.t2*Item.28.t2+ lambda29.t2*Item.29.t2+ lambda30.t2*Item.30.t2+ lambda31.t2*Item.31.t2+ lambda32.t2*Item.32.t2+ lambda33.t2*Item.33.t2+ lambda34.t2*Item.34.t2+ lambda35.t2*Item.35.t2+ lambda36.t2*Item.36.t2+ lambda37.t2*Item.37.t2+ lambda38.t2*Item.38.t2+ lambda39.t2*Item.39.t2+ lambda40.t2*Item.40.t2+ lambda41.t2*Item.41.t2+ lambda42.t2*Item.42.t2+ lambda43.t2*Item.43.t2+ lambda44.t2*Item.44.t2+ lambda45.t2*Item.45.t2+ lambda46.t2*Item.46.t2+ lambda47.t2*Item.47.t2+ lambda48.t2*Item.48.t2+ lambda49.t2*Item.49.t2+ lambda50.t2*Item.50.t2
d3 =~ lambda41.t3*Item.41.t3 + lambda42.t3*Item.42.t3 + lambda43.t3*Item.43.t3 + lambda44.t3*Item.44.t3 + lambda45.t3*Item.45.t3 + lambda46.t3*Item.46.t3 + lambda47.t3*Item.47.t3 + lambda48.t3*Item.48.t3 + lambda49.t3*Item.49.t3 + lambda50.t3*Item.50.t3 + lambda51.t3*Item.51.t3 + lambda52.t3*Item.52.t3 + lambda53.t3*Item.53.t3 + lambda54.t3*Item.54.t3 + lambda55.t3*Item.55.t3 + lambda56.t3*Item.56.t3 + lambda57.t3*Item.57.t3 + lambda58.t3*Item.58.t3 + lambda59.t3*Item.59.t3 + lambda60.t3*Item.60.t3 + lambda61.t3*Item.61.t3 + lambda62.t3*Item.62.t3 + lambda63.t3*Item.63.t3 + lambda64.t3*Item.64.t3 + lambda65.t3*Item.65.t3 + lambda66.t3*Item.66.t3 + lambda67.t3*Item.67.t3 + lambda68.t3*Item.68.t3 + lambda69.t3*Item.69.t3 + lambda70.t3*Item.70.t3

lambda1.t1+lambda2.t1+lambda3.t1+lambda4.t1+lambda5.t1+lambda6.t1+lambda7.t1+lambda8.t1+lambda9.t1+lambda10.t1+lambda11.t1+lambda12.t1+lambda13.t1+lambda14.t1+lambda15.t1+lambda16.t1+lambda17.t1+lambda18.t1+lambda19.t1+lambda20.t1+lambda21.t1+lambda22.t1+lambda23.t1+lambda24.t1+lambda25.t1+lambda26.t1+lambda27.t1+lambda28.t1+lambda29.t1+lambda30.t1==30
lambda21.t2+lambda22.t2+lambda23.t2+lambda24.t2+lambda25.t2+lambda26.t2+lambda27.t2+lambda28.t2+lambda29.t2+lambda30.t2+lambda31.t2+lambda32.t2+lambda33.t2+lambda34.t2+lambda35.t2+lambda36.t2+lambda37.t2+lambda38.t2+lambda39.t2+lambda40.t2+lambda41.t2+lambda42.t2+lambda43.t2+lambda44.t2+lambda45.t2+lambda46.t2+lambda47.t2+lambda48.t2+lambda49.t2+lambda50.t2==30
lambda41.t3 + lambda42.t3 + lambda43.t3 + lambda44.t3 + lambda45.t3 + lambda46.t3 + lambda47.t3 + lambda48.t3 + lambda49.t3 + lambda50.t3 + lambda51.t3 + lambda52.t3 + lambda53.t3 + lambda54.t3 + lambda55.t3 + lambda56.t3 + lambda57.t3 + lambda58.t3 + lambda59.t3 + lambda60.t3 + lambda61.t3 + lambda62.t3 + lambda63.t3 + lambda64.t3 + lambda65.t3 + lambda66.t3 + lambda67.t3 + lambda68.t3 + lambda69.t3 + lambda70.t3==30


d1~~d2
d2~~d3
d1~~d3

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



lambda21.t2 ==1.24865675112519
lambda22.t2 ==0.515599014600513
lambda23.t2 ==1.05145488196072
lambda24.t2 ==0.453964212371336
lambda25.t2 ==0.665997170349377
lambda26.t2 ==0.770344052021204
lambda27.t2 ==0.317495465956755
lambda28.t2 ==0.76607864230443
lambda29.t2 ==1.29065234352694
lambda30.t2 ==0.745288706316526
lambda31.t2 ==0.912844562700094
lambda32.t2 ==0.991692689326616
lambda33.t2 ==0.835813142483618
lambda34.t2 ==0.502224163738531
lambda35.t2 ==1.24889759071193
lambda36.t2 ==1.01090652901111
lambda37.t2 ==1.10708781059155
lambda38.t2 ==0.440527600332572
lambda39.t2 ==1.06183488177454
lambda40.t2 ==0.776734132791193
lambda41.t2 ==1.12355600081307
lambda42.t2 ==1.06158785348757
lambda43.t2 ==1.19948620166494
lambda44.t2 ==0.956462962617167
lambda45.t2 ==0.819960290219269
lambda46.t2 ==1.03585517480234
lambda47.t2 ==0.322652153248899
lambda48.t2 ==0.802286752805132
lambda49.t2 ==1.11803841197997
lambda50.t2 ==1.11531720122423

tlambda21.t2 ==-1.31480709232932
tlambda22.t2 ==-0.93320548507609
tlambda23.t2 ==0.633960376782047
tlambda24.t2 ==0.711031314271462
tlambda25.t2 ==0.743707481150658
tlambda26.t2 ==0.952547417648624
tlambda27.t2 ==-0.0562038392915776
tlambda28.t2 ==-0.265436814248522
tlambda29.t2 ==1.62421692172907
tlambda30.t2 ==0.335303345049908
tlambda31.t2 ==0.604055509052579
tlambda32.t2 ==-0.560640430193666
tlambda33.t2 ==-0.770683222667336
tlambda34.t2 ==1.0058309206772
tlambda35.t2 ==0.728934617019349
tlambda36.t2 ==-1.16098304177316
tlambda37.t2 ==-1.62497731719925
tlambda38.t2 ==-0.022502491767487
tlambda39.t2 ==1.8936278191813
tlambda40.t2 ==0.321785020739131
tlambda41.t2 ==2.20436439319413
tlambda42.t2 ==0.992264228311611
tlambda43.t2 ==-0.630859487814159
tlambda44.t2 ==-0.220636663848532
tlambda45.t2 ==-1.17162175329766
tlambda46.t2 ==-2.13143973278746
tlambda47.t2 ==0.321031549591306
tlambda48.t2 ==-1.2730213246044
tlambda49.t2 ==-0.219455302640744
tlambda50.t2 ==0.678606190168499

lambda41.t3 ==1.13674458788625
lambda42.t3 ==1.03740150741995
lambda43.t3 ==1.17813132908303
lambda44.t3 ==0.951498252333523
lambda45.t3 ==0.818258198646234
lambda46.t3 ==1.00660517016793
lambda47.t3 ==0.326924325648512
lambda48.t3 ==0.787731600234125
lambda49.t3 ==1.09120906273515
lambda50.t3 ==1.11460348435176
lambda51.t3 ==0.816233142073153
lambda52.t3 ==1.29287063622832
lambda53.t3 ==0.823998824550205
lambda54.t3 ==0.523945800862641
lambda55.t3 ==0.375846847777514
lambda56.t3 ==0.396986418341473
lambda57.t3 ==0.702432085544673
lambda58.t3 ==0.866761887121046
lambda59.t3 ==0.972944097944099
lambda60.t3 ==0.730579959622904
lambda61.t3 ==1.31354276183209
lambda62.t3 ==0.539129607596811
lambda63.t3 ==0.666819415059735
lambda64.t3 ==0.72650442932979
lambda65.t3 ==0.975108143170974
lambda66.t3 ==0.647650383815503
lambda67.t3 ==0.864115093598389
lambda68.t3 ==1.16520700739012
lambda69.t3 ==0.393811154881989
lambda70.t3 ==1.26307829776951

tlambda41.t3 ==2.25931240844049
tlambda42.t3 ==1.01436659963001
tlambda43.t3 ==-0.632071619276557
tlambda44.t3 ==-0.194947321121622
tlambda45.t3 ==-1.19548441260042
tlambda46.t3 ==-2.02208016717385
tlambda47.t3 ==0.294384369528725
tlambda48.t3 ==-1.26957735518161
tlambda49.t3 ==-0.202095523313382
tlambda50.t3 ==0.686814259672656
tlambda51.t3 ==1.67815538861616
tlambda52.t3 ==0.053281355180257
tlambda53.t3 ==-0.011247802320198
tlambda54.t3 ==-0.760476285713574
tlambda55.t3 ==0.394520185777281
tlambda56.t3 ==-0.0914460922169649
tlambda57.t3 ==0.0977817741751932
tlambda58.t3 ==-0.933009857204826
tlambda59.t3 ==-1.03637570503153
tlambda60.t3 ==0.263676886331173
tlambda61.t3 ==0.449197216531391
tlambda62.t3 ==-1.04749082181893
tlambda63.t3 ==-1.38628062154474
tlambda64.t3 ==0.456069174469632
tlambda65.t3 ==1.77266748692894
tlambda66.t3 ==0.31106898085597
tlambda67.t3 ==0.264735596225932
tlambda68.t3 ==0.196786727698108
tlambda69.t3 ==0.790992463976648
tlambda70.t3 ==0.093319078296131
'

lavaan.model.fit <- lavaan(lavaan.model.t123.t123fixed.nocov, 
                           data = cbind(U1,U2,U3), 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = c(colnames(U1),colnames(U2),colnames(U3)),
                           parameterization = "theta")

lavaan.model.t123.t123fixed.nocov.fit <- lavaan.model.fit

summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]

t123.t123fixed.nocov.sem.param<-get_lmb_tau_alpha_psi(lavaan.model.fit)
t123.t123fixed.nocov.tri.param<-get_a_d_b(t123.t123fixed.nocov.sem.param)

eta123.t123fixed.nocov <-predict(lavaan.model.fit)
mean(eta123.t123fixed.nocov[,1])
mean(eta123.t123fixed.nocov[,2])
mean(eta123.t123fixed.nocov[,3])
sd(eta123.t123fixed.nocov[,1])
sd(eta123.t123fixed.nocov[,2])
sd(eta123.t123fixed.nocov[,3])

lavaan.model.t123.t123fixed.asim.nocov.tag <- function(){}


cat(paste("lambda",1:30,".t1 ==",lambda_sim[1:30],sep="",collapse = "\n"))
cat(paste("tlambda",1:30,".t1 ==",tau_sim[1:30],sep="",collapse = "\n"))

cat(paste("lambda",21:50,".t2 ==",lambda_sim[21:50],sep="",collapse = "\n"))
cat(paste("tlambda",21:50,".t2 ==",tau_sim[21:50],sep="",collapse = "\n"))

cat(paste("lambda",41:70,".t3 ==",lambda_sim[41:70],sep="",collapse = "\n"))
cat(paste("tlambda",41:70,".t3 ==",tau_sim[41:70],sep="",collapse = "\n"))



lavaan.model.t123.t123fixed.asim.nocov <-'

d1 =~ lambda1.t1*Item.1.t1+ lambda2.t1*Item.2.t1+ lambda3.t1*Item.3.t1+ lambda4.t1*Item.4.t1+ lambda5.t1*Item.5.t1+ lambda6.t1*Item.6.t1+ lambda7.t1*Item.7.t1+ lambda8.t1*Item.8.t1+ lambda9.t1*Item.9.t1+ lambda10.t1*Item.10.t1+ lambda11.t1*Item.11.t1+ lambda12.t1*Item.12.t1+ lambda13.t1*Item.13.t1+ lambda14.t1*Item.14.t1+ lambda15.t1*Item.15.t1+ lambda16.t1*Item.16.t1+ lambda17.t1*Item.17.t1+ lambda18.t1*Item.18.t1+ lambda19.t1*Item.19.t1+ lambda20.t1*Item.20.t1+ lambda21.t1*Item.21.t1+ lambda22.t1*Item.22.t1+ lambda23.t1*Item.23.t1+ lambda24.t1*Item.24.t1+ lambda25.t1*Item.25.t1+ lambda26.t1*Item.26.t1+ lambda27.t1*Item.27.t1+ lambda28.t1*Item.28.t1+ lambda29.t1*Item.29.t1+ lambda30.t1*Item.30.t1
d2 =~ lambda21.t2*Item.21.t2+ lambda22.t2*Item.22.t2+ lambda23.t2*Item.23.t2+ lambda24.t2*Item.24.t2+ lambda25.t2*Item.25.t2+ lambda26.t2*Item.26.t2+ lambda27.t2*Item.27.t2+ lambda28.t2*Item.28.t2+ lambda29.t2*Item.29.t2+ lambda30.t2*Item.30.t2+ lambda31.t2*Item.31.t2+ lambda32.t2*Item.32.t2+ lambda33.t2*Item.33.t2+ lambda34.t2*Item.34.t2+ lambda35.t2*Item.35.t2+ lambda36.t2*Item.36.t2+ lambda37.t2*Item.37.t2+ lambda38.t2*Item.38.t2+ lambda39.t2*Item.39.t2+ lambda40.t2*Item.40.t2+ lambda41.t2*Item.41.t2+ lambda42.t2*Item.42.t2+ lambda43.t2*Item.43.t2+ lambda44.t2*Item.44.t2+ lambda45.t2*Item.45.t2+ lambda46.t2*Item.46.t2+ lambda47.t2*Item.47.t2+ lambda48.t2*Item.48.t2+ lambda49.t2*Item.49.t2+ lambda50.t2*Item.50.t2
d3 =~ lambda41.t3*Item.41.t3 + lambda42.t3*Item.42.t3 + lambda43.t3*Item.43.t3 + lambda44.t3*Item.44.t3 + lambda45.t3*Item.45.t3 + lambda46.t3*Item.46.t3 + lambda47.t3*Item.47.t3 + lambda48.t3*Item.48.t3 + lambda49.t3*Item.49.t3 + lambda50.t3*Item.50.t3 + lambda51.t3*Item.51.t3 + lambda52.t3*Item.52.t3 + lambda53.t3*Item.53.t3 + lambda54.t3*Item.54.t3 + lambda55.t3*Item.55.t3 + lambda56.t3*Item.56.t3 + lambda57.t3*Item.57.t3 + lambda58.t3*Item.58.t3 + lambda59.t3*Item.59.t3 + lambda60.t3*Item.60.t3 + lambda61.t3*Item.61.t3 + lambda62.t3*Item.62.t3 + lambda63.t3*Item.63.t3 + lambda64.t3*Item.64.t3 + lambda65.t3*Item.65.t3 + lambda66.t3*Item.66.t3 + lambda67.t3*Item.67.t3 + lambda68.t3*Item.68.t3 + lambda69.t3*Item.69.t3 + lambda70.t3*Item.70.t3

lambda1.t1+lambda2.t1+lambda3.t1+lambda4.t1+lambda5.t1+lambda6.t1+lambda7.t1+lambda8.t1+lambda9.t1+lambda10.t1+lambda11.t1+lambda12.t1+lambda13.t1+lambda14.t1+lambda15.t1+lambda16.t1+lambda17.t1+lambda18.t1+lambda19.t1+lambda20.t1+lambda21.t1+lambda22.t1+lambda23.t1+lambda24.t1+lambda25.t1+lambda26.t1+lambda27.t1+lambda28.t1+lambda29.t1+lambda30.t1==30
lambda21.t2+lambda22.t2+lambda23.t2+lambda24.t2+lambda25.t2+lambda26.t2+lambda27.t2+lambda28.t2+lambda29.t2+lambda30.t2+lambda31.t2+lambda32.t2+lambda33.t2+lambda34.t2+lambda35.t2+lambda36.t2+lambda37.t2+lambda38.t2+lambda39.t2+lambda40.t2+lambda41.t2+lambda42.t2+lambda43.t2+lambda44.t2+lambda45.t2+lambda46.t2+lambda47.t2+lambda48.t2+lambda49.t2+lambda50.t2==30
lambda41.t3 + lambda42.t3 + lambda43.t3 + lambda44.t3 + lambda45.t3 + lambda46.t3 + lambda47.t3 + lambda48.t3 + lambda49.t3 + lambda50.t3 + lambda51.t3 + lambda52.t3 + lambda53.t3 + lambda54.t3 + lambda55.t3 + lambda56.t3 + lambda57.t3 + lambda58.t3 + lambda59.t3 + lambda60.t3 + lambda61.t3 + lambda62.t3 + lambda63.t3 + lambda64.t3 + lambda65.t3 + lambda66.t3 + lambda67.t3 + lambda68.t3 + lambda69.t3 + lambda70.t3==30


d1~~d2
d2~~d3
d1~~d3

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


lambda1.t1 ==0.605768111800353
lambda2.t1 ==0.731050410854043
lambda3.t1 ==0.966925221330078
lambda4.t1 ==1.36099622796096
lambda5.t1 ==0.530766076424742
lambda6.t1 ==1.34945908926874
lambda7.t1 ==1.40384872926598
lambda8.t1 ==1.07026767624776
lambda9.t1 ==1.03303647931713
lambda10.t1 ==0.36637634602534
lambda11.t1 ==0.535810311280025
lambda12.t1 ==0.501241777354871
lambda13.t1 ==1.10108442615486
lambda14.t1 ==0.745127753482652
lambda15.t1 ==1.19840354876446
lambda16.t1 ==0.878612505388036
lambda17.t1 ==1.13703702498757
lambda18.t1 ==1.45934911260926
lambda19.t1 ==0.740346861849993
lambda20.t1 ==1.20733868545217
lambda21.t1 ==1.39213305652868
lambda22.t1 ==0.543058191871513
lambda23.t1 ==1.05954614111149
lambda24.t1 ==0.44131033602975
lambda25.t1 ==0.607779869245044
lambda26.t1 ==0.74749012049791
lambda27.t1 ==0.309506854475788
lambda28.t1 ==0.743111582925996
lambda29.t1 ==1.31573542387834
lambda30.t1 ==0.693712099515488

tlambda1.t1 ==-0.389937930318494
tlambda2.t1 ==0.992592029861341
tlambda3.t1 ==-0.592982404714925
tlambda4.t1 ==-0.904926768708513
tlambda5.t1 ==-0.0502078274584591
tlambda6.t1 ==2.11702243640856
tlambda7.t1 ==2.04591001146434
tlambda8.t1 ==-0.470962542678942
tlambda9.t1 ==1.14592959346492
tlambda10.t1 ==0.675038154936852
tlambda11.t1 ==-0.14004048721003
tlambda12.t1 ==0.426084941092017
tlambda13.t1 ==-0.440458571737896
tlambda14.t1 ==-0.52053983488637
tlambda15.t1 ==1.23237660215154
tlambda16.t1 ==-1.04487321031349
tlambda17.t1 ==0.96021058683591
tlambda18.t1 ==-2.20833423736645
tlambda19.t1 ==-0.753707119979238
tlambda20.t1 ==-1.7226096866107
tlambda21.t1 ==-1.44988199276628
tlambda22.t1 ==-0.958097198298048
tlambda23.t1 ==0.603043901012482
tlambda24.t1 ==0.664205970761459
tlambda25.t1 ==0.678074904751807
tlambda26.t1 ==0.888941640351674
tlambda27.t1 ==-0.0553714526997642
tlambda28.t1 ==-0.267270240482607
tlambda29.t1 ==1.6360919625726
tlambda30.t1 ==0.291173972427605

lambda21.t2 ==1.39213305652868
lambda22.t2 ==0.543058191871513
lambda23.t2 ==1.05954614111149
lambda24.t2 ==0.44131033602975
lambda25.t2 ==0.607779869245044
lambda26.t2 ==0.74749012049791
lambda27.t2 ==0.309506854475788
lambda28.t2 ==0.743111582925996
lambda29.t2 ==1.31573542387834
lambda30.t2 ==0.693712099515488
lambda31.t2 ==0.860258655077597
lambda32.t2 ==0.998314718478365
lambda33.t2 ==0.87372656527482
lambda34.t2 ==0.512594126217444
lambda35.t2 ==1.26600860000821
lambda36.t2 ==1.07927936332828
lambda37.t2 ==1.22707386689368
lambda38.t2 ==0.420615306566223
lambda39.t2 ==1.14419617631049
lambda40.t2 ==0.777055734011195
lambda41.t2 ==1.25845627980712
lambda42.t2 ==1.05412478709744
lambda43.t2 ==1.2137870297173
lambda44.t2 ==0.943638439061499
lambda45.t2 ==0.916239224674156
lambda46.t2 ==1.22133517237304
lambda47.t2 ==0.321188251861096
lambda48.t2 ==0.854559418374342
lambda49.t2 ==1.15430521582867
lambda50.t2 ==1.10779266331682

tlambda21.t2 ==-1.44988199276628
tlambda22.t2 ==-0.958097198298048
tlambda23.t2 ==0.603043901012482
tlambda24.t2 ==0.664205970761459
tlambda25.t2 ==0.678074904751807
tlambda26.t2 ==0.888941640351674
tlambda27.t2 ==-0.0553714526997642
tlambda28.t2 ==-0.267270240482607
tlambda29.t2 ==1.6360919625726
tlambda30.t2 ==0.291173972427605
tlambda31.t2 ==0.532410393103875
tlambda32.t2 ==-0.586221296557703
tlambda33.t2 ==-0.802919254560999
tlambda34.t2 ==1.01018782335366
tlambda35.t2 ==0.676014483289673
tlambda36.t2 ==-1.23811416492105
tlambda37.t2 ==-1.8191500240886
tlambda38.t2 ==-0.0368155588236749
tlambda39.t2 ==1.94089754741391
tlambda40.t2 ==0.306971103259464
tlambda41.t2 ==2.39696000513464
tlambda42.t2 ==0.977352928962532
tlambda43.t2 ==-0.695612063284296
tlambda44.t2 ==-0.25865623873325
tlambda45.t2 ==-1.28928946757585
tlambda46.t2 ==-2.37878193313096
tlambda47.t2 ==0.276949151716891
tlambda48.t2 ==-1.35641059480417
tlambda49.t2 ==-0.248017026280806
tlambda50.t2 ==0.620811639704351


lambda41.t3 ==1.25845627980712
lambda42.t3 ==1.05412478709744
lambda43.t3 ==1.2137870297173
lambda44.t3 ==0.943638439061499
lambda45.t3 ==0.916239224674156
lambda46.t3 ==1.22133517237304
lambda47.t3 ==0.321188251861096
lambda48.t3 ==0.854559418374342
lambda49.t3 ==1.15430521582867
lambda50.t3 ==1.10779266331682
lambda51.t3 ==0.855017182297926
lambda52.t3 ==1.30576906795805
lambda53.t3 ==0.808574744094203
lambda54.t3 ==0.581430407792557
lambda55.t3 ==0.376826142355992
lambda56.t3 ==0.410653537218092
lambda57.t3 ==0.66541916237691
lambda58.t3 ==0.903213000227068
lambda59.t3 ==1.07168634126639
lambda60.t3 ==0.771833357448251
lambda61.t3 ==1.36648169713211
lambda62.t3 ==0.63878187163351
lambda63.t3 ==0.833214719459134
lambda64.t3 ==0.68436506955056
lambda65.t3 ==1.05860219397237
lambda66.t3 ==0.596964489679552
lambda67.t3 ==0.856104874589451
lambda68.t3 ==1.19425460710447
lambda69.t3 ==0.392769582101202
lambda70.t3 ==1.32235173917388

tlambda41.t3 ==2.39696000513464
tlambda42.t3 ==0.977352928962532
tlambda43.t3 ==-0.695612063284296
tlambda44.t3 ==-0.25865623873325
tlambda45.t3 ==-1.28928946757585
tlambda46.t3 ==-2.37878193313096
tlambda47.t3 ==0.276949151716891
tlambda48.t3 ==-1.35641059480417
tlambda49.t3 ==-0.248017026280806
tlambda50.t3 ==0.620811639704351
tlambda51.t3 ==1.68212188422228
tlambda52.t3 ==-0.0230150789429684
tlambda53.t3 ==-0.050618317311655
tlambda54.t3 ==-0.759482225621904
tlambda55.t3 ==0.38409277404265
tlambda56.t3 ==-0.0757319217347629
tlambda57.t3 ==0.0297303526434214
tlambda58.t3 ==-1.05659622264679
tlambda59.t3 ==-1.16317344898993
tlambda60.t3 ==0.295494845849919
tlambda61.t3 ==0.409245954304556
tlambda62.t3 ==-1.0806544269744
tlambda63.t3 ==-1.54797770272375
tlambda64.t3 ==0.390896987794135
tlambda65.t3 ==1.81493196281325
tlambda66.t3 ==0.234230771410254
tlambda67.t3 ==0.208549708946123
tlambda68.t3 ==0.12433491678701
tlambda69.t3 ==0.762122593446743
tlambda70.t3 ==0.0404207082074428
'

working <- function(){}


lavaan.model.t123.t123fixed.asim.nocov.fit <- lavaan(
                           lavaan.model.t123.t123fixed.asim.nocov, 
                           data = cbind(U1,U2,U3), 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = c(colnames(U1),colnames(U2),colnames(U3)),
                           parameterization = "theta")

lavaan.model.t123.t123fixed.asim.nocov.fit <- lavaan.model.fit

summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]

t123.t123fixed.asim.nocov.sem.param<-get_lmb_tau_alpha_psi(lavaan.model.fit)
t123.t123fixed.asim.nocov.tri.param<-get_a_d_b(t123.t123fixed.asim.nocov.sem.param)

eta123.t123fixed.asim.nocov.free <-predict(lavaan.model.fit)
mean(eta123.t123fixed.asim.nocov.free[,1])
mean(eta123.t123fixed.asim.nocov.free[,2])
mean(eta123.t123fixed.asim.nocov.free[,3])
sd(eta123.t123fixed.asim.nocov.free[,1])
sd(eta123.t123fixed.asim.nocov.free[,2])
sd(eta123.t123fixed.asim.nocov.free[,3])
cov(eta123.t123fixed.asim.nocov.free)



UNTIL_HERE <- function(){}


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# IGNORE
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


plot(data)

lavaan.model.t2.fixed.tag <- function(){}

lavaan.model.t2 <-'

d2 =~ lambda21.t2*Item.21.t2+ lambda22.t2*Item.22.t2+ lambda23.t2*Item.23.t2+ lambda24.t2*Item.24.t2+ lambda25.t2*Item.25.t2+ lambda26.t2*Item.26.t2+ lambda27.t2*Item.27.t2+ lambda28.t2*Item.28.t2+ lambda29.t2*Item.29.t2+ lambda30.t2*Item.30.t2+ lambda31.t2*Item.31.t2+ lambda32.t2*Item.32.t2+ lambda33.t2*Item.33.t2+ lambda34.t2*Item.34.t2+ lambda35.t2*Item.35.t2+ lambda36.t2*Item.36.t2+ lambda37.t2*Item.37.t2+ lambda38.t2*Item.38.t2+ lambda39.t2*Item.39.t2+ lambda40.t2*Item.40.t2+ lambda41.t2*Item.41.t2+ lambda42.t2*Item.42.t2+ lambda43.t2*Item.43.t2+ lambda44.t2*Item.44.t2+ lambda45.t2*Item.45.t2+ lambda46.t2*Item.46.t2+ lambda47.t2*Item.47.t2+ lambda48.t2*Item.48.t2+ lambda49.t2*Item.49.t2+ lambda50.t2*Item.50.t2


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


lambda21.t2 ==1.39213305652868
lambda22.t2 ==0.543058191871513
lambda23.t2 ==1.05954614111149
lambda24.t2 ==0.44131033602975
lambda25.t2 ==0.607779869245044
lambda26.t2 ==0.74749012049791
lambda27.t2 ==0.309506854475788
lambda28.t2 ==0.743111582925996
lambda29.t2 ==1.31573542387834
lambda30.t2 ==0.693712099515488
lambda31.t2 ==0.860258655077597
lambda32.t2 ==0.998314718478365
lambda33.t2 ==0.87372656527482
lambda34.t2 ==0.512594126217444
lambda35.t2 ==1.26600860000821
lambda36.t2 ==1.07927936332828
lambda37.t2 ==1.22707386689368
lambda38.t2 ==0.420615306566223
lambda39.t2 ==1.14419617631049
lambda40.t2 ==0.777055734011195
lambda41.t2 ==1.25845627980712
lambda42.t2 ==1.05412478709744
lambda43.t2 ==1.2137870297173
lambda44.t2 ==0.943638439061499
lambda45.t2 ==0.916239224674156
lambda46.t2 ==1.22133517237304
lambda47.t2 ==0.321188251861096
lambda48.t2 ==0.854559418374342
lambda49.t2 ==1.15430521582867
lambda50.t2 ==1.10779266331682

tlambda21.t2 ==-1.44988199276628
tlambda22.t2 ==-0.958097198298048
tlambda23.t2 ==0.603043901012482
tlambda24.t2 ==0.664205970761459
tlambda25.t2 ==0.678074904751807
tlambda26.t2 ==0.888941640351674
tlambda27.t2 ==-0.0553714526997642
tlambda28.t2 ==-0.267270240482607
tlambda29.t2 ==1.6360919625726
tlambda30.t2 ==0.291173972427605
tlambda31.t2 ==0.532410393103875
tlambda32.t2 ==-0.586221296557703
tlambda33.t2 ==-0.802919254560999
tlambda34.t2 ==1.01018782335366
tlambda35.t2 ==0.676014483289673
tlambda36.t2 ==-1.23811416492105
tlambda37.t2 ==-1.8191500240886
tlambda38.t2 ==-0.0368155588236749
tlambda39.t2 ==1.94089754741391
tlambda40.t2 ==0.306971103259464
tlambda41.t2 ==2.39696000513464
tlambda42.t2 ==0.977352928962532
tlambda43.t2 ==-0.695612063284296
tlambda44.t2 ==-0.25865623873325
tlambda45.t2 ==-1.28928946757585
tlambda46.t2 ==-2.37878193313096
tlambda47.t2 ==0.276949151716891
tlambda48.t2 ==-1.35641059480417
tlambda49.t2 ==-0.248017026280806
tlambda50.t2 ==0.620811639704351

'

lavaan.model.fit <- lavaan(lavaan.model.t2, 
                           data = U2, 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = colnames(U2),
                           parameterization = "theta")


summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
eta2<-predict(lavaan.model.fit)
mean(eta2)
sd(eta2)


lavaan.model.t12.all.fixed.tag <- function(){}

lavaan.model.t12.all.fixed <-'

d1 =~ lambda1.t1*Item.1.t1+ lambda2.t1*Item.2.t1+ lambda3.t1*Item.3.t1+ lambda4.t1*Item.4.t1+ lambda5.t1*Item.5.t1+ lambda6.t1*Item.6.t1+ lambda7.t1*Item.7.t1+ lambda8.t1*Item.8.t1+ lambda9.t1*Item.9.t1+ lambda10.t1*Item.10.t1+ lambda11.t1*Item.11.t1+ lambda12.t1*Item.12.t1+ lambda13.t1*Item.13.t1+ lambda14.t1*Item.14.t1+ lambda15.t1*Item.15.t1+ lambda16.t1*Item.16.t1+ lambda17.t1*Item.17.t1+ lambda18.t1*Item.18.t1+ lambda19.t1*Item.19.t1+ lambda20.t1*Item.20.t1+ lambda21.t1*Item.21.t1+ lambda22.t1*Item.22.t1+ lambda23.t1*Item.23.t1+ lambda24.t1*Item.24.t1+ lambda25.t1*Item.25.t1+ lambda26.t1*Item.26.t1+ lambda27.t1*Item.27.t1+ lambda28.t1*Item.28.t1+ lambda29.t1*Item.29.t1+ lambda30.t1*Item.30.t1
d2 =~ lambda21.t2*Item.21.t2+ lambda22.t2*Item.22.t2+ lambda23.t2*Item.23.t2+ lambda24.t2*Item.24.t2+ lambda25.t2*Item.25.t2+ lambda26.t2*Item.26.t2+ lambda27.t2*Item.27.t2+ lambda28.t2*Item.28.t2+ lambda29.t2*Item.29.t2+ lambda30.t2*Item.30.t2+ lambda31.t2*Item.31.t2+ lambda32.t2*Item.32.t2+ lambda33.t2*Item.33.t2+ lambda34.t2*Item.34.t2+ lambda35.t2*Item.35.t2+ lambda36.t2*Item.36.t2+ lambda37.t2*Item.37.t2+ lambda38.t2*Item.38.t2+ lambda39.t2*Item.39.t2+ lambda40.t2*Item.40.t2+ lambda41.t2*Item.41.t2+ lambda42.t2*Item.42.t2+ lambda43.t2*Item.43.t2+ lambda44.t2*Item.44.t2+ lambda45.t2*Item.45.t2+ lambda46.t2*Item.46.t2+ lambda47.t2*Item.47.t2+ lambda48.t2*Item.48.t2+ lambda49.t2*Item.49.t2+ lambda50.t2*Item.50.t2

d1~~d2


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


Item.1.t1 ~~ 1*Item.1.t1 + 0*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + 0*Item.5.t1 + 0*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2 
Item.2.t1 ~~ 1*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + 0*Item.5.t1 + 0*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2 
Item.3.t1 ~~ 1*Item.3.t1 + 0*Item.4.t1 + 0*Item.5.t1 + 0*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.4.t1 ~~ 1*Item.4.t1 + 0*Item.5.t1 + 0*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.5.t1 ~~ 1*Item.5.t1 + 0*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.6.t1 ~~ 1*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.7.t1 ~~ 1*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.8.t1 ~~ 1*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.9.t1 ~~ 1*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.10.t1 ~~ 1*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.11.t1 ~~ 1*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.12.t1 ~~ 1*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.13.t1 ~~ 1*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.14.t1 ~~ 1*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.15.t1 ~~ 1*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.16.t1 ~~ 1*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.17.t1 ~~ 1*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.18.t1 ~~ 1*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.19.t1 ~~ 1*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.20.t1 ~~ 1*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.21.t1 ~~ 1*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 1*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.22.t1 ~~ 1*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 1*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.23.t1 ~~ 1*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 1*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.24.t1 ~~ 1*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 1*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.25.t1 ~~ 1*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 1*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.26.t1 ~~ 1*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 1*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.27.t1 ~~ 1*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 1*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.28.t1 ~~ 1*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 1*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.29.t1 ~~ 1*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 1*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.30.t1 ~~ 1*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 1*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.21.t2 ~~ 1*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.22.t2 ~~ 1*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.23.t2 ~~ 1*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.24.t2 ~~ 1*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.25.t2 ~~ 1*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.26.t2 ~~ 1*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.27.t2 ~~ 1*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.28.t2 ~~ 1*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.29.t2 ~~ 1*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.30.t2 ~~ 1*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.31.t2 ~~ 1*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.32.t2 ~~ 1*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.33.t2 ~~ 1*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.34.t2 ~~ 1*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.35.t2 ~~ 1*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.36.t2 ~~ 1*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.37.t2 ~~ 1*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.38.t2 ~~ 1*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.39.t2 ~~ 1*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.40.t2 ~~ 1*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.41.t2 ~~ 1*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.42.t2 ~~ 1*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.43.t2 ~~ 1*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.44.t2 ~~ 1*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.45.t2 ~~ 1*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.46.t2 ~~ 1*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.47.t2 ~~ 1*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.48.t2 ~~ 1*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.49.t2 ~~ 1*Item.49.t2 + 0*Item.50.t2                              
Item.50.t2 ~~ 1*Item.50.t2      


lambda1.t1 ==0.605768111800353
lambda2.t1 ==0.731050410854043
lambda3.t1 ==0.966925221330078
lambda4.t1 ==1.36099622796096
lambda5.t1 ==0.530766076424742
lambda6.t1 ==1.34945908926874
lambda7.t1 ==1.40384872926598
lambda8.t1 ==1.07026767624776
lambda9.t1 ==1.03303647931713
lambda10.t1 ==0.36637634602534
lambda11.t1 ==0.535810311280025
lambda12.t1 ==0.501241777354871
lambda13.t1 ==1.10108442615486
lambda14.t1 ==0.745127753482652
lambda15.t1 ==1.19840354876446
lambda16.t1 ==0.878612505388036
lambda17.t1 ==1.13703702498757
lambda18.t1 ==1.45934911260926
lambda19.t1 ==0.740346861849993
lambda20.t1 ==1.20733868545217
lambda21.t1 ==1.39213305652868
lambda22.t1 ==0.543058191871513
lambda23.t1 ==1.05954614111149
lambda24.t1 ==0.44131033602975
lambda25.t1 ==0.607779869245044
lambda26.t1 ==0.74749012049791
lambda27.t1 ==0.309506854475788
lambda28.t1 ==0.743111582925996
lambda29.t1 ==1.31573542387834
lambda30.t1 ==0.693712099515488

tlambda1.t1 ==-0.389937930318494
tlambda2.t1 ==0.992592029861341
tlambda3.t1 ==-0.592982404714925
tlambda4.t1 ==-0.904926768708513
tlambda5.t1 ==-0.0502078274584591
tlambda6.t1 ==2.11702243640856
tlambda7.t1 ==2.04591001146434
tlambda8.t1 ==-0.470962542678942
tlambda9.t1 ==1.14592959346492
tlambda10.t1 ==0.675038154936852
tlambda11.t1 ==-0.14004048721003
tlambda12.t1 ==0.426084941092017
tlambda13.t1 ==-0.440458571737896
tlambda14.t1 ==-0.52053983488637
tlambda15.t1 ==1.23237660215154
tlambda16.t1 ==-1.04487321031349
tlambda17.t1 ==0.96021058683591
tlambda18.t1 ==-2.20833423736645
tlambda19.t1 ==-0.753707119979238
tlambda20.t1 ==-1.7226096866107
tlambda21.t1 ==-1.44988199276628
tlambda22.t1 ==-0.958097198298048
tlambda23.t1 ==0.603043901012482
tlambda24.t1 ==0.664205970761459
tlambda25.t1 ==0.678074904751807
tlambda26.t1 ==0.888941640351674
tlambda27.t1 ==-0.0553714526997642
tlambda28.t1 ==-0.267270240482607
tlambda29.t1 ==1.6360919625726
tlambda30.t1 ==0.291173972427605


lambda21.t2 ==1.39213305652868
lambda22.t2 ==0.543058191871513
lambda23.t2 ==1.05954614111149
lambda24.t2 ==0.44131033602975
lambda25.t2 ==0.607779869245044
lambda26.t2 ==0.74749012049791
lambda27.t2 ==0.309506854475788
lambda28.t2 ==0.743111582925996
lambda29.t2 ==1.31573542387834
lambda30.t2 ==0.693712099515488
lambda31.t2 ==0.860258655077597
lambda32.t2 ==0.998314718478365
lambda33.t2 ==0.87372656527482
lambda34.t2 ==0.512594126217444
lambda35.t2 ==1.26600860000821
lambda36.t2 ==1.07927936332828
lambda37.t2 ==1.22707386689368
lambda38.t2 ==0.420615306566223
lambda39.t2 ==1.14419617631049
lambda40.t2 ==0.777055734011195
lambda41.t2 ==1.25845627980712
lambda42.t2 ==1.05412478709744
lambda43.t2 ==1.2137870297173
lambda44.t2 ==0.943638439061499
lambda45.t2 ==0.916239224674156
lambda46.t2 ==1.22133517237304
lambda47.t2 ==0.321188251861096
lambda48.t2 ==0.854559418374342
lambda49.t2 ==1.15430521582867
lambda50.t2 ==1.10779266331682

tlambda21.t2 ==-1.44988199276628
tlambda22.t2 ==-0.958097198298048
tlambda23.t2 ==0.603043901012482
tlambda24.t2 ==0.664205970761459
tlambda25.t2 ==0.678074904751807
tlambda26.t2 ==0.888941640351674
tlambda27.t2 ==-0.0553714526997642
tlambda28.t2 ==-0.267270240482607
tlambda29.t2 ==1.6360919625726
tlambda30.t2 ==0.291173972427605
tlambda31.t2 ==0.532410393103875
tlambda32.t2 ==-0.586221296557703
tlambda33.t2 ==-0.802919254560999
tlambda34.t2 ==1.01018782335366
tlambda35.t2 ==0.676014483289673
tlambda36.t2 ==-1.23811416492105
tlambda37.t2 ==-1.8191500240886
tlambda38.t2 ==-0.0368155588236749
tlambda39.t2 ==1.94089754741391
tlambda40.t2 ==0.306971103259464
tlambda41.t2 ==2.39696000513464
tlambda42.t2 ==0.977352928962532
tlambda43.t2 ==-0.695612063284296
tlambda44.t2 ==-0.25865623873325
tlambda45.t2 ==-1.28928946757585
tlambda46.t2 ==-2.37878193313096
tlambda47.t2 ==0.276949151716891
tlambda48.t2 ==-1.35641059480417
tlambda49.t2 ==-0.248017026280806
tlambda50.t2 ==0.620811639704351
'


lavaan.model.fit <- lavaan(lavaan.model.t12.all.fixed, 
                           data = cbind(U1,U2), 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = c(colnames(U1),colnames(U2)),
                           parameterization = "theta")


summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
eta12<-predict(lavaan.model.fit)
mean(eta12[,1])
sd(eta12[,1])

mean(eta12[,2])
sd(eta12[,2])



lavaan.model.t12.t1common.fixed.tag <- function(){}

lavaan.model.t12.t1common.fixed.tag <-'

d1 =~ lambda1.t1*Item.1.t1+ lambda2.t1*Item.2.t1+ lambda3.t1*Item.3.t1+ lambda4.t1*Item.4.t1+ lambda5.t1*Item.5.t1+ lambda6.t1*Item.6.t1+ lambda7.t1*Item.7.t1+ lambda8.t1*Item.8.t1+ lambda9.t1*Item.9.t1+ lambda10.t1*Item.10.t1+ lambda11.t1*Item.11.t1+ lambda12.t1*Item.12.t1+ lambda13.t1*Item.13.t1+ lambda14.t1*Item.14.t1+ lambda15.t1*Item.15.t1+ lambda16.t1*Item.16.t1+ lambda17.t1*Item.17.t1+ lambda18.t1*Item.18.t1+ lambda19.t1*Item.19.t1+ lambda20.t1*Item.20.t1+ lambda21.t1*Item.21.t1+ lambda22.t1*Item.22.t1+ lambda23.t1*Item.23.t1+ lambda24.t1*Item.24.t1+ lambda25.t1*Item.25.t1+ lambda26.t1*Item.26.t1+ lambda27.t1*Item.27.t1+ lambda28.t1*Item.28.t1+ lambda29.t1*Item.29.t1+ lambda30.t1*Item.30.t1
d2 =~ lambda21.t2*Item.21.t2+ lambda22.t2*Item.22.t2+ lambda23.t2*Item.23.t2+ lambda24.t2*Item.24.t2+ lambda25.t2*Item.25.t2+ lambda26.t2*Item.26.t2+ lambda27.t2*Item.27.t2+ lambda28.t2*Item.28.t2+ lambda29.t2*Item.29.t2+ lambda30.t2*Item.30.t2+ lambda31.t2*Item.31.t2+ lambda32.t2*Item.32.t2+ lambda33.t2*Item.33.t2+ lambda34.t2*Item.34.t2+ lambda35.t2*Item.35.t2+ lambda36.t2*Item.36.t2+ lambda37.t2*Item.37.t2+ lambda38.t2*Item.38.t2+ lambda39.t2*Item.39.t2+ lambda40.t2*Item.40.t2+ lambda41.t2*Item.41.t2+ lambda42.t2*Item.42.t2+ lambda43.t2*Item.43.t2+ lambda44.t2*Item.44.t2+ lambda45.t2*Item.45.t2+ lambda46.t2*Item.46.t2+ lambda47.t2*Item.47.t2+ lambda48.t2*Item.48.t2+ lambda49.t2*Item.49.t2+ lambda50.t2*Item.50.t2

d1~~d2


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


Item.1.t1 ~~ 1*Item.1.t1 + 0*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + 0*Item.5.t1 + 0*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2 
Item.2.t1 ~~ 1*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + 0*Item.5.t1 + 0*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2 
Item.3.t1 ~~ 1*Item.3.t1 + 0*Item.4.t1 + 0*Item.5.t1 + 0*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.4.t1 ~~ 1*Item.4.t1 + 0*Item.5.t1 + 0*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.5.t1 ~~ 1*Item.5.t1 + 0*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.6.t1 ~~ 1*Item.6.t1 + 0*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.7.t1 ~~ 1*Item.7.t1 + 0*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.8.t1 ~~ 1*Item.8.t1 + 0*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.9.t1 ~~ 1*Item.9.t1 + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.10.t1 ~~ 1*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.11.t1 ~~ 1*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.12.t1 ~~ 1*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.13.t1 ~~ 1*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.14.t1 ~~ 1*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.15.t1 ~~ 1*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.16.t1 ~~ 1*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.17.t1 ~~ 1*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.18.t1 ~~ 1*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.19.t1 ~~ 1*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.20.t1 ~~ 1*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.21.t1 ~~ 1*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 1*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.22.t1 ~~ 1*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 1*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.23.t1 ~~ 1*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 1*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.24.t1 ~~ 1*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 1*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.25.t1 ~~ 1*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 1*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.26.t1 ~~ 1*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 1*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.27.t1 ~~ 1*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 1*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.28.t1 ~~ 1*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 1*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.29.t1 ~~ 1*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 1*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.30.t1 ~~ 1*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 1*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.21.t2 ~~ 1*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.22.t2 ~~ 1*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.23.t2 ~~ 1*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.24.t2 ~~ 1*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.25.t2 ~~ 1*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.26.t2 ~~ 1*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.27.t2 ~~ 1*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.28.t2 ~~ 1*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.29.t2 ~~ 1*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.30.t2 ~~ 1*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.31.t2 ~~ 1*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.32.t2 ~~ 1*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.33.t2 ~~ 1*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.34.t2 ~~ 1*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.35.t2 ~~ 1*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.36.t2 ~~ 1*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.37.t2 ~~ 1*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.38.t2 ~~ 1*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.39.t2 ~~ 1*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.40.t2 ~~ 1*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.41.t2 ~~ 1*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.42.t2 ~~ 1*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.43.t2 ~~ 1*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.44.t2 ~~ 1*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.45.t2 ~~ 1*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.46.t2 ~~ 1*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.47.t2 ~~ 1*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.48.t2 ~~ 1*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.49.t2 ~~ 1*Item.49.t2 + 0*Item.50.t2                              
Item.50.t2 ~~ 1*Item.50.t2      


lambda1.t1 ==0.605768111800353
lambda2.t1 ==0.731050410854043
lambda3.t1 ==0.966925221330078
lambda4.t1 ==1.36099622796096
lambda5.t1 ==0.530766076424742
lambda6.t1 ==1.34945908926874
lambda7.t1 ==1.40384872926598
lambda8.t1 ==1.07026767624776
lambda9.t1 ==1.03303647931713
lambda10.t1 ==0.36637634602534
lambda11.t1 ==0.535810311280025
lambda12.t1 ==0.501241777354871
lambda13.t1 ==1.10108442615486
lambda14.t1 ==0.745127753482652
lambda15.t1 ==1.19840354876446
lambda16.t1 ==0.878612505388036
lambda17.t1 ==1.13703702498757
lambda18.t1 ==1.45934911260926
lambda19.t1 ==0.740346861849993
lambda20.t1 ==1.20733868545217
lambda21.t1 ==1.39213305652868
lambda22.t1 ==0.543058191871513
lambda23.t1 ==1.05954614111149
lambda24.t1 ==0.44131033602975
lambda25.t1 ==0.607779869245044
lambda26.t1 ==0.74749012049791
lambda27.t1 ==0.309506854475788
lambda28.t1 ==0.743111582925996
lambda29.t1 ==1.31573542387834
lambda30.t1 ==0.693712099515488


lambda21.t2 ==1.39213305652868
lambda22.t2 ==0.543058191871513
lambda23.t2 ==1.05954614111149
lambda24.t2 ==0.44131033602975
lambda25.t2 ==0.607779869245044
lambda26.t2 ==0.74749012049791
lambda27.t2 ==0.309506854475788
lambda28.t2 ==0.743111582925996
lambda29.t2 ==1.31573542387834
lambda30.t2 ==0.693712099515488



tlambda1.t1 ==-0.389937930318494
tlambda2.t1 ==0.992592029861341
tlambda3.t1 ==-0.592982404714925
tlambda4.t1 ==-0.904926768708513
tlambda5.t1 ==-0.0502078274584591
tlambda6.t1 ==2.11702243640856
tlambda7.t1 ==2.04591001146434
tlambda8.t1 ==-0.470962542678942
tlambda9.t1 ==1.14592959346492
tlambda10.t1 ==0.675038154936852
tlambda11.t1 ==-0.14004048721003
tlambda12.t1 ==0.426084941092017
tlambda13.t1 ==-0.440458571737896
tlambda14.t1 ==-0.52053983488637
tlambda15.t1 ==1.23237660215154
tlambda16.t1 ==-1.04487321031349
tlambda17.t1 ==0.96021058683591
tlambda18.t1 ==-2.20833423736645
tlambda19.t1 ==-0.753707119979238
tlambda20.t1 ==-1.7226096866107
tlambda21.t1 ==-1.44988199276628
tlambda22.t1 ==-0.958097198298048
tlambda23.t1 ==0.603043901012482
tlambda24.t1 ==0.664205970761459
tlambda25.t1 ==0.678074904751807
tlambda26.t1 ==0.888941640351674
tlambda27.t1 ==-0.0553714526997642
tlambda28.t1 ==-0.267270240482607
tlambda29.t1 ==1.6360919625726
tlambda30.t1 ==0.291173972427605


tlambda21.t2 ==-1.44988199276628
tlambda22.t2 ==-0.958097198298048
tlambda23.t2 ==0.603043901012482
tlambda24.t2 ==0.664205970761459
tlambda25.t2 ==0.678074904751807
tlambda26.t2 ==0.888941640351674
tlambda27.t2 ==-0.0553714526997642
tlambda28.t2 ==-0.267270240482607
tlambda29.t2 ==1.6360919625726
tlambda30.t2 ==0.291173972427605
'


lavaan.model.fit <- lavaan(lavaan.model.t12.t1common.fixed.tag, 
                           data = cbind(U1,U2), 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = c(colnames(U1),colnames(U2)),
                           parameterization = "theta")


summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
eta12<-predict(lavaan.model.fit)


lambda2 <- lavInspect(lavaan.model.fit,what = 'est')$lambda
colnames(lambda2) <- c("lambda1","lambda2")

#getting tau values
tau <- lavInspect(lavaan.model.fit,what = 'est')$tau
colnames(tau) <- c("tau")

alpha <- lavInspect(lavaan.model.fit,what = 'mean.lv')
psi <- lavInspect(lavaan.model.fit,what = 'cov.lv')

a_est <- rep(NA,60)
d_est <- rep(NA,60)


for(i in seq(1,60,1)){
  if(i<31){
    a_est[i] <- lambda2[i,1]*sqrt(psi[1,1])*1.7
    d_est[i] <- (-tau[i] +lambda2[i,1]*alpha[1]) *1.7
    
  }else{
    a_est[i] <- lambda2[i,2]*sqrt(psi[2,2])*1.7
    d_est[i] <- (-tau[i] +lambda2[i,2]*alpha[2]) *1.7
  }
}

b_est = -d_est/a_est

a_comp <- data.frame(cbind(sim = c(a[1:30],a[21:50]), 
                           est = a_est,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                     label=1:60)
str(a_comp)
a_comp[,"sim"]<-as.numeric(a_comp[,"sim"])
a_comp[,"est"]<-as.numeric(a_comp[,"est"])

ggplot(a_comp, 
       aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("a")


ggplot(a_comp, 
       aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("a")+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')



b_comp <- data.frame(cbind(sim = c(b[1:30],b[21:50]), 
                           est = b_est,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20)),
                           label=1:60))
str(b_comp)
b_comp[,"sim"]<-as.numeric(b_comp[,"sim"])
b_comp[,"est"]<-as.numeric(b_comp[,"est"])
ggplot(b_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

ggplot(b_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("b")+
  geom_smooth(data=b_comp[41:60,],method="lm",aes(x=sim,y=est))+
  geom_smooth(data=b_comp[31:40,],method="lm",aes(x=sim,y=est))

lm(formula = est ~ sim, data = b_comp[31:40,])
lm(formula = est ~ sim, data = b_comp[41:60,])



l_comp <- data.frame(cbind(sim = c(lambda_sim[1:30],lambda_sim[21:50]), 
                           est = c(lambda2[1:30,1],lambda2[31:60,2]),
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                     label = 1:60)
l_comp[,"sim"]<-as.numeric(l_comp[,"sim"])
l_comp[,"est"]<-as.numeric(l_comp[,"est"])
ggplot(l_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')



t_comp <- data.frame(cbind(sim = c(tau_sim[1:30],tau_sim[21:50]),
                           est = tau,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                     label = 1:60)
colnames(t_comp)[2] <- "est"
t_comp[,"sim"]<-as.numeric(t_comp[,"sim"])
t_comp[,"est"]<-as.numeric(t_comp[,"est"])

ggplot(data=t_comp, aes(x=sim, y=est, color=tag,label = label)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  scale_x_continuous()

ggplot(data=t_comp, aes(x=sim, y=est, color=tag,label = label)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')


lavaan.model.t12.common.fixed.tag <- function(){}

lavaan.model.t12.common.fixed <-'

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

Item.1.t1  ~~ 1*Item.1.t1  + 0*Item.2.t1  + 0*Item.3.t1  + 0*Item.4.t1  + 0*Item.5.t1  + 0*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2 
Item.2.t1  ~~ 1*Item.2.t1  + 0*Item.3.t1  + 0*Item.4.t1  + 0*Item.5.t1  + 0*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2 
Item.3.t1  ~~ 1*Item.3.t1  + 0*Item.4.t1  + 0*Item.5.t1  + 0*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.4.t1  ~~ 1*Item.4.t1  + 0*Item.5.t1  + 0*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.5.t1  ~~ 1*Item.5.t1  + 0*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.6.t1  ~~ 1*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.7.t1  ~~ 1*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.8.t1  ~~ 1*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.9.t1  ~~ 1*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.10.t1 ~~ 1*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.11.t1 ~~ 1*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.12.t1 ~~ 1*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.13.t1 ~~ 1*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.14.t1 ~~ 1*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.15.t1 ~~ 1*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.16.t1 ~~ 1*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.17.t1 ~~ 1*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.18.t1 ~~ 1*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.19.t1 ~~ 1*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.20.t1 ~~ 1*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.21.t1 ~~ 1*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 1*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.22.t1 ~~ 1*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 1*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.23.t1 ~~ 1*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 1*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.24.t1 ~~ 1*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 1*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.25.t1 ~~ 1*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 1*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.26.t1 ~~ 1*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 1*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.27.t1 ~~ 1*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 1*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.28.t1 ~~ 1*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 1*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.29.t1 ~~ 1*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 1*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.30.t1 ~~ 1*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 1*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.21.t2 ~~ 1*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.22.t2 ~~ 1*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.23.t2 ~~ 1*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.24.t2 ~~ 1*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.25.t2 ~~ 1*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.26.t2 ~~ 1*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.27.t2 ~~ 1*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.28.t2 ~~ 1*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.29.t2 ~~ 1*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.30.t2 ~~ 1*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.31.t2 ~~ 1*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.32.t2 ~~ 1*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.33.t2 ~~ 1*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.34.t2 ~~ 1*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.35.t2 ~~ 1*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.36.t2 ~~ 1*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.37.t2 ~~ 1*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.38.t2 ~~ 1*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.39.t2 ~~ 1*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.40.t2 ~~ 1*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.41.t2 ~~ 1*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.42.t2 ~~ 1*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.43.t2 ~~ 1*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.44.t2 ~~ 1*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.45.t2 ~~ 1*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.46.t2 ~~ 1*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.47.t2 ~~ 1*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.48.t2 ~~ 1*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.49.t2 ~~ 1*Item.49.t2 + 0*Item.50.t2                              
Item.50.t2 ~~ 1*Item.50.t2 

lambda21.t1 ==1.39213305652868
lambda22.t1 ==0.543058191871513
lambda23.t1 ==1.05954614111149
lambda24.t1 ==0.44131033602975
lambda25.t1 ==0.607779869245044
lambda26.t1 ==0.74749012049791
lambda27.t1 ==0.309506854475788
lambda28.t1 ==0.743111582925996
lambda29.t1 ==1.31573542387834
lambda30.t1 ==0.693712099515488

lambda21.t2 ==1.39213305652868
lambda22.t2 ==0.543058191871513
lambda23.t2 ==1.05954614111149
lambda24.t2 ==0.44131033602975
lambda25.t2 ==0.607779869245044
lambda26.t2 ==0.74749012049791
lambda27.t2 ==0.309506854475788
lambda28.t2 ==0.743111582925996
lambda29.t2 ==1.31573542387834
lambda30.t2 ==0.693712099515488

tlambda21.t1 ==-1.44988199276628
tlambda22.t1 ==-0.958097198298048
tlambda23.t1 ==0.603043901012482
tlambda24.t1 ==0.664205970761459
tlambda25.t1 ==0.678074904751807
tlambda26.t1 ==0.888941640351674
tlambda27.t1 ==-0.0553714526997642
tlambda28.t1 ==-0.267270240482607
tlambda29.t1 ==1.6360919625726
tlambda30.t1 ==0.291173972427605

tlambda21.t2 ==-1.44988199276628
tlambda22.t2 ==-0.958097198298048
tlambda23.t2 ==0.603043901012482
tlambda24.t2 ==0.664205970761459
tlambda25.t2 ==0.678074904751807
tlambda26.t2 ==0.888941640351674
tlambda27.t2 ==-0.0553714526997642
tlambda28.t2 ==-0.267270240482607
tlambda29.t2 ==1.6360919625726
tlambda30.t2 ==0.291173972427605

'


lavaan.model.fit <- lavaan(lavaan.model.t12.common.fixed, 
                           data = cbind(U1,U2), 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = c(colnames(U1),colnames(U2)),
                           parameterization = "theta")


summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
eta12.common.fixed <-predict(lavaan.model.fit)
mean(eta12.common.fixed[,1])
mean(eta12.common.fixed[,2])
sd(eta12.common.fixed[,1])
sd(eta12.common.fixed[,2])

#-------Rever
lambda2 <- lavInspect(lavaan.model.fit,what = 'est')$lambda
colnames(lambda2) <- c("lambda1","lambda2")

#getting tau values
tau <- lavInspect(lavaan.model.fit,what = 'est')$tau
colnames(tau) <- c("tau")

alpha <- lavInspect(lavaan.model.fit,what = 'mean.lv')
psi <- lavInspect(lavaan.model.fit,what = 'cov.lv')

a_est <- rep(NA,60)
d_est <- rep(NA,60)


for(i in seq(1,60,1)){
  if(i<31){
    a_est[i] <- lambda2[i,1]*sqrt(psi[1,1])*1.7
    d_est[i] <- (-tau[i] +lambda2[i,1]*alpha[1]) *1.7
    
  }else{
    a_est[i] <- lambda2[i,2]*sqrt(psi[2,2])*1.7
    d_est[i] <- (-tau[i] +lambda2[i,2]*alpha[2]) *1.7
  }
}

b_est = -d_est/a_est

a_comp <- data.frame(cbind(sim = c(a[1:30],a[21:50]), 
                           est = a_est,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                     label=1:60)
str(a_comp)
a_comp[,"sim"]<-as.numeric(a_comp[,"sim"])
a_comp[,"est"]<-as.numeric(a_comp[,"est"])

ggplot(a_comp, 
       aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("a")


ggplot(a_comp, 
       aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("a")+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')



b_comp <- data.frame(cbind(sim = c(b[1:30],b[21:50]), 
                           est = b_est,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20)),
                           label=1:60))
str(b_comp)
b_comp[,"sim"]<-as.numeric(b_comp[,"sim"])
b_comp[,"est"]<-as.numeric(b_comp[,"est"])
ggplot(b_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

ggplot(b_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("b")+
  geom_smooth(data=b_comp[41:60,],method="lm",aes(x=sim,y=est))

lm(formula = est ~ sim, data = b_comp[41:60,])



l_comp <- data.frame(cbind(sim = c(lambda_sim[1:30],lambda_sim[21:50]), 
                           est = c(lambda2[1:30,1],lambda2[31:60,2]),
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                     label = 1:60)
l_comp[,"sim"]<-as.numeric(l_comp[,"sim"])
l_comp[,"est"]<-as.numeric(l_comp[,"est"])
ggplot(l_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')



t_comp <- data.frame(cbind(sim = c(tau_sim[1:30],tau_sim[21:50]),
                           est = tau,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                           label = 1:60)
colnames(t_comp)[2] <- "est"
t_comp[,"sim"]<-as.numeric(t_comp[,"sim"])
t_comp[,"est"]<-as.numeric(t_comp[,"est"])

ggplot(data=t_comp, aes(x=sim, y=est, color=tag,label = label)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  scale_x_continuous()

ggplot(data=t_comp, aes(x=sim, y=est, color=tag,label = label)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')


lavaan.model.t12.common.tag <- function(){}

lavaan.model.t12.common<-'

d1 =~ lambda1.t1*Item.1.t1+ lambda2.t1*Item.2.t1+ lambda3.t1*Item.3.t1+ lambda4.t1*Item.4.t1+ lambda5.t1*Item.5.t1+ lambda6.t1*Item.6.t1+ lambda7.t1*Item.7.t1+ lambda8.t1*Item.8.t1+ lambda9.t1*Item.9.t1+ lambda10.t1*Item.10.t1+ lambda11.t1*Item.11.t1+ lambda12.t1*Item.12.t1+ lambda13.t1*Item.13.t1+ lambda14.t1*Item.14.t1+ lambda15.t1*Item.15.t1+ lambda16.t1*Item.16.t1+ lambda17.t1*Item.17.t1+ lambda18.t1*Item.18.t1+ lambda19.t1*Item.19.t1+ lambda20.t1*Item.20.t1+ lambda21.t1*Item.21.t1+ lambda22.t1*Item.22.t1+ lambda23.t1*Item.23.t1+ lambda24.t1*Item.24.t1+ lambda25.t1*Item.25.t1+ lambda26.t1*Item.26.t1+ lambda27.t1*Item.27.t1+ lambda28.t1*Item.28.t1+ lambda29.t1*Item.29.t1+ lambda30.t1*Item.30.t1
d2 =~ lambda21.t2*Item.21.t2+ lambda22.t2*Item.22.t2+ lambda23.t2*Item.23.t2+ lambda24.t2*Item.24.t2+ lambda25.t2*Item.25.t2+ lambda26.t2*Item.26.t2+ lambda27.t2*Item.27.t2+ lambda28.t2*Item.28.t2+ lambda29.t2*Item.29.t2+ lambda30.t2*Item.30.t2+ lambda31.t2*Item.31.t2+ lambda32.t2*Item.32.t2+ lambda33.t2*Item.33.t2+ lambda34.t2*Item.34.t2+ lambda35.t2*Item.35.t2+ lambda36.t2*Item.36.t2+ lambda37.t2*Item.37.t2+ lambda38.t2*Item.38.t2+ lambda39.t2*Item.39.t2+ lambda40.t2*Item.40.t2+ lambda41.t2*Item.41.t2+ lambda42.t2*Item.42.t2+ lambda43.t2*Item.43.t2+ lambda44.t2*Item.44.t2+ lambda45.t2*Item.45.t2+ lambda46.t2*Item.46.t2+ lambda47.t2*Item.47.t2+ lambda48.t2*Item.48.t2+ lambda49.t2*Item.49.t2+ lambda50.t2*Item.50.t2

d1~~d2


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

Item.1.t1  ~~ 1*Item.1.t1  + 0*Item.2.t1  + 0*Item.3.t1  + 0*Item.4.t1  + 0*Item.5.t1  + 0*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2 
Item.2.t1  ~~ 1*Item.2.t1  + 0*Item.3.t1  + 0*Item.4.t1  + 0*Item.5.t1  + 0*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2 
Item.3.t1  ~~ 1*Item.3.t1  + 0*Item.4.t1  + 0*Item.5.t1  + 0*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.4.t1  ~~ 1*Item.4.t1  + 0*Item.5.t1  + 0*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.5.t1  ~~ 1*Item.5.t1  + 0*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.6.t1  ~~ 1*Item.6.t1  + 0*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.7.t1  ~~ 1*Item.7.t1  + 0*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.8.t1  ~~ 1*Item.8.t1  + 0*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.9.t1  ~~ 1*Item.9.t1  + 0*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.10.t1 ~~ 1*Item.10.t1 + 0*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.11.t1 ~~ 1*Item.11.t1 + 0*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.12.t1 ~~ 1*Item.12.t1 + 0*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.13.t1 ~~ 1*Item.13.t1 + 0*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.14.t1 ~~ 1*Item.14.t1 + 0*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.15.t1 ~~ 1*Item.15.t1 + 0*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.16.t1 ~~ 1*Item.16.t1 + 0*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.17.t1 ~~ 1*Item.17.t1 + 0*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.18.t1 ~~ 1*Item.18.t1 + 0*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.19.t1 ~~ 1*Item.19.t1 + 0*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.20.t1 ~~ 1*Item.20.t1 + 0*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.21.t1 ~~ 1*Item.21.t1 + 0*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 1*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.22.t1 ~~ 1*Item.22.t1 + 0*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 1*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.23.t1 ~~ 1*Item.23.t1 + 0*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 1*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.24.t1 ~~ 1*Item.24.t1 + 0*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 1*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.25.t1 ~~ 1*Item.25.t1 + 0*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 1*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.26.t1 ~~ 1*Item.26.t1 + 0*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 1*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.27.t1 ~~ 1*Item.27.t1 + 0*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 1*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.28.t1 ~~ 1*Item.28.t1 + 0*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 1*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.29.t1 ~~ 1*Item.29.t1 + 0*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 1*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.30.t1 ~~ 1*Item.30.t1 + 0*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 1*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.21.t2 ~~ 1*Item.21.t2 + 0*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.22.t2 ~~ 1*Item.22.t2 + 0*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.23.t2 ~~ 1*Item.23.t2 + 0*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.24.t2 ~~ 1*Item.24.t2 + 0*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.25.t2 ~~ 1*Item.25.t2 + 0*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.26.t2 ~~ 1*Item.26.t2 + 0*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.27.t2 ~~ 1*Item.27.t2 + 0*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.28.t2 ~~ 1*Item.28.t2 + 0*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.29.t2 ~~ 1*Item.29.t2 + 0*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.30.t2 ~~ 1*Item.30.t2 + 0*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.31.t2 ~~ 1*Item.31.t2 + 0*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.32.t2 ~~ 1*Item.32.t2 + 0*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.33.t2 ~~ 1*Item.33.t2 + 0*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.34.t2 ~~ 1*Item.34.t2 + 0*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.35.t2 ~~ 1*Item.35.t2 + 0*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.36.t2 ~~ 1*Item.36.t2 + 0*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.37.t2 ~~ 1*Item.37.t2 + 0*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.38.t2 ~~ 1*Item.38.t2 + 0*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.39.t2 ~~ 1*Item.39.t2 + 0*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.40.t2 ~~ 1*Item.40.t2 + 0*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.41.t2 ~~ 1*Item.41.t2 + 0*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.42.t2 ~~ 1*Item.42.t2 + 0*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.43.t2 ~~ 1*Item.43.t2 + 0*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.44.t2 ~~ 1*Item.44.t2 + 0*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.45.t2 ~~ 1*Item.45.t2 + 0*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.46.t2 ~~ 1*Item.46.t2 + 0*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.47.t2 ~~ 1*Item.47.t2 + 0*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.48.t2 ~~ 1*Item.48.t2 + 0*Item.49.t2 + 0*Item.50.t2                              
Item.49.t2 ~~ 1*Item.49.t2 + 0*Item.50.t2                              
Item.50.t2 ~~ 1*Item.50.t2 

lambda21.t1 ==   lambda21.t2
lambda22.t1 ==   lambda22.t2
lambda23.t1 ==   lambda23.t2
lambda24.t1 ==   lambda24.t2
lambda25.t1 ==   lambda25.t2
lambda26.t1 ==   lambda26.t2
lambda27.t1 ==   lambda27.t2
lambda28.t1 ==   lambda28.t2
lambda29.t1 ==   lambda29.t2
lambda30.t1 ==   lambda30.t2

tlambda21.t1 == tlambda21.t2
tlambda22.t1 == tlambda22.t2
tlambda23.t1 == tlambda23.t2
tlambda24.t1 == tlambda24.t2
tlambda25.t1 == tlambda25.t2
tlambda26.t1 == tlambda26.t2
tlambda27.t1 == tlambda27.t2
tlambda28.t1 == tlambda28.t2
tlambda29.t1 == tlambda29.t2
tlambda30.t1 == tlambda30.t2

'


lavaan.model.fit <- lavaan(lavaan.model.t12.common, 
                           data = cbind(U1,U2), 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = c(colnames(U1),colnames(U2)),
                           parameterization = "theta")


summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
eta12.common <-predict(lavaan.model.fit)
mean(eta12.common[,1])
mean(eta12.common[,2])
sd(eta12.common[,1])
sd(eta12.common[,2])

#-------Rever
lambda2 <- lavInspect(lavaan.model.fit,what = 'est')$lambda
colnames(lambda2) <- c("lambda1","lambda2")

#getting tau values
tau <- lavInspect(lavaan.model.fit,what = 'est')$tau
colnames(tau) <- c("tau")

alpha <- lavInspect(lavaan.model.fit,what = 'mean.lv')
psi <- lavInspect(lavaan.model.fit,what = 'cov.lv')

a_est <- rep(NA,60)
d_est <- rep(NA,60)


for(i in seq(1,60,1)){
  if(i<31){
    a_est[i] <- lambda2[i,1]*sqrt(psi[1,1])*1.7
    d_est[i] <- (-tau[i] +lambda2[i,1]*alpha[1]) *1.7
    
  }else{
    a_est[i] <- lambda2[i,2]*sqrt(psi[2,2])*1.7
    d_est[i] <- (-tau[i] +lambda2[i,2]*alpha[2]) *1.7
  }
}

b_est = -d_est/a_est

a_comp <- data.frame(cbind(sim = c(a[1:30],a[21:50]), 
                           est = a_est,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                     label=1:60)
str(a_comp)
a_comp[,"sim"]<-as.numeric(a_comp[,"sim"])
a_comp[,"est"]<-as.numeric(a_comp[,"est"])

ggplot(a_comp, 
       aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("a")


ggplot(a_comp, 
       aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("a")+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')



b_comp <- data.frame(cbind(sim = c(b[1:30],b[21:50]), 
                           est = b_est,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20)),
                           label=1:60))
str(b_comp)
b_comp[,"sim"]<-as.numeric(b_comp[,"sim"])
b_comp[,"est"]<-as.numeric(b_comp[,"est"])
ggplot(b_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

ggplot(b_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("b")+
  geom_smooth(data=b_comp[41:60,],method="lm",aes(x=sim,y=est))+
  geom_smooth(data=b_comp[31:40,],method="lm",aes(x=sim,y=est))
  
lm(formula = est ~ sim, data = b_comp[31:40,])
lm(formula = est ~ sim, data = b_comp[41:60,])



l_comp <- data.frame(cbind(sim = c(lambda_sim[1:30],lambda_sim[21:50]), 
                           est = c(lambda2[1:30,1],lambda2[31:60,2]),
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                     label = 1:60)
l_comp[,"sim"]<-as.numeric(l_comp[,"sim"])
l_comp[,"est"]<-as.numeric(l_comp[,"est"])
ggplot(l_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')



t_comp <- data.frame(cbind(sim = c(tau_sim[1:30],tau_sim[21:50]),
                           est = tau,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                     label = 1:60)
colnames(t_comp)[2] <- "est"
t_comp[,"sim"]<-as.numeric(t_comp[,"sim"])
t_comp[,"est"]<-as.numeric(t_comp[,"est"])

ggplot(data=t_comp, aes(x=sim, y=est, color=tag,label = label)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  scale_x_continuous()

ggplot(data=t_comp, aes(x=sim, y=est, color=tag,label = label)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')


lavaan.model.t1.fixed.tag <- function(){}

lavaan.model.t1 <-'

d1 =~ lambda1.t1*Item.1.t1+ lambda2.t1*Item.2.t1+ lambda3.t1*Item.3.t1+ lambda4.t1*Item.4.t1+ lambda5.t1*Item.5.t1+ lambda6.t1*Item.6.t1+ lambda7.t1*Item.7.t1+ lambda8.t1*Item.8.t1+ lambda9.t1*Item.9.t1+ lambda10.t1*Item.10.t1+ lambda11.t1*Item.11.t1+ lambda12.t1*Item.12.t1+ lambda13.t1*Item.13.t1+ lambda14.t1*Item.14.t1+ lambda15.t1*Item.15.t1+ lambda16.t1*Item.16.t1+ lambda17.t1*Item.17.t1+ lambda18.t1*Item.18.t1+ lambda19.t1*Item.19.t1+ lambda20.t1*Item.20.t1+ lambda21.t1*Item.21.t1+ lambda22.t1*Item.22.t1+ lambda23.t1*Item.23.t1+ lambda24.t1*Item.24.t1+ lambda25.t1*Item.25.t1+ lambda26.t1*Item.26.t1+ lambda27.t1*Item.27.t1+ lambda28.t1*Item.28.t1+ lambda29.t1*Item.29.t1+ lambda30.t1*Item.30.t1

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

lambda1.t1 ==0.605768111800353
lambda2.t1 ==0.731050410854043
lambda3.t1 ==0.966925221330078
lambda4.t1 ==1.36099622796096
lambda5.t1 ==0.530766076424742
lambda6.t1 ==1.34945908926874
lambda7.t1 ==1.40384872926598
lambda8.t1 ==1.07026767624776
lambda9.t1 ==1.03303647931713
lambda10.t1 ==0.36637634602534
lambda11.t1 ==0.535810311280025
lambda12.t1 ==0.501241777354871
lambda13.t1 ==1.10108442615486
lambda14.t1 ==0.745127753482652
lambda15.t1 ==1.19840354876446
lambda16.t1 ==0.878612505388036
lambda17.t1 ==1.13703702498757
lambda18.t1 ==1.45934911260926
lambda19.t1 ==0.740346861849993
lambda20.t1 ==1.20733868545217
lambda21.t1 ==1.39213305652868
lambda22.t1 ==0.543058191871513
lambda23.t1 ==1.05954614111149
lambda24.t1 ==0.44131033602975
lambda25.t1 ==0.607779869245044
lambda26.t1 ==0.74749012049791
lambda27.t1 ==0.309506854475788
lambda28.t1 ==0.743111582925996
lambda29.t1 ==1.31573542387834
lambda30.t1 ==0.693712099515488

tlambda1.t1 ==-0.389937930318494
tlambda2.t1 ==0.992592029861341
tlambda3.t1 ==-0.592982404714925
tlambda4.t1 ==-0.904926768708513
tlambda5.t1 ==-0.0502078274584591
tlambda6.t1 ==2.11702243640856
tlambda7.t1 ==2.04591001146434
tlambda8.t1 ==-0.470962542678942
tlambda9.t1 ==1.14592959346492
tlambda10.t1 ==0.675038154936852
tlambda11.t1 ==-0.14004048721003
tlambda12.t1 ==0.426084941092017
tlambda13.t1 ==-0.440458571737896
tlambda14.t1 ==-0.52053983488637
tlambda15.t1 ==1.23237660215154
tlambda16.t1 ==-1.04487321031349
tlambda17.t1 ==0.96021058683591
tlambda18.t1 ==-2.20833423736645
tlambda19.t1 ==-0.753707119979238
tlambda20.t1 ==-1.7226096866107
tlambda21.t1 ==-1.44988199276628
tlambda22.t1 ==-0.958097198298048
tlambda23.t1 ==0.603043901012482
tlambda24.t1 ==0.664205970761459
tlambda25.t1 ==0.678074904751807
tlambda26.t1 ==0.888941640351674
tlambda27.t1 ==-0.0553714526997642
tlambda28.t1 ==-0.267270240482607
tlambda29.t1 ==1.6360919625726
tlambda30.t1 ==0.291173972427605

'
lavaan.model.fit <- lavaan(lavaan.model.t1, 
                           data = U1, 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = colnames(U1),
                           parameterization = "theta")

summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
eta1<-predict(lavaan.model.fit)
mean(eta1)
sd(eta1)


lavaan.model.t123.tag <- function(){}

lavaan.model.t123 <-'

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



lambda21.t1 ==   lambda21.t2
lambda22.t1 ==   lambda22.t2
lambda23.t1 ==   lambda23.t2
lambda24.t1 ==   lambda24.t2
lambda25.t1 ==   lambda25.t2
lambda26.t1 ==   lambda26.t2
lambda27.t1 ==   lambda27.t2
lambda28.t1 ==   lambda28.t2
lambda29.t1 ==   lambda29.t2
lambda30.t1 ==   lambda30.t2

tlambda21.t1 == tlambda21.t2
tlambda22.t1 == tlambda22.t2
tlambda23.t1 == tlambda23.t2
tlambda24.t1 == tlambda24.t2
tlambda25.t1 == tlambda25.t2
tlambda26.t1 == tlambda26.t2
tlambda27.t1 == tlambda27.t2
tlambda28.t1 == tlambda28.t2
tlambda29.t1 == tlambda29.t2
tlambda30.t1 == tlambda30.t2


lambda41.t2 ==   lambda41.t3
lambda42.t2 ==   lambda42.t3
lambda43.t2 ==   lambda43.t3
lambda44.t2 ==   lambda44.t3
lambda45.t2 ==   lambda45.t3
lambda46.t2 ==   lambda46.t3
lambda47.t2 ==   lambda47.t3
lambda48.t2 ==   lambda48.t3
lambda49.t2 ==   lambda49.t3
lambda50.t2 ==   lambda50.t3

tlambda41.t2 == tlambda41.t3
tlambda42.t2 == tlambda42.t3
tlambda43.t2 == tlambda43.t3
tlambda44.t2 == tlambda44.t3
tlambda45.t2 == tlambda45.t3
tlambda46.t2 == tlambda46.t3
tlambda47.t2 == tlambda47.t3
tlambda48.t2 == tlambda48.t3
tlambda49.t2 == tlambda49.t3
tlambda50.t2 == tlambda50.t3

'



lavaan.model.fit <- lavaan(lavaan.model.t123, 
                           data = cbind(U1,U2,U3), 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = c(colnames(U1),colnames(U2),colnames(U3)),
                           parameterization = "theta")


summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
eta123.common <-predict(lavaan.model.fit)
mean(eta123.common[,1])
mean(eta123.common[,2])
mean(eta123.common[,3])
sd(eta123.common[,1])
sd(eta123.common[,2])


lambda2 <- lavInspect(lavaan.model.fit,what = 'est')$lambda
colnames(lambda2) <- c("lambda1","lambda2","lambda3")


#getting tau values
tau <- lavInspect(lavaan.model.fit,what = 'est')$tau
colnames(tau) <- c("tau")

alpha <- lavInspect(lavaan.model.fit,what = 'mean.lv')
psi <- lavInspect(lavaan.model.fit,what = 'cov.lv')

a_est <- rep(NA,90)
d_est <- rep(NA,90)


for(i in seq(1,90,1)){
  if(i<31){
    a_est[i] <- lambda2[i,1]*sqrt(psi[1,1])*1.7
    d_est[i] <- (-tau[i] +lambda2[i,1]*alpha[1]) *1.7
    
  }else if( (i>30) & (i<61) ){
    a_est[i] <- lambda2[i,2]*sqrt(psi[2,2])*1.7
    d_est[i] <- (-tau[i] +lambda2[i,2]*alpha[2]) *1.7
    
  }else{
    a_est[i] <- lambda2[i,3]*sqrt(psi[3,3])*1.7
    d_est[i] <- (-tau[i] +lambda2[i,3]*alpha[3]) *1.7
  }
}

b_est = -d_est/a_est

a_comp <- data.frame(cbind(sim = c(a[1:30],a[21:50],a[41:70] ), 
                           est = a_est,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",10),rep("t23_1",10),rep("t23_2",10),rep("t3",20) )),
                     label=1:90)
str(a_comp)
a_comp[,"sim"]<-as.numeric(a_comp[,"sim"])
a_comp[,"est"]<-as.numeric(a_comp[,"est"])

ggplot(a_comp, 
       aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("a")


ggplot(a_comp, 
       aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("a")+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')



b_comp <- data.frame(cbind(sim = c(b[1:30],b[21:50],b[41:70]), 
                           est = b_est,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",10),rep("t23_1",10),rep("t23_2",10),rep("t3",20) )),
                     label=1:90)
str(b_comp)
b_comp[,"sim"]<-as.numeric(b_comp[,"sim"])
b_comp[,"est"]<-as.numeric(b_comp[,"est"])
ggplot(b_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

ggplot(b_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("b")+
  geom_smooth(data=b_comp[1:20,],method="lm",aes(x=sim,y=est))+
  geom_smooth(data=b_comp[21:40,],method="lm",aes(x=sim,y=est))+
  geom_smooth(data=b_comp[41:50,],method="lm",aes(x=sim,y=est))+
  geom_smooth(data=b_comp[51:70,],method="lm",aes(x=sim,y=est))+
  geom_smooth(data=b_comp[71:90,],method="lm",aes(x=sim,y=est))

lm(formula = est ~ sim, data = b_comp[31:40,])
lm(formula = est ~ sim, data = b_comp[41:60,])



l_comp <- data.frame(cbind(sim = c(lambda_sim[1:30],lambda_sim[21:50]), 
                           est = c(lambda2[1:30,1],lambda2[31:60,2]),
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                     label = 1:60)
l_comp[,"sim"]<-as.numeric(l_comp[,"sim"])
l_comp[,"est"]<-as.numeric(l_comp[,"est"])
ggplot(l_comp, aes(x=sim, y=est, color=tag)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')



t_comp <- data.frame(cbind(sim = c(tau_sim[1:30],tau_sim[21:50]),
                           est = tau,
                           tag = c(rep("t1",20),rep("t12_1",10),rep("t12_2",10),rep("t2",20))),
                     label = 1:60)
colnames(t_comp)[2] <- "est"
t_comp[,"sim"]<-as.numeric(t_comp[,"sim"])
t_comp[,"est"]<-as.numeric(t_comp[,"est"])

ggplot(data=t_comp, aes(x=sim, y=est, color=tag,label = label)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  scale_x_continuous()

ggplot(data=t_comp, aes(x=sim, y=est, color=tag,label = label)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)+
  ggrepel::geom_label_repel(aes(label = label),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')



#=================================


lavaan.model.fit <- cfa(lavaan.model.t2, data = U2 , std.lv=FALSE ,ordered = colnames(U2) )

lavaan.model.fit <- lavaan(lavaan.model.t2, 
                           data = U2, 
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           auto.fix.first = FALSE,
                           auto.var = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           auto.var = TRUE,
                           ordered = colnames(U2),
                           parameterization = "theta")


summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)[c("cfi","tli","rmsea")]
eta2<-predict(lavaan.model.fit)


summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)
fitMeasures(lavaan.model.fit)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]

tmp <- lavPredict(lavaan.model.fit,method="EBM") 
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

alpha <- lavInspect(lavaan.model.fit,what = 'mean.lv')
psi <- lavInspect(lavaan.model.fit,what = 'cov.lv')



item.par.sim <- matrix(0,90,4)
colnames(item.par.sim) <- c("aj_t1_lav","aj_t2_lav","aj_t3_lav","dj_lav")

a_est <- rep(NA,30)
d_est <- rep(NA,30)

for(i in seq(1,30,1)){
  a_est[i] <- lambda2[i]*sqrt(psi)*1.7
  d_est[i] <- (-tau[i] +lambda2[i]*alpha) *1.7
}
cbind(a[1:30], a_est)
cbind(d[1:30], d_est)


cbind(tau_sim[1:30]/1.7, tau)
cbind(lambda_sim[1:30]/1.7, lambda2)











cbind(a[21:50], a_est)
cbind(d[21:50], d_est)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%  REPRODU??O DAS PROFICI?NCIAS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estima??o dos par?metros dos itens: Colunas a,d,c
profic1 = fscores(mirt.2PL) #estimativas das profici?ncias individuais
mean(profic1)
sd(profic1)
plot(Theta[,1],profic1, main="Recupera??o das profici?ncias", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")


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
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estima??o dos par?metros dos itens: Colunas a,d,c
profic2 = fscores(mirt.2PL) #estimativas das profici?ncias individuais
mean(profic2)
sd(profic2)
plot(Theta[,2],profic2, main="Recupera??o das profici?ncias", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

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
#PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estima??o dos par?metros dos itens: Colunas a,d,c
profic3 = fscores(mirt.2PL) #estimativas das profici?ncias individuais
mean(profic3)
sd(profic3)
plot(Theta[,3],profic3, main="Recupera??o das profici?ncias", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

models2 <- 'F1 = 1-30
            F2 = 21-50
            F3 = 41-70'
mod_long2 <- multipleGroup( data=Ut, 
                           model=models2, 
                           group = group_names,
                           itemtype='2PL',
                           invariance = c(itemnames,'free_means','free_var','slopes','intercepts')) 


par2<-coef(mod_long2,simplify=TRUE,IRTpars = TRUE)
par2
str(coef(mod_long2,simplify=TRUE))
profic4 = fscores(mod_long2) #estimativas das proficiencias individuais
mean(profic4)


plot(a[1:70],par2$G1[1][['items']][,1], 
     main="Recupera??o dos par?metros de discrimina??o", 
     xlab="Valores verdadeiros",
     ylab="Estimativas",
     asp = 1);
lines(c(-4,4),c(-4,4), col = "blue")

plot(b[1:70],par2$G1[1][['items']][,2], 
     main="Recupera??o dos par?metros de dificuldade", 
     xlab="Valores verdadeiros",
     ylab="Estimativas",
     asp = 1);
lines(c(-4,4),c(-4,4), col = "blue")

plot(Theta[,1],profic4[1:N], main="Recupera??o das profici?ncias", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
mean(profic[1:N])
sd(profic[1:N])

plot(Theta[,2],profic4[(N+1):(2*N)], main="Recupera??o das profici?ncias", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
mean(profic[(N+1):(2*N)])
sd(profic[(N+1):(2*N)])


plot(Theta[,3],profic4[(2*N+1):(3*N)], main="Recupera??o das profici?ncias", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
mean(profic[(2*N+1):(3*N)])
sd(profic[(2*N+1):(3*N)])

cor(cbind(profic4[1:N],
          profic4[(N+1):(2*N)],
          profic4[(2*N+1):(3*N)] ))



