
rm(list=ls())
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 
if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(GPArotation)) install.packages("GPArotation"); library(GPArotation)
if(!require(pryr)) install.packages("pryr"); library(pryr)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(cowplot)) install.packages("cowplot"); library(cowplot)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(ltm)) install.packages("ltm"); library(ltm)




###Generate person parameters

set.seed(1) # Resetando a semente

N <- 2000 ## subjects
I= 4  # Number of Items
PL=2 # Logistic Model (1,2,3 parameters)
SigmaType <- 1 # 0 = Covariance Uniform, 1 = Covariancia AR1, 2 =  Covariancia de bandas
rho<-0.7
test_n <-10

coefs <- matrix(ncol=6,nrow=I)
colnames(coefs)=c("a1","b1","c1","a2","b2","c2")

if (PL==1) {a = rep(1,I)} else {a = runif(I,0.5, 2.5)}    # U(0.5 , 2.5)
b = runif(I,-2, 2.0)     # U(-2 , 2)
if (PL<=2) {c = rep(0,I)} else{c = runif(I,0.0, 0.3) } # U(0 , 0.3)
  
d=-a*b # MIRT trabalha com o intercepto (d=-ab) e n?o com a dificuldade (b)

a_sample <- matrix(rep(NA,4*test_n),nrow = 4,ncol = test_n)
b_sample <- matrix(rep(NA,4*test_n),nrow = 4,ncol = test_n)

for(i in (1:test_n)){
  if(SigmaType==0){
    Sigma <- lazyCor(c(rho,rho,rho)) #Matriz de Covari?ncia Uniforme
  }else if(SigmaType==1){
    Sigma <- lazyCor(c(rho,rho*rho,rho)) #Matriz de Covari?ncia AR(1)
  }else if(SigmaType==2){
    Sigma <- lazyCor(c(rho,0,0)) #Matriz de Covari?ncia de bandas
  }else{
    Sigma <- NULL
  }
  
  Sigma
  
  Theta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  
  head(Theta)
  cor(Theta)
  summary(Theta)
  var(Theta)
  dim(Theta)
  
  t1_index_range <- c(1:I) 
  t2_index_range <- c(1:I) 
  t3_index_range <- c(1:I) 
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # momento 1
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  eta  = Theta[,1]%*% t(a[t1_index_range]) +  matrix(d[t1_index_range],N,length(t1_index_range),byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
  P = c[t1_index_range] + (1-c[t1_index_range])/(1+exp(-eta));  P=t(P) # n x I
  X = runif(N*length(t1_index_range));  dim(X)=c(N,length(t1_index_range))   # matriz n x I de U(0,1)
  #U1 = 1*(X<P)  ; U1=as.data.frame(U1) # AQUI TEMOS OS DADOS DICOT?MICOS
  U1 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,1])))
  colnames(U1)=paste0("Item.",t1_index_range,".t1")
  alpha <- ltm::cronbach.alpha(U1)
  print(alpha)
  
  rm(P,X,eta)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # momento 2
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  eta  = Theta[,2]%*% t(a[t2_index_range]) +  matrix(d[t2_index_range],N,length(t2_index_range),byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
  P = c[t2_index_range] + (1-c[t2_index_range])/(1+exp(-eta));  P=t(P) # n x I
  X = runif(N*length(t2_index_range));  dim(X)=c(N,length(t2_index_range))   # matriz n x I de U(0,1)
  #U2 = 1*(X<P)  ; U2=as.data.frame(U2) # AQUI TEMOS OS DADOS DICOT?MICOS
  U2 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,1],ncol=1,nrow = length(Theta[,2])))
  colnames(U2)=paste0("Item.",t2_index_range,".t2")
  alpha <- ltm::cronbach.alpha(U2)
  print(alpha)
  
  rm(P,X,eta)
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # momento 3
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  eta  = Theta[,3]%*% t(a[t3_index_range]) +  matrix(d[t3_index_range],N,length(t3_index_range),byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
  P = c[t3_index_range] + (1-c[t3_index_range])/(1+exp(-eta));  P=t(P) # n x I
  X = runif(N*length(t3_index_range));  dim(X)=c(N,length(t3_index_range))   # matriz n x I de U(0,1)
  #U3 = 1*(X<P)  ; U3=as.data.frame(U3) # AQUI TEMOS OS DADOS DICOT?MICOS
  U3 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Theta[,3],ncol=1,nrow = length(Theta[,3])))
  colnames(U3)=paste0("Item.",t3_index_range,".t3")
  alpha <- ltm::cronbach.alpha(U3)
  print(alpha)
  
  rm(P,X,eta)



  
  head(U1)
  head(U2)
  head(U3)
  U<-cbind(U1,U2,U3)
  head(U)
  dim(U)
  
  mNA = rep(NA,(N*0));  dim(mNA)=c(N,0)   # matriz n x I de U(0,1)
  dim(mNA)
  
  itemnames <- c(1:4)
  U1t1 <- cbind(U1,mNA,mNA)
  names(U1t1)<-itemnames
  U2t2 <- cbind(mNA,U2,mNA)
  names(U2t2)<-itemnames
  U3t3 <- cbind(mNA,mNA,U3)
  names(U3t3)<-itemnames
  
  #group names
  group_names= paste0("G",rep(1:3, each=N)) #G1...G1,G2...G2, G3..G3
  
  group_names[0:10]
  group_names[(N-5):(N+5)]
  group_names[(2*N-5):(2*N+5)]
  
  Ut <-rbind(U1t1,U2t2,U3t3)
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ESTIMA??O PELO MIRT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  mirt.model.tag <-function(){}
  
  
  models <- 'F1 = 1-4'
  mod_long <- multipleGroup( data=Ut, 
                             model=models, 
                             group = group_names,
                             itemtype='2PL',
                             invariance = c(itemnames,'free_means','free_var','slopes','intercepts')) 
  
  par<-coef(mod_long,simplify=TRUE,IRTpars = TRUE)
  par
  str(coef(mod_long,simplify=TRUE))
  profic = fscores(mod_long) #estimativas das proficiências individuais
  mean(profic)
  
  par2<-mod2values(mod_long) # getting everything that was estimated. Like pars='values'
  
  a_sample[,i]<-par$G1$items[,"a"]
  b_sample[,i]<-par$G1$items[,"b"]
}



a_est<- rowMeans(a_sample,na.rm = TRUE)
b_est<- rowMeans(b_sample,na.rm = TRUE)
data.graph <- data.frame(a_true= a[1:4], 
                         a_est= a_est)

ggplot(data.graph, aes(a_true[1:4],a_est)) + geom_point() + geom_abline(intercept = 0, slope = 1)

data.graph <- data.frame(b_true= b[1:4], 
                                b_est= b_est)
ggplot(data.graph, aes(b_true,b_est)) + geom_point() + geom_abline(intercept = 0, slope = 1)
        


p1<-paste("lambda",t1_index_range,".t1*Item.",t1_index_range,".t1",sep="",collapse = " + ")
p1<-paste("d1 =~",p1,sep="",collapse = "")
p2<-paste("lambda",t2_index_range,".t2*Item.",t2_index_range,".t2",sep="",collapse = " + ")
p2<-paste("d2 =~",p2,sep="",collapse = "")
p3<-paste("lambda",t3_index_range,".t3*Item.",t3_index_range,".t3",sep="",collapse = " + ")
p3<-paste("d3 =~",p3,sep="",collapse = "")

p1
p2
p3
cat(paste("Item.",t1_index_range,".t1 | tau",t1_index_range,".t1*t1",sep="",collapse = "\n"))
cat(paste("Item.",t2_index_range,".t2 | tau",t2_index_range,".t2*t1",sep="",collapse = "\n"))
cat(paste("Item.",t3_index_range,".t3 | tau",t3_index_range,".t3*t1",sep="",collapse = "\n"))

cat(paste("Item.",t1_index_range,".t1 ~ mean",t1_index_range,".t1*1",sep="",collapse = "\n"))
p1<-paste("mean",t1_index_range,".t1",sep="",collapse = " + ")
paste("0 == ",p1,sep="",collapse = "")

cat(paste("Item.",t2_index_range,".t2 ~ mean",t2_index_range,".t2*1",sep="",collapse = "\n"))
p1<-paste("mean",t2_index_range,".t2",sep="",collapse = " + ")
paste("0 == ",p1,sep="",collapse = "")


cat(paste("Item.",t3_index_range,".t3 ~ mean",t3_index_range,".t3*1",sep="",collapse = "\n"))
p1<-paste("mean",t3_index_range,".t3",sep="",collapse = " + ")
paste("0 == ",p1,sep="",collapse = "")



itemnames <- c( paste0("Item.",t1_index_range,".t1"),paste0("Item.",t2_index_range,".t2"),  paste0("Item.",t3_index_range,".t3"))

p2<-"" 
p1<-""
for(i in 1:length(itemnames)){
  p1<-paste0(itemnames[i]," ~~ 1*",itemnames[i])
  for(j in (i+1):length(itemnames)){
    p1 <- paste(p1,paste0("0*",itemnames[j]),sep=" + ",collapse = "\n")
  }
  p2 <- paste(p2,p1,sep="",collapse = "\n")
  print(p1)
}
p2

cat(paste("Item.",21:30,".t1 ~~ Item.",21:30,".t2" ,sep="",collapse = "\n"))
cat(paste("Item.",41:50,".t2 ~~ Item.",41:50,".t3" ,sep="",collapse = "\n"))
cat(paste("Item.",t1_index_range,".t1 ~~ ",t1_index_range,".t1*t1",sep="",collapse = "\n"))

lavaan.model1.tag <-function(){}

lavaan.model.all.free <-'

d1 =~ lambda1.t1*Item.1.t1 + lambda2.t1*Item.2.t1 + lambda3.t1*Item.3.t1 + lambda4.t1*Item.4.t1
d2 =~ lambda1.t2*Item.1.t2 + lambda2.t2*Item.2.t2 + lambda3.t2*Item.3.t2 + lambda4.t2*Item.4.t2
d3 =~ lambda1.t3*Item.1.t3 + lambda2.t3*Item.2.t3 + lambda3.t3*Item.3.t3 + lambda4.t3*Item.4.t3

lambda1.t1+lambda2.t1+lambda3.t1+lambda4.t1==4

d1~~d1 + d2 + d3
d2~~d2 + d3
d3~~d3

#d1~~d2 + d3
#d2~~d3


Item.1.t1 | tau1.t1*t1
Item.2.t1 | tau2.t1*t1
Item.3.t1 | tau3.t1*t1
Item.4.t1 | tau4.t1*t1

Item.1.t2 | tau1.t2*t1
Item.2.t2 | tau2.t2*t1
Item.3.t2 | tau3.t2*t1
Item.4.t2 | tau4.t2*t1

Item.1.t3 | tau1.t3*t1
Item.2.t3 | tau2.t3*t1
Item.3.t3 | tau3.t3*t1
Item.4.t3 | tau4.t3*t1

Item.1.t1 ~ mean1.t1*1
Item.2.t1 ~ mean2.t1*1
Item.3.t1 ~ mean3.t1*1
Item.4.t1 ~ mean4.t1*1

Item.1.t2 ~ mean1.t2*1
Item.2.t2 ~ mean2.t2*1
Item.3.t2 ~ mean3.t2*1
Item.4.t2 ~ mean4.t2*1

Item.1.t3 ~ mean1.t3*1
Item.2.t3 ~ mean2.t3*1
Item.3.t3 ~ mean3.t3*1
Item.4.t3 ~ mean4.t3*1

0 == mean1.t1 + mean2.t1 + mean3.t1 + mean4.t1

Item.1.t1 ~~ 1*Item.1.t1 + 0*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + start(1.0)*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + start(1.0)*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t1 ~~ 1*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + 0*Item.1.t2 + start(1.0)*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + start(1.0)*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t1 ~~ 1*Item.3.t1 + 0*Item.4.t1 + 0*Item.1.t2 + 0*Item.2.t2 + start(1.0)*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + start(1.0)*Item.3.t3 + 0*Item.4.t3
Item.4.t1 ~~ 1*Item.4.t1 + 0*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + start(1.0)*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + start(1.0)*Item.4.t3
Item.1.t2 ~~ 1*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + start(1.0)*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t2 ~~ 1*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + start(1.0)*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t2 ~~ 1*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + start(1.0)*Item.3.t3 + 0*Item.4.t3
Item.4.t2 ~~ 1*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + start(1.0)*Item.4.t3
Item.1.t3 ~~ 1*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t3 ~~ 1*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t3 ~~ 1*Item.3.t3 + 0*Item.4.t3
Item.4.t3 ~~ 1*Item.4.t3 

'

lavaan.model.loadings.fixed <-'

d1 =~ lambda1.t1*Item.1.t1 + lambda2.t1*Item.2.t1 + lambda3.t1*Item.3.t1 + lambda4.t1*Item.4.t1
d2 =~ lambda1.t2*Item.1.t2 + lambda2.t2*Item.2.t2 + lambda3.t2*Item.3.t2 + lambda4.t2*Item.4.t2
d3 =~ lambda1.t3*Item.1.t3 + lambda2.t3*Item.2.t3 + lambda3.t3*Item.3.t3 + lambda4.t3*Item.4.t3

lambda1.t1+lambda2.t1+lambda3.t1+lambda4.t1==4

d1~~d1 + d2 + d3
d2~~d2 + d3
d3~~d3

#d1~~d2 + d3
#d2~~d3


Item.1.t1 | tau1.t1*t1
Item.2.t1 | tau2.t1*t1
Item.3.t1 | tau3.t1*t1
Item.4.t1 | tau4.t1*t1

Item.1.t2 | tau1.t2*t1
Item.2.t2 | tau2.t2*t1
Item.3.t2 | tau3.t2*t1
Item.4.t2 | tau4.t2*t1

Item.1.t3 | tau1.t3*t1
Item.2.t3 | tau2.t3*t1
Item.3.t3 | tau3.t3*t1
Item.4.t3 | tau4.t3*t1

Item.1.t1 ~ mean1.t1*1
Item.2.t1 ~ mean2.t1*1
Item.3.t1 ~ mean3.t1*1
Item.4.t1 ~ mean4.t1*1

Item.1.t2 ~ mean1.t2*1
Item.2.t2 ~ mean2.t2*1
Item.3.t2 ~ mean3.t2*1
Item.4.t2 ~ mean4.t2*1

Item.1.t3 ~ mean1.t3*1
Item.2.t3 ~ mean2.t3*1
Item.3.t3 ~ mean3.t3*1
Item.4.t3 ~ mean4.t3*1

0 == mean1.t1 + mean2.t1 + mean3.t1 + mean4.t1

Item.1.t1 ~~ 1*Item.1.t1 + 0*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + start(1.0)*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + start(1.0)*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t1 ~~ 1*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + 0*Item.1.t2 + start(1.0)*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + start(1.0)*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t1 ~~ 1*Item.3.t1 + 0*Item.4.t1 + 0*Item.1.t2 + 0*Item.2.t2 + start(1.0)*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + start(1.0)*Item.3.t3 + 0*Item.4.t3
Item.4.t1 ~~ 1*Item.4.t1 + 0*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + start(1.0)*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + start(1.0)*Item.4.t3
Item.1.t2 ~~ 1*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + start(1.0)*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t2 ~~ 1*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + start(1.0)*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t2 ~~ 1*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + start(1.0)*Item.3.t3 + 0*Item.4.t3
Item.4.t2 ~~ 1*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + start(1.0)*Item.4.t3
Item.1.t3 ~~ 1*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t3 ~~ 1*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t3 ~~ 1*Item.3.t3 + 0*Item.4.t3
Item.4.t3 ~~ 1*Item.4.t3 

lambda1.t1 == lambda1.t2
lambda2.t1 == lambda2.t2
lambda3.t1 == lambda3.t2
lambda4.t1 == lambda4.t2

lambda1.t2 == lambda1.t3
lambda2.t2 == lambda2.t3
lambda3.t2 == lambda3.t3
lambda4.t2 == lambda4.t3
'

lavaan.model.all.fixed <-'

d1 =~ lambda1.t1*Item.1.t1 + lambda2.t1*Item.2.t1 + lambda3.t1*Item.3.t1 + lambda4.t1*Item.4.t1
d2 =~ lambda1.t2*Item.1.t2 + lambda2.t2*Item.2.t2 + lambda3.t2*Item.3.t2 + lambda4.t2*Item.4.t2
d3 =~ lambda1.t3*Item.1.t3 + lambda2.t3*Item.2.t3 + lambda3.t3*Item.3.t3 + lambda4.t3*Item.4.t3

lambda1.t1+lambda2.t1+lambda3.t1+lambda4.t1==4

d1~~d1 + d2 + d3
d2~~d2 + d3
d3~~d3

#d1~~d2 + d3
#d2~~d3


Item.1.t1 | tau1.t1*t1
Item.2.t1 | tau2.t1*t1
Item.3.t1 | tau3.t1*t1
Item.4.t1 | tau4.t1*t1

Item.1.t2 | tau1.t2*t1
Item.2.t2 | tau2.t2*t1
Item.3.t2 | tau3.t2*t1
Item.4.t2 | tau4.t2*t1

Item.1.t3 | tau1.t3*t1
Item.2.t3 | tau2.t3*t1
Item.3.t3 | tau3.t3*t1
Item.4.t3 | tau4.t3*t1

Item.1.t1 ~ mean1.t1*1
Item.2.t1 ~ mean2.t1*1
Item.3.t1 ~ mean3.t1*1
Item.4.t1 ~ mean4.t1*1

Item.1.t2 ~ mean1.t2*1
Item.2.t2 ~ mean2.t2*1
Item.3.t2 ~ mean3.t2*1
Item.4.t2 ~ mean4.t2*1

Item.1.t3 ~ mean1.t3*1
Item.2.t3 ~ mean2.t3*1
Item.3.t3 ~ mean3.t3*1
Item.4.t3 ~ mean4.t3*1

0 == mean1.t1 + mean2.t1 + mean3.t1 + mean4.t1

Item.1.t1 ~~ 1*Item.1.t1 + 0*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + start(1.0)*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + start(1.0)*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t1 ~~ 1*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + 0*Item.1.t2 + start(1.0)*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + start(1.0)*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t1 ~~ 1*Item.3.t1 + 0*Item.4.t1 + 0*Item.1.t2 + 0*Item.2.t2 + start(1.0)*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + start(1.0)*Item.3.t3 + 0*Item.4.t3
Item.4.t1 ~~ 1*Item.4.t1 + 0*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + start(1.0)*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + start(1.0)*Item.4.t3
Item.1.t2 ~~ 1*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + start(1.0)*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t2 ~~ 1*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + start(1.0)*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t2 ~~ 1*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + start(1.0)*Item.3.t3 + 0*Item.4.t3
Item.4.t2 ~~ 1*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + start(1.0)*Item.4.t3
Item.1.t3 ~~ 1*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t3 ~~ 1*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t3 ~~ 1*Item.3.t3 + 0*Item.4.t3
Item.4.t3 ~~ 1*Item.4.t3 

lambda1.t1 == lambda1.t2
lambda2.t1 == lambda2.t2
lambda3.t1 == lambda3.t2
lambda4.t1 == lambda4.t2

lambda1.t2 == lambda1.t3
lambda2.t2 == lambda2.t3
lambda3.t2 == lambda3.t3
lambda4.t2 == lambda4.t3

mean1.t1 == mean1.t2
mean2.t1 == mean2.t2
mean3.t1 == mean3.t2
mean4.t1 == mean4.t2

mean1.t2 == mean1.t3
mean2.t2 == mean2.t3
mean3.t2 == mean3.t3
mean4.t2 == mean4.t3

'
lavaan.model.all.fixed.allrestricted <-'

d1 =~ lambda1.t1*Item.1.t1 + lambda2.t1*Item.2.t1 + lambda3.t1*Item.3.t1 + lambda4.t1*Item.4.t1
d2 =~ lambda1.t2*Item.1.t2 + lambda2.t2*Item.2.t2 + lambda3.t2*Item.3.t2 + lambda4.t2*Item.4.t2
d3 =~ lambda1.t3*Item.1.t3 + lambda2.t3*Item.2.t3 + lambda3.t3*Item.3.t3 + lambda4.t3*Item.4.t3

lambda1.t1+lambda2.t1+lambda3.t1+lambda4.t1==4
lambda1.t2+lambda2.t2+lambda3.t2+lambda4.t2==4
lambda1.t3+lambda2.t3+lambda3.t3+lambda4.t3==4

d1~~d1 + d2 + d3
d2~~d2 + d3
d3~~d3

#d1~~d2 + d3
#d2~~d3


Item.1.t1 | tau1.t1*t1
Item.2.t1 | tau2.t1*t1
Item.3.t1 | tau3.t1*t1
Item.4.t1 | tau4.t1*t1

Item.1.t2 | tau1.t2*t1
Item.2.t2 | tau2.t2*t1
Item.3.t2 | tau3.t2*t1
Item.4.t2 | tau4.t2*t1

Item.1.t3 | tau1.t3*t1
Item.2.t3 | tau2.t3*t1
Item.3.t3 | tau3.t3*t1
Item.4.t3 | tau4.t3*t1

Item.1.t1 ~ mean1.t1*1
Item.2.t1 ~ mean2.t1*1
Item.3.t1 ~ mean3.t1*1
Item.4.t1 ~ mean4.t1*1

Item.1.t2 ~ mean1.t2*1
Item.2.t2 ~ mean2.t2*1
Item.3.t2 ~ mean3.t2*1
Item.4.t2 ~ mean4.t2*1

Item.1.t3 ~ mean1.t3*1
Item.2.t3 ~ mean2.t3*1
Item.3.t3 ~ mean3.t3*1
Item.4.t3 ~ mean4.t3*1

0 == mean1.t1 + mean2.t1 + mean3.t1 + mean4.t1
0 == mean1.t2 + mean2.t2 + mean3.t2 + mean4.t2
0 == mean1.t3 + mean2.t3 + mean3.t3 + mean4.t3

Item.1.t1 ~~ 1*Item.1.t1 + 0*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + start(1.0)*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + start(1.0)*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t1 ~~ 1*Item.2.t1 + 0*Item.3.t1 + 0*Item.4.t1 + 0*Item.1.t2 + start(1.0)*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + start(1.0)*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t1 ~~ 1*Item.3.t1 + 0*Item.4.t1 + 0*Item.1.t2 + 0*Item.2.t2 + start(1.0)*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + start(1.0)*Item.3.t3 + 0*Item.4.t3
Item.4.t1 ~~ 1*Item.4.t1 + 0*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + start(1.0)*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + start(1.0)*Item.4.t3
Item.1.t2 ~~ 1*Item.1.t2 + 0*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + start(1.0)*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t2 ~~ 1*Item.2.t2 + 0*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + start(1.0)*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t2 ~~ 1*Item.3.t2 + 0*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + start(1.0)*Item.3.t3 + 0*Item.4.t3
Item.4.t2 ~~ 1*Item.4.t2 + 0*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + start(1.0)*Item.4.t3
Item.1.t3 ~~ 1*Item.1.t3 + 0*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.2.t3 ~~ 1*Item.2.t3 + 0*Item.3.t3 + 0*Item.4.t3
Item.3.t3 ~~ 1*Item.3.t3 + 0*Item.4.t3
Item.4.t3 ~~ 1*Item.4.t3 

lambda1.t1 == lambda1.t2
lambda2.t1 == lambda2.t2
lambda3.t1 == lambda3.t2
lambda4.t1 == lambda4.t2

lambda1.t2 == lambda1.t3
lambda2.t2 == lambda2.t3
lambda3.t2 == lambda3.t3
lambda4.t2 == lambda4.t3

mean1.t1 == mean1.t2
mean2.t1 == mean2.t2
mean3.t1 == mean3.t2
mean4.t1 == mean4.t2

mean1.t2 == mean1.t3
mean2.t2 == mean2.t3
mean3.t2 == mean3.t3
mean4.t2 == mean4.t3

'



lavaan.model.est <-function(){}

lavaan.model <- lavaan.model.all.free
lavaan.model <- lavaan.model.loadings.fixed
lavaan.model <- lavaan.model.all.fixed
lavaan.model <- lavaan.model.all.fixed.allrestricted

# no auto vars
itemnames <- colnames(U)
lavaan.model.fit <- lavaan(lavaan.model, 
                           data = U, 
                           int.ov.free = TRUE,
                           int.lv.free = TRUE,
                           meanstructure = TRUE,
                           std.lv =FALSE,
                           ordered = itemnames,
                           parameterization = "theta")

lavaan.model.all.free.fit <- lavaan.model.fit
lavaan.model.loadings.fixed.fit <- lavaan.model.fit
lavaan.model.all.fixed.fit <- lavaan.model.fit
lavaan.model.all.fixed.allrestricted.fit <- lavaan.model.fit

anova(lavaan.model.all.free.fit,lavaan.model.loadings.fixed.fit )
fitMeasures(lavaan.model.all.free.fit)[c("cfi")]-fitMeasures(lavaan.model.loadings.fixed.fit)[c("cfi")]

anova(lavaan.model.all.free.fit,lavaan.model.all.fixed.fit )
fitMeasures(lavaan.model.all.free.fit)[c("cfi")]-fitMeasures(lavaan.model.all.fixed.fit)[c("cfi")]

anova(lavaan.model.all.free.fit,lavaan.model.all.fixed.allrestricted.fit )
fitMeasures(lavaan.model.all.free.fit)[c("cfi")]-fitMeasures(lavaan.model.all.fixed.allrestricted.fit)[c("cfi")]


# no auto vars
View(lavParTable(lavaan.model,
                 int.ov.free = TRUE,
                 int.lv.free = TRUE,
                 meanstructure = TRUE,
                 std.lv =FALSE,
                 parameterization = "theta"))



summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)
fitMeasures(lavaan.model.fit)[c("chisq","pvalue","df",'tli',"cfi","rmsea","srmr")]
inspect(lavaan.model.fit)
inspect(lavaan.model.fit,"partable")
inspect(lavaan.model.fit,"est")

lavTables(lavaan.model.fit)

lavInspect(lavaan.model.fit,what = 'cov.all')

#library("semPlot")
#semPaths(lavaan.model.fit,title=FALSE, curvePivot = TRUE)
#semPaths(lavaan.model.fit,"std",edge.label.cex=0.5, curvePivot = TRUE)
#semPaths(lavaan.model.fit,"std",edge.label.cex=0.5, curvePivot = TRUE, intercepts=FALSE)

#getting lambda values
lavInspect(lavaan.model.fit,what = 'est')$lambda #loadings 
lambda2 <- lavInspect(lavaan.model.fit,what = 'est')$lambda

#getting tau values
lavInspect(lavaan.model.fit,what = 'est')$tau #intercepts
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

#getting nu values
lavInspect(lavaan.model.fit,what = 'est')$nu # same as lavInspect(lavaan.model.fit,what = "cov.lv")
nu<- lavInspect(lavaan.model.fit,what = 'est')$nu
nu


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#
# Calculo dos par?metros
#
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

lavaan.model.parameter.gather <-function(){}

item.par.sim <- matrix(0,12,5)
colnames(item.par.sim) <- c("aj_t1_lav","aj_t2_lav","aj_t3_lav","dj_lav", "bj_lav")


#formula setting n<-0
for(i in seq(1,12,1)){# i items
  for(j in c(1,2,3)){ # j moments
    item.par.sim[i,j] <- lambda2[i,j]*sqrt(theta_var[j])*1.702
    item.par.sim[i,4] <- (-tau[i]+lambda2[i,j]*mu[j])*1.702
  }
}

#derived formula
for(i in seq(1,12,1)){# i items
  for(j in c(1,2,3)){ # j moments
    item.par.sim[i,j] <- lambda2[i,j]*sqrt(theta_var[j])*1.702
    item.par.sim[i,4] <- (nu[i,1]-tau[i]+lambda2[i,j]*mu[j])*1.702
  }
}



item.par.sim

# d --> b d=-a*b
for(i in seq(1,12,1)){# i items
  if(i>=0 && i<=4){
    item.par.sim[i,5] <- -item.par.sim[i,4]/item.par.sim[i,1] #b
  }else if(i>=5 && i<=8){
    item.par.sim[i,5] <- -item.par.sim[i,4]/item.par.sim[i,2] #b
  }else{
    item.par.sim[i,5] <- -item.par.sim[i,4]/item.par.sim[i,3] #b
  }
}

item.par.sim

lavaan.model.parameter.plot <-function(){}


data.graph <- data.frame(a.true=c(a,a,a),
                         a.est=c(item.par.sim[1:4,"aj_t1_lav"],
                                  item.par.sim[5:8,"aj_t2_lav"],
                                  item.par.sim[9:12,"aj_t3_lav"]),
                         group =c(rep("t1",4),rep("t2",4),rep("t3",4))
                         )
ggplot(data.graph, aes(a.true,a.est,color = group)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("a common items - Should be the same")


data.graph <- data.frame(b.true=c(b,b,b),
                         b.est=c(item.par.sim[1:4,"bj_lav"],
                                 item.par.sim[5:8,"bj_lav"],
                                 item.par.sim[9:12,"bj_lav"]),
                         group =c(rep("t1",4),rep("t2",4),rep("t3",4))
)
ggplot(data.graph, aes(b.true,b.est,color = group)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1)+
  ggtitle("a common items - Should be the same")



# http://lavaan.ugent.be/tutorial/groups.html
# HS.model <- '  visual =~ x1 + x2 + x3
#               textual =~ x4 + x5 + x6
#               speed   =~ x7 + x8 + x9 '
# fit <- cfa(HS.model, 
#            data = HolzingerSwineford1939, 
#            group = "school")
# summary(fit)



