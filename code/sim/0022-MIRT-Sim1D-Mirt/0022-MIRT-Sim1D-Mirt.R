#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#  SINTAXE ELABORADA POR HELITON TAVARES PARA ESTUDAR A RECUPERAÇÃO DE PARÂMETROS DE ITENS
#  E PROFICIÊNCIA ESTIMADAS PELA TEORIA DA RESPOSTA AO ITEM USANDO O MIRT
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rm(list=ls())



if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 
if(!require(ltm)) install.packages("ltm"); library(ltm)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(rlist)) install.packages("rlist"); library(rlist)
library(ggplot2)

set.seed(1) # Resetando a semente

I= 50  # Number of Items
N = 10000
ITERATIONS <- 30

PL=3 # Logistic Model (1,2,3 parameters)
a = runif(I,0.5, 2.5)    # U(0.5 , 2.5)
b = runif(I,-2, 2.0)     # U(-2 , 2)
c = runif(I,0.0, 0.3)    # U(0 , 0.3)

d=-a*b # MIRT trabalha com o intercepto (d=-ab) e não com a dificuldade (b)
if (PL<=2) c=c*0; if (PL==1) a=rep(1,I)

#######
# Checking how the graph for theta looks like for original item parameters values.
#######
parameters = mirt(U, 1, itemtype = '3PL', pars = "values")
str(parameters)

parameters$value[parameters$name=="a1"] #list all lines labed "a1"
parameters$value[parameters$name=="a1"] <- a
parameters$est[parameters$name=="a1"] <- FALSE
parameters$value[parameters$name=="d"] #list all lines labed "d"
parameters$value[parameters$name=="d"] <- d
parameters$est[parameters$name=="d"] <- FALSE

parameters$value[parameters$name=="g"] #list all lines labed "d"
parameters$value[parameters$name=="g"] <- c
parameters$est[parameters$name=="g"] <- FALSE


mirt.2PL = mirt(U, 1, itemtype = '3PL', pars = parameters)
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
profic2 = fscores(mirt.2PL) #estimativas das proficiências individuais
plot(Theta,profic2, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")




labcd <- list()


for(k in 1:ITERATIONS)
{
  set.seed(k) # Resetando a semente
  
  Theta = rnorm(N,0,1)   # N(0 , 1)
  eta  = Theta%*% t(a) +  matrix(d,N,I,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
  P = c + (1-c)/(1+exp(-eta));  P=t(P) # n x I
  X = runif(N*I);  dim(X)=c(N,I)   # matriz n x I de U(0,1)
  U = 1*(X<P)  ; U=as.data.frame(U) # AQUI TEMOS OS DADOS DICOTÔMICOS
  colnames(U)=paste0("Item.",1:I)
  
  rm(P,X,eta)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ESTIMAÇÃO PELO MIRT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  mirt.3PL = mirt(U, 1, itemtype = '3PL')  # 
  PAR=coef(mirt.3PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
  profic = fscores(mirt.3PL) #estimativas das proficiências individuais
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  #%%%%%%%%%%%%%%%%%%%%  TRANSFORMAÇÃO PARA OBTER A DIFICULDADE (b) %%%%%%%%%%%%%%%%%%%%%%%%
  PAR=cbind(PAR, -PAR[,2]/PAR[,1]) # Coloquei o "b"na última coluna
  colnames(PAR)=c("a","d","c","b")
  labcd <- list.append(labcd,PAR)
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
}


labcd_acum = matrix(0,50,4)
for(i in 1:ITERATIONS){
  for(j in 1:4){
    labcd_acum[,j]=labcd_acum[,j]+labcd[[i]][,j]
  }
}
labcdmean=labcd_acum/ITERATIONS

names <-1:50


plot(a,labcdmean[,1], main="Recuperação dos parâmetros de discriminação", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
text(a,labcdmean[,1], labels=names, cex= 0.7,pos=3)

plot(b,labcdmean[,4], main="Recuperação dos parâmetros de dificuldade",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(c,labcdmean[,3], main="Recuperação dos parâmetros de acerto casual",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")


parameters = mirt(U, 1, itemtype = '3PL', pars = "values")
str(parameters)

parameters$value[parameters$name=="a1"] #list all lines labed "a1"
parameters$value[parameters$name=="a1"] <- labcdmean[,1]
parameters$est[parameters$name=="a1"] <- FALSE
parameters$value[parameters$name=="d"] #list all lines labed "d"
parameters$value[parameters$name=="d"] <- labcdmean[,2]
parameters$est[parameters$name=="d"] <- FALSE

parameters$value[parameters$name=="g"] #list all lines labed "d"
parameters$value[parameters$name=="g"] <- labcdmean[,3]
parameters$est[parameters$name=="g"] <- FALSE


mirt.2PL = mirt(U, 1, itemtype = '3PL', pars = parameters)
PAR=coef(mirt.2PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
profic2 = fscores(mirt.2PL) #estimativas das proficiências individuais
plot(Theta,profic2, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")






#%%%%%%%%%%%%%%%%%%%%%%%% REPRODUÇÃO DOS PARÂMETROS DOS ITENS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(a,PAR[,1], main="Recuperação dos parâmetros de discriminação", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
text(a,PAR[,1], labels=names, cex= 0.7,pos=3)

plot(b,PAR[,4], main="Recuperação dos parâmetros de dificuldade",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(c,PAR[,3], main="Recuperação dos parâmetros de acerto casual",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%  REPRODUÇÃO DAS PROFICIÊNCIAS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(Theta,profic, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
hist(profic, main="Recuperação das proficiências", xlab="Estimativas",ylab="Frequência")

residuals<-(Theta-profic)
describe(residuals)
plot(residuals)


head(cbind(profic,Theta))
mean(profic)
mean(Theta)


pbis <-matrix(0,nrow = I,ncol=2)
pbis1 <-matrix(0,nrow = I,ncol=1)
for(i in 1:I){
  pbis[i,1]<-i
  pbis[i,2]<-biserial.cor(rowSums(U),U[,i],use = "complete.obs",level=2)
#  pbis1[i]<-cor.test(rowSums(U),U[,i]) #verificar porque não funciona
}

colnames(pbis)=c("item","pbis")
df.pbis<-data.frame(pbis)
p<-ggplot(data=df.pbis, aes(x=item, y=pbis)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=names), vjust=1.6, size=2, color="white")
  
p

acomp<-cbind(a,PAR[,1],pbis)
acomp[11,]


mirt.3PL = mirt(U[,c(-10,-11,-27,-30)], 1, itemtype = '3PL')  # 
PAR=coef(mirt.3PL,simplify=TRUE)$items[,1:3] # Estimação dos parâmetros dos itens: Colunas a,d,c
profic = fscores(mirt.3PL) #estimativas das proficiências individuais
dim(PAR)
PAR=cbind(PAR, -PAR[,2]/PAR[,1]) # Coloquei o "b"na última coluna
colnames(PAR)=c("a","d","c","b")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
names <-1:50
#%%%%%%%%%%%%%%%%%%%%%%%% REPRODUÇÃO DOS PARÂMETROS DOS ITENS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(a[c(-10,-11,-27,-30)],PAR[,1], main="Recuperação dos parâmetros de discriminação", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
text(a[c(-10,-11,-27,-30)],PAR[,1], labels=names, cex= 0.7,pos=2)

plot(b,PAR[,4], main="Recuperação dos parâmetros de dificuldade",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(c,PAR[,3], main="Recuperação dos parâmetros de acerto casual",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(Theta,profic, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

library(psych)
describe(profic)
residuals<-(Theta-profic)
describe(residuals)
plot(residuals)
