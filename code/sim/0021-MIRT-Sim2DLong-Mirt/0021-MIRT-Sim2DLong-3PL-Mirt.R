#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#  SINTAXE ELABORADA POR HELITON TAVARES PARA ESTUDAR A RECUPERA��O DE PAR�METROS DE ITENS
#  E PROFICI�NCIA ESTIMADAS PELA TEORIA DA RESPOSTA AO ITEM USANDO O MIRT
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 
if(!require(MASS)) install.packages("mirt"); library(MASS)
if(!require(rockchalk)) install.packages("mirt"); library(rockchalk)
if(!require(GPArotation)) install.packages("mirt"); library(GPArotation)


###Generate person parameters
## subjects
N <- 1000
#theta <- rnorm(N,0,1)


set.seed(1) # Resetando a semente

I= 30  # Number of Items
N = 10000

PL=3 # Logistic Model (1,2,3 parameters)
a = runif(I,0.5, 2.5)    # U(0.5 , 2.5)
b = runif(I,-2, 2.0)     # U(-2 , 2)
c = runif(I,0.0, 0.3)    # U(0 , 0.3)

d=-a*b # MIRT trabalha com o intercepto (d=-ab) e n�o com a dificuldade (b)
if (PL<=2) c=c*0; if (PL==1) a=rep(1,I)

Theta = rnorm(N,0,1)   # N(0 , 1)

Theta <- mvrnorm(n=N, mu=c(0,0), Sigma = lazyCor(0.4, 2) )
head(Theta)
cor(Theta)
summary(Theta)
var(Theta)


eta  = Theta[,1]%*% t(a) +  matrix(d,N,I,byrow=TRUE);  eta=t(eta) # N x I (a'theta+d)
P = c + (1-c)/(1+exp(-eta));  P=t(P) # n x I
X = runif(N*I);  dim(X)=c(N,I)   # matriz n x I de U(0,1)
U = 1*(X<P)  ; U=as.data.frame(U) # AQUI TEMOS OS DADOS DICOT�MICOS
colnames(U)=paste0("Item.",1:I)

rm(P,X,eta)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ESTIMA��O PELO MIRT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mirt.3PL = mirt(U, 1, itemtype = '3PL')  # 
PAR=coef(mirt.3PL,simplify=TRUE)$items[,1:3] # Estima��o dos par�metros dos itens: Colunas a,d,c
profic = fscores(mirt.3PL) #estimativas das profici�ncias individuais
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%  TRANSFORMA��O PARA OBTER A DIFICULDADE (b) %%%%%%%%%%%%%%%%%%%%%%%%
PAR=cbind(PAR, -PAR[,2]/PAR[,1]) # Coloquei o "b"na �ltima coluna
colnames(PAR)=c("a","d","c","b")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%% REPRODU��O DOS PAR�METROS DOS ITENS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(a,PAR[,1], main="Recupera��o dos par�metros de discrimina��o", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(b,PAR[,4], main="Recupera��o dos par�metros de dificuldade",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(c,PAR[,3], main="Recupera��o dos par�metros de acerto casual",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%  REPRODU��O DAS PROFICI�NCIAS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(Theta,profic, main="Recupera��o das profici�ncias", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
hist(profic, main="Recupera��o das profici�ncias", xlab="Estimativas",ylab="Frequ�ncia")






