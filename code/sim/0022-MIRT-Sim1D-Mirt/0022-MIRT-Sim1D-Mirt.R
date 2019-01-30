#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#  SINTAXE ELABORADA POR HELITON TAVARES PARA ESTUDAR A RECUPERAÇÃO DE PARÂMETROS DE ITENS
#  E PROFICIÊNCIA ESTIMADAS PELA TEORIA DA RESPOSTA AO ITEM USANDO O MIRT
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 

set.seed(1) # Resetando a semente

I= 50  # Number of Items
N = 10000

PL=3 # Logistic Model (1,2,3 parameters)
a = runif(I,0.5, 2.5)    # U(0.5 , 2.5)
b = runif(I,-2, 2.0)     # U(-2 , 2)
c = runif(I,0.0, 0.3)    # U(0 , 0.3)

d=-a*b # MIRT trabalha com o intercepto (d=-ab) e não com a dificuldade (b)
if (PL<=2) c=c*0; if (PL==1) a=rep(1,I)

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
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%% REPRODUÇÃO DOS PARÂMETROS DOS ITENS %%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(a,PAR[,1], main="Recuperação dos parâmetros de discriminação", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(b,PAR[,4], main="Recuperação dos parâmetros de dificuldade",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
plot(c,PAR[,3], main="Recuperação dos parâmetros de acerto casual",xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%  REPRODUÇÃO DAS PROFICIÊNCIAS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(Theta,profic, main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
hist(profic, main="Recuperação das proficiências", xlab="Estimativas",ylab="Frequência")







