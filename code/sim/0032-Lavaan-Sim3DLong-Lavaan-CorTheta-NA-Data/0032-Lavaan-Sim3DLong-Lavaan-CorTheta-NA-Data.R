
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

if(SigmaType==0){
  Sigma <- lazyCor(c(rho,rho,rho)) #Matriz de Covariância Uniforme
}else if(SigmaType==1){
  Sigma <- lazyCor(c(rho,rho*rho,rho)) #Matriz de Covariância AR(1)
}else if(SigmaType==2){
  Sigma <- lazyCor(c(rho,0,0)) #Matriz de Covariância de bandas
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

colnames(Ut)=paste0("Item",1:70)

p1<-paste("lambda",1:70,"*Item",1:70,sep="",collapse = " + ")
p1
p1<-paste("d1 =~",p1,sep="",collapse = "")
p1

cat(paste("Item",1:70," | tlambda",1:70,"*t1",sep="",collapse = "\n"))

cat(paste("Item.",21:30,".t1 ~~ Item.",21:30,".t2" ,sep="",collapse = "\n"))
cat(paste("Item.",41:50,".t2 ~~ Item.",41:50,".t3" ,sep="",collapse = "\n"))

lavaan.model <-'

d1 =~lambda1*Item1 + lambda2*Item2 + lambda3*Item3 + lambda4*Item4 + lambda5*Item5 + lambda6*Item6 + lambda7*Item7 + lambda8*Item8 + lambda9*Item9 + lambda10*Item10 + lambda11*Item11 + lambda12*Item12 + lambda13*Item13 + lambda14*Item14 + lambda15*Item15 + lambda16*Item16 + lambda17*Item17 + lambda18*Item18 + lambda19*Item19 + lambda20*Item20 + lambda21*Item21 + lambda22*Item22 + lambda23*Item23 + lambda24*Item24 + lambda25*Item25 + lambda26*Item26 + lambda27*Item27 + lambda28*Item28 + lambda29*Item29 + lambda30*Item30 + lambda31*Item31 + lambda32*Item32 + lambda33*Item33 + lambda34*Item34 + lambda35*Item35 + lambda36*Item36 + lambda37*Item37 + lambda38*Item38 + lambda39*Item39 + lambda40*Item40 + lambda41*Item41 + lambda42*Item42 + lambda43*Item43 + lambda44*Item44 + lambda45*Item45 + lambda46*Item46 + lambda47*Item47 + lambda48*Item48 + lambda49*Item49 + lambda50*Item50 + lambda51*Item51 + lambda52*Item52 + lambda53*Item53 + lambda54*Item54 + lambda55*Item55 + lambda56*Item56 + lambda57*Item57 + lambda58*Item58 + lambda59*Item59 + lambda60*Item60 + lambda61*Item61 + lambda62*Item62 + lambda63*Item63 + lambda64*Item64 + lambda65*Item65 + lambda66*Item66 + lambda67*Item67 + lambda68*Item68 + lambda69*Item69 + lambda70*Item70

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
Item31 | tlambda31*t1
Item32 | tlambda32*t1
Item33 | tlambda33*t1
Item34 | tlambda34*t1
Item35 | tlambda35*t1
Item36 | tlambda36*t1
Item37 | tlambda37*t1
Item38 | tlambda38*t1
Item39 | tlambda39*t1
Item40 | tlambda40*t1
Item41 | tlambda41*t1
Item42 | tlambda42*t1
Item43 | tlambda43*t1
Item44 | tlambda44*t1
Item45 | tlambda45*t1
Item46 | tlambda46*t1
Item47 | tlambda47*t1
Item48 | tlambda48*t1
Item49 | tlambda49*t1
Item50 | tlambda50*t1
Item51 | tlambda51*t1
Item52 | tlambda52*t1
Item53 | tlambda53*t1
Item54 | tlambda54*t1
Item55 | tlambda55*t1
Item56 | tlambda56*t1
Item57 | tlambda57*t1
Item58 | tlambda58*t1
Item59 | tlambda59*t1
Item60 | tlambda60*t1
Item61 | tlambda61*t1
Item62 | tlambda62*t1
Item63 | tlambda63*t1
Item64 | tlambda64*t1
Item65 | tlambda65*t1
Item66 | tlambda66*t1
Item67 | tlambda67*t1
Item68 | tlambda68*t1
Item69 | tlambda69*t1
Item70 | tlambda70*t1


'

lavaan.model.fit <- cfa(lavaan.model, data = Ut , 
                        std.lv=TRUE,
                        parameterization="delta",
                        missing = "fiml")
summary ( lavaan.model.fit , standardized = TRUE )
fitMeasures(lavaan.model.fit)






#library("semPlot")
#semPaths(lavaan.model.fit,title=FALSE, curvePivot = TRUE)
#semPaths(lavaan.model.fit,"std",edge.label.cex=0.5, curvePivot = TRUE)
#semPaths(lavaan.model.fit,"std",edge.label.cex=0.5, curvePivot = TRUE, intercepts=FALSE)


#getting lambda values
lavInspect(lavaan.model.fit,what = 'est')$lambda #loadings 
lambda2 <- lavInspect(lavaan.model.fit,what = 'est')$lambda
colnames(lambda2) <- c("lambda1","lambda2")

#getting tau values
lavInspect(lavaan.model.fit,what = 'est')$tau #intercepts
tau <- lavInspect(lavaan.model.fit,what = 'est')$tau
colnames(tau) <- c("tau")

#getting mean values
lavInspect(lavaan.model.fit,what = 'est')$alpha # same as lavInspect(lavaan.model.fit,what = "mean.lv")
mu <- lavInspect(lavaan.model.fit,what = 'est')$alpha

#getting cov values
lavInspect(lavaan.model.fit,what = 'est')$psi # same as lavInspect(lavaan.model.fit,what = "cov.lv")
theta_var<- diag(lavInspect(lavaan.model.fit,what = 'est')$psi)


##################################################################
#
# Calculo dos parâmetros
#
##################################################################


item.par.sim <- matrix(0,90,5)
colnames(item.par.sim) <- c("aj_t1_lav","aj_t2_lav","aj_t3_lav","dj_lav", "bj_lav")





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

for(i in seq(1,90,1)){# i items
  if(i>=0 && i<=30){
    item.par.sim[i,5] <- -item.par.sim[i,4]/item.par.sim[i,1] #b
  }else if(i>=31 && i<=60){
    item.par.sim[i,5] <- -item.par.sim[i,4]/item.par.sim[i,2] #b
  }else{
    item.par.sim[i,5] <- -item.par.sim[i,4]/item.par.sim[i,3] #b
  }
}

item.par.sim

plot(a[1:30],item.par.sim[1:30,1], 
     main = "Estimativas de a - momento 1 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(b[1:30],-item.par.sim[1:30,5], 
     main = "Estimativas de b - momento 1 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(a[21:50],item.par.sim[31:60,2], 
     main = "Estimativas de a - momento 2 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(b[21:50],-item.par.sim[31:60,5], 
     main = "Estimativas de b - momento 2 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

data<-cbind(b[21:50],-item.par.sim[31:60,5])
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

plot(b[41:70],-item.par.sim[61:90,5], 
     main = "Estimativas de b  - momento 3 ", 
     xlab = "Valor real", 
     ylab = "Est. LAVAAN",
     asp=1)
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

data<-cbind(b[41:70],-item.par.sim[61:90,5])
colnames(data)=c("real","est")
reg1 <- lm(est~real,data=data.frame(data) )
abline(reg1)
summary(reg1)


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

models2 <- 'F1 = 1-30
            F2 = 21-50
            F3 = 41-70'
mod_long2 <- multipleGroup( data=Ut, 
                           model=models2, 
                           group = group_names,
                           itemtype='2PL',
                           invariance = c(itemnames,'free_means','free_var','slopes','intercepts'),
                           technical = list(NCYCLES = 1000)) 


par2<-coef(mod_long2,simplify=TRUE)
par2
str(coef(mod_long2,simplify=TRUE))

extract.mirt(mod_long2,'time')
extract.mirt(mod_long2,'covdata')

profic4 = fscores(mod_long2) #estimativas das proficiencias individuais
mean(profic4)


plot(a[1:70],par2$G1[1][['items']][,1], 
     main="Recuperação dos parâmetros de discriminação", 
     xlab="Valores verdadeiros",
     ylab="Estimativas",
     asp = 1);
lines(c(-4,4),c(-4,4), col = "blue")

plot(b[1:70],par2$G1[1][['items']][,2], 
     main="Recuperação dos parâmetros de dificuldade", 
     xlab="Valores verdadeiros",
     ylab="Estimativas",
     asp = 1);
lines(c(-4,4),c(-4,4), col = "blue")

plot(Theta[,1],profic4[1:N], main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
mean(profic[1:N])
sd(profic[1:N])

plot(Theta[,2],profic4[(N+1):(2*N)], main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
mean(profic[(N+1):(2*N)])
sd(profic[(N+1):(2*N)])


plot(Theta[,3],profic4[(2*N+1):(3*N)], main="Recuperação das proficiências", xlab="Valores verdadeiros",ylab="Estimativas"); lines(c(-4,4),c(-4,4), col = "blue")
mean(profic[(2*N+1):(3*N)])
sd(profic[(2*N+1):(3*N)])

cor(cbind(profic4[1:N],
          profic4[(N+1):(2*N)],
          profic4[(2*N+1):(3*N)] ))



