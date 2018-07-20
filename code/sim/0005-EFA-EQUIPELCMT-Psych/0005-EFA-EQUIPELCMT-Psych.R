rm(list=ls())

library(psych)

#working on laptop
setwd("C:\\Users\\hugo\\Google Drive\\IME-USP\\Tese\\longitudinalSEM\\dados\\EQUIPE-2017-S1-PROVA034-QUIZ538")

#working on server00
setwd("G:\\Meu Drive\\IME-USP\\Tese\\longitudinalSEM\\dados\\EQUIPE-2017-S1-PROVA034-QUIZ538")

mydata = read.csv("EQUIPE-2017-S1-PROVA034-QUIZ538-DICOTOMICA.TXT")
#confirmando dimensoes
ncol(mydata)
nrow(mydata)
dim(mydata)
str(mydata)
# mudando o tipo das colunas de entrada
mydata.factor <- mydata
mydata.factor[,2:176]<- lapply(mydata.factor[,2:176],as.integer)
str(mydata.factor)

# provas de LC e MT
LC.MT = cbind(mydata.factor[,47:61],mydata.factor[,63:86],mydata.factor[,132:176])
head(LC.MT)
dim(LC.MT)
str(LC.MT)


test <- as.matrix(LC.MT,nrow=738,ncol=74)
corMat <- tetrachoric(test)



### run EFA with WLSMV for ordered items
fit <- irt.fa(x=corMat,
              #rotate="varimax",
              rotate="oblimin",
              #cor="poly",
              nfactors=2)


fit
fit$fa
fit$tau
fit$rho

corMat <- cor(LC.MT)
cor.plot(corMat,main='corMat itens')
dev.off()

#does not work if nfactors = 2.
fit <- fa(corMat$rho,
              #rotate="varimax",
              rotate="oblimin",
           covar=FALSE,
              nfactors=2)


fit

