rm(list=ls())

library(stats)
library(semTools)

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


### run EFA with WLSMV for ordered items
ef2_irt <- efaUnrotate(data=LC.MT,
                       estimator="wlsmv", 
                       nf=2,
                       start=TRUE,
                       parameterization = "delta",
                       ordered =colnames(LC.MT)
                       )

summary(ef2_irt, std = TRUE)
inspect(ef2_irt, "std")

## use oblique rotation
ef2_ob <- oblqRotate(ef2_irt)
summary(ef2_ob,suppress=.001)
