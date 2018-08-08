library(mirt)

data(LSAT7)
fulldata <- expand.table(LSAT7)
mod1 <- mirt(fulldata,1,SE=TRUE)
mod2 <- mirt(fulldata,1, itemtype = 'Rasch')
mod3 <- mirt(fulldata,2)

itemplot(mod1, 2)
itemplot(mod1, 2, CE = TRUE)
itemplot(mod1, 2, type = 'info')
itemplot(mod1, 2, type = 'info', CE = TRUE)

mods <- list(twoPL = mod1, onePL = mod2)
itemplot(mods, 1, type = 'RE')

#multidimensional
itemplot(mod3, 4, type = 'info')
itemplot(mod3, 4, type = 'infocontour')
itemplot(mod3, 4, type = 'tracecontour')


#############################################################################


rm(list=ls())

library(mirt)
library(lavaan)
library(xtable)

################################################################
#
# Leitura e preparação dos dados
#
################################################################


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

################################################################
#
# Código - pacote MIRT
#
################################################################

model.2d <- mirt.model('
               F1 = 1-39
               F2 = 40-84
               COV = F1*F2')

model.2d <- mirt.model('
               F1 = 1-39
               F2 = 40-84')
model.2d <- mirt.model('
               F1 = 1-84
               F2 = 1-84
               COV = F1*F2')

##########################################################################################
#3PL
##########################################################################################


mirt.fit = mirt(LC.MT, model=model.2d, itemtype = '3PL', method = 'EM', SE=TRUE)  # 
item.par.est=coef(mirt.fit,
                  #rotate="oblimin",
                  rotate="varimax",
                  simplify=TRUE)
#checking parameter recovery
item.par.est$items
summary(mirt.fit)
profic = fscores(mirt.fit) #estimativas das profici?ncias individuais
head(profic)


#multidimensional plot
itemplot(mirt.fit, item=4, type = 'trace')
itemplot(mirt.fit, item=4, type = 'info')
itemplot(mirt.fit, item=4, type = 'infocontour')
itemplot(mirt.fit, item=4, type = 'tracecontour')


# plot as png 
setEPS()
postscript("MIRT-3D-3PLGRAPH.eps")
itemplot(mirt.fit, item=4, type = 'trace')
dev.off()

##########################################################################################
#2PL
##########################################################################################

mirt.fit = mirt(LC.MT, model=model.2d, itemtype = '2PL', method = 'EM', SE=TRUE)  # 
item.par.est=coef(mirt.fit,
                  #rotate="oblimin",
                  rotate="varimax",
                  simplify=TRUE)
#checking parameter recovery
item.par.est$items
summary(mirt.fit)
profic = fscores(mirt.fit) #estimativas das profici?ncias individuais
head(profic)


# plot as png 
setEPS()
postscript("MIRT-3D-2PLGRAPH.eps")
itemplot(mirt.fit, item=4, type = 'trace')
dev.off()

