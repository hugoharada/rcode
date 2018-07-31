
rm(list=ls())

library(lavaan)
library(mirt)

################################################################
#
# Data preparation
#
################################################################


#working on laptop
setwd("C:\\Users\\hugo\\Google Drive\\IME-USP\\Tese\\longitudinalSEM\\dados\\EQUIPE-2017-S1-PROVA034-QUIZ538")
source("C:\\Users\\hugo\\Google Drive\\IME-USP\\Tese\\longitudinalSEM\\dados\\EQUIPE-2017-S1-PROVA034-QUIZ538\\useful_functions.r", local = TRUE, encoding = "UTF-8")

#working on server00
setwd("G:\\Meu Drive\\IME-USP\\Tese\\longitudinalSEM\\dados\\EQUIPE-2017-S1-PROVA034-QUIZ538")
source('G:\\Meu Drive\\IME-USP\\Tese\\longitudinalSEM\\dados\\EQUIPE-2017-S1-PROVA034-QUIZ538\\useful_functions.r', local = TRUE, encoding = "UTF-8")

mydata = read.csv("EQUIPE-2017-S1-PROVA034-QUIZ538-DICOTOMICA.TXT")
#confirmando dimensoes
ncol(mydata)
nrow(mydata)
dim(mydata)

# mudando o tipo das colunas de entrada para lavann
mydata.factor <- mydata
mydata.factor[,2:176]<- lapply(mydata.factor[,2:176],as.ordered)


#somente prova de LC
lcdata <- cbind(mydata[,47:61],mydata[,63:86])
head(lcdata)

################################################################
#
# MIRT model
#
################################################################

mirt.LC.2PL = mirt(lcdata, 1, itemtype = '2PL',method = 'EM',SE = TRUE)  # 
mirt.obj <-mirt.LC.2PL

ModelFit <- M2(mirt.obj)
ModelFit
rmsea_eval(ModelFit["RMSEA"])


#note that you do not get SE if you use the simplify=TRUE option
PAR<-coef(mirt.obj,IRTpars = TRUE,printSE =TRUE) 
PAR
a_irt<-sapply(PAR,function(x) x[1,1])
a_irt_se <- sapply(PAR,function(x)x[2,1])

b_irt<-sapply(PAR,function(x) x[1,2])
b_irt_se <- sapply(PAR,function(x)x[2,2])

head(PAR)

#Check scores
profic = fscores(mirt.obj) #estimativas das profici?ncias individuais
head(profic)


plot(mirt.obj,type='info')
plot(mirt.obj,type='itemscore')
plot(mirt.obj,type='itemscore', facet_items = FALSE)
plot(mirt.obj,type='SE')


################################################################
#
# Lavaan model
#
################################################################


lavaan.lc.2pl.model <-'
LC =~ L051*LC051+L052*LC052+L053*LC053+L054*LC054+L055*LC055+L056*LC056+L057*LC057+L058*LC058+L059*LC059+L060*LC060+L061*LC061+L062*LC062+L063*LC063+L064*LC064+L065*LC065+L067*LC067+L068*LC068+L069*LC069+L070*LC070+L071*LC071+L072*LC072+L073*LC073+L074*LC074+L075*LC075+L076*LC076+L077*LC077+L078*LC078+L079*LC079+L080*LC080+L081*LC081+L082*LC082+L083*LC083+L084*LC084+L085*LC085+L086*LC086+L087*LC087+L088*LC088+L089*LC089+L090*LC090
LC051 | t051*t1
LC052 | t052*t1
LC053 | t053*t1
LC054 | t054*t1
LC055 | t055*t1
LC056 | t056*t1
LC057 | t057*t1
LC058 | t058*t1
LC059 | t059*t1
LC060 | t060*t1
LC061 | t061*t1
LC062 | t062*t1
LC063 | t063*t1
LC064 | t064*t1
LC065 | t065*t1
LC067 | t067*t1
LC068 | t068*t1
LC069 | t069*t1
LC070 | t070*t1
LC071 | t071*t1
LC072 | t072*t1
LC073 | t073*t1
LC074 | t074*t1
LC075 | t075*t1
LC076 | t076*t1
LC077 | t077*t1
LC078 | t078*t1
LC079 | t079*t1
LC080 | t080*t1
LC081 | t081*t1
LC082 | t082*t1
LC083 | t083*t1
LC084 | t084*t1
LC085 | t085*t1
LC086 | t086*t1
LC087 | t087*t1
LC088 | t088*t1
LC089 | t089*t1
LC090 | t090*t1
'

lavaan.lc.2pl.model.fit <- cfa(lavaan.lc.2pl.model, data = lcdata , std.lv=TRUE )
summary ( lavaan.lc.2pl.model.fit , standardized = TRUE )
head(PAR)
fitted(lavaan.lc.2pl.model.fit)
str(fitted(lavaan.lc.2pl.model.fit))

fitMeasures(lavaan.lc.2pl.model.fit)


#getting lambda values
lavInspect(lavaan.lc.2pl.model.fit,what = 'est')$lambda
lambda <- lavInspect(lavaan.lc.2pl.model.fit,what = 'est')$lambda

#getting tau values
lavInspect(lavaan.lc.2pl.model.fit,what = 'est')$tau
tau <- lavInspect(lavaan.lc.2pl.model.fit,what = 'est')$tau

alpha <- lambda/sqrt(1-lambda^2)
a_sem<- alpha *1.7

beta <- -tau/sqrt(1-lambda^2)
b_sem <- -beta/alpha


################################################################
#
# Comparação das estimativas.
#
################################################################


# a comparison
a_array <- cbind(a_irt[1:39],a_sem, abs(a_irt[1:39]-a_sem))
colnames(a_array) <- c("a_irt", "a_sem", "diff")
head(a_array)
a_array
summary(a_array)

# plot as png 
dev.copy(png,'MEE-TRI-A-PLOT1.png')
dev.off()

# plot as png 
setEPS()
postscript("MEE-TRI-A-PLOT1.eps")
plot(a_irt[1:39],a_sem, main = "MEE-TRI", xlab = "Parametro de discrimição - est. TRI", ylab = "Parametro de discrimição - est. MEE")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)
dev.off()


# b comparison
b_array <- cbind(b_irt[1:39],b_sem, abs(b_irt[1:39]-b_sem))
colnames(b_array) <- c("b_irt", "b_sem", "diff")
head(b_array)
b_array
summary(b_array)

setEPS()
postscript("MEE-TRI-B-PLOT1.eps")
plot(b_array[,1],b_array[,2], main = "MEE-TRI", xlab = "Parametro de dificuldade - est. TRI", ylab = "Parametro de discrimição - est. MEE")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)
dev.off()



d_array <- -a_array[1:39,2]*b_array[,2]
d_array


