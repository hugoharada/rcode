
rm(list=ls())

library(CTT)
library(xtable)
library(reshape2)
library(ggplot2)
library(dplyr)
library(reshape2)
library(directlabels)

library(gridExtra)
library(gtable)


################################################################
#
# Data preparation
#
################################################################



#working on server00
setwd("H:\\Meu Drive\\IME-USP\\RLanguage\\repository\\rcode\\code\\dat")


mydata = read.csv("EQUIPE-2017-S1-PROVA034-QUIZ538-GAB-RAW.TXT")
#confirmando dimensoes
ncol(mydata)
nrow(mydata)
dim(mydata)
colnames(mydata)
str(mydata)
head(mydata)
gabarito <- mydata[1,2:176]
data <-mydata[2:739,2:176]
ItemNames <- colnames(gabarito)
str(ItemNames)

################################################################
#
# Analises de todos os itens como uma unica prova
#
################################################################

#scoring
raw_scores <- score(data,gabarito,output.scored=TRUE, rel=TRUE)
str(raw_scores)

# Analise de distratores
iteman<-distractorAnalysis(data,gabarito,nGroups=3)
str(iteman)
head(iteman)
class(iteman)

names(iteman) #list
class(iteman[[1]]) #dataframe
iteman[[1]] #iteman[["CH001"]]
d<- iteman[[1]]
d_trimmed <- data.frame(paste(d[1][[1]],d[2][[1]]),d[[7]],d[[8]],d[[9]])
d_trimmed<-cbind(paste(d_trimmed[1][[1]],d_trimmed[2][[1]]),d_trimmed) # combine with paste to indicate correct answer.
colnames(d_trimmed)=c("Distractor", "g1","g2","g3")
d_trimmed <- d_trimmed[1:5,] # erase I and 0 lines. 
e<-melt(d_trimmed)
e[,2]<- as.numeric(e[,2])
### Isso já funciona com uma legenda.
ggplot(e,aes(variable, value, colour=Distractor)) +geom_line()+geom_point(aes(shape=Distractor)) +
  geom_dl(aes(label = Distractor), method = list(dl.combine("last.points"),dl.trans(x=x+0.2), cex = 0.8)) +
  scale_x_continuous(name = "grupos", breaks = c(1,2,3), labels=c("low","med","upp"))

################################################################
#
# Analises de todos os itens como uma unica prova
#
################################################################


plot_names <- names(iteman)
data <-list()
lplots <-list()

for(i in 1:10){
  data <-c(data, list(iteman[[i]]))
  data[[i]] <- data.frame(paste(data[[i]][1][[1]],data[[i]][2][[1]]),data[[i]][[7]],data[[i]][[8]],data[[i]][[9]])  # combine with paste to indicate correct answer.
  colnames(data[[i]])=c("Distractor", "g1","g2","g3")
  data[[i]] <- data[[i]][1:5,] # erase I and 0 lines. 
  data[[i]]<-melt(data[[i]])
  data[[i]][,2]<- as.numeric(data[[i]][,2]) # transform 2nd col to numeric to draw lines. 
  
  mydata<- data[[i]]
  p<-ggplot(mydata,aes(variable, value, colour=Distractor)) +geom_line()+geom_point(aes(shape=Distractor)) +
    geom_dl(aes(label = Distractor), method = list(dl.combine("last.points"),dl.trans(x=x+0.2), cex = 0.8)) +
    scale_x_continuous(name = "grupos", breaks = c(1,2,3), labels=c("low","med","upp"))+
    ggtitle(paste("Analise do item ", plot_names[i])) +
    xlab("Grupos") + ylab("p")
  lplots <-c(lplots, list(p))

  png(file=paste("ML",i,"-1D.png",sep = "")  ,
      width=600, height=350)
  print(p)
  dev.off()
  
  
}

grid.arrange(grobs = lplots, ncol=3)

setwd("H:\\Meu Drive\\IME-USP\\RLanguage\\repository\\rcode\\code\\sim\\0019-CTT-EQUIPES01-CTT-ItemLikeGraphs\\imagens")

for(i in 1:10){
  png(file=paste("ML",i,"-1D.png",sep = "")  ,
      width=600, height=350)
  print(p)
  dev.off()
}


