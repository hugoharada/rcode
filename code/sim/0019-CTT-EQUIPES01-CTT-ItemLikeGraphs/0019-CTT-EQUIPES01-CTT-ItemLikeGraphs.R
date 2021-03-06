
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
if(!require(ltm)) install.packages("ltm"); library(ltm)


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
gabarito <- mydata[1,2:ncol(mydata)]
data <-mydata[2:nrow(mydata),2:ncol(mydata)]

ItemNames <- colnames(gabarito)
str(ItemNames)

#caucaia files
setwd("E:\\REPOSITORIO_4QUATROS\\comum\\provas\\ADAPTATIVA_PROVAS\\054-CearaAnalise\\analise\\estatisticas\\bilog\\EF2LC")
readdata = read.table("DataMatrixItemanParaCalibragem.txt", header=FALSE)
str(readdata) # data.frame with one list of factors.
readdata$V1 <-as.character(readdata$V1) # data.frame with one list of characteres.
str(readdata) # data.frame with one list of factors.

nr <- nrow(readdata) #number of rows
nc <- nchar(readdata[[1]][1]) # number of columns
uncompress_data <- NULL
for(i in 1:nr){
  uncompress_data <- rbind(uncompress_data,strsplit(readdata[[1]][i],""))
}
str(uncompress_data)
head(uncompress_data)

str(data)
id_length <- 5
data <-matrix(nrow=nr,ncol=nc-5)
gabarito <- uncompress_data[[1]][6:23]
for(i in 3:nr){
  data[i,] <- uncompress_data[[i]][6:23]
}

################################################################
#
# Analises de todos os itens como uma unica prova
#
################################################################

#scoring
raw_scores <- score(data[3:3545,],gabarito,output.scored=TRUE, rel=TRUE)
str(raw_scores)

biserial.cor(rowSums(raw_scores$scored),raw_scores$scored[,1],use = "complete.obs",level=2)

cor.test(rowSums(raw_scores$scored),raw_scores$scored[,1])

# Analise de distratores
rm(iteman)
iteman<-distractorAnalysis(data[3:3545,],gabarito,nGroups=3,na.rm=TRUE)
iteman
str(iteman)
head(iteman)
class(iteman)


scoreddata <- mult.choice(data,gabarito)

biserial.cor(rowSums(scoreddata),scoreddata[,1],use = "complete.obs")

names(iteman) #list
class(iteman[[1]]) #dataframe
iteman[[9]] #iteman[["CH001"]]
d<- iteman[[9]]
d_trimmed <- data.frame(paste(d[1][[1]],d[2][[1]]),d[[7]],d[[8]],d[[9]])
d_trimmed<-cbind(paste(d_trimmed[1][[1]],d_trimmed[2][[1]]),d_trimmed) # combine with paste to indicate correct answer.
colnames(d_trimmed)=c("Distractor", "g1","g2","g3")
d_trimmed <- d_trimmed[1:5,] # erase I and 0 lines. 
e<-melt(d_trimmed)
e[,2]<- as.numeric(e[,2])
### Isso j� funciona com uma legenda.
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
  p_correct <- iteman[[i]]["rspP"][match('*', iteman[[i]][,1]),1] # get percentage of correct answers
  p_correct_string <- sprintf("%3.2f%%",p_correct*100) # make it friendly in %
  
  data[[i]] <- data.frame(paste(data[[i]][1][[1]],data[[i]][2][[1]]),data[[i]][[7]],data[[i]][[8]],data[[i]][[9]])  # combine with paste to indicate correct answer.
  colnames(data[[i]])=c("Distractor", "g1","g2","g3")
  data[[i]] <- data[[i]][1:5,] # erase I and 0 lines. 
  data[[i]]<-melt(data[[i]])
  data[[i]][,2]<- as.numeric(data[[i]][,2]) # transform 2nd col to numeric to draw lines. 
  
  mydata<- data[[i]]
  p<-ggplot(mydata,aes(variable, value, colour=Distractor)) +geom_line()+geom_point(aes(shape=Distractor)) +
    geom_dl(aes(label = Distractor), method = list(dl.combine("last.points"),dl.trans(x=x+0.2), cex = 0.8)) +
    scale_x_continuous(name = "grupos", breaks = c(1,2,3), labels=c("inf","med","sup"))+
    ggtitle(paste("Analise do item: ", plot_names[i], ". p = ", p_correct_string)) +
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


f <- iteman[[1]] #dataframe
i <- 1
p_correct <- iteman[[i]]["rspP"][match('*', iteman[[i]][,1]),1]
p_correct_string <- sprintf("%3.2f%%",p_correct*100)
