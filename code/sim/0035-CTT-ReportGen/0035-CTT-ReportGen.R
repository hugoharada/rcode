
rm(list=ls())

if(!require(CTT)) install.packages("CTT"); library(CTT)
if(!require(xtable)) install.packages("xtable"); library(xtable)
if(!require(reshape2)) install.packages("reshape2"); library(reshape2)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(directlabels)) install.packages("directlabels"); library(directlabels)
if(!require(gridExtra)) install.packages("gridExtra"); library(gridExtra)
if(!require(gtable)) install.packages("gtable"); library(gtable)
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


vItemSel <- rep(FALSE,ncol(mydata))
names(vItemSel) <- colnames(mydata)
vItemSel

#this command prints the labels for all CH itens
vChsel<-paste("CH",sprintf("%03d",1:45),sep = "")
vChsel<-c("ITEM",vChsel)
vChsel

vItemSel[vChsel]<- TRUE # set only CH itens to TRUE

vChData <- mydata[vItemSel] # select only CH itens.

#vItemSel["CH002"]<-FALSE # set a bad item to FALSE
#mydata[vItemSel] # select only CH itens.

################################################################
#
# Analises de todos os itens como uma unica prova
#
################################################################

#scoring

gabarito <- mydata[1,2:ncol(vChData)]
data <-mydata[2:nrow(vChData),2:ncol(vChData)]


raw_scores <- score(data,gabarito,output.scored=TRUE, rel=TRUE)
str(raw_scores)

raw_scores$reliability$alpha #alpha
raw_scores$reliability$nPerson #person number
raw_scores$reliability$nItem #item
raw_scores$reliability$scaleMean #item
raw_scores$reliability$scaleSD #item


biserial.cor(rowSums(raw_scores$scored),raw_scores$scored[,1],use = "complete.obs",level=2)

cor.test(rowSums(raw_scores$scored),raw_scores$scored[,1])

# Analise de distratores
rm(iteman)
iteman<-distractorAnalysis(data,gabarito,nGroups=3)
iteman
str(iteman)
head(iteman)
class(iteman)

scoreddata <- mult.choice(data,gabarito)

biserial.cor(rowSums(scoreddata),scoreddata[,1],use = "complete.obs")


iteman[["CH001"]]["correct"]

names(iteman) #list
class(iteman[[1]]) 
iteman[[9]] #iteman[["CH001"]]
d<- iteman[[9]]
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
