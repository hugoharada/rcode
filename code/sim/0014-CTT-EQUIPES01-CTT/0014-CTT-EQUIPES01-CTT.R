
rm(list=ls())

library(CTT)
library(xtable)

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

iteman[[1]] #iteman[["CH001"]]
str(iteman[[1]])

iteman[[1]][["lower"]][1]
iteman[[1]][["mid66"]][1]
iteman[[1]][["upper"]][1]


plot(0,0,main = "distractor", type='n', xlim=c(1,3), ylim = c(0,1))
cl <- rainbow(5)
i_distr <-3
dista <- c(iteman[[1]][["lower"]][i_distr],iteman[[1]][["mid66"]][i_distr],iteman[[1]][["upper"]][i_distr])
lines(dista, type="l", col=cl[i_distr])



plot(0,0,main = "distractor", type='n', xlim=c(1,3), ylim = c(0,1))
cl <- rainbow(5)

i_item <-3

for(i_distr in 1:5){
  dista <- c(iteman[[i_item]][["lower"]][i_distr],iteman[[i_item]][["mid66"]][i_distr],iteman[[i_item]][["upper"]][i_distr])
  lines(dista, type="o", col=cl[i_distr], ljoin=2)
}


# Sumario das estatisticas dos itens
str(raw_scores$scored)
dataframe <- as.data.frame(raw_scores$scored)
str(dataframe)
itemReportData <- itemAnalysis(dataframe, itemReport=TRUE)
itemReportData$itemReport
str(itemReportData)

finalReport = cbind(ItemNames,itemReportData$itemReport)
finalReport


################################################################
#
# Analises de todos os itens CH
#
################################################################

gabarito <- mydata[1,2:46]
head(gabarito)
data <-mydata[2:739,2:46]

ItemNames <- colnames(gabarito)
str(ItemNames)

#scoring
raw_scores <- score(data,gabarito,output.scored=TRUE, rel=TRUE)
str(raw_scores)

#histograma das notas
raw_scores$score
str(raw_scores$score)
histogram(raw_scores$score)

# Analise de distratores
distractorAnalysis(data,gabarito)

# Sumario das estatisticas dos itens
str(raw_scores$scored)
dataframe <- as.data.frame(raw_scores$scored)
str(dataframe)
itemReportData <- itemAnalysis(dataframe, itemReport=TRUE)
itemReportData$itemReport
rownames(itemReportData$itemReport)<-ItemNames

finalReport = cbind(itemReportData$itemReport[,2:5])
finalReport
xtable(finalReport,caption="Estatísticas CTT para os Itens da prova de Ciencias Humanas (CH)", label="equipes01_ch_ctt") #output to Latex


################################################################
#
# Analises de todos os itens LC
#
################################################################

gabarito <- mydata[1,47:86]
head(gabarito)
data <-mydata[2:739,47:86]

ItemNames <- colnames(gabarito)
str(ItemNames)

#scoring
raw_scores <- score(data,gabarito,output.scored=TRUE, rel=TRUE)
raw_scores
str(raw_scores)

#histograma das notas

# plot as png 
setEPS()
postscript("EQUIPE-S01-LC-HIST.eps")
raw_scores$score
str(raw_scores$score)
hist(raw_scores$score, main = "Histograma dos Escores", xlab = "Escore bruto", ylab = "Frequencia")
dev.off()


# Analise de distratores
distractorAnalysis(data,gabarito)

# Sumario das estatisticas dos itens
str(raw_scores$scored)
dataframe <- as.data.frame(raw_scores$scored)
str(dataframe)
itemReportData <- itemAnalysis(dataframe, itemReport=TRUE)
itemReportData$itemReport
rownames(itemReportData$itemReport)<-ItemNames

finalReport = cbind(itemReportData$itemReport[,2:5])
finalReport
xtable(finalReport,caption="Estatísticas CTT para os Itens da prova de Linguagens e Códigos (LC)", label="equipes01_lc_ctt") #output to Latex


################################################################
#
# Analises de todos os itens CN
#
################################################################
mydata[1,87:131]
gabarito <- mydata[1,87:131]
head(gabarito)
data <-mydata[2:739,87:131]

ItemNames <- colnames(gabarito)
str(ItemNames)

#scoring
raw_scores <- score(data,gabarito,output.scored=TRUE, rel=TRUE)
str(raw_scores)

#histograma das notas
setEPS()
postscript("EQUIPE-S01-CN-HIST.eps")
raw_scores$score
str(raw_scores$score)
hist(raw_scores$score, main = "Histograma dos Escores", xlab = "Escore bruto", ylab = "Frequencia")
dev.off()


# Analise de distratores
distractorAnalysis(data,gabarito)

# Sumario das estatisticas dos itens
str(raw_scores$scored)
dataframe <- as.data.frame(raw_scores$scored)
str(dataframe)
itemReportData <- itemAnalysis(dataframe, itemReport=TRUE)
itemReportData$itemReport
rownames(itemReportData$itemReport)<-ItemNames

finalReport = cbind(itemReportData$itemReport[,2:5])
finalReport
xtable(finalReport,caption="Estatísticas CTT para os Itens da prova de Ciências Naturais (CN))", label="equipes01_cn_ctt") #output to Latex


################################################################
#
# Analises de todos os itens MT
#
################################################################
mydata[1,132:176]
gabarito <- mydata[1,132:176]
head(gabarito)
data <-mydata[2:739,132:176]

ItemNames <- colnames(gabarito)
str(ItemNames)

#scoring
raw_scores <- score(data,gabarito,output.scored=TRUE, rel=TRUE)
str(raw_scores)

#histograma das notas
setEPS()
postscript("EQUIPE-S01-MT-HIST.eps")
raw_scores$score
str(raw_scores$score)
hist(raw_scores$score, main = "Histograma dos Escores", xlab = "Escore bruto", ylab = "Frequencia")
dev.off()


# Analise de distratores
distractorAnalysis(data,gabarito)

# Sumario das estatisticas dos itens
str(raw_scores$scored)
dataframe <- as.data.frame(raw_scores$scored)
str(dataframe)
itemReportData <- itemAnalysis(dataframe, itemReport=TRUE)
itemReportData$itemReport
rownames(itemReportData$itemReport)<-ItemNames

finalReport = cbind(itemReportData$itemReport[,2:5])
finalReport
xtable(finalReport,caption="Estatísticas CTT para os Itens da prova de Matemática (MT))", label="equipes01_mt_ctt") #output to Latex



