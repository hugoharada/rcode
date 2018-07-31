
rm(list=ls())

library(CTT)
library(xtable)

################################################################
#
# Data preparation
#
################################################################



#working on server00
setwd("G:\\Meu Drive\\IME-USP\\RLanguage\\repository\\rcode\\code\\dat")

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
distractorAnalysis(data,gabarito)

# Sumario das estatisticas dos itens
str(raw_scores$scored)
dataframe <- as.data.frame(raw_scores$scored)
str(dataframe)
itemReportData <- itemAnalysis(dataframe, itemReport=TRUE)
itemReportData$itemReport
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
xtable(finalReport,caption="Estatísticas CTT para os Itens da prova de Matemática (MT))", label="equipes01_mt_ctt") #output to Latex



