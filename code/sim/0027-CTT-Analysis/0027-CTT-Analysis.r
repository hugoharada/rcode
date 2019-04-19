
rm(list=ls())

library(psych)

if(!require(tidyr)) install.packages("tidyr"); library(tidyr)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 

setwd("H:\\Meu Drive\\IME-USP\\RLanguage\\repository\\rcode\\code\\sim\\0027-CTT-Analysis")


dia01<-read.csv("dia1-dicotomica.txt",sep = ",", header = FALSE)
names(dia01)<-c("ra", "1i","2i","3i","4i","5i","1e","2e","3e","4e","5e", 6:90)


dia01.numbers<-read.csv("dia1-letters2numbers.txt",sep = ";", header = FALSE)
names(dia01.numbers)<-c("ra", "1i","2i","3i","4i","5i","1e","2e","3e","4e","5e", 6:90)
dim(dia01.numbers)
str(dia01.numbers)

dia02<-read.csv("dia2-dicotomica.txt",sep = ",", header = FALSE)
names(dia02)<-c("ra", 91:180)

sim_contents<-read.csv("TestContent.csv",sep = ",", header = FALSE,encoding = 'UTF-8')
head(sim_contents)
#names(sim_contents)<-c("ra", 91:180)



colSums(dia02)/nrow(dia02)


dia01.numbers<-read.csv("dia1-letters2numbers.txt",sep = ";", header = FALSE)
names(dia01.numbers)<-c("ra", "1i","2i","3i","4i","5i","1e","2e","3e","4e","5e", 6:90)
dim(dia01.numbers)
str(dia01.numbers)

score.multiple.choice(key = dia01.numbers[1,2:96],
                      data = dia01.numbers[,2:96],
                      score = TRUE,
                      missing=FALSE)


dia01.numbers<-read.csv("teste-00.txt",sep = ";", header = FALSE)
names(dia01.numbers)<-c(1:5)
names(dia01.numbers)<-make.names(names(dia01.numbers))
dimnames(dia01.numbers)
str(dia01.numbers)

item.keys <- as.vector(dia01.numbers[1,])
item.keys
names(item.keys)<-c(1:5)
names(item.keys)<-make.names(names(item.keys))
dimnames(item.keys)



score.multiple.choice(key = item.keys,
                      data = dia01.numbers,
                      score = FALSE,
                      total=TRUE,
                      missing=FALSE)

if(!require(readxl)) install.packages("readxl"); library(readxl)
setwd("H:\\Meu Drive\\IME-USP\\RLanguage\\repository\\rcode\\code\\sim\\0027-CTT-Analysis")
# xlsx files
my_data <- read_excel("ReadingTestData.xlsx",sheet = "LEITURA")
my_data <- read_excel("ReadingTestData.xlsx",sheet = "LEITURA",col_types = c("numeric",rep("text",15)))
score <- read_excel("ReadingTestData.xlsx",sheet = "SCORE",col_types = c("numeric",rep("numeric",15)))
qtype <- read_excel("ReadingTestData.xlsx",sheet = "ITEM_TYPE")
affiliation <- read_excel("ReadingTestData.xlsx",sheet = "LISTING")

gabarito <- read_excel("ReadingTestData.xlsx",sheet = "GABARITO")
p <-colSums(score)/nrow(score)
p
i_score <-rowSums(score[2:15])
cbind(score[,1],i_score)


tscore <- gather(score,"Item","Correct",-RA)
tscore
tscore$Item <-as.numeric(tscore$Item)
dbjoin <- inner_join(tscore,qtype, by="Item")
dbjoin

dbjoin2 <- inner_join(dbjoin,affiliation, by="RA")
dbjoin2

dbjoin2 %>% group_by(RA)  %>% summarise(avg=mean(Correct))
dbjoin2 %>% group_by(Item) %>% summarise(avg=mean(Correct))
dbjoin2 %>% group_by(type) %>% summarise(avg=mean(Correct))
dbjoin2 %>% group_by(SUBTYPE) %>% summarise(avg=mean(Correct))

dbjoin2 %>% filter(RA == 232343 )%>% group_by(SUBTYPE) %>% summarise(avg=mean(Correct))

dbjoin2 %>% filter(Colegio == "ABC") %>% group_by(type) %>% summarise(avg=mean(Correct))

