
rm(list=ls())

library(CTT)
library(xtable)
library(ggplot2)
library(dplyr)
library(reshape2)
library(directlabels)

### creating wide data format
data.wide <- cbind(c(1,2,3,4),c(4,5,6,7),c(8,9,10,11) )
colnames(data.wide)=c("A","B","C")
str(data.wide)
data.wide

#chaging it to long format to use with ggplot
data.long <- melt(data.wide)
data.long
data.long <- data.frame(data.long)
data.long


### Isso já funciona com uma legenda.
ggplot(data.long,aes(Var1, value, colour=Var2)) +geom_line()

### Variando o tipo de linha
ggplot(data.long,aes(Var1, value, colour=Var2)) +geom_line(aes(linetype=Var2)) 

### Variando o tipo de line e marker 
ggplot(data.long,aes(Var1, value, colour=Var2)) +geom_line(aes(linetype=Var2)) +geom_point(aes(shape=Var2))

### Com direct labels dá para colocar os rótulos nos gráficos
ggplot(data.long,aes(Var1, value, colour=Var2))+geom_line(aes(linetype=Var2)) +geom_point(aes(shape=Var2))+
  geom_dl(aes(label = Var2), method = list(dl.combine("last.points"),dl.trans(x=x+0.2), cex = 0.8)) 

### Com direct labels dá para colocar os rótulos nos gráficos
ggplot(data.long,aes(Var1, value, colour=Var2))+geom_line(aes(linetype=Var2)) +geom_point(aes(shape=Var2))+
  geom_dl(aes(label = Var2), method = list(dl.combine("last.points"),dl.trans(x=x+0.2), cex = 0.8)) +
  scale_x_continuous(name = "grupos", breaks = c(1,2,3,4), labels=c("low","med1","med2","upp"))
  



