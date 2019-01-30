
library(mirt)

setwd("H:\\Meu Drive\\IME-USP\\RLanguage\\repository\\rcode\\code\\sim\\0016-MIRT-Maia")
setwd("G:\\My Drive\\IME-USP\\RLanguage\\repository\\rcode\\code\\sim\\0016-MIRT-Maia")

data = read.csv("IRTBIPOLAR.txt",sep = '\t')
str(data)
head(data)
dim(data)


##########################################################################################
#Rasch
##########################################################################################

mirt.fit.1d = mirt(data, model=1, itemtype = 'Rasch', method = 'EM', SE=TRUE)  # 
item.par.est=coef(mirt.fit.1d,
                  rotate="varimax",
                  simplify=TRUE)
#checking parameter recovery
item.par.est$items
summary(mirt.fit.1d)
profic = fscores(mirt.fit.1d) #estimativas das profici?ncias individuais
head(profic)

#multidimensional plot
itemplot(mirt.fit.1d, item=4, type = 'trace')
itemplot(mirt.fit.1d, item=4, type = 'info')
par(mfrow=c(3,1))

plot(mirt.fit.1d,type='info')
plot(mirt.fit.1d,type='itemscore', facet_items = FALSE)
plot(mirt.fit.1d,type='SE')
plot(mirt.fit.1d,type='itemscore')


##########################################################################################
#2PL
##########################################################################################

mirt.fit.2d = mirt(data, model=1, itemtype = '2PL', method = 'EM', SE=TRUE)  # 
item.par.est=coef(mirt.fit.2d,
                  rotate="varimax",
                  simplify=TRUE)
item.par.est=coef(mirt.fit.2d,
                  rotate="varimax")
#checking parameter recovery
item.par.est$items
summary(mirt.fit.2d)
profic = fscores(mirt.fit.2d) #estimativas das profici?ncias individuais
head(profic)

#multidimensional plot
itemplot(mirt.fit.2d, item=4, type = 'trace', CE=TRUE)
itemplot(mirt.fit.2d, item=4, type = 'infotrace')
itemplot(mirt.fit.2d, item=4, type = 'SE')
itemplot(mirt.fit.2d, item=4, type = 'infoSE')
itemplot(mirt.fit.2d, item=4, type = 'info')
par(mfrow=c(3,1))

plot(mirt.fit.2d,type='info')
plot(mirt.fit.2d,type='itemscore', facet_items = FALSE)
plot(mirt.fit.2d,type='SE')
plot(mirt.fit.2d,type='itemscore')

##########################################################################################
# Teste de Unidimensionalidade 
##########################################################################################

mirt.fit.1d = mirt(data, model=1, itemtype = '2PL', method = 'EM', SE=TRUE)  # 
mirt.fit.2d = mirt(data, model=2, itemtype = '2PL', method = 'EM', SE=TRUE)  # 
anova(mirt.fit.1d, mirt.fit.2d)

mirt.fit.3d = mirt(data, model=3, itemtype = '2PL', method = 'EM', SE=TRUE)  # 
anova(mirt.fit.2d, mirt.fit.3d)

##########################################################################################
# Teste de local dependency
##########################################################################################
# checking for local dependency Yen Q3 statistic. Correlated if > 0.2
#http://eprints.whiterose.ac.uk/106017/1/Critical%20Values%20for%20Yen%E2%80%99s%20Q3.%20Identification%20of%20Local%20Dependence%20in%20the%20Rasch%20model%20using%20Residual%20Correlations.pdf
residuals(mirt.fit.1d,restype = "Q3",suppress=0.2, df.p=TRUE)

write.table(q3, "G:\\My Drive\\IME-USP\\RLanguage\\repository\\rcode\\code\\sim\\0016-MIRT-Maia\\mydata.txt", sep=";")


#print 1D graphs

#getting item names
item.par.est=coef(mirt.fit.1d,
                  rotate="varimax",
                  simplify=TRUE)
names<-rownames(item.par.est$items)
for(i in 1:16){
  png(file=paste("ML",i,"-1D.png",sep = "")  ,
      width=600, height=350)
  print(itemplot(mirt.fit.1d, item=i, type = 'trace', CE=TRUE,main=paste(names[i]," - item",i)))
  dev.off()
}

###info curve for 1D model
for(i in 1:16){
  png(file=paste("ML",i,"-1D-info.png",sep = "")  ,
      width=600, height=350)
  print(itemplot(mirt.fit.1d, item=i, type = 'info', main=paste(names[i]," - 2diteminfo",i),ylim=c(0,2)))
  dev.off()
}



#print 2D graphs
item.par.est=coef(mirt.fit.2d,
                  rotate="varimax",
                  simplify=TRUE)
names<-rownames(item.par.est$items)
for(i in 1:16){
  png(file=paste("ML",i,"-2D.png",sep = "")  ,
  width=600, height=350)
  print(itemplot(mirt.fit.2d, item=i, type = 'trace', CE=TRUE,main=paste(names[i]," - item",i)))
  dev.off()
}

###info curve for 2D model
for(i in 1:16){
  png(file=paste("ML",i,"-2D-info.png",sep = "")  ,
      width=600, height=350)
  print(itemplot(mirt.fit.2d, item=i, type = 'info', main=paste(names[i]," - 2diteminfo",i),ylim=c(0,2)))
  dev.off()
}



### test code
i<-1
png(file=paste("ML",i,"-1D-info.png",sep = "")  ,
    width=600, height=350)
print(itemplot(mirt.fit.1d, item=i, type = 'info', main=paste(names[i]," - item",i)))
dev.off()

paste(names[i]," - ",i)

