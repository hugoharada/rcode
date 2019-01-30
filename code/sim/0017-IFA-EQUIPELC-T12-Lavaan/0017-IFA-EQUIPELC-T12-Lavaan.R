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
setwd("H:\\Meu Drive\\IME-USP\\RLanguage\\repository\\rcode\\code\\sim\\0017-IFA-EQUIPELC-T12-Lavaan")


mydata = read.csv("EQUIPE-2017-S1S3-DICOTOMICA.TXT")
#confirmando dimensoes
ncol(mydata)
nrow(mydata)
dim(mydata)
str(mydata)
head(mydata)

lc <-mydata[,2:81]

################################################################
#
# Codigo - pacote MIRT
#
################################################################

model.2d <- mirt.model('
               F1 = 1-40
               F2 = 41-80
               COV = F1*F2')
 
mirt.fit = mirt(lc, model=model.2d, itemtype = '2PL', method = 'EM', SE=TRUE)  # 
item.par.est=coef(mirt.fit,
                   rotate="oblimin",
                   simplify=TRUE)
#checking parameter recovery
item.par.est$items
summary(mirt.fit)
profic = fscores(mirt.fit) #estimativas das profici?ncias individuais
head(profic)


################################################################
#
# Código - pacote lavaan
#
################################################################


lavaan.lcmt.2pl.model <-'
D1 =~ D1_LC1_051*LC1_051+D1_LC1_052*LC1_052+D1_LC1_053*LC1_053+D1_LC1_054*LC1_054+D1_LC1_055*LC1_055+D1_LC1_056*LC1_056+D1_LC1_057*LC1_057+D1_LC1_058*LC1_058+D1_LC1_059*LC1_059+D1_LC1_060*LC1_060+D1_LC1_061*LC1_061+D1_LC1_062*LC1_062+D1_LC1_063*LC1_063+D1_LC1_064*LC1_064+D1_LC1_065*LC1_065+D1_LC1_066*LC1_066+D1_LC1_067*LC1_067+D1_LC1_068*LC1_068+D1_LC1_069*LC1_069+D1_LC1_070*LC1_070+D1_LC1_071*LC1_071+D1_LC1_072*LC1_072+D1_LC1_073*LC1_073+D1_LC1_074*LC1_074+D1_LC1_075*LC1_075+D1_LC1_076*LC1_076+D1_LC1_077*LC1_077+D1_LC1_078*LC1_078+D1_LC1_079*LC1_079+D1_LC1_080*LC1_080+D1_LC1_081*LC1_081+D1_LC1_082*LC1_082+D1_LC1_083*LC1_083+D1_LC1_084*LC1_084+D1_LC1_085*LC1_085+D1_LC1_086*LC1_086+D1_LC1_087*LC1_087+D1_LC1_088*LC1_088+D1_LC1_089*LC1_089+D1_LC1_090*LC1_090
D2 =~ D2_LC2_006*LC2_006+D2_LC2_007*LC2_007+D2_LC2_008*LC2_008+D2_LC2_009*LC2_009+D2_LC2_010*LC2_010+D2_LC2_011*LC2_011+D2_LC2_012*LC2_012+D2_LC2_013*LC2_013+D2_LC2_014*LC2_014+D2_LC2_015*LC2_015+D2_LC2_016*LC2_016+D2_LC2_017*LC2_017+D2_LC2_018*LC2_018+D2_LC2_019*LC2_019+D2_LC2_020*LC2_020+D2_LC2_021*LC2_021+D2_LC2_022*LC2_022+D2_LC2_023*LC2_023+D2_LC2_024*LC2_024+D2_LC2_025*LC2_025+D2_LC2_026*LC2_026+D2_LC2_027*LC2_027+D2_LC2_028*LC2_028+D2_LC2_029*LC2_029+D2_LC2_030*LC2_030+D2_LC2_031*LC2_031+D2_LC2_032*LC2_032+D2_LC2_033*LC2_033+D2_LC2_034*LC2_034+D2_LC2_035*LC2_035+D2_LC2_036*LC2_036+D2_LC2_037*LC2_037+D2_LC2_038*LC2_038+D2_LC2_039*LC2_039+D2_LC2_040*LC2_040+D2_LC2_041*LC2_041+D2_LC2_042*LC2_042+D2_LC2_043*LC2_043+D2_LC2_044*LC2_044+D2_LC2_045*LC2_045
D1~~D2

D2_LC2_006 = D1_LC1_051*gradient + intercept

LC1_051 | tLC1_051*t1
LC1_052 | tLC1_052*t1
LC1_053 | tLC1_053*t1
LC1_054 | tLC1_054*t1
LC1_055 | tLC1_055*t1
LC1_056 | tLC1_056*t1
LC1_057 | tLC1_057*t1
LC1_058 | tLC1_058*t1
LC1_059 | tLC1_059*t1
LC1_060 | tLC1_060*t1
LC1_061 | tLC1_061*t1
LC1_062 | tLC1_062*t1
LC1_063 | tLC1_063*t1
LC1_064 | tLC1_064*t1
LC1_065 | tLC1_065*t1
LC1_066 | tLC1_066*t1
LC1_067 | tLC1_067*t1
LC1_068 | tLC1_068*t1
LC1_069 | tLC1_069*t1
LC1_070 | tLC1_070*t1
LC1_071 | tLC1_071*t1
LC1_072 | tLC1_072*t1
LC1_073 | tLC1_073*t1
LC1_074 | tLC1_074*t1
LC1_075 | tLC1_075*t1
LC1_076 | tLC1_076*t1
LC1_077 | tLC1_077*t1
LC1_078 | tLC1_078*t1
LC1_079 | tLC1_079*t1
LC1_080 | tLC1_080*t1
LC1_081 | tLC1_081*t1
LC1_082 | tLC1_082*t1
LC1_083 | tLC1_083*t1
LC1_084 | tLC1_084*t1
LC1_085 | tLC1_085*t1
LC1_086 | tLC1_086*t1
LC1_087 | tLC1_087*t1
LC1_088 | tLC1_088*t1
LC1_089 | tLC1_089*t1
LC1_090 | tLC1_090*t1
LC2_006 | tLC2_006*t1
LC2_007 | tLC2_007*t1
LC2_008 | tLC2_008*t1
LC2_009 | tLC2_009*t1
LC2_010 | tLC2_010*t1
LC2_011 | tLC2_011*t1
LC2_012 | tLC2_012*t1
LC2_013 | tLC2_013*t1
LC2_014 | tLC2_014*t1
LC2_015 | tLC2_015*t1
LC2_016 | tLC2_016*t1
LC2_017 | tLC2_017*t1
LC2_018 | tLC2_018*t1
LC2_019 | tLC2_019*t1
LC2_020 | tLC2_020*t1
LC2_021 | tLC2_021*t1
LC2_022 | tLC2_022*t1
LC2_023 | tLC2_023*t1
LC2_024 | tLC2_024*t1
LC2_025 | tLC2_025*t1
LC2_026 | tLC2_026*t1
LC2_027 | tLC2_027*t1
LC2_028 | tLC2_028*t1
LC2_029 | tLC2_029*t1
LC2_030 | tLC2_030*t1
LC2_031 | tLC2_031*t1
LC2_032 | tLC2_032*t1
LC2_033 | tLC2_033*t1
LC2_034 | tLC2_034*t1
LC2_035 | tLC2_035*t1
LC2_036 | tLC2_036*t1
LC2_037 | tLC2_037*t1
LC2_038 | tLC2_038*t1
LC2_039 | tLC2_039*t1
LC2_040 | tLC2_040*t1
LC2_041 | tLC2_041*t1
LC2_042 | tLC2_042*t1
LC2_043 | tLC2_043*t1
LC2_044 | tLC2_044*t1
LC2_045 | tLC2_045*t1

'

lavaan.lcmt.2pl.model.fit <- cfa(lavaan.lcmt.2pl.model, data = lc , std.lv=TRUE )
summary ( lavaan.lcmt.2pl.model.fit , standardized = TRUE )
fitMeasures(lavaan.lcmt.2pl.model.fit)

################################################################
#
# Comparação das estimativas.
#
################################################################

#getting lambda values
lavInspect(lavaan.lcmt.2pl.model.fit,what = 'est')$lambda
lambda2 <- lavInspect(lavaan.lcmt.2pl.model.fit,what = 'est')$lambda
colnames(lambda2) <- c("lambda1","lambda2")

#getting tau values
lavInspect(lavaan.lcmt.2pl.model.fit,what = 'est')$tau
tau <- lavInspect(lavaan.lcmt.2pl.model.fit,what = 'est')$tau
colnames(tau) <- c("tau")


item.par.sim <- matrix(0,84,3)
colnames(item.par.sim) <- c("aj1_lav","aj2_lav","dj_lav")

for(i in seq(1,84,1)){
  for(j in c(1,2)){
    item.par.sim[i,j] <- lambda2[i,j]/sqrt(1-t(lambda2[i,])%*%lambda2[i,])*1.7
  }
  item.par.sim[i,3] <- tau[i]/sqrt(1-t(lambda2[i,])%*%lambda2[i,])*1.7
  
}

item.par.est$items[1,]
item.par.sim[1,]

mirt.par.est<-cbind(item.par.est$items[,1:3])
colnames(mirt.par.est) <- c("aj1_mirt","aj2_mirt","dj_mirt")

comp.table<-cbind(mirt.par.est, lambda2, tau, item.par.sim)


##Longtable output for latex.

tmp<-xtable(comp.table,latex.environments = "center",caption="Equivalência de parâmetros bidimensionais - Mirt vs Lavaan")

add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\hline\n\\endhead\n",
                  "\\hline\n",
                  "\\multicolumn{", dim(tmp)[2] + 1, "}{l}",
                  "{\\footnotesize Continued on next page}\n",
                  "\\endfoot\n",
                  "\\endlastfoot\n")
add.to.row$command <- command

print(tmp, hline.after=c(-1), add.to.row = add.to.row,
      tabular.environment = "longtable")

# EPS output

setEPS()
postscript("fMultiDimEquipeS01LcMtMirtLavEqui.eps")
par(mfrow=c(3,1))

plot(comp.table[,1],comp.table[,7], 
     main = "Estimativas de aj1 ", 
     xlab = "Est. MIRT", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(comp.table[,2],comp.table[,8], 
     main = "Estimativas de aj2 ", 
     xlab = "Est. MIRT", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

plot(comp.table[,3],-comp.table[,9], 
     main = "Estimativas de dj ", 
     xlab = "Est. MIRT", 
     ylab = "Est. LAVAAN")
abline(c(0,0),c(1,1),lty=2,col="gray",lwd=0.1)

dev.off()

