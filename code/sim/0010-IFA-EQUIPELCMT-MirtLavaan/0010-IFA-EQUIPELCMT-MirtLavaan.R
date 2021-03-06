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
setwd("C:\\Users\\hugo\\Google Drive\\IME-USP\\Tese\\longitudinalSEM\\dados\\EQUIPE-2017-S1-PROVA034-QUIZ538")

#working on server00
setwd("G:\\Meu Drive\\IME-USP\\Tese\\longitudinalSEM\\dados\\EQUIPE-2017-S1-PROVA034-QUIZ538")

mydata = read.csv("EQUIPE-2017-S1-PROVA034-QUIZ538-DICOTOMICA.TXT")
#confirmando dimensoes
ncol(mydata)
nrow(mydata)
dim(mydata)
str(mydata)
# mudando o tipo das colunas de entrada
mydata.factor <- mydata
mydata.factor[,2:176]<- lapply(mydata.factor[,2:176],as.integer)
str(mydata.factor)

# provas de LC e MT
LC.MT = cbind(mydata.factor[,47:61],mydata.factor[,63:86],mydata.factor[,132:176])
head(LC.MT)
dim(LC.MT)
str(LC.MT)

################################################################
#
# Código - pacote MIRT
#
################################################################

model.2d <- mirt.model('
               F1 = 1-39
               F2 = 40-84
               COV = F1*F2')
 
mirt.fit = mirt(LC.MT, model=model.2d, itemtype = '2PL', method = 'EM', SE=TRUE)  # 
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
D1 =~ D1_LC051*LC051+D1_LC052*LC052+D1_LC053*LC053+D1_LC054*LC054+D1_LC055*LC055+D1_LC056*LC056+D1_LC057*LC057+D1_LC058*LC058+D1_LC059*LC059+D1_LC060*LC060+D1_LC061*LC061+D1_LC062*LC062+D1_LC063*LC063+D1_LC064*LC064+D1_LC065*LC065+D1_LC067*LC067+D1_LC068*LC068+D1_LC069*LC069+D1_LC070*LC070+D1_LC071*LC071+D1_LC072*LC072+D1_LC073*LC073+D1_LC074*LC074+D1_LC075*LC075+D1_LC076*LC076+D1_LC077*LC077+D1_LC078*LC078+D1_LC079*LC079+D1_LC080*LC080+D1_LC081*LC081+D1_LC082*LC082+D1_LC083*LC083+D1_LC084*LC084+D1_LC085*LC085+D1_LC086*LC086+D1_LC087*LC087+D1_LC088*LC088+D1_LC089*LC089+D1_LC090*LC090
D2 =~ D2_MT136*MT136+D2_MT137*MT137+D2_MT138*MT138+D2_MT139*MT139+D2_MT140*MT140+D2_MT141*MT141+D2_MT142*MT142+D2_MT143*MT143+D2_MT144*MT144+D2_MT145*MT145+D2_MT146*MT146+D2_MT147*MT147+D2_MT148*MT148+D2_MT149*MT149+D2_MT150*MT150+D2_MT151*MT151+D2_MT152*MT152+D2_MT153*MT153+D2_MT154*MT154+D2_MT155*MT155+D2_MT156*MT156+D2_MT157*MT157+D2_MT158*MT158+D2_MT159*MT159+D2_MT160*MT160+D2_MT161*MT161+D2_MT162*MT162+D2_MT163*MT163+D2_MT164*MT164+D2_MT165*MT165+D2_MT166*MT166+D2_MT167*MT167+D2_MT168*MT168+D2_MT169*MT169+D2_MT170*MT170+D2_MT171*MT171+D2_MT172*MT172+D2_MT173*MT173+D2_MT174*MT174+D2_MT175*MT175+D2_MT176*MT176+D2_MT177*MT177+D2_MT178*MT178+D2_MT179*MT179+D2_MT180*MT180

D1~~D2
LC051 | t51*t1
LC052 | t52*t1
LC053 | t53*t1
LC054 | t54*t1
LC055 | t55*t1
LC056 | t56*t1
LC057 | t57*t1
LC058 | t58*t1
LC059 | t59*t1
LC060 | t60*t1
LC061 | t61*t1
LC062 | t62*t1
LC063 | t63*t1
LC064 | t64*t1
LC065 | t65*t1
LC067 | t67*t1
LC068 | t68*t1
LC069 | t69*t1
LC070 | t70*t1
LC071 | t71*t1
LC072 | t72*t1
LC073 | t73*t1
LC074 | t74*t1
LC075 | t75*t1
LC076 | t76*t1
LC077 | t77*t1
LC078 | t78*t1
LC079 | t79*t1
LC080 | t80*t1
LC081 | t81*t1
LC082 | t82*t1
LC083 | t83*t1
LC084 | t84*t1
LC085 | t85*t1
LC086 | t86*t1
LC087 | t87*t1
LC088 | t88*t1
LC089 | t89*t1
LC090 | t90*t1
MT136 | t136*t1
MT137 | t137*t1
MT138 | t138*t1
MT139 | t139*t1
MT140 | t140*t1
MT141 | t141*t1
MT142 | t142*t1
MT143 | t143*t1
MT144 | t144*t1
MT145 | t145*t1
MT146 | t146*t1
MT147 | t147*t1
MT148 | t148*t1
MT149 | t149*t1
MT150 | t150*t1
MT151 | t151*t1
MT152 | t152*t1
MT153 | t153*t1
MT154 | t154*t1
MT155 | t155*t1
MT156 | t156*t1
MT157 | t157*t1
MT158 | t158*t1
MT159 | t159*t1
MT160 | t160*t1
MT161 | t161*t1
MT162 | t162*t1
MT163 | t163*t1
MT164 | t164*t1
MT165 | t165*t1
MT166 | t166*t1
MT167 | t167*t1
MT168 | t168*t1
MT169 | t169*t1
MT170 | t170*t1
MT171 | t171*t1
MT172 | t172*t1
MT173 | t173*t1
MT174 | t174*t1
MT175 | t175*t1
MT176 | t176*t1
MT177 | t177*t1
MT178 | t178*t1
MT179 | t179*t1
MT180 | t180*t1
'

lavaan.lcmt.2pl.model.fit <- cfa(lavaan.lcmt.2pl.model, data = LC.MT , std.lv=TRUE )
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

