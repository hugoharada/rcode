
rm(list=ls())

library(lavaan)
library(mirt)

dat <- expand.table(LSAT6)
head(dat)
#descript(LSAT6)


##########################################################################
# Unidimensional case
##########################################################################


#estimating parameters using MIRT.
mirt.lSAT6.2PL = mirt(dat, 1, itemtype = '2PL', method = 'EM')  # 
PAR=coef(mirt.lSAT6.2PL,simplify=TRUE,IRTpars=TRUE)
PAR$items[,1]/1.7 #normal ogive scaling
PAR

#estimating parameters using lavaan.
lavaan.lsat6.2pl.model <-'
Theta =~ l1*Item_1 + l2*Item_2 + l3*Item_3 + l4*Item_4 + l5*Item_5
Item_1 | th1 *t1
Item_2 | th2 *t1
Item_3 | th3 *t1
Item_4 | th4 *t1
Item_5 | th5 *t1

'
#fitting model
lavaan.lsat6.2pl.model.fit <- cfa(lavaan.lsat6.2pl.model, data = dat , std.lv=TRUE , ordered =c("Item_1","Item_2","Item_3","Item_4","Item_5"))
summary(lavaan.lsat6.2pl.model.fit)
#getting lambda values
lambda <- lavInspect(lavaan.lsat6.2pl.model.fit,what = 'est')$lambda
#getting tau values
tau <- lavInspect(lavaan.lsat6.2pl.model.fit,what = 'est')$tau

# Convert regression to IRT
# alpha1 := ( l1)/ sqrt (1- l1^2)
# beta1 := ( - th1 )/ sqrt (1- l1^2)
# a_IRT := alpha1*1.7
# b_IRT := -beta1/alpha1

a_sem <- (lambda/sqrt(1-lambda*lambda))
b_sem <- tau/sqrt(1-lambda*lambda)/a_sem
a_sem <- 1.7*a_sem #normal ogive scaling

#making comparison tables. a and b values are close.
a_comp <- cbind(a_sem,PAR$items[,1])
colnames(a_comp)<-c("a_sem","a_tri")
a_comp

b_comp <- cbind(b_sem,PAR$items[,2])
colnames(b_comp)<-c("b_sem","b_tri")
b_comp

##########################################################################
# Bidimensional case
##########################################################################

#estimating parameters using MIRT.
mirt.lSAT6.2PL.2D = mirt(dat, 2, itemtype = '2PL', method = 'EM')  # 
PAR=coef(mirt.lSAT6.2PL.2D,simplify=TRUE)
PAR

#estimating bidimensional parameters using lavaan. ??
#estimating parameters using lavaan.
lavaan.lsat6.2pl.model.2D <-'
Theta1 =~ l11*Item_1 + l12*Item_2 + l13*Item_3 + 0*Item_4 + l15*Item_5
Theta2 =~ l21*Item_1 + l22*Item_2 + 0*Item_3 + l24*Item_4 + l25*Item_5

Item_1 | th1 *t1
Item_2 | th2 *t1
Item_3 | th3 *t1
Item_4 | th4 *t1
Item_5 | th5 *t1
Theta1 ~~ 0*Theta2
'
lavaan.lsat6.2pl.model.2D.fit <- cfa(lavaan.lsat6.2pl.model.2D, 
                                     data = dat , 
                                     std.lv=TRUE , #residual variance set to 1.0, 
                                     std.ov=TRUE,
                                     ordered =c("Item_1","Item_2","Item_3","Item_4","Item_5"),
                                     debug=TRUE)

summary ( lavaan.lsat6.2pl.model.2D.fit , standardized = TRUE )

fitMeasures(lavaan.lsat6.2pl.model.2D.fit)

