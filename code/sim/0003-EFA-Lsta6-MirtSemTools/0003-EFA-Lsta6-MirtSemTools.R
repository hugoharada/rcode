
rm(list=ls())

library(lavaan)
library(mirt)


library(semTools)
#install.packages("semTools")

dat <- expand.table(LSAT6)
head(dat)
#descript(LSAT6)


### run EFA with WLSMV for ordered items
ef2_irt <- efaUnrotate(data=dat,estimator="wlsmv",nf=2,
                       ordered =colnames(dat))

summary(ef2_irt, std = TRUE)
inspect(ef2_irt, "std")

## use oblique rotation
ef2_ob <- oblqRotate(ef2_irt)
summary(ef2_ob,suppress=.001)
