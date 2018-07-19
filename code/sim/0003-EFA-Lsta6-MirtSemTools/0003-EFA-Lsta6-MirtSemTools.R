
rm(list=ls())

library(lavaan)
library(mirt)


library(semTools)
#install.packages("semTools")

dat <- expand.table(LSAT6)
head(dat)
#descript(LSAT6)



