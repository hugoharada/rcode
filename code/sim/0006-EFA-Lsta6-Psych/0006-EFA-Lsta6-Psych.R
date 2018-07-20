rm(list=ls())


library(mirt)
dat <- expand.table(LSAT6)
detach("package:mirt", unload=TRUE)

library(psych)


head(dat)
str(dat)

#does not work if nfactors = 2.
fit <- irt.fa(x=dat,
              #rotate="varimax",
              rotate="oblimin",
              nfactors=2)

fit
fit$fa
fit$tau
fit$rho

detach("package:psych", unload=TRUE)
