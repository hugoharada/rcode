
# Yves Roussel - Old and New approaches for the analyses of categorical data in a SEM framework. Slide 12/32

#generate ordered data
Y <- sample(1:4, size=100,replace=TRUE)
prop<- table(Y)/sum(table(Y))
cprop <- c(0,cumsum(prop))


library(MASS)
X1 <- rnorm(100)
X2 <- rnorm(100)
X3 <- rnorm(100)

fit <- polr(ordered(Y)~ X1 + X2 + X3,method = "probit")
fit$zeta
