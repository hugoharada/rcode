library(lavaan)
library(ggplot2)

data(HolzingerSwineford1939)

## fit model
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)
head(lavPredict(fit))
head(lavPredict(fit, type = "ov"))

N<-1000
xnorm <- runif(N,-3, 5)
pxnorm <-pnorm(xnorm,mean = 1, sd=1)
pxlogis <- plogis(xnorm,location = 1, scale=1)

data <- data.frame(rbind(
  cbind(xnorm,pxnorm,rep("normal",N)),
  cbind(xnorm,pxlogis,rep("logit",N))
))
colnames(data)<-c("x","p","dist")
head(data)
str(data)
data$x <- as.numeric(as.character(data$x))
data$p <- as.numeric(as.character(data$p))
ggplot(data,aes(x=x,y=p,group=dist))+geom_line(aes(linetype=dist))+geom_vline(xintercept =  1)

xnorm <- runif(N,-3, 5)
pxnorm <-pnorm(xnorm,mean = 1, sd=1)
pxlogis <- plogis(xnorm,location = 1, scale=1/1.702)

data <- data.frame(rbind(
  cbind(xnorm,pxnorm,rep("normal",N)),
  cbind(xnorm,pxlogis,rep("logit",N))
))
colnames(data)<-c("x","p","dist")
head(data)
str(data)
data$x <- as.numeric(as.character(data$x))
data$p <- as.numeric(as.character(data$p))
ggplot(data,aes(x=x,y=p,group=dist))+geom_line(aes(linetype=dist))+geom_vline(xintercept =  1)


