rm(list=ls())

if(!require(MASS)) install.packages("MASS"); library(MASS)
if(!require(rockchalk)) install.packages("rockchalk"); library(rockchalk)
if(!require(lavaan)) install.packages("lavaan"); library(lavaan)
if(!require(mirt)) install.packages("mirt"); library(mirt)
if(!require(mirtCAT)) install.packages("mirtCAT"); library(mirtCAT) 
if(!require(ltm)) install.packages("ltm"); library(ltm)
if(!require(pryr)) install.packages("pryr"); library(pryr)
if(!require(cowplot)) install.packages("cowplot"); library(cowplot)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(directlabels)) install.packages("directlabels"); library(directlabels)
if(!require(gridExtra)) install.packages("gridExtra"); library(gridExtra)
if(!require(gtable)) install.packages("gtable"); library(gtable)
if(!require(ltm)) install.packages("ltm"); library(ltm)
if(!require(readxl)) install.packages("readxl"); library(readxl)
if(!require(tidyr)) install.packages("tidyr"); library(tidyr)
if(!require(stringr)) install.packages("stringr"); library(stringr)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(xlsx)) install.packages("xlsx"); library(xlsx)

function_definition <- function(){}



#' is_var_defined
#'
#' Check if a variable is defined in memory
#' @param var - Variable to check
#'
#' @return true or false
#' @export
#'
#' @examples
#' a <- 2
#' is_var_defined(a) #returns TRUE
#' is_var_defined(b) #returns FALSE
is_var_defined <- function(var) {
  var <- deparse(substitute(var))
  env <- parent.frame()
  exists(var, env)
}

#' remove_var
#'
#' Delete var from memory
#'
#' @param var - name of a variable
#'
#' @return
#' @export
#'
#' @examples
#' remove_var(prova_lc) #removes var prova_lc  from memory
#'
#'
remove_var <-function(var){
  if(is_var_defined(var)){rm(var)}
}



get_time<-function(){
  return(Sys.time())
}
get_delta_time<-function(start_time){
  return(Sys.time()-start_time)
}




#' Title
#'
#' @param lambda - loadings
#' @param nu     - latent response variable intercepts
#' @param tau    - latent response variable threshold
#' @param mu     - latent variable mean - common factor mean
#' @param psi    - latent variable var  - common factor var
#'
#' @return a, b, d IRT parameters - alpha = a ; d = beta 
#' @export
#'
#' @examples
irt_param_calc2 <- function(lambda, nu, tau, mu, psi,alpha){
  
  a <- lambda*sqrt(psi)*1.7 / sqrt( 1 - psi*lambda^2)
  d <- (nu - tau + lambda*alpha)*1.7 / sqrt( 1 - psi*lambda^2)
  b <- -d/a
  return(c(a=a,b=b,d=d))
  
}

irt_param_calc <- function(lambda, nu, tau, mu, alpha,psi,parameterization){
  
  a <- switch(parameterization,
    ff_dm = {lambda/sqrt(1-lambda^2)*1.7},
    ff_tc = {lambda*1.7},
    im_dm = {lambda*sqrt(psi)*1.7 / sqrt( 1 - psi*lambda^2)},
    im_tc = {lambda*sqrt(psi)*1.7},
    ie_dm = {lambda*sqrt(psi)*1.7 / sqrt( 1 - psi*lambda^2)},
    ie_tc = {lambda*sqrt(psi)*1.7}
  )
  d <- switch(parameterization,
    ff_dm = {(nu - tau)/sqrt(1-lambda^2)*1.7},
    ff_tc = {(nu - tau)*1.7},
    im_dm = {(nu - tau + lambda*alpha)*1.7 / sqrt( 1 - psi*lambda^2)},
    im_tc = {(nu - tau + lambda*alpha)*1.7},
    ie_dm = {(nu - tau + lambda*alpha)*1.7 / sqrt( 1 - psi*lambda^2)},
    ie_tc = {(nu - tau + lambda*alpha)*1.7}
  )
  
  b <- -d/a
  return(c("a"=a,"b"=b,"d"=d))
}

aggregate_results <-function(loop_tmp,est.param, parameterization){
  
  est.param$a$mean[,parameterization] <-colMeans(loop_tmp$a,na.rm=TRUE)
  est.param$b$mean[,parameterization] <-colMeans(loop_tmp$b,na.rm=TRUE)
  est.param$d$mean[,parameterization] <-colMeans(loop_tmp$d,na.rm=TRUE)
  est.param$lambda$mean[,parameterization] <-colMeans(loop_tmp$lambda,na.rm=TRUE)
  est.param$tau$mean[,parameterization] <-colMeans(loop_tmp$tau,na.rm=TRUE)
  est.param$nu$mean[,parameterization] <-colMeans(loop_tmp$nu,na.rm=TRUE)
  est.param$psi$mean[parameterization] <-mean(loop_tmp$psi,na.rm=TRUE)
  est.param$alpha$mean[parameterization] <-mean(loop_tmp$alpha,na.rm=TRUE)
  
  est.param$a$sd[,parameterization] <-apply(loop_tmp$a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$b$sd[,parameterization] <-apply(loop_tmp$b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$d$sd[,parameterization] <-apply(loop_tmp$d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$lambda$sd[,parameterization] <-apply(loop_tmp$lambda,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$tau$sd[,parameterization] <-apply(loop_tmp$tau,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$nu$sd[,parameterization] <-apply(loop_tmp$nu,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$psi$sd[parameterization]   <- sd(loop_tmp$psi,na.rm = TRUE)
  est.param$alpha$sd[parameterization] <- sd(loop_tmp$alpha,na.rm = TRUE)
  
  est.param$eta$sim$mean[parameterization] <-mean(loop_tmp$eta$sim$mean)
  est.param$eta$ebm$mean[parameterization] <-mean(loop_tmp$eta$ebm$mean)
  est.param$eta$ml$mean[parameterization]  <-mean(loop_tmp$eta$ml$mean)
  
  est.param$eta$sim$sd[parameterization]   <-mean(loop_tmp$eta$sim$sd)
  est.param$eta$ebm$sd[parameterization]   <-mean(loop_tmp$eta$ebm$sd)
  est.param$eta$ml$sd[parameterization]    <-mean(loop_tmp$eta$ml$sd)
  
  est.param$fit$tli[parameterization]  <-mean(loop_tmp$fit$tli)
  est.param$fit$cfi[parameterization]  <-mean(loop_tmp$fit$cfi)
  est.param$fit$rmsea[parameterization]  <-mean(loop_tmp$fit$rmsea)
  est.param$fit$time[parameterization]  <-mean(loop_tmp$fit$time)
  return(est.param)
}


write_to_excel <-function(est.param,y_star_mean=y_star_mean,parType=parType,N=N,loopn=loopn,I=I,SigmaType=SigmaType){
  filename <- build_filename(y_star_mean,parType,N,loopn,I,SigmaType)
  
  write.xlsx(data.frame(est.param$eta), filename, sheetName = "eta", col.names = TRUE, row.names = TRUE, append = TRUE)
  write.xlsx(data.frame(est.param$fit), filename, sheetName = "fit", col.names = TRUE, row.names = TRUE, append = TRUE)
  
  tmp <- rbind(cbind(rep("mean",nrow(est.param$a$mean)), est.param$a$mean), 
               rep("",ncol(est.param$a$mean)+1),
               cbind(rep("sd",nrow(est.param$a$sd)), est.param$a$sd),
               rep("",ncol(est.param$a$mean)+1),
               cbind(rep("res",nrow(est.param$a$sd)),est.param$a$mean-est.param$a$mean[,"sim"]) )
  
  write.xlsx(data.frame(tmp), filename, sheetName = "a", col.names = TRUE, row.names = TRUE, append = TRUE)
  
  
  tmp <- rbind(cbind(rep("mean",nrow(est.param$b$mean)), est.param$b$mean), 
               rep("",ncol(est.param$b$mean)+1),
               cbind(rep("sd",nrow(est.param$b$sd)), est.param$b$sd),
               rep("",ncol(est.param$b$mean)+1),
               cbind(rep("res",nrow(est.param$b$sd)),est.param$b$mean-est.param$b$mean[,"sim"]) )
  
  write.xlsx(data.frame(tmp), filename, sheetName = "b", col.names = TRUE, row.names = TRUE, append = TRUE)
  
  
  tmp <- rbind(cbind(rep("mean",nrow(est.param$d$mean)), est.param$d$mean), 
               rep("",ncol(est.param$d$mean)+1),
               cbind(rep("sd",nrow(est.param$d$sd)), est.param$d$sd),
               rep("",ncol(est.param$d$mean)+1),
               cbind(rep("res",nrow(est.param$d$sd)),est.param$d$mean-est.param$d$mean[,"sim"]) )
  
  write.xlsx(data.frame(tmp), filename, sheetName = "d", col.names = TRUE, row.names = TRUE, append = TRUE)
}



get_lavaan_indicator_param <- function(mod.fit,indicator_index,latent_var_index=1){
  lambda <- lavInspect(mod.fit,what = "est")$lambda[indicator_index]
  tau <- lavInspect(mod.fit,what = "est")$tau[indicator_index]
  nu <- lavInspect(mod.fit,what = "est")$nu[indicator_index]
  mu <- lavInspect(mod.fit,what = "mu")[indicator_index]
  return(c(lambda=lambda, nu=nu, tau=tau, mu=mu, alpha=alpha,psi=psi))
}
  

build_filename<-function(y_star_mean,parType,N,loopn,I,SigmaType){
  mean_string <- switch(y_star_mean,
                        "Alpha0p0",
                        "Alpha0p5",
                        "Alpha1p0")
  
  return(paste0(mean_string,"_parType",parType,"_N",N,"_loopn",loopn,"_I",I,"_SigmaType",SigmaType,".xlsx"))
  
}


p_calc<- function(a=1,b,c=0,D=1, theta){
  p <- c + (1-c)/(1+exp(-a*D*(theta-b)))
}

i_calc<- function(a=1,b,c=0,D=1, theta){
  p <- c + (1-c)/(1+exp(-a*(theta-b)))
  i <- D^2*a^2*(1-p)/p*((p-c)/(1-c))^2
}

sample_p <- function(upper_limit=6, lower_limit=-6, a,b,c,n=200){
  
  theta_n <- runif(n=n,min=lower_limit,max=upper_limit)
  p <- p_calc(a=a,b=b,c=c,theta=theta_n)
  i <- i_calc(a=a,b=b,c=c,theta=theta_n)
  return <-cbind(theta_n,p,i)   
}


# plot_cci(a=1,b=-2,c=.18)
# plot_cci(a=1,b=-2,c=.18,plot_info=TRUE)
# plot_cci(a=.45,b=-1,c=.15,500)
# plot_cci(a=1,b=-1,c=0,500,item_id = "CH34")
# plot_cci(a=1,b=-1,c=0,500,item_id = "CN76",plot_info = TRUE)
# setwd("C:\\Users\\hugo\\Downloads")
# plot_cci(a=1,b=-1,c=0,500,item_id = "CN76",plot_info = TRUE,filename="cci_cii.pdf")
# if filename is passed. image is saved in working dir.
plot_cci <- function(a=1,b,c=0,n=200,filename=NULL,item_id="", plot_info=FALSE){
  
  data <- sample_p(a=a,b=b,c=c,n=n)
  title <- sprintf("Curva Característica do Item %s", item_id)
  subtitle <- sprintf("a = %.2f; b = %.2f; c = %.2f",a,b,c)
  pfinal<-ggplot(data.frame(data),aes(x=theta_n,y=p))+geom_line(colour="blue")+
    ylim(0,1)+geom_hline(yintercept=0.5,linetype="dotted")+
    labs(x="Proficiência", y="Probabilidade de acerto",title =title, subtitle = subtitle )
  if(plot_info==TRUE){
    title <- sprintf("Curva de Informação do Item %s", item_id)
    h<-ggplot(data.frame(data),aes(x=theta_n,y=i))+geom_line(colour="red")+
      ylim(0,NA)+geom_hline(yintercept=0.5,linetype="dotted")+
      labs(x="Proficiência", y="Informação",title =title, subtitle = subtitle )
    require(cowplot)
    title <- sprintf("Item %s", item_id)
    title <- ggdraw() + draw_label(title, fontface='bold')
    pfinal <- cowplot::plot_grid(title, pfinal,h,nrow=3,rel_heights=c(0.1, 1,1))
  }
  print(pfinal)
  if(!is.null(filename)){
    ggsave(filename)
  }
  return(pfinal)
}


thresholdCalc <- function(){}

threshold_calc <- function(binary_vector){
  prop <- table(binary_vector)/sum(table(binary_vector))
  cprop <- c(0, cumsum(prop))
  th <- qnorm(cprop)
  return(th)
}


# Threshold calculation 1

# generate ‘ordered’ data with 4 categories
Y <- sample(1:4, size = 100, replace = TRUE)
Y
prop <- table(Y)/sum(table(Y))
cprop <- c(0, cumsum(prop))
th <- qnorm(cprop)
th
# Threshold calculation 2 - quando há uma dependência de variáveis exógenas.

X1 <- rnorm(100); X2 <- rnorm(100); X3 <- rnorm(100)
fit <- polr(ordered(Y) ~ X1 + X2 + X3, method = "probit")
fit$zeta
cbind(X1,X2,X3)

s2 <- sem("Y ~ X1 + X2 + X3", data=data.frame(Y,X1,X2,X3), ordered=c("Y","X1","X2","X3"))
summary(s2)


# Generate Longitudinal Items from a Bivariate LIR
data_creation <-function(){}

#carregando modelos
source("./models.r")

N <- 1000    ## subjects
loopn <-10   ## number of runs

# N <- 10000 ## subjects
# loopn <-1000   ## number of runs

I= 13  # Number of Items
PL=2 # Logistic Model (1,2,3 parameters)
SigmaType <- 1 # 0 = Covariance Uniform, 1 = Covariancia AR1, 2 =  Covariancia de bandas 3 = Covariancia Nula
rho<-0.7
y_star_mean <- 1 #'0p0'=1,'0p5'=2, '1p0'=3 

coefs <- matrix(ncol=6,nrow=I)
colnames(coefs)=c("a1","b1","c1","a2","b2","c2")



#parType <- 1 #random
#parType <- 2 # a=1, b varying
#parType <- 3 # a and b varying.
parType <- 4  # a=0.7, b varying
if(parType==1){
  if (PL==1) {a = rep(1,I)} else {a = runif(I,0.5, 2.5)}    # U(0.5 , 2.5)
  b = runif(I,-2, 2.0)     # U(-2 , 2)
  if (PL<=2) {c = rep(0,I)} else{c = runif(I,0.0, 0.3) } # U(0 , 0.3)
}else if(parType==2){
  a <- rep(1,13)
  b <- seq(-3.0,3.0,by=0.5)
}else if(parType==3){
  a <- c(rep(seq(0.75,1.5,by=0.25),3),0.75)
  b <- seq(-3,3,by=0.5)
}else if(parType==4){
  a <- rep(0.7,13)
  b <- seq(-3.0,3.0,by=0.5)
}
d=-a*b # MIRT trabalha com o intercepto (d=-ab) e nao com a dificuldade (b)
pars <- cbind(a,b,d)

plot_data <- cbind(sample_p(a=pars[1,"a"],b=pars[1,"b"],c=0),1)
for(i in 1:nrow(pars)){
  plot_data <- rbind(plot_data, cbind(sample_p(a=pars[i,"a"],b=pars[i,"b"],c=0),i))
}
colnames(plot_data)[4]<-"item"
ggplot(data=data.frame(plot_data),aes(x=theta_n,y=p,group=item,color=item))+geom_line()
ggplot(data=data.frame(plot_data),aes(x=theta_n,y=i,group=item,color=item))+geom_line()


if(SigmaType==0){
  Sigma <- lazyCor(c(rho,rho,rho)) #Matriz de Covariancia Uniforme
}else if(SigmaType==1){
  Sigma <- lazyCor(c(rho,rho*rho,rho)) #Matriz de Covariancia AR(1)
}else if(SigmaType==2){
  Sigma <- lazyCor(c(rho,0,0)) #Matriz de Covariancia de bandas
}else if(SigmaType==3){
  Sigma <- lazyCor(c(0,0,0)) #Matriz de Covariancia nula
}else{
  Sigma <- NULL
}
Sigma

# fixed_factor,		    delta_marginal 		  ystar_thre_free  	<-	"ff_dm_yt"
# fixed_factor,		    delta_marginal 		  ystar_mean_free  	<-	"ff_dm_ym"
# fixed_factor, 		  theta_conditional	  ystar_thre_free 	<- 	"ff_tc_yt"
# fixed_factor, 		  theta_conditional	  ystar_mean_free 	<- 	"ff_tc_ym"

# indicator_marker, 	delta_marginal 		  ystar_thre_free 	<- 	"im_dm_yt"
# indicator_marker, 	delta_marginal 		  ystar_mean_free 	<- 	"im_dm_ym"
# indicator_marker, 	theta_conditional 	ystar_thre_free 	<-  "im_tc_yt"
# indicator_marker, 	theta_conditional 	ystar_mean_free 	<-  "im_tc_ym"

# indicator_effects, 	delta_marginal   	  ystar_thre_free 	<- 	"ie_dm_yt"
# indicator_effects, 	delta_marginal   	  ystar_mean_free 	<- 	"ie_dm_ym"
# indicator_effects, 	theta_conditional 	ystar_thre_free 	<-	"ie_tc_yt"
# indicator_effects, 	theta_conditional 	ystar_mean_free 	<-	"ie_tc_ym"

working1 <- function(){}

experiments <- c("sim", "mirt", "ff_dm_yt","ff_dm_ym","ff_tc_yt","ff_tc_ym","im_dm_yt","im_dm_ym","im_tc_yt","im_tc_ym","ie_dm_yt","ie_dm_ym","ie_tc_yt","ie_tc_ym")
  

placeholder <- matrix(data=rep(NA,length(experiments)*I),ncol = length(experiments),nrow = I)
colnames(placeholder) <- experiments

placeholder_vector <- rep(NA,length(experiments))
names(placeholder_vector) <- experiments


loop_placeholder <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(loop_placeholder) <- paste("item",1:I,sep="")

loop_placeholder_vector <- rep(NA,loopn)


est.param <-list(
  a   = list(mean = placeholder,  sd= placeholder),
  b   = list(mean = placeholder, sd= placeholder),
  d   = list(mean = placeholder, sd =placeholder),
  lambda = list(mean = placeholder, sd =placeholder),
  tau = list(mean = placeholder, sd =placeholder),
  nu = list(mean = placeholder, sd =placeholder),
  psi = list(mean = placeholder_vector,  sd= placeholder_vector),
  alpha = list(mean = placeholder_vector,  sd= placeholder_vector),
  eta = list(sim = list(mean = placeholder_vector, sd = placeholder_vector),
             ebm = list(mean = placeholder_vector, sd = placeholder_vector),
             ml  = list(mean = placeholder_vector, sd = placeholder_vector)),
  fit = list(tli = placeholder_vector,cfi = placeholder_vector,rmsea = placeholder_vector, time=placeholder_vector),
  param = list(N = N, loopn = loopn, item_n = I)
)


est.param2 <-est.param

est.param$a$mean[,"sim"]<-a
est.param$b$mean[,"sim"]<-b
est.param$d$mean[,"sim"]<-d


loop_tmp.init <-list(
  a   = loop_placeholder,
  b   = loop_placeholder,
  d   = loop_placeholder,
  lambda = loop_placeholder,
  tau = loop_placeholder,
  nu = loop_placeholder,
  psi = loop_placeholder_vector,
  alpha = loop_placeholder_vector,
  eta = list(ebm = list(mean = loop_placeholder_vector,  sd= loop_placeholder_vector),
             ml = list(mean = loop_placeholder_vector,  sd= loop_placeholder_vector)),
  fit = list(tli = loop_placeholder_vector,
             cfi = loop_placeholder_vector,
             rmsea = loop_placeholder_vector,
             time = loop_placeholder_vector)
)



Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )

head(Eta)
cor(Eta)
var(Eta)
dim(Eta)

mean(Eta[,1])
mean(Eta[,2])
mean(Eta[,3])

var(Eta[,1])
var(Eta[,2])
var(Eta[,3])


dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,1],ncol=1,nrow = length(Eta[,1])))
dat.0p5 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,2],ncol=1,nrow = length(Eta[,2])))
dat.1p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,3],ncol=1,nrow = length(Eta[,3])))


dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,2])))
itemnames <- paste("item",1:I,"_",1,sep="")
colnames(dat) <-     itemnames
str(dat)


#expected thresholds
prop <- colSums(dat)/nrow(dat)
prop
th <- qnorm(1-prop)
th

mirt.estimations<- function(){}


loop_tmp <- loop_tmp.init

for(i in 1:loopn){
  Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
  dat <- dat.0p0
  colnames(dat) <- paste("item",1:I,sep="")
  mod<-mirt(data=dat,model = 1,itemtype = "2PL")
  M2(mod)
  itemfit(mod)
  coef(mod, simplify=TRUE)
  loop_tmp$a[i,] <- coef(mod, simplify=TRUE)$item[,"a1"]
  loop_tmp$d[i,] <- coef(mod, simplify=TRUE)$item[,"d"]
  loop_tmp$b[i,] <- -coef_d[i,]/coef_a[i,]
}


est.param$a$mean[,"mirt"] <-colMeans(loop_tmp$a,na.rm=TRUE)
est.param$b$mean[,"mirt"] <-colMeans(loop_tmp$b,na.rm=TRUE)
est.param$d$mean[,"mirt"] <-colMeans(loop_tmp$d,na.rm=TRUE)

est.param$a$sd[,"mirt"] <-apply(loop_tmp$a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
est.param$b$sd[,"mirt"] <-apply(loop_tmp$b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
est.param$d$sd[,"mirt"] <-apply(loop_tmp$d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})

est.param$a$mean
est.param$b$mean

# Specify Model Parameters for numerical itens

# mod.fixed_factor.delta_marginal.simple<- function(){}
# 
# 
# mod.fixed_factor.delta_marginal <- '
# 
# eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1
# 
# '
# itemnames <- c( "item1_1","item2_1","item3_1","item4_1","item5_1","item6_1","item7_1","item8_1","item9_1","item10_1")
# colnames(dat)<- itemnames
# 
# sem.model <- mod.fixed_factor.delta_marginal
# 
# remove_var(mod.fit)
# 
# start_time <- get_time()
# mod.fit <- lavaan(model = sem.model, 
#                           data = dat, 
#                           std.lv = TRUE,
#                           int.ov.free = TRUE,
#                           int.lv.free = FALSE,
#                           meanstructure =FALSE,
#                           auto.fix.first = FALSE,
#                           auto.var = TRUE,
#                           auto.th = TRUE,
#                           auto.delta = TRUE,
#                           auto.cov.y = TRUE,
#                           ordered = itemnames,
#                           parameterization = "delta")
# 
# print(get_delta_time(start_time))
# 
# summary(mod.fit)
# fitMeasures(mod.fit)[c("npar","df",'tli',"cfi","rmsea")]
# 
# parTable(mod.fit)
# lavInspect(mod.fit,what = "partable")
# lavInspect(mod.fit,what = "est")
# lavInspect(mod.fit,what = "start")
# vcov(mod.fit)
# 
# #check y* stats
# lavInspect(mod.fit,what = "mu")
# lavInspect(mod.fit,what = "vy")
# 
# 
# 
# coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
# colnames(coef_a) <- paste("item",1:I,sep="")
# coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
# colnames(coef_d) <- paste("item",1:I,sep="")
# for(i in 1:loopn){
#   Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
#   dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
#   dat <- dat.0p0
#   colnames(dat)<- itemnames
#   
#   mod.fixed_factor.delta_marginal <- '
# 
#   eta1_1=~l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1
# 
#   '
#   sem.model <- mod.fixed_factor.delta_marginal
#   
#   remove_var(mod.fit)
#   
#   mod.fit <- lavaan(model = sem.model, 
#                             data = dat, 
#                             std.lv = TRUE,
#                             int.ov.free = TRUE,
#                             int.lv.free = FALSE,
#                             meanstructure =FALSE,
#                             auto.fix.first = FALSE,
#                             auto.var = TRUE,
#                             auto.th = TRUE,
#                             auto.delta = TRUE,
#                             auto.cov.y = TRUE,
#                             ordered = itemnames,
#                             parameterization = "delta")
#   
# 
#   for(j in (1:I)){
#     lambda <- lavInspect(mod.fit,what = "est")$lambda[j]
#     psi <- lavInspect(mod.fit,what = "est")$psi
#     tau <- lavInspect(mod.fit,what = "est")$tau[j]
#     mu <- lavInspect(mod.fit,what = "mu")[j]
#     
#     coef_a[i,j] <- lambda/sqrt(1-lambda^2)*1.7
#     coef_d[i,j] <-  -tau/sqrt(1-lambda^2)*1.7
#   }
#   
# }
# coef_a_mean[,"ff_dm_yt"]<-colMeans(coef_a,na.rm=TRUE)
# coef_d_mean[,"ff_dm_yt"]<-colMeans(coef_d,na.rm=TRUE)
# coef_a_mean
# coef_d_mean


mod.fixed_factor.delta_marginal.ystar_thre_free <- function(){}


sem.model <- mod.fixed_factor.delta_marginal.ystar_thre_free
remove_var(mod.fit)
dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
colnames(dat)<- itemnames
start_time <- get_time()
mod.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = TRUE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = itemnames,
                          parameterization = "delta")
print(get_delta_time(start_time))

scores.ebm <- lavPredict(mod.fit, method = "EBM")
scores.ml <- lavPredict(mod.fit, method = "ML")

summary(mod.fit)
summary(mod.fit,fit.measures=TRUE)

fitMeasures(mod.fit)[c("npar","df",'tli',"cfi","rmsea")]

parTable(mod.fit)
lavInspect(mod.fit,what = "free")
lavInspect(mod.fit,what = "partable")
lavInspect(mod.fit,what = "est")
lavInspect(mod.fit,what = "start")
vcov(mod.fit)

#check y* stats
lavInspect(mod.fit,what = "mu")
lavInspect(mod.fit,what = "vy")

loop_tmp <-loop_tmp.init
loop_tmp2 <- loop_tmp.init

for(i in 1:loopn){
  Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
  colnames(dat)<- itemnames
  sem.model <- mod.fixed_factor.delta_marginal.ystar_thre_free
  remove_var(mod.fit)
  start_time <- get_time()
  mod.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = TRUE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = itemnames,
                            parameterization = "delta")
  
  scores.ebm <- lavPredict(mod.fit, method = "EBM")
  scores.ml <- lavPredict(mod.fit, method = "ML")
  
  time <- get_delta_time(start_time)

  loop_tmp$eta$ebm$mean[i] <-mean(scores.ebm)
  loop_tmp$eta$ebm$sd[i] <-sd(scores.ebm)
  loop_tmp$eta$ml$mean[i] <-mean(scores.ml)
  loop_tmp$eta$ml$sd[i] <-sd(scores.ml)
  loop_tmp$fit$time[i] <-time
  loop_tmp$fit$tli[i] <-fitMeasures(mod.fit)['tli']
  loop_tmp$fit$cli[i] <-fitMeasures(mod.fit)["cfi"]
  loop_tmp$fit$rmsea[i] <-fitMeasures(mod.fit)["rmsea"]
  
  
  psi <- lavInspect(mod.fit,what = "est")$psi[1,1]
  alpha <- lavInspect(mod.fit,what = "est")$alpha[1,1]
  for(j in (1:I)){
    lambda <- lavInspect(mod.fit,what = "est")$lambda[j]
    tau <- lavInspect(mod.fit,what = "est")$tau[j]
    nu <- lavInspect(mod.fit,what = "est")$nu[j]
    mu <- lavInspect(mod.fit,what = "mu")[j]
    
    
    loop_tmp2$a[i,j] <- lambda/sqrt(1-lambda^2)*1.7
    loop_tmp2$d[i,j] <-  -tau/sqrt(1-lambda^2)*1.7
    loop_tmp2$b[i,j] <- -coef_d[i,j]/coef_a[i,j]

    tmp<-irt_param_calc(lambda=lambda,
                   nu=nu,
                   tau=tau,
                   alpha=alpha,
                   psi = psi)
    loop_tmp$a[i,j] <- tmp["a"]
    loop_tmp$d[i,j] <- tmp["d"]
    loop_tmp$b[i,j] <- tmp["b"]
    loop_tmp$lambda[i,j] <- lambda
    loop_tmp$tau[i,j] <- tau
    loop_tmp$nu[i,j] <- nu
    loop_tmp$alpha <- alpha
    loop_tmp$psi <- psi
    
  } #for(j in (1:I))
} #for(i in 1:loopn)



working <- function(){}

loop_tmp <- loop_tmp.init
for(i in 1:loopn){
  Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
  colnames(dat)<- itemnames
  sem.model <- mod.fixed_factor.delta_marginal.ystar_thre_free
  remove_var(mod.fit)
  start_time <- get_time()
  mod.fit <- lavaan(model = sem.model, 
                    data = dat, 
                    std.lv = TRUE,
                    int.ov.free = TRUE,
                    int.lv.free = FALSE,
                    meanstructure =TRUE,
                    auto.fix.first = FALSE,
                    auto.var = TRUE,
                    auto.th = TRUE,
                    auto.delta = TRUE,
                    auto.cov.y = TRUE,
                    ordered = itemnames,
                    parameterization = "delta")
  
  scores.ebm <- lavPredict(mod.fit, method = "EBM")
  scores.ml <- lavPredict(mod.fit, method = "ML")
  
  time <- get_delta_time(start_time)
  
  loop_tmp$eta$sim$mean[i] <-mean(Eta[,y_star_mean])
  loop_tmp$eta$sim$sd[i]   <-sd(Eta[,y_star_mean])
  loop_tmp$eta$ebm$mean[i] <-mean(scores.ebm)
  loop_tmp$eta$ebm$sd[i] <-sd(scores.ebm)
  loop_tmp$eta$ml$mean[i] <-mean(scores.ml)
  loop_tmp$eta$ml$sd[i] <-sd(scores.ml)
  loop_tmp$fit$time[i] <-time
  loop_tmp$fit$tli[i] <-fitMeasures(mod.fit)["tli"]
  loop_tmp$fit$cfi[i] <-fitMeasures(mod.fit)["cfi"]
  loop_tmp$fit$rmsea[i] <-fitMeasures(mod.fit)["rmsea"]
  loop_tmp$alpha[i] <- lavInspect(mod.fit,what = "est")$alpha[latent_var_index,latent_var_index]
  loop_tmp$psi[i] <-lavInspect(mod.fit,what = "est")$psi[latent_var_index,latent_var_index]
  
  for(j in (1:I)){
    lavaan_param_vector <- get_lavaan_param(mod.fit=mod.fit,indicator_index = j)
    
    tmp<-irt_param_calc(lambda=lavaan_param_vector["lambda"],
                        nu=lavaan_param_vector["nu"],
                        tau=lavaan_param_vector["tau"],
                        alpha=loop_tmp$alpha[i],
                        psi = loop_tmp$psi[i],
                        parameterization = "ff_dm")
    loop_tmp$a[i,j] <- tmp[1]
    loop_tmp$d[i,j] <- tmp[3]
    loop_tmp$b[i,j] <- tmp[2]
    loop_tmp$lambda[i,j] <- lavaan_param_vector["lambda"]
    loop_tmp$tau[i,j] <- lavaan_param_vector["tau"]
    loop_tmp$nu[i,j] <- lavaan_param_vector["nu"]
    
  } #for(j in (1:I))
} #for(i in 1:loopn)

est.param <- aggregate_results(loop_tmp = loop_tmp,est.param=est.param,parameterization = "ff_dm_yt")
write_to_excel(est.param)

est.param$a$mean
est.param$b$mean


mod.fixed_factor.delta_marginal.ystar_mean_free<- function(){}



sem.model <- mod.fixed_factor.delta_marginal.ystar_mean_free
remove_var(mod.fit)
colnames(dat)<- itemnames
start_time <- get_time()
mod.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = TRUE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = itemnames,
                          parameterization = "delta")
print(get_delta_time(start_time))

summary(mod.fit)
fitMeasures(mod.fit)[c("npar","df",'tli',"cfi","rmsea")]

parTable(mod.fit)
lavInspect(mod.fit,what = "free")
lavInspect(mod.fit,what = "partable")
lavInspect(mod.fit,what = "est")
lavInspect(mod.fit,what = "start")
vcov(mod.fit)

#check y* stats
lavInspect(mod.fit,what = "mu")
lavInspect(mod.fit,what = "vy")



coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_b <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_b) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
  colnames(dat)<- itemnames
  sem.model <- mod.fixed_factor.delta_marginal.ystar_mean_free
  remove_var(mod.fit)
  mod.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = TRUE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = itemnames,
                            parameterization = "delta")
  
  
  for(j in (1:I)){
    lambda <- lavInspect(mod.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod.fit,what = "est")$psi
    tau <- lavInspect(mod.fit,what = "est")$tau[j]
    nu <- lavInspect(mod.fit,what = "est")$nu[j]
    mu <- lavInspect(mod.fit,what = "mu")[j]

    coef_a[i,j] <- lambda/sqrt(1-lambda^2)*1.7
    coef_d[i,j] <-  (nu-tau)/sqrt(1-lambda^2)*1.7
    coef_b[i,j] <- -coef_d[i,j]/coef_a[i,j]
  }
  
}

coef_a_mean[,"ie_dm_yt"]<-colMeans(coef_a,na.rm=TRUE)
coef_b_mean[,"ie_dm_yt"]<-colMeans(coef_b,na.rm=TRUE)
coef_d_mean[,"ie_dm_yt"]<-colMeans(coef_d,na.rm=TRUE)

coef_a_sd[,"ie_dm_yt"]<- apply(coef_a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_b_sd[,"ie_dm_yt"]<- apply(coef_b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_d_sd[,"ie_dm_yt"]<- apply(coef_d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})

coef_a_mean
coef_b_mean
coef_d_mean

write.table(coef_a_mean, 'coef_a_mean_ff_dm.txt')
write.table(coef_b_mean, 'coef_b_mean_ff_dm.txt')
write.table(coef_d_mean, 'coef_d_mean_ff_dm.txt')

write.table(coef_a_sd, 'coef_a_sd_ff_dm.txt')
write.table(coef_b_sd, 'coef_b_sd_ff_dm.txt')
write.table(coef_d_sd, 'coef_d_sd_ff_dm.txt')




mod.fixed_factor.theta_conditional.ystar_thre_free<- function(){}


sem.model <- mod.fixed_factor.theta_conditional.ystar_thre_free
remove_var(mod.fit)
colnames(dat)<- itemnames
start_time <- get_time()
mod.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = TRUE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = itemnames,
                          parameterization = "theta")
print(get_delta_time(start_time))

summary(mod.fit)
fitMeasures(mod.fit)[c("df",'tli',"cfi","rmsea")]


coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_b <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_b) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
  colnames(dat) <- itemnames
  sem.model <- mod.fixed_factor.delta_marginal.ystar_thre_free
  remove_var(mod.fit)
  mod.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = TRUE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = itemnames,
                            parameterization = "theta")
  
  
  for(j in (1:I)){
    lambda <- lavInspect(mod.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod.fit,what = "est")$psi
    tau <- lavInspect(mod.fit,what = "est")$tau[j]
    mu <- lavInspect(mod.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*1.7
    coef_d[i,j] <-  -(tau)*1.7
    coef_b[i,j] <- -coef_d[i,j]/coef_a[i,j]
  }
  
}

coef_a_mean[,"ff_tc_yt"]<-colMeans(coef_a,na.rm=TRUE)
coef_b_mean[,"ff_tc_yt"]<-colMeans(coef_b,na.rm=TRUE)
coef_d_mean[,"ff_tc_yt"]<-colMeans(coef_d,na.rm=TRUE)

coef_a_sd[,"ff_tc_yt"]<- apply(coef_a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_b_sd[,"ff_tc_yt"]<- apply(coef_b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_d_sd[,"ff_tc_yt"]<- apply(coef_d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})

coef_a_mean
coef_b_mean
coef_d_mean

write.table(coef_a_mean, 'coef_a_mean_ff_tc.txt')
write.table(coef_b_mean, 'coef_b_mean_ff_tc.txt')
write.table(coef_d_mean, 'coef_d_mean_ff_tc.txt')

write.table(coef_a_sd, 'coef_a_sd_ff_tc.txt')
write.table(coef_b_sd, 'coef_b_sd_ff_tc.txt')
write.table(coef_d_sd, 'coef_d_sd_ff_tc.txt')

mod.indicator_effects.delta_marginal.ystar_thre_free<- function(){}


sem.model <- mod.indicator_effects.delta_marginal.ystar_thre_free
remove_var(mod.fit)
start_time <- get_time()
mod.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = FALSE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = itemnames,
                          parameterization = "delta")
print(get_delta_time(start_time))


summary(mod.fit)
fitMeasures(mod.fit)[c("df",'tli',"cfi","rmsea")]

parTable(mod.fit)
lavInspect(mod.fit,what = "free")
lavInspect(mod.fit,what = "partable")
lavInspect(mod.fit,what = "est")
lavInspect(mod.fit,what = "start")
vcov(mod.fit)

#check y* stats
lavInspect(mod.fit,what = "mu")
lavInspect(mod.fit,what = "vy")

coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_b <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_b) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
  colnames(dat) <- itemnames
  sem.model <- mod.indicator_effects.delta_marginal.ystar_thre_free
  remove_var(mod.fit)
  mod.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = FALSE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = itemnames,
                            parameterization = "delta")
  
  
  for(j in (1:I)){
    lambda <- lavInspect(mod.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod.fit,what = "est")$psi
    tau <- lavInspect(mod.fit,what = "est")$tau[j]
    mu <- lavInspect(mod.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*sqrt(psi)*1.7 / sqrt( 1 - psi*lambda^2)
    coef_d[i,j] <-   -(tau - lambda*mu)*1.7 / sqrt( 1 - psi*lambda^2)
    coef_b[i,j] <- -coef_d[i,j]/coef_a[i,j]
  }
  
}

coef_a_mean[,"ie_dm_yt"]<-colMeans(coef_a,na.rm=TRUE)
coef_b_mean[,"ie_dm_yt"]<-colMeans(coef_b,na.rm=TRUE)
coef_d_mean[,"ie_dm_yt"]<-colMeans(coef_d,na.rm=TRUE)

coef_a_sd[,"ie_dm_yt"]<- apply(coef_a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_b_sd[,"ie_dm_yt"]<- apply(coef_b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_d_sd[,"ie_dm_yt"]<- apply(coef_d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})

coef_a_mean
coef_b_mean
coef_d_mean

write.table(coef_a_mean, 'coef_a_mean_ie_dm.txt')
write.table(coef_b_mean, 'coef_b_mean_ie_dm.txt')
write.table(coef_d_mean, 'coef_d_mean_ie_dm.txt')

write.table(coef_a_sd, 'coef_a_sd_ie_dm.txt')
write.table(coef_b_sd, 'coef_b_sd_ie_dm.txt')
write.table(coef_d_sd, 'coef_d_sd_ie_dm.txt')




mod.indicator_effects.theta_conditional.ystar_thre_free<- function(){}


sem.model <- mod.indicator_effects.theta_conditional.ystar_thre_free
remove_var(mod.fit)
start_time <- get_time()
mod.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = FALSE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = itemnames,
                          parameterization = "theta")
print(get_delta_time(start_time))

summary(mod.fit)
fitMeasures(mod.fit)[c("df",'tli',"cfi","rmsea")]

parTable(mod.fit)
lavInspect(mod.fit,what = "free")
lavInspect(mod.fit,what = "partable")
lavInspect(mod.fit,what = "est")
lavInspect(mod.fit,what = "start")
vcov(mod.fit)

#check y* stats
lavInspect(mod.fit,what = "mu")
lavInspect(mod.fit,what = "vy")

coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_b <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_b) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
  colnames(dat) <- itemnames
  sem.model <- mod.indicator_effects.theta_conditional.ystar_thre_free
  remove_var(mod.fit)
  mod.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = FALSE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = itemnames,
                            parameterization = "theta")
  for(j in (1:I)){
    lambda <- lavInspect(mod.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod.fit,what = "est")$psi
    tau <- lavInspect(mod.fit,what = "est")$tau[j]
    mu <- lavInspect(mod.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*sqrt(psi)*1.7
    coef_d[i,j] <- -(tau - lambda*mu)*1.7
    coef_b[i,j] <- -coef_d[i,j]/coef_a[i,j]
  }
  
}

coef_a_mean[,"ie_tc_yt"]<-colMeans(coef_a,na.rm=TRUE)
coef_b_mean[,"ie_tc_yt"]<-colMeans(coef_b,na.rm=TRUE)
coef_d_mean[,"ie_tc_yt"]<-colMeans(coef_d,na.rm=TRUE)

coef_a_sd[,"ie_tc_yt"]<- apply(coef_a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_b_sd[,"ie_tc_yt"]<- apply(coef_b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_d_sd[,"ie_tc_yt"]<- apply(coef_d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})

coef_a_mean
coef_b_mean
coef_d_mean

write.table(coef_a_mean, 'coef_a_mean_ie_tc.txt')
write.table(coef_b_mean, 'coef_b_mean_ie_tc.txt')
write.table(coef_d_mean, 'coef_d_mean_ie_tc.txt')

write.table(coef_a_sd, 'coef_a_sd_ie_tc.txt')
write.table(coef_b_sd, 'coef_b_sd_ie_tc.txt')
write.table(coef_d_sd, 'coef_d_sd_ie_tc.txt')



mod.indicator_marker.delta_marginal.ystar_thre_free<- function(){}


sem.model <- mod.indicator_marker.delta_marginal.ystar_thre_free
remove_var(mod.fit)
start_time <- get_time()
mod.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = FALSE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = itemnames,
                          parameterization = "delta")
print(get_delta_time(start_time))

summary(mod.fit)
fitMeasures(mod.fit)[c("df",'tli',"cfi","rmsea")]

parTable(mod.fit)
lavInspect(mod.fit,what = "free")
lavInspect(mod.fit,what = "partable")
lavInspect(mod.fit,what = "est")
lavInspect(mod.fit,what = "start")
vcov(mod.fit)

#check y* stats
lavInspect(mod.fit,what = "mu")
lavInspect(mod.fit,what = "vy")

coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_b <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_b) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
  colnames(dat) <- itemnames
  sem.model <- mod.indicator_marker.delta_marginal.ystar_thre_free
  remove_var(mod.fit)
  mod.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = FALSE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = itemnames,
                            parameterization = "delta")
  for(j in (1:I)){
    lambda <- lavInspect(mod.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod.fit,what = "est")$psi
    tau <- lavInspect(mod.fit,what = "est")$tau[j]
    mu <- lavInspect(mod.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*sqrt(psi)*1.7
    coef_d[i,j] <- -(tau - lambda*mu)*1.7
    coef_b[i,j] <- -coef_d[i,j]/coef_a[i,j]
    
  }
  
}

coef_a_mean[,"im_dm_yt"]<-colMeans(coef_a,na.rm=TRUE)
coef_b_mean[,"im_dm_yt"]<-colMeans(coef_b,na.rm=TRUE)
coef_d_mean[,"im_dm_yt"]<-colMeans(coef_d,na.rm=TRUE)

coef_a_sd[,"im_dm_yt"]<- apply(coef_a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_b_sd[,"im_dm_yt"]<- apply(coef_b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_d_sd[,"im_dm_yt"]<- apply(coef_d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})

coef_a_mean
coef_b_mean
coef_d_mean

write.table(coef_a_mean, 'coef_a_mean_im_dm.txt')
write.table(coef_b_mean, 'coef_b_mean_im_dm.txt')
write.table(coef_d_mean, 'coef_d_mean_im_dm.txt')

write.table(coef_a_sd, 'coef_a_sd_im_dm.txt')
write.table(coef_b_sd, 'coef_b_sd_im_dm.txt')
write.table(coef_d_sd, 'coef_d_sd_im_dm.txt')

mod.indicator_marker.theta_conditional.ystar_thre_free<- function(){}

sem.model <- mod.indicator_marker.theta_conditional.ystar_thre_free
remove_var(mod.fit)
start_time <- get_time()
mod.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = FALSE,
                          int.ov.free = TRUE,
                          int.lv.free = FALSE,
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = itemnames,
                          parameterization = "theta")
print(get_delta_time(start_time))

summary(mod.fit)
fitMeasures(mod.fit)[c("df",'tli',"cfi","rmsea")]

parTable(mod.fit)
lavInspect(mod.fit,what = "free")
lavInspect(mod.fit,what = "partable")
lavInspect(mod.fit,what = "est")
lavInspect(mod.fit,what = "start")
vcov(mod.fit)

#check y* stats
lavInspect(mod.fit,what = "mu")
lavInspect(mod.fit,what = "vy")

coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:I,sep="")
coef_b <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_b) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:I,sep="")
for(i in 1:loopn){
  Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
  colnames(dat) <- itemnames
  sem.model <- mod.indicator_marker.theta_conditional.ystar_thre_free
  remove_var(mod.fit)
  mod.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = FALSE,
                            int.ov.free = TRUE,
                            int.lv.free = FALSE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = itemnames,
                            parameterization = "theta")
  for(j in (1:I)){
    lambda <- lavInspect(mod.fit,what = "est")$lambda[j]
    psi <- lavInspect(mod.fit,what = "est")$psi
    tau <- lavInspect(mod.fit,what = "est")$tau[j]
    mu <- lavInspect(mod.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*sqrt(psi)*1.7
    coef_d[i,j] <- -(tau - lambda*mu)*1.7
    coef_b[i,j] <- -coef_d[i,j]/coef_a[i,j]
    
  }
  
}

coef_a_mean[,"im_tc_yt"]<-colMeans(coef_a,na.rm=TRUE)
coef_b_mean[,"im_tc_yt"]<-colMeans(coef_b,na.rm=TRUE)
coef_d_mean[,"im_tc_yt"]<-colMeans(coef_d,na.rm=TRUE)

coef_a_sd[,"im_tc_yt"]<- apply(coef_a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_b_sd[,"im_tc_yt"]<- apply(coef_b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
coef_d_sd[,"im_tc_yt"]<- apply(coef_d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})

coef_a_mean
coef_b_mean
coef_d_mean

write.table(coef_a_mean, 'coef_a_mean_im_tc.txt')
write.table(coef_b_mean, 'coef_b_mean_im_tc.txt')
write.table(coef_d_mean, 'coef_d_mean_im_tc.txt')

write.table(coef_a_sd, 'coef_a_sd_im_tc.txt')
write.table(coef_b_sd, 'coef_b_sd_im_tc.txt')
write.table(coef_d_sd, 'coef_d_sd_im_tc.txt')


write.xlsx(coef_a_sd, "parType4_N10000_loopn1000_resultados_final.xlsx", sheetName = "a_sd", col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(coef_b_sd, "parType4_N10000_loopn1000_resultados_final.xlsx", sheetName = "b_sd", col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(coef_d_sd, "parType4_N10000_loopn1000_resultados_final.xlsx", sheetName = "d_sd", col.names = TRUE, row.names = TRUE, append = TRUE)

write.xlsx(coef_a_mean, "parType4_N10000_loopn1000_resultados_final.xlsx", sheetName = "a_mean", col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(coef_b_mean, "parType4_N10000_loopn1000_resultados_final.xlsx", sheetName = "b_mean", col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(coef_d_mean, "parType4_N10000_loopn1000_resultados_final.xlsx", sheetName = "d_mean", col.names = TRUE, row.names = TRUE, append = TRUE)


mod.long.indicator_effects.delta_marginal<- function(){}

mod.long.indicator_effects.delta_marginal <- '

lv1 =~ l1*item1 + l2*item2 + l3*item3 
lv2 =~ l4*item4 + l5*item5 + l6*item6 
l1 + l2 + l3 == 3 


lv1 ~ lv1mean*1
lv2 ~ lv2mean*1

lv1 ~~ lv1var*lv1
lv2 ~~ lv2var*lv2

lv2~lv1


## LIR means
item1 ~ int1*1
item2 ~ int2*1
item3 ~ int3*1

item4 ~ int4*1
item5 ~ int5*1
item6 ~ int6*1

#int1+int2+int3==0

int1+l1*lv1mean==0
int2+l2*lv1mean==0
int3+l3*lv1mean==0

int4+l4*lv2mean==0
int5+l5*lv2mean==0
int6+l6*lv2mean==0



## thresholds link LIRs to observed items
item1 | thr1*t1
item2 | thr2*t1
item3 | thr3*t1

item4 | thr4*t1
item5 | thr5*t1
item6 | thr6*t1


## LIR (co)variances
item1 ~~ var1*item1 +    0*item2 +    0*item3 + var14*item4 +     0*item5 +     0*item6
item2 ~~              var2*item2 +    0*item3 +     0*item4 + var25*item5 +     0*item6
item3 ~~                           var3*item3 +     0*item4 +     0*item5 + var36*item6
item4 ~~                                         var4*item4 +     0*item5 +     0*item6
item5 ~~                                                       var5*item5 +     0*item6
item6 ~~                                                                     var6*item6

lv1var*l1^2 + var1 == 1
lv1var*l2^2 + var2 == 1
lv1var*l3^2 + var3 == 1

lv2var*l4^2 + var4 == 1
lv2var*l5^2 + var5 == 1
lv2var*l6^2 + var6 == 1


l1==l4
l2==l5
l3==l6

int1==int4
int2==int5
int3==int6

thr1==thr4
thr2==thr5
thr3==thr6

item1mean := int1+l1*lv1mean
item2mean := int2+l2*lv1mean
item3mean := int3+l3*lv1mean

item4mean := int1+l4*lv2mean
item5mean := int2+l5*lv2mean
item6mean := int3+l6*lv2mean


item1var := lv1var*l1^2 + var1 
item2var := lv1var*l2^2 + var2 
item3var := lv1var*l3^2 + var3 

item4var := lv2var*l4^2 + var4 
item5var := lv2var*l5^2 + var5 
item6var := lv2var*l6^2 + var6 



'

dat<- cbind(dat.0p0[,1:3],dat.0p5[,1:3])
head(dat)
colnames(dat)<- paste("item",1:6,sep="")
itemnames <- paste("item",1:6,sep="")

sem.model <- mod.long.indicator_effects.delta_marginal
remove_var(mod.fit)
mod.fit <- lavaan(model = sem.model, 
                          data = dat, 
                          std.lv = FALSE,
                          int.ov.free = TRUE,
                          int.lv.free = TRUE, #<- set to TRUE for longitudinal models
                          meanstructure =TRUE,
                          auto.fix.first = FALSE,
                          auto.var = TRUE,
                          auto.th = TRUE,
                          auto.delta = TRUE,
                          auto.cov.y = TRUE,
                          ordered = itemnames,
                          parameterization = "delta")


summary(mod.fit)
fitMeasures(mod.fit)[c("df",'tli',"cfi","rmsea")]

parTable(mod.fit)
lavInspect(mod.fit,what = "free")
lavInspect(mod.fit,what = "partable")
lavInspect(mod.fit,what = "est")
lavInspect(mod.fit,what = "start")
vcov(mod.fit)

#check y* stats
lavInspect(mod.fit,what = "mu")
lavInspect(mod.fit,what = "vy")

#check eta stats
lavInspect(mod.fit,what = "est")$alpha
lavInspect(mod.fit,what = "est")$psi


coef_a <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_a) <- paste("item",1:6,sep="")
coef_b <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_b) <- paste("item",1:I,sep="")
coef_d <- matrix(data=NA, nrow = loopn, ncol = I)
colnames(coef_d) <- paste("item",1:6,sep="")

for(i in 1:loopn){
  Eta <- mvrnorm(n=N, mu=c(0,0.5,1), Sigma )
  dat.0p0 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,1],ncol=1,nrow = length(Eta[,1])))
  dat.0p5 =simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,2],ncol=1,nrow = length(Eta[,2])))
  dat<- cbind(dat.0p0[,1:3],dat.0p5[,1:3])
  head(dat)
  colnames(dat)<- paste("item",1:6,sep="")
  itemnames <- paste("item",1:6,sep="")
  remove_var(mod.fit)
  mod.fit <- lavaan(model = sem.model, 
                            data = dat, 
                            std.lv = FALSE,
                            int.ov.free = TRUE,
                            int.lv.free = TRUE,
                            meanstructure =TRUE,
                            auto.fix.first = FALSE,
                            auto.var = TRUE,
                            auto.th = TRUE,
                            auto.delta = TRUE,
                            auto.cov.y = TRUE,
                            ordered = itemnames,
                            parameterization = "delta")
  
  for(j in (1:6)){
    if(j<4){
      lambda <- lavInspect(mod.fit,what = "est")$lambda[j,1]
      psi <- lavInspect(mod.fit,what = "est")$psi[1,1]
    }else{
      lambda <- lavInspect(mod.fit,what = "est")$lambda[j,2]
      psi <- lavInspect(mod.fit,what = "est")$psi[2,2]
    }
    tau <- lavInspect(mod.fit,what = "est")$tau[j]
    mu <- lavInspect(mod.fit,what = "mu")[j]
    
    coef_a[i,j] <- lambda*sqrt(psi)*1.7 / sqrt( 1 - psi*lambda^2)
    coef_d[i,j] <-   -(tau - lambda*mu)*1.7 / sqrt( 1 - psi*lambda^2)
  }
  
}

along <-colMeans(coef_a,na.rm=TRUE)
dlong <-colMeans(coef_d,na.rm=TRUE)
along
c(a[1:3],a[1:3])
dlong
c(d[1:3],d[1:3])





