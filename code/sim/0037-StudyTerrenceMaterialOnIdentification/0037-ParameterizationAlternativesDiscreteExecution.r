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
    ff_dm = {lambda*1.7/sqrt(1-lambda^2)},
    ff_tc = {lambda*1.7},
    im_dm = {lambda*sqrt(psi)*1.7 / sqrt( 1 - psi*lambda^2)},
    im_tc = {lambda*sqrt(psi)*1.7},
    ie_dm = {lambda*sqrt(psi)*1.7 / sqrt( 1 - psi*lambda^2)},
    ie_tc = {lambda*sqrt(psi)*1.7}
  )
  d <- switch(parameterization,
    ff_dm = {(nu - tau)*1.7/sqrt(1-lambda^2)},
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
  est.param$mu$mean[,parameterization] <-colMeans(loop_tmp$mu,na.rm=TRUE)
  est.param$vy$mean[,parameterization] <-colMeans(loop_tmp$vy,na.rm=TRUE)
  est.param$theta$mean[,parameterization] <-colMeans(loop_tmp$theta,na.rm=TRUE)
  
  est.param$psi$mean[parameterization] <-mean(loop_tmp$psi,na.rm=TRUE)
  est.param$alpha$mean[parameterization] <-mean(loop_tmp$alpha,na.rm=TRUE)
  
  est.param$a$sd[,parameterization] <-apply(loop_tmp$a,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$b$sd[,parameterization] <-apply(loop_tmp$b,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$d$sd[,parameterization] <-apply(loop_tmp$d,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$lambda$sd[,parameterization] <-apply(loop_tmp$lambda,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$tau$sd[,parameterization] <-apply(loop_tmp$tau,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$nu$sd[,parameterization] <-apply(loop_tmp$nu,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$mu$sd[,parameterization] <-apply(loop_tmp$mu,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$vy$sd[,parameterization] <-apply(loop_tmp$vy,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  est.param$theta$sd[,parameterization] <-apply(loop_tmp$theta,MARGIN = 2, function(x){sd(x,na.rm = TRUE)})
  
  est.param$psi$sd[parameterization]   <- sd(loop_tmp$psi,na.rm = TRUE)
  est.param$alpha$sd[parameterization] <- sd(loop_tmp$alpha,na.rm = TRUE)
  
  est.param$eta$sim$mean[parameterization] <-mean(loop_tmp$eta$sim$mean,na.rm=TRUE)
  est.param$eta$ebm$mean[parameterization] <-mean(loop_tmp$eta$ebm$mean,na.rm=TRUE)
  est.param$eta$ml$mean[parameterization]  <-mean(loop_tmp$eta$ml$mean,na.rm=TRUE)
  
  est.param$eta$sim$sd[parameterization]   <-mean(loop_tmp$eta$sim$sd,na.rm=TRUE)
  est.param$eta$ebm$sd[parameterization]   <-mean(loop_tmp$eta$ebm$sd,na.rm=TRUE)
  est.param$eta$ml$sd[parameterization]    <-mean(loop_tmp$eta$ml$sd,na.rm=TRUE)
  
  est.param$fit$tli[parameterization]  <-mean(loop_tmp$fit$tli,na.rm=TRUE)
  est.param$fit$cfi[parameterization]  <-mean(loop_tmp$fit$cfi,na.rm=TRUE)
  est.param$fit$rmsea[parameterization]  <-mean(loop_tmp$fit$rmsea,na.rm=TRUE)
  est.param$fit$time[parameterization]  <-mean(loop_tmp$fit$time,na.rm=TRUE)
  return(est.param)
}


write_to_mean_sd <-function(param, param_text, filename){

  if(!is.vector(param$mean)){ 
    tmp <- rbind(cbind(rep("mean",nrow(param$mean)), param$mean), 
                 rep("",ncol(param$mean)+1),
                 cbind(rep("sd",nrow(param$sd)), param$sd),
                 rep("",ncol(param$mean)+1),
                 cbind(rep("res",nrow(param$sd)),param$mean-param$mean[,"sim"]) )
  }else{
    tmp <- rbind(c("mean", param$mean), 
                 rep("",length(param$mean)+1),
                 c("sd", param$sd))
  }
  write.xlsx(data.frame(tmp), filename, sheetName = param_text, col.names = TRUE, row.names = TRUE, append = TRUE)
}


write_to_excel <-function(est.param,y_star_mean,parType,N,loopn,I,SigmaType,iteration=42){
  filename <- build_filename(y_star_mean=y_star_mean,
                             parType=parType,
                             N=N,
                             loopn=loopn,
                             I=I,
                             SigmaType=SigmaType,
                             iteration=iteration)
  
  write.xlsx(data.frame(est.param$eta), filename, sheetName = "eta", col.names = TRUE, row.names = TRUE, append = TRUE)
  write.xlsx(data.frame(est.param$fit), filename, sheetName = "fit", col.names = TRUE, row.names = TRUE, append = TRUE)

  write_to_mean_sd(param = est.param$a,param_text="a",filename = filename)
  write_to_mean_sd(param = est.param$b,param_text="b",filename = filename)
  write_to_mean_sd(param = est.param$d,param_text="d",filename = filename)

  write_to_mean_sd(param = est.param$lambda,param_text="lambda",filename = filename)
  write_to_mean_sd(param = est.param$tau,param_text="tau",filename = filename)
  write_to_mean_sd(param = est.param$nu,param_text="nu",filename = filename)

  write_to_mean_sd(param = est.param$mu,param_text="mu",filename = filename)
  write_to_mean_sd(param = est.param$vy,param_text="vy",filename = filename)
  write_to_mean_sd(param = est.param$theta,param_text="theta",filename = filename)
  write_to_mean_sd(param = est.param$psi,param_text="psi",filename = filename)
  write_to_mean_sd(param = est.param$alpha,param_text="alpha",filename = filename)

}


write_to_excel_old <-function(est.param,y_star_mean,parType,N,loopn,I,SigmaType,iteration=42){
  filename <- build_filename(y_star_mean=y_star_mean,
                             parType=parType,
                             N=N,
                             loopn=loopn,
                             I=I,
                             SigmaType=SigmaType,
                             iteration=iteration)
  
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



get_lavaan_indicator_param <- function(mod.fit,indicator_index){
  lambda <- lavInspect(mod.fit,what = "est")$lambda[indicator_index]
  tau <- lavInspect(mod.fit,what = "est")$tau[indicator_index]
  nu <- lavInspect(mod.fit,what = "est")$nu[indicator_index]
  mu <- lavInspect(mod.fit,what = "mu")[indicator_index]
  vy <- lavInspect(mod.fit,what = "vy")[indicator_index]
  theta <- lavInspect(mod.fit,what = "est")$theta[indicator_index,indicator_index]
  
  tmp <-c(lambda, nu, tau, mu,vy,theta)
  names(tmp)<- c("lambda","nu","tau","mu","vy","theta")
  return(tmp)
}
  

build_filename<-function(y_star_mean,parType,N,loopn,I,SigmaType,iteration=42){
  mean_string <- switch(y_star_mean,
                        "Alpha0p0",
                        "Alpha0p5",
                        "Alpha1p0")
  if(iteration!=42){partial_results <- paste0("_partial",iteration)}else{partial_results<-"_complete"}
  
  return(paste0(mean_string,"_parType",parType,"_N",N,"_loopn",loopn,"_I",I,"_SigmaType",SigmaType,partial_results,".xlsx"))
  
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


threshold_calc <- function( vectorY){
  prop <- table(vectorY)/sum(table(vectorY))
  cprop <- c(0, cumsum(prop))
  th <- qnorm(cprop)
  return(th)
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

# Generate Longitudinal Items from a Bivariate LIR
data_creation <-function(){}

#carregando modelos
source("./models.r")

set.seed(12) # Resetando a semente
N <- 1500    ## subjects
#N <- 10000 ## subjects
#loopn <-500   ## number of runs

latent_var_index <- 1

I= 13  # Number of Items
PL=2 # Logistic Model (1,2,3 parameters)
SigmaType <- 1 # 0 = Covariance Uniform, 1 = Covariancia AR1, 2 =  Covariancia de bandas 3 = Covariancia Nula
rho<-0.7
y_star_mean <- 1 #'0p0'=1,'0p5'=2, '1p0'=3 
mu<-c(0,0.5,1)
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

# 1  fixed_factor,		  delta_marginal 		  ystar_thre_free  	<-	"ff_dm_yt"
# 2  fixed_factor,		  delta_marginal 		  ystar_mean_free  	<-	"ff_dm_ym"
# 3  fixed_factor, 		  theta_conditional	  ystar_thre_free 	<- 	"ff_tc_yt"
# 4  fixed_factor, 		  theta_conditional	  ystar_mean_free 	<- 	"ff_tc_ym"

# 5  indicator_marker, 	delta_marginal 		  ystar_thre_free 	<- 	"im_dm_yt"
# 6  indicator_marker, 	delta_marginal 		  ystar_mean_free 	<- 	"im_dm_ym"
# 7  indicator_marker, 	theta_conditional 	ystar_thre_free 	<-  "im_tc_yt"
# 8  indicator_marker, 	theta_conditional 	ystar_mean_free 	<-  "im_tc_ym"

# 9  indicator_effects, delta_marginal   	  ystar_thre_free 	<- 	"ie_dm_yt"
# 10 indicator_effects, delta_marginal   	  ystar_mean_free 	<- 	"ie_dm_ym"
# 11 indicator_effects, theta_conditional 	ystar_thre_free 	<-	"ie_tc_yt"
# 12 indicator_effects, theta_conditional 	ystar_mean_free 	<-	"ie_tc_ym"

# 13 fixed_factor,		  delta_marginal 		  ystar_mean_free  	<-	"ff_dm_ym_t0p5" threshold=0.5
# 14 fixed_factor, 		  theta_conditional	  ystar_mean_free 	<- 	"ff_tc_ym_t0p5" threshold=0.5
# 15 indicator_marker, 	delta_marginal 		  ystar_mean_free 	<- 	"im_dm_ym_t0p5" threshold=0.5
# 16 indicator_marker, 	theta_conditional 	ystar_mean_free 	<-  "im_tc_ym_t0p5" threshold=0.5
# 17 indicator_effects, delta_marginal   	  ystar_mean_free 	<- 	"ie_dm_ym_t0p5" threshold=0.5
# 18 indicator_effects, theta_conditional 	ystar_mean_free 	<-	"ie_tc_ym_t0p5" threshold=0.5


experiments <- c("sim", "mirt", "ff_dm_yt","ff_dm_ym","ff_tc_yt","ff_tc_ym",
                 "im_dm_yt","im_dm_ym","im_tc_yt","im_tc_ym",
                 "ie_dm_yt","ie_dm_ym","ie_tc_yt","ie_tc_ym",
                 "ff_dm_ym_t0p5","ff_tc_ym_t0p5",
                 "im_dm_ym_t0p5","im_tc_ym_t0p5",
                 "ie_dm_ym_t0p5","ie_tc_ym_t0p5")
names(experiments) <- c("sim","mirt",1:(length(experiments)-2))  

Eta <- mvrnorm(n=N, mu=mu, Sigma )

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






# 1  fixed_factor,		  delta_marginal 		  ystar_thre_free  	<-	"ff_dm_yt"
# 2  fixed_factor,		  delta_marginal 		  ystar_mean_free  	<-	"ff_dm_ym"
# 3  fixed_factor, 		  theta_conditional	  ystar_thre_free 	<- 	"ff_tc_yt"
# 4  fixed_factor, 		  theta_conditional	  ystar_mean_free 	<- 	"ff_tc_ym"

# 5  indicator_marker, 	delta_marginal 		  ystar_thre_free 	<- 	"im_dm_yt"
# 6  indicator_marker, 	delta_marginal 		  ystar_mean_free 	<- 	"im_dm_ym"
# 7  indicator_marker, 	theta_conditional 	ystar_thre_free 	<-  "im_tc_yt"
# 8  indicator_marker, 	theta_conditional 	ystar_mean_free 	<-  "im_tc_ym"

# 9  indicator_effects, delta_marginal   	  ystar_thre_free 	<- 	"ie_dm_yt"
# 10 indicator_effects, delta_marginal   	  ystar_mean_free 	<- 	"ie_dm_ym"
# 11 indicator_effects, theta_conditional 	ystar_thre_free 	<-	"ie_tc_yt"
# 12 indicator_effects, theta_conditional 	ystar_mean_free 	<-	"ie_tc_ym"

# 13 fixed_factor,		  delta_marginal 		  ystar_mean_free  	<-	"ff_dm_ym_t0p5" threshold=0.5
# 14 fixed_factor, 		  theta_conditional	  ystar_mean_free 	<- 	"ff_tc_ym_t0p5" threshold=0.5
# 15 indicator_marker, 	delta_marginal 		  ystar_mean_free 	<- 	"im_dm_ym_t0p5" threshold=0.5
# 16 indicator_marker, 	theta_conditional 	ystar_mean_free 	<-  "im_tc_ym_t0p5" threshold=0.5
# 17 indicator_effects, delta_marginal   	  ystar_mean_free 	<- 	"ie_dm_ym_t0p5" threshold=0.5
# 18 indicator_effects, theta_conditional 	ystar_mean_free 	<-	"ie_tc_ym_t0p5" threshold=0.5

working <- function(){}

itemnames <- paste("item",1:I,"_",1,sep="")

sim <- 13
    
switch (sim,
  {#1
    param_index <-"ff_dm_yt"
    calc_sel <- "ff_dm"
    std_lv <- TRUE
    delta_theta <- "delta"
    sem.model <- mod.fixed_factor.delta_marginal.ystar_thre_free
  },
  {#2
    param_index <-"ff_dm_ym"
    calc_sel <- "ff_dm"
    std_lv <- TRUE
    delta_theta <- "delta"
    sem.model <- mod.fixed_factor.delta_marginal.ystar_mean_free
  },
  {#3
    param_index <-"ff_tc_yt"
    calc_sel <- "ff_tc"
    std_lv <- TRUE
    delta_theta <- "theta"
    sem.model <- mod.fixed_factor.theta_conditional.ystar_thre_free
  },
  {#4
    param_index <-"ff_tc_ym"
    calc_sel <- "ff_tc"
    std_lv <- TRUE
    delta_theta <- "theta"
    sem.model <- mod.fixed_factor.theta_conditional.ystar_mean_free
  },
  {#5
    param_index <-"im_dm_yt"
    calc_sel <- "im_dm"
    std_lv <- FALSE
    delta_theta <- "delta"
    sem.model <- mod.indicator_marker.delta_marginal.ystar_thre_free
  },
  {#6
    param_index <-"im_dm_ym"
    calc_sel <- "im_dm"
    std_lv <- FALSE
    delta_theta <- "delta"
    sem.model <- mod.indicator_marker.delta_marginal.ystar_mean_free
  },
  {#7
    param_index <-"im_tc_yt"
    calc_sel <- "im_tc"
    std_lv <- FALSE
    delta_theta <- "theta"
    sem.model <- mod.indicator_marker.theta_conditional.ystar_thre_free
  },
  {#8
    param_index <-"im_tc_ym"
    calc_sel <- "im_tc"
    std_lv <- FALSE
    delta_theta <- "theta"
    sem.model <- mod.indicator_marker.theta_conditional.ystar_mean_free
  },
  {#9
    param_index <-"ie_dm_yt"
    calc_sel <- "ie_dm"
    std_lv <- FALSE
    delta_theta <- "delta"
    sem.model <- mod.indicator_effects.delta_marginal.ystar_thre_free
  },
  {#10
    param_index <-"ie_dm_ym"
    calc_sel <- "ie_dm"
    std_lv <- FALSE
    delta_theta <- "delta"
    sem.model <- mod.indicator_effects.delta_marginal.ystar_mean_free
  },
  {#11
    param_index <-"ie_tc_yt"
    calc_sel <- "ie_tc"
    std_lv <- FALSE
    delta_theta <- "theta"
    sem.model <- mod.indicator_effects.theta_conditional.ystar_thre_free
  },
  {#12
    param_index <-"ie_tc_ym"
    calc_sel <- "ie_tc"
    std_lv <- FALSE
    delta_theta <- "theta"
    sem.model <- mod.indicator_effects.theta_conditional.ystar_mean_free
  },
  {#13
    param_index <-"ff_dm_ym_t0p5"
    calc_sel <- "ff_dm"
    std_lv <- TRUE
    delta_theta <- "delta"
    sem.model <- mod.fixed_factor.delta_marginal.ystar_mean_free.threqu0p5
  },
  {#14
    param_index <-"ff_tc_ym_t0p5"
    calc_sel <- "ff_tc"
    std_lv <- TRUE
    delta_theta <- "theta"
    sem.model <- mod.fixed_factor.theta_conditional.ystar_mean_free.threqu0p5
  },
  {#15
    param_index <-"im_dm_ym_t0p5"
    calc_sel <- "im_dm"
    std_lv <- FALSE
    delta_theta <- "delta"
    sem.model <- mod.indicator_marker.delta_marginal.ystar_mean_free.threqu0p5
  },
  {#16
    param_index <-"im_tc_ym_t0p5"
    calc_sel <- "im_tc"
    std_lv <- FALSE
    delta_theta <- "theta"
    sem.model <- mod.indicator_marker.theta_conditional.ystar_mean_free.threqu0p5
  },
  {#17
    param_index <-"ie_dm_ym_t0p5"
    calc_sel <- "ie_dm"
    std_lv <- FALSE
    delta_theta <- "delta"
    sem.model <- mod.indicator_effects.delta_marginal.ystar_mean_free.threqu0p5
  },
  {#18
    param_index <-"ie_tc_ym_t0p5"
    calc_sel <- "ie_tc"
    std_lv <- FALSE
    delta_theta <- "theta"
    sem.model <- mod.indicator_marker.theta_conditional.ystar_mean_free.threqu0p5
  }
)#switch (sim,

  Eta <- mvrnorm(n=N, mu=mu, Sigma )
  dat <- simdata(a=a,d=d,N=N,itemtype = '2PL', Theta = matrix(Eta[,y_star_mean],ncol=1,nrow = length(Eta[,y_star_mean])))
  colnames(dat)<- itemnames
  remove_var(mod.fit)
  start_time <- get_time()
  mod.fit <- lavaan(model = sem.model, 
                    data = dat, 
                    std.lv = std_lv,
                    int.ov.free = TRUE,
                    int.lv.free = FALSE,
                    meanstructure =TRUE,
                    auto.fix.first = FALSE,
                    auto.var = TRUE,
                    auto.th = TRUE,
                    auto.delta = TRUE,
                    auto.cov.y = TRUE,
                    ordered = itemnames,
                    parameterization = delta_theta)
  
  #scores.ebm <- lavPredict(mod.fit, method = "EBM")
  #scores.ml <- lavPredict(mod.fit, method = "ML")
  time <- get_delta_time(start_time)
  
  fit.info <- function(){}
  
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
  lavInspect(mod.fit,what = "est")$nu
  lavInspect(mod.fit,what = "th") #model-implied *standardized* thresholds
  lavInspect(mod.fit,what = "est")$tau #model-implied *standardized* thresholds
  
 threshold_calc(dat[,1])


