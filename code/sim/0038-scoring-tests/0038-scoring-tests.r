
rm(list=ls())

key <- c("A","B","C","D","E","A","B","C","D","E")

valid_10_10 <- rep(TRUE,10)
valid_09_10 <- c(rep(TRUE,2),FALSE,rep(TRUE,7))
valid_08_10 <- c(rep(TRUE,2),FALSE,FALSE, rep(TRUE,6))

NOT_APPLIED_CHAR <- '9'
OMITTED_CHAR <- '_'
INVALID_SCORE <- NA # NA, 0 OR 1. DEFAULT IS NA.
OMITTED_SCORE <- 0 # O OR NA. DEFAULT IS 0.

answer <- rbind(c("A","B","C","D","E","A","B","C","D","E"), #1 no mistake 1
                c("A","B","D","D","E","A","B","C","D","E"), #2 one mistake 
                c("A","C","C","D","E","A","C","C","E","E"), #3 three mistakes
                c("E","A","B","C","D","E","A","B","C","D"), #4 all wrong
                c("E","A","9","C","D","E","A","B","C","D"), #5 one not applied
                c("E","A","_","C","D","E","A","B","C","D"), #6 one omitted.
                c("9","9","9","C","D","E","A","9","C","D"), #7 one not applied
                c("9","9","9","C","D","E","A","9","C","D"), #8 one not applied
                c("9","9","9","C","D","E","A","9","C","D")  #9 one not applied
) #all wrong

# key = A



ifelse(key==answer[2,],1,0) #line vs line ok
ifelse(valid_10_10,ifelse(key==answer[2,],1,0),INVALID_SCORE) #line vs line ok
ifelse(valid_09_10,ifelse(key==answer[2,],1,0),INVALID_SCORE) #line vs line ok
ifelse(valid_08_10,ifelse(key==answer[2,],1,0),INVALID_SCORE) #line vs line ok

ifelse(valid_09_10,
       ifelse(answer[2,]==NOT_APPLIED_CHAR,NA,
              ifelse(answer[2,]==OMITTED_CHAR,OMITTED_SCORE,
                     ifelse(key==answer[2,],1,0))),
       INVALID_SCORE) #line vs line ok





scoring_vector<-function(answer_vector,
                             key_vector,
                             valid_vector,
                             NOT_APPLIED_CHAR= '9',
                             OMITTED_CHAR= '_',
                             INVALID_SCORE = NA, # NA, 0 OR 1. DEFAULT IS NA.
                             OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
                             ){
  if(length(answer_vector)!= length(key_vector)){stop("answer and key have different lengths")}
  if(length(answer_vector)!= length(valid_vector)){stop("answer and valid vector have different lengths")}
  return(
    ifelse(answer_vector!=NOT_APPLIED_CHAR,
           ifelse(valid_vector,
                  ifelse(answer_vector==OMITTED_CHAR,OMITTED_SCORE,
                         ifelse(key==answer_vector,1,0)),
                  INVALID_SCORE),
           NA)
  ) #works
}

scoring<-function(data_matrix, 
                  key_vector,
                  valid_vector,
                  NOT_APPLIED_CHAR= '9',
                  OMITTED_CHAR= '_',
                  INVALID_SCORE = NA, # NA, 0 OR 1. DEFAULT IS NA.
                  OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
){
  scored <- matrix(rep(NA,nrow(data_matrix)*ncol(data_matrix)),nrow=nrow(data_matrix),ncol = ncol(data_matrix),byrow = TRUE)
  scored <- t(apply(X=data_matrix, MARGIN = 1,FUN=scoring_vector, 
                    key_vector = key_vector,
                    valid_vector = valid_vector,
                    NOT_APPLIED_CHAR= NOT_APPLIED_CHAR,
                    OMITTED_CHAR= OMITTED_CHAR,
                    INVALID_SCORE = INVALID_SCORE, # NA, 0 OR 1. DEFAULT IS NA.
                    OMITTED_SCORE = OMITTED_SCORE # O OR NA. DEFAULT IS 0.
  ))
  return(scored)
}


#pbis_calc_vector(scored_vector =scored[,3], scores,ifDeleted = TRUE)
#if(!require(CTT)) install.packages("CTT"); library(CTT)
#x<-CTT::score(items=answer,key=key,rel=TRUE,output.scored=TRUE)
#x$reliability$pBis
#pbis_calc_vector(scored_vector =scored[,3], scores,ifDeleted = FALSE)
#if(!require(PerFit)) install.packages("PerFit"); library(PerFit)
#PerFit::r.pbis(t(scored))
pbis_calc_vector <- function(scored_vector, scores_vector, na.rm = TRUE, ifDeleted=TRUE){
  
  true_vector_1 <- (scored_vector == 1)
  true_vector_0 <- (scored_vector == 0)
  n1 <- sum(true_vector_1,na.rm = na.rm)
  n0 <- sum(true_vector_0,na.rm = na.rm)
  if(ifDeleted){
    scores_vector_tmp = scores_vector-scored_vector
  }else{
    scores_vector_tmp = scores_vector
  }
  m1 <- mean(scores_vector_tmp[true_vector_1],na.rm = na.rm)
  m0 <- mean(scores_vector_tmp[true_vector_0],na.rm = na.rm)
  m <- mean(scores_vector_tmp,na.rm = na.rm)
  n <- n1+n0
  sd_bias <- sd(scores_vector_tmp,na.rm = na.rm)*sqrt((n-1)/n) # using biased sd
  return((m1-m)/sd_bias*sqrt(n1/n0))
}



# pbis_calc(scored_matrix=scored, scores_vector = scores,ifDeleted = TRUE)
# if(!require(CTT)) install.packages("CTT"); library(CTT)
# x<-CTT::score(items=answer,key=key,rel=TRUE,output.scored=TRUE)
# x$reliability$pBis
# pbis_calc(scored_matrix=scored, scores_vector = scores,ifDeleted = FALSE)
# if(!require(PerFit)) install.packages("PerFit"); library(PerFit)
# PerFit::r.pbis(t(scored))
pbis_calc <- function(scored_matrix, scores_vector, ifDeleted=TRUE){
  
  return(apply(scored_matrix,MARGIN = 2,FUN=pbis_calc_vector,scores_vector=scores_vector,ifDeleted=ifDeleted))
}


#bis_calc_vector(scored_vector =scored[,3], scores,ifDeleted = TRUE)
bis_calc_vector <- function(scored_vector, scores_vector, ifDeleted=TRUE){
  
  true_vector_1 <- (scored_vector == 1)
  true_vector_0 <- (scored_vector == 0)
  n1 <- sum(true_vector_1)
  n0 <- sum(true_vector_0)
  if(ifDeleted){
    scores_vector_tmp = scores_vector-scored_vector
  }else{
    scores_vector_tmp = scores_vector
  }
  m1 <- mean(scores_vector_tmp[true_vector_1])
  m <- mean(scores_vector_tmp)
  n <- n1+n0
  sd_bias <- sd(scores_vector_tmp)*sqrt((n-1)/n) # using biased sd
  return((m1-m)/sd_bias*(n1/n)/dnorm(n1/n)) #biserial
}
# bis_calc_vector(scored_vector =scored[,3], scores,ifDeleted = TRUE)
# if(!require(CTT)) install.packages("CTT"); library(CTT)
# x<-CTT::score(items=answer,key=key,rel=TRUE,output.scored=TRUE)
# x$reliability$bis[3] #closebut not exact... 

bis_calc <- function(scored_matrix, scores_vector, ifDeleted=TRUE){
  
  return(apply(scored_matrix,MARGIN = 2,FUN=bis_calc_vector,scores_vector=scores_vector,ifDeleted=ifDeleted))
}
# bis_calc(scored_matrix=scored, scores_vector = scores,ifDeleted = TRUE)

calc_stats <-function(scored_data,na.rm=TRUE){
  scores <- rowSums(scored_data,na.rm=na.rm)
  n_applied_items <- apply(scored_data, MARGIN = 2, FUN = function(x){sum((x==0)|(x==1),na.rm=na.rm)})
  return(list(
    scores = scores,
    nPerson = nrow(scored_data),
    nItem = ncol(scored_data),
    scaleMean = mean(scores,na.rm=na.rm),
    scaleSD = sd(scores,na.rm=na.rm),
    itemMean = colSums(scored_data,na.rm=na.rm)/n_applied_items
  ))
}

calc_stats(scored)


create_ctt_score_struct <- function(data_matrix, 
                                    key_vector,
                                    valid_vector,
                                    NOT_APPLIED_CHAR= '9',
                                    OMITTED_CHAR= '_',
                                    INVALID_SCORE = NA, # NA, 0 OR 1. DEFAULT IS NA.
                                    OMITTED_SCORE = 0, # O OR NA. DEFAULT IS 0.
                                    na.rm=TRUE,
                                    ifDeleted=TRUE){
  scored_data = scoring(data_matrix = data_matrix, 
                        key_vector=key_vector,
                        valid_vector=valid_vector,
                        NOT_APPLIED_CHAR= NOT_APPLIED_CHAR,
                        OMITTED_CHAR= OMITTED_CHAR,
                        INVALID_SCORE = INVALID_SCORE, 
                        OMITTED_SCORE = OMITTED_SCORE)
  scores = rowSums(scored_data,na.rm = na.rm)
  alpha <- (CTT::score(items=data_matrix,key=key,rel=TRUE))$reliability$alpha
    
  n_applied_items <- apply(scored_data, MARGIN = 2, FUN = function(x){sum((x==0)|(x==1),na.rm=na.rm)})
  return(
    list(
      scored = scored_data,
      scores = scores,
      key = key_vector,
      reliability = list(
        alpha = alpha,
        nPerson = nrow(scored_data),
        nItem = ncol(scored_data),
        scaleMean = mean(scores,na.rm=na.rm),
        scaleSD = sd(scores,na.rm=na.rm),
        itemMean = colSums(scored_data,na.rm=na.rm)/n_applied_items,
        pbis = pbis_calc(scored_matrix=scored_data, scores_vector = scores,ifDeleted = ifDeleted)
      )
    )
    
  )
}


                  


scored <- scoring(data_matrix = answer, 
        key_vector = key,
        valid_vector = valid_09_10,
        NOT_APPLIED_CHAR= '9',
        OMITTED_CHAR= '_',
        INVALID_SCORE = 0, # NA, 0 OR 1. DEFAULT IS NA.
        OMITTED_SCORE = NA # O OR NA. DEFAULT IS 0.
)
scored


scoring_vector(answer_vector = answer[2,],
                   key_vector =key,
                   valid_vector = valid_10_10,
                   NOT_APPLIED_CHAR= '9',
                   OMITTED_CHAR= '_',
                   INVALID_SCORE = 1, # NA, 0 OR 1. DEFAULT IS NA.
                   OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
)[3] #0 expected

scoring_vector(answer_vector = answer[1,],
                   key_vector =key,
                   valid_vector = valid_10_10,
                   NOT_APPLIED_CHAR= '9',
                   OMITTED_CHAR= '_',
                   INVALID_SCORE = 1, # NA, 0 OR 1. DEFAULT IS NA.
                   OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
)[3] #1 expected

scoring_vector(answer_vector = answer[2,],
                   key_vector =key,
                   valid_vector = valid_09_10,
                   NOT_APPLIED_CHAR= '9',
                   OMITTED_CHAR= '_',
                   INVALID_SCORE = NA, # NA, 0 OR 1. DEFAULT IS NA.
                   OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
)[3] #NA expected.
scoring_vector(answer_vector = answer[2,],
                   key_vector =key,
                   valid_vector = valid_09_10,
                   NOT_APPLIED_CHAR= '9',
                   OMITTED_CHAR= '_',
                   INVALID_SCORE = 0, # NA, 0 OR 1. DEFAULT IS NA.
                   OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
)[3] #0 expected.

#not applied tests
scoring_vector(answer_vector = answer[5,],
                   key_vector =key,
                   valid_vector = valid_10_10,
                   NOT_APPLIED_CHAR= '9',
                   OMITTED_CHAR= '_',
                   INVALID_SCORE = 1, # NA, 0 OR 1. DEFAULT IS NA.
                   OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
)[3] #NA expected

#not applied tests
scoring_vector(answer_vector = answer[5,],
                   key_vector =key,
                   valid_vector = valid_09_10,
                   NOT_APPLIED_CHAR= '9',
                   OMITTED_CHAR= '_',
                   INVALID_SCORE = 1, # NA, 0 OR 1. DEFAULT IS NA.
                   OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
)[3] #NA expected




working<-function( ){}

#omitted tests
scoring_vector(answer_vector = answer[6,],
                   key_vector =key,
                   valid_vector = valid_09_10,
                   NOT_APPLIED_CHAR= '9',
                   OMITTED_CHAR= '_',
                   INVALID_SCORE = 1, # NA, 0 OR 1. DEFAULT IS NA.
                   OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
)[3] #1 expected

scoring_vector(answer_vector = answer[6,],
                   key_vector =key,
                   valid_vector = valid_09_10,
                   NOT_APPLIED_CHAR= '9',
                   OMITTED_CHAR= '_',
                   INVALID_SCORE = 0, # NA, 0 OR 1. DEFAULT IS NA.
                   OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
)[3] #0 expected

scoring_vector(answer_vector = answer[6,],
                   key_vector =key,
                   valid_vector = valid_10_10,
                   NOT_APPLIED_CHAR= '9',
                   OMITTED_CHAR= '_',
                   INVALID_SCORE = 1, # NA, 0 OR 1. DEFAULT IS NA.
                   OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
)[3] #0 expected

scoring_vector(answer_vector = answer[6,],
                   key_vector =key,
                   valid_vector = valid_10_10,
                   NOT_APPLIED_CHAR= '9',
                   OMITTED_CHAR= '_',
                   INVALID_SCORE = 0, # NA, 0 OR 1. DEFAULT IS NA.
                   OMITTED_SCORE = NA # O OR NA. DEFAULT IS 0.
)[3] #NA expected



str(as.numeric(valid_08_10))

keymatrix <- matrix(rep(key,nrow(answer)),nrow=nrow(answer),ncol = ncol(answer),byrow = TRUE)

ifelse(keymatrix==answer,1,0) #matrix vs matrix ok

scoring_vector_tmp<-function(answer_vector,key_vector){
  return(ifelse(key_vector==answer_vector,1,0)) #works
}

scoring1<-function(data_matrix, key_vector ){
  scored <- matrix(rep(NA,nrow(data_matrix)*ncol(data_matrix)),nrow=nrow(data_matrix),ncol = ncol(data_matrix),byrow = TRUE)
  scored <- t(apply(X=data_matrix, MARGIN = 1,FUN=scoring_vector, key_vector = key_vector))
  return(scored)
}


scoring_vector(key_vector=key, answer_vector=answer[3,],valid_vector = valid_10_10)

t(apply(X=answer, MARGIN = 1,FUN=scoring_vector, key_vector = key))

scored <-scoring(data_matrix = answer, key_vector = key, valid_vector = valid_10_10)
scores <- rowSums(scored,na.rm = TRUE)

calc_stats(scored)
create_ctt_score_struct(data_matrix = answer, 
                        key_vector = key, 
                        valid_vector = valid_10_10)



set.seed(10)
answer <- matrix(data = sample(c('A','B','C','D','E',"_"),size=15*1000, replace = TRUE,prob =c(rep(0.98/5,5),0.02)),
                 nrow = 1000, ncol =15, byrow = TRUE)
key <- answer[1,]

scored <-scoring(data_matrix = answer, key_vector = key, valid_vector = rep(TRUE,length(key)))
scores <- rowSums(scored,na.rm = TRUE)
group <- ntile(n=3,scores)
library(dplyr)
library(tidyr)
library(ggplot2)



data1 <-cbind(data,group)
colnames(data1) <-c(1:15, "group")

data_distractor_analyses_gn <- data.frame(data1) %>% 
  tidyr::gather("Item","Distratores",c(1:15)) %>% 
  dplyr::group_by(Item,group,Distratores) %>% 
  dplyr::summarise(n=n()) %>%
  dplyr::group_by(Item,group,) %>%
  mutate(Percentagem=n/sum(n)*100)

data_distractor_analyses_g1 <- data.frame(data1) %>% 
  tidyr::gather("Item","Distratores",1:15) %>% 
  dplyr::group_by(Item,Distratores) %>% 
  dplyr::summarise(NumRespostas=n()) %>%
  dplyr::group_by(Item) %>%
  mutate(Percentagem=NumRespostas/sum(NumRespostas)*100) %>%
  mutate(PercentagemChar=sprintf("%.2f%%",Percentagem))
  

#' Title
#'
#' @param data_matrix - Matrix containing user responses. 
#' @param key_vector  - Key vector. 
#' @param n_groups    - Number of groups into which the respondents are divided. 
#'                      n_groups should be 1 if there are few respondents (N < 90).
#'                      When n_groups=1, a bar chart is generated.
#'
#' @return
#' @export
#'
#' @examples
distractor_analysis_data_gen <- function(data_matrix, 
                                     key_vector,
                                     valid_vector,
                                     NOT_APPLIED_CHAR= '9',
                                     OMITTED_CHAR= '_',
                                     INVALID_SCORE = NA, # NA, 0 OR 1. DEFAULT IS NA.
                                     OMITTED_SCORE = 0, # O OR NA. DEFAULT IS 0.
                                     na.rm=TRUE,
                                     ifDeleted=TRUE,
                                     n_groups){

  
  tmp <- create_ctt_score_struct(data_matrix, 
                                        key_vector,
                                        valid_vector,
                                        NOT_APPLIED_CHAR= '9',
                                        OMITTED_CHAR= '_',
                                        INVALID_SCORE = NA, # NA, 0 OR 1. DEFAULT IS NA.
                                        OMITTED_SCORE = 0, # O OR NA. DEFAULT IS 0.
                                        na.rm=TRUE,
                                        ifDeleted=TRUE)
      
  
  group <- dplyr::ntile(n=n_groups,tmp$scores)
  
  data <- cbind(data_matrix,group)
  colnames(data)<- c(1:ncol(data_matrix),"group")
  
  if(n_groups == 1 ){
    data_distractor_analyses <- data.frame(data) %>% 
      tidyr::gather("Item","Distratores",c(1:ncol(data_matrix)) )  %>% 
      dplyr::group_by(Item,Distratores) %>% 
      dplyr::summarise(NumRespostas=n()) %>%
      dplyr::group_by(Item) %>%
      mutate(Percentagem=NumRespostas/sum(NumRespostas)*100) %>%
      mutate(PercentagemChar=sprintf("%.2f%%",Percentagem))
  }else{
    data_distractor_analyses <- data.frame(data) %>% 
      tidyr::gather("Item","Distratores",c(1:ncol(data_matrix))) %>% 
      dplyr::group_by(Item,group,Distratores) %>% 
      dplyr::summarise(NumRespostas=n()) %>%
      dplyr::group_by(Item,group,) %>%
      mutate(Percentagem=NumRespostas/sum(NumRespostas)*100)
    
  }
  
  return(
    list(ctt_stats = tmp,
         distractor_analysis=data_distractor_analyses,
         n_groups =n_groups)
  )
} # distractor_analysis_data_gen


# single group data struct rearrange

distractor_analysis_data_struct_n_1 <- function(distractor_analysis_data_struct){
  if(distractor_analysis_data_struct$n_groups !=1){
    tmp <- distractor_analysis_data_struct$distractor_analysis %>% 
      dplyr::group_by(Item,Distratores) %>% 
      dplyr::summarise(NumRespostas=sum(NumRespostas)) %>%
      dplyr::group_by(Item) %>%
      mutate(Percentagem=NumRespostas/sum(NumRespostas)*100) %>%
      mutate(PercentagemChar=sprintf("%.2f%%",Percentagem))
    distractor_analysis_data_struct$distractor_analysis <- tmp
    distractor_analysis_data_struct$n_groups <- 1
  }
  return(distractor_analysis_data_struct)
}



plot_item_distractor_analysis <- function(distractor_analysis_data_struct,
                                          item_index,
                                          title_enable = TRUE){
  item_data <- distractor_analysis_data_struct$distractor_analysis %>% 
    filter(Item == paste0('X',item_index))
  
  if(title_enable==TRUE){
    p <- distractor_analysis_data_struct$ctt_stats$reliability$itemMean[item_index]
    pbis <- distractor_analysis_data_struct$ctt_stats$reliability$pbis[item_index]
    key <- distractor_analysis_data_struct$ctt_stats$key[item_index]
    n <- distractor_analysis_data_struct$ctt_stats$reliability$nPerson
    
    title <- paste0("Item ", item_index,":  ")
    if(!is.null(p)){ title <- paste0(title,sprintf("pbis = %0.2f%%  ",p*100))}
    if(!is.null(pbis)){ title <- paste0(title,sprintf("pbis = %0.2f ",pbis))}
    if(!is.null(key)){ title <- paste0(title,"\nGabarito = ",key,". ")}
    if(!is.null(n)){ title <- paste0(title,"N = ",n,". ")}
  }else{
    title <- ""
  }
  
  if(distractor_analysis_data_struct$n_groups ==1 ){
    p<-ggplot2::ggplot(data =data.frame(item_data),aes(x= Distratores, y=NumRespostas, label=PercentagemChar)) +
      geom_bar(stat = "identity", width = 0.5)+
      scale_x_discrete(name = "Distratores", breaks = pull( item_data,'Distratores'), labels = pull( item_data,'Distratores'))+
      #scale_x_discrete(limits=item_data[,'Distratores'])+
      geom_text(color ="white",position = position_stack(vjust = 0.5),size=3)+ theme_bw(base_size = 12)+
      labs(title=title, x = "Distratores", y="Número de respondentes")
  }else{
    p<-ggplot2::ggplot(data =data.frame(item_data),aes(x=group,y=Percentagem,group=Distratores))+
      geom_line(aes(colour=Distratores))+
      geom_point(aes(shape=Distratores,colour=Distratores),size=3)+
      scale_y_continuous(name="Percentagem", breaks=seq(0,100,10)) +
      scale_x_discrete(name = "Grupos", breaks = c(1:distractor_analysis_data_struct$n_groups), 
                       labels=c(paste("g",1:distractor_analysis_data_struct$n_groups,sep="")))+
      labs(title=title)
  }
  print(p)
  return(p)
}#plot_item_distractor_analysis


plot_item_distractor_analysis_compound <- function(distractor_analysis_data_struct,item_index){
  
  if(distractor_analysis_data_struct$n_groups == 1) {stop("Cannot generate compound graph when ngroups == 1")}
  
  title <- paste0("Item ", item_index,"")
  ptitle <- cowplot::ggdraw() + cowplot::draw_label(title, fontface='bold')
  
  p1 <- plot_item_distractor_analysis(distractor_analysis_data_struct= distractor_analysis_data_struct,
                                      item_index = item_index)
  
  p2 <- plot_item_distractor_analysis(distractor_analysis_data_struct= distractor_analysis_data_struct_n_1(distractor_analysis_data_struct),
                                      item_index = item_index,
                                      title_enable=FALSE)
  pfinal <- plot_grid(ptitle, p1,p2,nrow=3,rel_heights=c(0.1, 1,1))
  print(pfinal)
  return(pfinal)
  
}



distractor_analysis_data_struct1 <- distractor_analysis_data_gen(data_matrix = answer, 
                             key_vector = key,
                             valid_vector = rep(TRUE,length(key)),
                             NOT_APPLIED_CHAR= '9',
                             OMITTED_CHAR= '_',
                             INVALID_SCORE = NA, # NA, 0 OR 1. DEFAULT IS NA.
                             OMITTED_SCORE = 0, # O OR NA. DEFAULT IS 0.
                             na.rm=TRUE,
                             ifDeleted=TRUE,
                             n_groups=1)


distractor_analysis_data_struct2 <- distractor_analysis_data_gen(data_matrix = answer, 
                                                                 key_vector = key,
                                                                 valid_vector = rep(TRUE,length(key)),
                                                                 NOT_APPLIED_CHAR= '9',
                                                                 OMITTED_CHAR= '_',
                                                                 INVALID_SCORE = NA, # NA, 0 OR 1. DEFAULT IS NA.
                                                                 OMITTED_SCORE = 0, # O OR NA. DEFAULT IS 0.
                                                                 na.rm=TRUE,
                                                                 ifDeleted=TRUE,
                                                                 n_groups=3)

  
plot_item_distractor_analysis(distractor_analysis_data_struct= distractor_analysis_data_struct1,
                              item_index=1)
plot_item_distractor_analysis(distractor_analysis_data_struct= distractor_analysis_data_struct2,
                              item_index=1)
plot_item_distractor_analysis(distractor_analysis_data_struct= distractor_analysis_data_struct_n_1(distractor_analysis_data_struct2),
                              item_index=1)
plot_item_distractor_analysis(distractor_analysis_data_struct= distractor_analysis_data_struct_n_1(distractor_analysis_data_struct1),
                              item_index=1)


plot_item_distractor_analysis(distractor_analysis_data_struct= distractor_analysis_data_struct1,
                              item_index=1)
plot_item_distractor_analysis(distractor_analysis_data_struct= distractor_analysis_data_struct1,
                              item_index=6)


plot_item_distractor_analysis_compound(distractor_analysis_data_struct= distractor_analysis_data_struct2,
                              item_index=1)
plot_item_distractor_analysis_compound(distractor_analysis_data_struct= distractor_analysis_data_struct_n_1(distractor_analysis_data_struct2),
                              item_index=1)


item_index <-2

item_data <- data_distractor_analyses %>% 
  filter(Item == paste0('X',item_index))

p <- tmp$reliability$itemMean[item_index]
pbis <- tmp$reliability$pbis[item_index]
key <- tmp$key
n <- tmp$reliability$nPerson

title <- paste0("Item ", item_index,": ")
if(!is.null(p)){ title <- paste0(title,"p = ",p,". ")}
if(!is.null(pbis)){ title <- paste0(title,"pbis = ",pbis,".")}
if(!is.null(key)){ title <- paste0(title,"\nGabarito = ",key,". ")}
if(!is.null(n)){ title <- paste0(title,"N = ",n,". ")}

if(n_groups ==1 ){
  ggplot2::ggplot(data =data.frame(item_data),aes(x= Distratores, y=NumRespostas, label=PercentagemChar)) +
    geom_bar(stat = "identity", width = 0.5)+
    scale_x_discrete(name = "Distratores", breaks = pull( item_data,'Distratores'), labels = pull( item_data,'Distratores'))+
    #scale_x_discrete(limits=item_data[,'Distratores'])+
    geom_text(color ="white",position = position_stack(vjust = 0.5),size=3)+ theme_bw(base_size = 12)+
    labs(title=title, x = "Distratores", y="Número de respondentes")
}else{
  ggplot2::ggplot(data =data.frame(item_data),aes(x=group,y=Percentagem,group=Distratores))+
    geom_line(aes(colour=Distratores))+
    geom_point(aes(shape=Distratores,colour=Distratores),size=3)+
    scale_y_continuous(name="Percentagem", breaks=seq(0,100,10)) +
    scale_x_discrete(name = "Grupos", breaks = c(1:nGroups), labels=c(paste("g",1:nGroups,sep="")))+
    labs(title=title)
}





item_data <- data_distractor_analyses_gn %>% filter(Item == paste0('X',item_index))

p <- "32%"
pbis <- "0.32"
key <- "A"
n <- sum(item_data[,n])

title <- paste0("Item ", item_index,": ")
if(!is.null(p)){ title <- paste0(title,"p = ",p,". ")}
if(!is.null(pbis)){ title <- paste0(title,"pbis = ",pbis,".")}
if(!is.null(key)){ title <- paste0(title,"\nGabarito = ",key,". ")}
if(!is.null(n)){ title <- paste0(title,"N = ",n,". ")}

ggplot2::ggplot(data =data.frame(item_data),aes(x=group,y=Percentagem,group=Distratores))+
  geom_line(aes(colour=Distratores))+
  geom_point(aes(shape=Distratores,colour=Distratores),size=3)+
  scale_y_continuous(name="Percentagem", breaks=seq(0,100,10)) +
  scale_x_discrete(name = "Grupos", breaks = c(1:nGroups), labels=c(paste("g",1:nGroups,sep="")))+
  labs(title=title)

item_data <- data_distractor_analyses_g1 %>% filter(Item == paste0('X',item_index))


title <- paste0("Item ", item_index,".")
if(!is.null(p)){ title <- paste0(title,"p = ",p,". ")}
if(!is.null(pbis)){ title <- paste0(title,"pbis = ",pbis,".")}
if(!is.null(key)){ title <- paste0(title,"\nGabarito = ",key,". ")}
if(!is.null(n)){ title <- paste0(title,"N = ",n,". ")}


ggplot(data =data.frame(item_data),aes(x= Distratores, y=NumRespostas, label=PercentagemChar)) +
  geom_bar(stat = "identity", width = 0.5)+
  scale_x_discrete(name = "Distratores", breaks = pull( item_data,'Distratores'), labels = pull( item_data,'Distratores'))+
  #scale_x_discrete(limits=item_data[,'Distratores'])+
  geom_text(color ="white",position = position_stack(vjust = 0.5),size=3)+ theme_bw(base_size = 12)+
  labs(title=title, x = "Distratores", y="Número de respondentes")


create_ctt_score_struct(data_matrix = answer, key_vector = key, valid_vector =  rep(TRUE,length(key)))


head(data)
table(data[,1])


if(!require(CTT)) install.packages("CTT"); library(CTT)

answer <- matrix(data = sample(c('A','B','C','D','E',"_"),size=15*1000, replace = TRUE,prob =c(rep(0.98/5,5),0.02)),
                 nrow = 1000, ncol =15, byrow = TRUE)
key <- answer[1,]
valid_vector <-rep(TRUE, length(key))
calc_alpha_ctt_package <- function(data_matrix, 
                                   key_vector,
                                   valid_vector){
  
  x<-CTT::score(items=answer,key=key,rel=TRUE,output.scored=TRUE)
  return(x$reliability$alpha)
  
}

calc_alpha_ctt_package(data_matrix = answer,
                       key_vector=key,
                       valid_vector = valid_vector)

x<-CTT::score(items=answer,key=key,rel=TRUE)
x$reliability$alpha

(CTT::score(items=answer,key=key,rel=TRUE))$reliability#alpha










#leaving here as it shows equivalent formulas for pbis when ifDeleted = FALSE
n1 <- sum(scored[,3]==1)
n0 <- sum(scored[,3]==0)
m1 <- mean(scores[scored[,3]==1])
m0 <- mean(scores[scored[,3]==0])
m <- mean(scores)
n <- n1+n0
sd_bias <- sd(scores)*sqrt((n-1)/n) # using biased sd
sd_unbias <- sd(scores)
(m1-m)/sd_bias*sqrt(n1/n0)       #point biserial - using biased sd
(m1-m0)/sd_bias*sqrt(n1*n0/n^2) #point biserial  - using biased sd
(m1-m0)/sd_unbias*sqrt(n1*n0/n/(n-1)) #point biserial - using unbiased sd
(m1-m)/sd*sqrt(n1/n/dnorm(n1/n)) #biserial



#leaving here as it shows equivalent formulas for pbis when ifDeleted = TRUE
n1 <- sum(scored[,3]==1)
n0 <- sum(scored[,3]==0)
scores_tmp <- scores-scored[,3]
m1 <- mean(scores_tmp[scored[,3]==1])
m0 <- mean(scores_tmp[scored[,3]==0])
m <- mean(scores_tmp)
n <- n1+n0
sd_bias <- sd(scores_tmp)*sqrt((n-1)/n) # using biased sd
sd_unbias <- sd(scores_tmp)
(m1-m)/sd_bias*sqrt(n1/n0)       #point biserial - using biased sd
(m1-m0)/sd_bias*sqrt(n1*n0/n^2) #point biserial  - using biased sd
(m1-m0)/sd_unbias*sqrt(n1*n0/n/(n-1)) #point biserial - using unbiased sd
(m1-m)/sd_bias*(n1/n)/dnorm(n1/n) #biserial

