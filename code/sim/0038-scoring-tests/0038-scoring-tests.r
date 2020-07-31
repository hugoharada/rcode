
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

calc_stats <-function(scored_data,na.rm=TRUE){
  score <- rowSums(scored_data,na.rm=na.rm)
  n_applied_items <- apply(scored_data, MARGIN = 2, FUN = function(x){sum((x==0)|(x==1),na.rm=na.rm)})
  return(list(
    scores = score,
    nPerson = nrow(scored_data),
    nItem = ncol(scored_data),
    scaleMean = mean(score,na.rm=na.rm),
    scaleSD = sd(score,na.rm=na.rm),
    n_applied_items,
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
                                    OMITTED_SCORE = 0 # O OR NA. DEFAULT IS 0.
                                    ){
  scored_data = scoring(data_matrix = data_matrix, 
                        key_vector=key_vector,
                        valid_vector=valid_vector,
                        NOT_APPLIED_CHAR= NOT_APPLIED_CHAR,
                        OMITTED_CHAR= OMITTED_CHAR,
                        INVALID_SCORE = INVALID_SCORE, 
                        OMITTED_SCORE = OMITTED_SCORE)
  score = rowSums(scored_data)
  return(
    list(
      scored = scored_data,
      score = score,
      reliability = list(
        nPerson = nrow(scored_data),
        nItem = ncol(scored_data),
        scaleMean = mean(score),
        scaleSD = sd(score),
        itemMean = colSums(scored_data)/nrow(scored_data)
      )
    )
    
  )
}

scoring_vector(key_vector=key, answer_vector=answer[3,],valid_vector = valid_10_10)

t(apply(X=answer, MARGIN = 1,FUN=scoring_vector, key_vector = key))

scored<-scoring(data_matrix = answer, key_vector = key)
scores <- rowSums(scored)

calc_stats(scored)


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

