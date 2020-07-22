
key <- c("A","B","C","D","E","A","B","C","D","E")

answer <- rbind(c("A","B","C","D","E","A","B","C","D","E"), #no mistake
                c("A","B","D","D","E","A","B","C","D","E"), #one mistake
                c("A","C","C","D","E","A","C","C","E","E"), #three mistakes
                c("E","A","B","C","D","E","A","B","C","D")) #all wrong


ifelse(key==answer[2,],1,0) #line vs line ok

keymatrix <- matrix(rep(key,nrow(answer)),nrow=nrow(answer),ncol = ncol(answer),byrow = TRUE)

ifelse(keymatrix==answer,1,0) #matrix vs matrix ok

scoring_vector<-function(answer_vector,key_vector){
  return(ifelse(key_vector==answer_vector,1,0)) #works
}

scoring<-function(data_matrix, key_vector ){
  scored <- matrix(rep(NA,nrow(data_matrix)*ncol(data_matrix)),nrow=nrow(data_matrix),ncol = ncol(data_matrix),byrow = TRUE)
  scored <- t(apply(X=data_matrix, MARGIN = 1,FUN=scoring_vector, key_vector = key_vector))
  return(scored)
}

calc_stats <-function(scored_data){
  score <- rowSums(scored_data)
  return(list(
    scores = score,
    nPerson = nrow(scored_data),
    nItem = ncol(scored_data),
    scaleMean = mean(score),
    scaleSD = sd(score),
    itemMean = colSums(scored_data)/nrow(scored_data)
  ))
}

pbis_calc <-function(scored_data_matrix,score_vector){
  tmp <- cbind(scored_data_matrix,score_vector)
  
}


create_ctt_score_struct <- function(data_matrix, key_vector){
  scored_data = scoring(data_matrix, key_vector)
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

scoring_vector(key_vector=key, answer_vector=answer[3,])

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
pbis_calc_vector <- function(scored_vector, scores_vector, ifDeleted=TRUE){

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
  m0 <- mean(scores_vector_tmp[true_vector_0])
  m <- mean(scores_vector_tmp)
  n <- n1+n0
  sd_bias <- sd(scores_vector_tmp)*sqrt((n-1)/n) # using biased sd
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
bis_calc_vector(scored_vector =scored[,3], scores,ifDeleted = TRUE)
if(!require(CTT)) install.packages("CTT"); library(CTT)
x<-CTT::score(items=answer,key=key,rel=TRUE,output.scored=TRUE)
x$reliability$bis[3] #closebut not exact... 

bis_calc <- function(scored_matrix, scores_vector, ifDeleted=TRUE){
  
  return(apply(scored_matrix,MARGIN = 2,FUN=bis_calc_vector,scores_vector=scores_vector,ifDeleted=ifDeleted))
}
bis_calc(scored_matrix=scored, scores_vector = scores,ifDeleted = TRUE)



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

