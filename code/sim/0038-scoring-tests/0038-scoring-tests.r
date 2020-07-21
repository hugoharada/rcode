
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
  scores <- rowSums(scored_data)
  return(list(
    scores = scores,
    nPerson = nrow(scored_data),
    nItem = ncol(scored_data),
    scaleMean = mean(scores),
    scaleSD = sd(scores),
    itemMean = colSums(scored_data)/nrow(scored_data)
  ))
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

list(
  a = 1,
  b = list (c =3,d =6)
)


scoring_vector(key_vector=key, answer_vector=answer[3,])

t(apply(X=answer, MARGIN = 1,FUN=scoring_vector, key_vector = key))

scored<-scoring(data_matrix = answer, key_vector = key)
scores <- rowSums(scored)
calc_stats(scored)

lc_esp_em3_raw_scores$reliability$nPerson #person number
lc_esp_em3_raw_scores$reliability$nItem #item
lc_esp_em3_raw_scores$reliability$scaleMean #item
lc_esp_em3_raw_scores$reliability$scaleSD #item
lc_esp_em3_raw_scores$reliability$pBis #item
lc_esp_em3_raw_scores$reliability$itemMean #item


