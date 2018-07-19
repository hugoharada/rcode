
# veja magalhães p. 237 para referências.

n1<-150
for(n in c(5,10,20,30,50,100,500,1000,5000)){
  med <- c();
  dp_a <- c();
  dp_p <- c();
  i <- 1
  while(i<n1) {
    amostra <- rnorm(n,5,10)
    med <- c(med,mean(amostra)) #coleção de médias
    i <- i + 1
  }
  print("===================="); 
  print(sprintf("n= %d", n ))
  print(sprintf("media de X_bar= %f, expected is 5. ",mean(med))); #média das médias
  print(sprintf("desvio padrão de X_bar= %f",sd(med))); #desvio padrão da coleção de médias
  print(sprintf("desvio padrão de população = %f , expected is 10 ",sd(med)*sqrt(n))); #desvio padrão da população que originou as médias.
}








