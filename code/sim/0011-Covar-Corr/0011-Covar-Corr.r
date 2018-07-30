

c1 <- rnorm(81,9,2)
c2 <- rnorm(81,5,1)
c3 <- rnorm(81,14,4)

x <- matrix(cbind(c1,c2,c3),81,3)
x
var(x)
cor(x)

#x^2 
t(x)%*%(x)/81

#variance - formula one
ones <- rep(1,81)
(t(x)%*%(diag(81)-1/81*ones%*%t(ones))%*%x)/80
var(x)

#variance - formula two
z<- x - ones%*%t(ones)%*%x/81
t(z)%*%(z)/80
var(x)


dvar <- matrix(diag(var(x)),1,3)

