
###################################################################################
# apply
###################################################################################
rm(list=ls())
shell("cls")
?apply
m <- matrix(c(1:10, 11:20), nrow = 10, ncol = 2, byrow = FALSE)
m
apply(X=m, MARGIN=1, FUN=mean) # mean of the rows
apply(m, 1, mean) # mean of the rows

apply(X=m, MARGIN=2, FUN=mean) # mean of the columns
apply(m, 2, mean) # mean of the columns

apply(m, 1:2, function(x) x^2) # apply to every element
apply(m, MARGIN=c(1,2), function(x) x^2) # apply to every element
apply(m, MARGIN=1:2, function(x) x^2) # apply to every element

?sort
m[,2]
# if you have extra arguments to pass, just name them after FUN
# the following calls work the same as the default value for decreasing is FALSE
apply(X=m, MARGIN=2, FUN=sort)
apply(X=m, MARGIN=2, FUN=sort, decreasing = FALSE)
# to sort in decreasing order
apply(X=m, MARGIN=2, FUN=sort, decreasing = TRUE)

###################################################################################
# by
###################################################################################
rm(list=ls())
shell("cls")
?by

l1 <- c("fator1","fator2","fator1","fator2")
l2 <- c(1,2,3,4)
l3 <- c(2,4,6,8)
df <-data.frame(l1,l2,l3)
df
# on data you pass the colums you would like to apply FUN
# on INDICES indicate how you will split your data
#
# in our example, the dataframe will be split in lines with
# factor "fator1" and with factor "fator2" and the colMeans
# will be calculated.
#
#       l1 l2 l3
# 1 fator1  1  2
# 3 fator1  3  6
# 
# 2 fator2  2  4
# 4 fator2  4  8

by(data=df[,2:3], INDICES = l1, FUN=colMeans) # colmeans
by(data=df[,2:3], INDICES = l1, FUN=rowMeans) # rowmeans
by(data=df[,2:3], INDICES = l1, FUN=sum) # sum all elements

###################################################################################
# eapply
# Apply a Function Over Values in an Environment
###################################################################################
rm(list=ls())
shell("cls")
?eapply
# a new environment
#  An environment, as the name suggests, is a self-contained object with its own variables and functions. 
e <- new.env()
# two environment variables, a and b
e$a <- 1:10
e$b <- 11:20
# mean of the variables
eapply(e, mean)


###################################################################################
# lapply
# Description: "lapply returns a list of the same length as X, each element of 
# which is the result of applying FUN to the corresponding element of X."
#
###################################################################################
rm(list=ls())
shell("cls")
?lapply

# create a list with 2 elements
l <- list(a = 1:10, b = 11:20)
# the mean of the values in each element
lapply(X=l, FUN=mean)
lapply(l, mean)
# the sum of the values in each element
lapply(l, sum)


###################################################################################
# sapply
# Description: "sapply is a user-friendly version of lapply by default returning 
# a vector or matrix if appropriate."
###################################################################################
rm(list=ls())
shell("cls")
?sapply

l <- list(a = 1:10, b = 11:20) # create a list with 2 elements
l
l.mean <- sapply(X=l, FUN=mean) # mean of values using sapply
l.mean
class(l.mean) # what type of object was returned?
str(l.mean) # what type of object was returned?
l.mean[['a']]




###################################################################################
# vapply
# Description: "vapply is similar to sapply, but has a pre-specified type of return 
# value, so it can be safer (and sometimes faster) to use."
###################################################################################
rm(list=ls())
shell("cls")
?vapply

l <- list(a = 1:10, b = 11:20)
l
?fivenum # fivenum of values using vapply
# A FUN.VALUE is supplied to vapply, which you can think of as a kind of 
# template for the output. 
l.fivenum <- vapply(X=l, FUN=fivenum, FUN.VALUE=c(Min.=0, "1st Qu."=0, Median=0, "3rd Qu."=0, Max.=0))
l.fivenum <- vapply(l, fivenum, c(Min.=0, "1st Qu."=0, Median=0, "3rd Qu."=0, Max.=0))
l.fivenum
class(l.fivenum)
str(l.fivenum)

###################################################################################
# replicate
# Description: "replicate is a wrapper for the common use of sapply for repeated 
# evaluation of an expression (which will usually involve random number generation)."
###################################################################################
rm(list=ls())
shell("cls")
?replicate
replicate(10, rnorm(10))
str(replicate(10, rnorm(10)))


###################################################################################
# mapply
# Description: "mapply is a multivariate version of sapply. mapply applies 
# FUN to the first elements of each (.) argument, the second elements, 
# the third elements, and so on."
###################################################################################
rm(list=ls())
shell("cls")
?mapply

# l1[1],l2[1] são passados como argumentos para sum()
# l1[2],l2[2] são passados como argumentos para sum()
l1 <- c(1,2,3,4)
l2 <- c(2,4,6,8)
mapply(FUN=sum,l1,l2)

# l1[1] is repeated l2[1] times
# l1[2] is repeated l2[2] times
?rep #rep.int(x, times)
l1 <- list(1,2,3,4)
l2 <- list(2,4,6,8)
mapply(FUN=rep,l1,l2)
str(mapply(FUN=rep,l1,l2))



###################################################################################
# rapply
# Description: "rapply is a recursive version of lapply."
###################################################################################
rm(list=ls())
shell("cls")
?rapply

l <- list(a = 1:10, b = 11:20)
rapply(l, log2) # log2 of each value in the list
str(rapply(l, log2))
rapply(l, log2, how = "list")# log2 of each value in each list
str(rapply(l, log2, how = "list"))
rapply(l, mean) # what if the function is the mean?
str(rapply(l, mean))
rapply(l, mean, how = "list")
str(rapply(l, mean, how = "list"))


###################################################################################
# tapply
# Description: "Apply a function to each cell of a ragged array, 
# that is to each (non-empty) group of values given by a unique 
# combination of the levels of certain factors."
###################################################################################
rm(list=ls())
shell("cls")
?tapply

attach(iris)
head(iris)
tapply(X=iris$Petal.Length, INDEX=Species, FUN=mean) # mean petal length by species
tapply(iris$Petal.Length, Species, mean) # mean petal length by species

#muito parecido com o by
by(data=iris$Petal.Length, INDICES = Species, FUN=mean) # colmeans

