
###################################################################################
# apply
###################################################################################
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
# by
###################################################################################
shell("cls")
?by

