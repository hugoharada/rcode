
lv_n <- 1
time_n <- 1
item_first <-1
item_n <-10

paste("eta",lv_n,"_",time_n,"=",paste0("l", (item_first):(item_n),"_",time_n,"*item",(item_first):(item_n),"_",time_n,sep="+",collapse = ""),sep="")
#"eta1_1=l1_1*item1_1+l2_1*item2_1+l3_1*item3_1+l4_1*item4_1+l5_1*item5_1+l6_1*item6_1+l7_1*item7_1+l8_1*item8_1+l9_1*item9_1+l10_1*item10_1+"

paste0("item",(item_first):(item_n),"_",time_n)
#"item1_1"  "item2_1"  "item3_1"  "item4_1"  "item5_1"  "item6_1"  "item7_1"  "item8_1"  "item9_1"  "item10_1"

paste("eta",lv_n,"_",time_n," ~ ","eta",lv_n,"_",time_n,"_mean*1",sep="")
paste("eta",lv_n,"_",time_n," ~~ ","eta",lv_n,"_",time_n,"_var*","eta",lv_n,"_",time_n,sep="")

#latent var means and var
#"eta1_1 ~ eta1_1_mean*1"
#"eta1_1 ~~ eta1_1_var*eta1_1"


for(i in (item_first):(item_n)){
  print(
    paste("item",i,"_",time_n," ~ int",i,"_",time_n,"*1",sep="", collapse = "##")
  )
}
paste0(paste("int",(item_first):(item_n),"_",time_n,sep="", collapse = " + "),"==0")

## LIR intercepts
# item1_1 ~ int1_1*1
# item2_1 ~ int2_1*1
# item3_1 ~ int3_1*1
# item4_1 ~ int4_1*1
# item5_1 ~ int5_1*1
# item6_1 ~ int6_1*1
# item7_1 ~ int7_1*1
# item8_1 ~ int8_1*1
# item9_1 ~ int9_1*1
# item10_1 ~ int10_1*1


#LIR constraint
for(i in (item_first):(item_n)){
  print(
  paste0(
    paste("int",i,"_",time_n," + l",i,"_",time_n,"*eta",lv_n,"_",time_n,"_mean",sep=""),
    " == 0"
  )
  )
}

# int1_1 + l1_1*eta1_1_mean
# int2_1 + l2_1*eta1_1_mean
# int3_1 + l3_1*eta1_1_mean
# int4_1 + l4_1*eta1_1_mean
# int5_1 + l5_1*eta1_1_mean
# int6_1 + l6_1*eta1_1_mean
# int7_1 + l7_1*eta1_1_mean
# int8_1 + l8_1*eta1_1_mean
# int9_1 + l9_1*eta1_1_mean
# int10_1 + l10_1*eta1_1_mean


item_n<-3
for(i in (item_first):(item_n)){
  print(
    paste("item",i,"_",time_n," | thr",i,"_",time_n,"*t1",sep="")
  )
}

paste0(paste("thr",(item_first):(item_n),"_",time_n,sep="", collapse = " + "),"==0")

## thresholds link  LRVs to observed items
# [1] "item1_1 | thr1_1*t1"
# [1] "item2_1 | thr2_1*t1"
# [1] "item3_1 | thr3_1*t1"
# [1] "thr1_1 + thr2_1 + thr3_1==0"

for(i in (item_first):(item_n)){
  print(
    if(i!=item_n){
      paste0("item",i,"_",time_n," ~~ var",i,"_",time_n,"*item",i,"_",time_n,
             paste0("+ 0*","item",(i+1):(item_n),"_",time_n, collapse = ""),sep="")
    }else{
      paste0("item",i,"_",time_n," ~~ var",i,"_",time_n,"*item",i,"_",time_n,sep="")
    }
  )
}
## LRVs (co)variances
# [1] "item1_1 ~~ var1_1*item1_1+ 0*item2_1+ 0*item3_1"
# [1] "item2_1 ~~ var2_1*item2_1+ 0*item3_1"
# [1] "item3_1 ~~ var3_1*item3_1"



#LRVs var constraint
for(i in (item_first):(item_n)){
  print(
    paste0(paste("eta",lv_n,"_",time_n,"_var*l",i,"_",time_n,"^2 + var",i,"_",time_n,sep="", collapse = "##"),
    " == 1")
  )
}

## LRVs variance constraints
# [1] "eta1_1_var*l1_1^2 + var1_1 == 1"
# [1] "eta1_1_var*l2_1^2 + var2_1 == 1"
# [1] "eta1_1_var*l3_1^2 + var3_1 == 1"

