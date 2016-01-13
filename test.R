source("general.R")
source("vnsearch.R")

A=readFile("DS4.csv")
obj=function(perm){
  scoreFun(A,perm)
}

start=sample(nrow(A))
#start
#n4(start)
obj(start)
#n3 seems to be the optimal neighbourhood, probably finding the optimal solution - despite random starts results are consistent
res=vnsearch(start,obj,list(n3))
#res
obj(res)
