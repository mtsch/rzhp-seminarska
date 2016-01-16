source("general.R")
source("vnsearch.R")

A=readFile("DS3.csv")
obj=function(perm){
  scoreFun(A,perm)
}

end.res=matrix(ncol=nrow(A),nrow=0)
end.obj=c()
t=proc.time()
while((proc.time()-t)["elapsed"]<1000){
  start=sample(nrow(A))
  #start
  #n4(start)
  #obj(start)
  #res=vnsearch(start,obj,list(n1,n2,n3,n4,combine(n1,n1),combine(n2,n2),combine(n1,n2),combine(n1,n3),combine(n1,n4),combine(n2,n4),combine(n2,n3)),printneighs = T)
  res=vnsearch(start,obj,list(n3),printneighs = F)
  #res
  o=obj(res)
  print(o)
  print((proc.time()-t)["elapsed"])
  end.res=rbind(end.res,res)
  end.obj=c(end.obj,o)
}
best.i=which.max(end.obj)
print(end.res[best.i,])
print(end.obj[best.i])
print(length(end.obj))