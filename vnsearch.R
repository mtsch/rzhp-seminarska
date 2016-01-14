n1=function(perm){ #swap two neighbour elements
	l=length(perm)
	res=matrix(nrow=l-1,ncol=l)
	for(i in 1:(l-1)){
		tmp=perm
		tmp[i:(i+1)]=rev(tmp[i:(i+1)])
		#print(tmp)
		res[i,]=tmp
	}
	res
}

n2=function(perm){#swap two parts of permutation
	l=length(perm)
	res=matrix(nrow=l-1,ncol=l)
	for(i in 1:(l-1)){
		res[i,]=c(perm[(i+1):length(perm)],perm[1:i])
	}
	res
}


n3=function(perm){#swap any two elements
	l=length(perm)
	res=matrix(nrow=l*(l-1)/2,ncol=l)
	k=1
	for(i in 1:(l-1)){
		for(j in (i+1):l){
		  #print(c(i,j,k))
			tmp=perm
			t=tmp[i]
			tmp[i]=tmp[j]
			tmp[j]=t
			res[k,]=tmp
			k=k+1
		}
	}
	res
}

n4=function(perm){#reverse part of permutation
	l=length(perm)
	res=matrix(nrow=l*(l-1)/2,ncol=l)
	k=1
	for(i in 1:(l-1)){
		for(j in (i+1):l){
			tmp=perm
			tmp[i:j]=rev(tmp[i:j])
			res[k,]=tmp
			k=k+1
		}
	}
	res
}

combine=function(na,nb){#combine two neighborhoods
  function(perm){
    l=length(perm)
    res=matrix(ncol=l,nrow=0)
    part=na(perm)
    for(i in 1:nrow(part)){
      res=rbind(res,nb(part[i,]))
    }
    res
  }
}

vnsearch=function(start,obj,neighs,printsteps=F,printneighs=F){
	best=start
	searching=T
	if(printneighs)
	  nUsed=rep(0,length(neighs))
	while(searching){
		searching=F
		start=best
		if(printsteps)
		  print(start)
		besti=-1
		bestobj=obj(start)
		for(n in 1:length(neighs)){
			neigh=neighs[[n]](start)
			for(i in 1:nrow(neigh)){
				o=obj(neigh[i,])
				if(o>bestobj){
					bestobj=o
					best=neigh[i,]
					besti=i
				}
			}
			if(besti!=-1){
				searching=T
				if(printneighs)
				  nUsed[n]=nUsed[n]+1
				break
			}
		}
	}
	if(printneighs)
	  print(nUsed)
	best
}