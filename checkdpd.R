checkdpd=function(smp,alp){
  mu=seq(-1.0,1.0,0.04)
  sig=seq(0.1,2.0,0.04)
  k=1
  st=list()
  if(alp==0){
    for(i in mu){
      for(j in sig){
        sumlikelihood=sum(-log(dnorm(smp,mean=i,sd=j)))
        st[[k]]=c(i,j,sumlikelihood)
        k=k+1
      }
    }
    valuef=sapply(st,function(x) x[3])
    m=max(valuef)
    indexmin=which(valuef==m)
    #return(c(st[[indexmin]][1],st[[indexmin]][2]))
    return(st)
  }
  else{
    for(i in mu){
      for(j in sig){
      
        integ=integrate(function(x){(dnorm(x,mean=i,sd=j)^(1+alp))},-Inf,Inf)$value
        summation=((1+alp)/(alp*length(smp)))*sum((dnorm(smp,mean=i,sd=j)^alp))
        st[[k]]=c(i,j,integ-summation)
        k=k+1
      }
    }
    valuef=sapply(st,function(x) x[3])
    m=min(valuef)
    indexmin=which(valuef==m)
    #return(c(st[[indexmin]][1],st[[indexmin]][2]))
    return(st)
  }
}