checkldpd=function(smp,alp){
  mu=seq(-1.5,1.5,0.1)
  sig=seq(0.1,2.5,0.1)
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
    
    return(st)
    
  }
  for(i in mu){
    for(j in sig){
      
      integ=log(integrate(function(x){(dnorm(x,mean=i,sd=j)^(1+alp))},-Inf,Inf)$value)
      summation=((1+alp)/(alp))*log((1/length(smp))*sum((dnorm(smp,mean=i,sd=j)^alp)))
      st[[k]]=c(i,j,integ-summation)
      k=k+1
    }
  }
  
  return(st)
}