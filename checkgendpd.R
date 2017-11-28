checkgendpd=function(smp,alp,beta){
  mu=seq(-1.0,1.0,0.04)
  sig=seq(0.1,2.0,0.04)
  k=1
  st=list()
  if(beta==0){
    for(i in mu){
      for(j in sig){
        checkdpd(smp,alp)
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
        xtr=2/beta
        integ1=integrate(function(x){(dnorm(x,mean=i,sd=j))*exp(beta*dnorm(x,mean=i,sd=j))},-Inf,Inf,rel.tol=1e-7)$value
        integ2=integrate(function(x){(1/beta)*exp(beta*dnorm(x,mean=i,sd=j))},i-10*j,i+10*j,rel.tol=1e-7)$value
        summation=(sum(exp(dnorm(smp,mean=i,sd=j)*beta)))
        st[[k]]=c(i,j,xtr*(integ-summation))
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