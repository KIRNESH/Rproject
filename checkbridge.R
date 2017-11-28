source("checkdpd.R")
source("checkldpd.R")

checkbridge=function(smp,alp,lam){
  mu=seq(-1.5,1.5,0.1)
  sig=seq(0.1,2.5,0.1)
  k=1
  st=list()
  if(lam==1){st=checkdpd(smp,alp)}
  else if(lam==0){st=checkldpd(smp,alp)}
  else if(alp==0){
    st=checkdpd(smp,alp)
  }
  else{
    for(i in mu){
      for(j in sig){
        
        integ=integrate(function(x){(dnorm(x,mean=i,sd=j)^(1+alp))},-Inf,Inf)$value
        summation=(1/length(smp))*sum((dnorm(smp,mean=i,sd=j)^alp))
        lambar=1-lam
        fun=(1/lambar)*log(lam+lambar*integ)-(1/lambar)*((1+alp)/alp)*log(lam+lambar*summation)
        st[[k]]=c(i,j,fun)
        k=k+1
      }
    }
  }
  return(st)
}