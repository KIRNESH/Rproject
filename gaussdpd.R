library("numDeriv")
library("nleqslv")


gauss <- function(f1,f2,tol = 1e-7 , x0 = rep(1,2), N = 1000){
  h = 1e-7
  i = 1;
  x1 = x0
  p = list()
  
  while (i <= N) {
    mu=try(f1(x0),silent=T)
    
    sigma=try(f2(x0),silent=T)
    if(class(mu)=="try-error"){return("Start with new solution")}
    if(class(sigma)=="try-error"){return("Start with new solution")}
    x1=c(mu,sigma)
    
    p[[i]] = x1
    i = i + 1
    
    if (norm(abs(x1 - x0),type="2")/norm(x1,type="2") < tol) break
    x0 = x1
  }
  return(p)
}
gaussdpd=function(smp,alp,x0){
  
  v=seq(0,2000,1)
  
  
  fmd=function(p){if(p[2]>=0){
    sum((dnorm(smp,mean=p[1],sd=p[2])^alp)*(smp))/length(smp)}
    else{
      retrun("Start with new solution")
    }
  }
  fwd=function(p){if(p[2]>=0){
    sum((dnorm(smp,mean=p[1],sd=p[2])^alp))/length(smp)}
    else{
      return("Start with new solution")
    }
  }
  fmint=fwint=function(p){
    if(p[2]>=0){
      integrate(function(x){(dnorm(x,mean=p[1],sd=p[2])^(1+alp))}
                ,-Inf,Inf,rel.tol=1e-7)$value}
    else{
      return("Start with new solution")
    }
    
  }
  if(class(fwint)=="try-error"){return("Start with new solution")}
  fwint=function(p){
    
    if(p[2]>=0){
      
      try(integrate(function(x){(dnorm(x,mean=p[1],sd=p[2])^(1+alp))*x*exp(beta*dnorm(x,mean=p[1],sd=p[2]))}
                    ,-Inf,Inf,rel.tol=1e-7)$value,silent=T)}
    else{
      return("Start with new solution")
    }
  }
  if(class(fwint)=="try-error"){return("Start with new solution")}
  fmu=function(p){try(fmd(p)/fwd(p),silent=T)}
  
  fsd=function(p){if(p[2]>=0){sum((dnorm(smp,mean=p[1],sd=p[2])^alp)*((smp-p[1])^2))/length(smp)}
    else{return("Start with new solution")}}
  
  fqint=function(p){
    if(p[2]>=0){
      
      try(integrate(function(x){(dnorm(x,mean=p[1],sd=p[2])^(1+alp))*((x-p[1])^2)},-Inf,Inf,rel.tol=1e-7)$value,silent=T)}
    else{
      return("Start with new solution")}
  }
  if(class(fqint)=="try-error"){return("Start with new solution")}
  fsigma=function(p){try(sqrt(((fsd(p)-fqint(p))/(fwd(p)-fmint(p)))),silent=T)}
  
  
  k=gauss(fmu,fsigma,x0=x0)
  
  
  
  return(k[[length(k)]])
}