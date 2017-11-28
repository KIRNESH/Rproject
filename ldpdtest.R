library("numDeriv")
library("nleqslv")
source("checkldpd.R")
newton <- function(f1,f2,tol = 1e-7 , x0 = rep(1,2), N = 1000){
  h = 1e-7
  i = 1;
  x1 = x0
  p = list()
  
  while (i <= N) {
    f1value=try(f1(x0),silent=T)
    f2value=try(f2(x0),silent=T)
    if(class(f1value)=="try-error"){return("Start with 1new solution")}
    if(class(f2value)=="try-error"){return("Start with 2new solution")}
    j1=grad(f1,x0)
    j2=grad(f2,x0)
    j=rbind(j1,j2)
    
    
    f=rbind(f1value,f2value)
    if(det(j)==0){return("Start with 3new solution")}
    invj=try(solve(j),silent=TRUE)
    if(class(invj)=="try-error"){return("Start with 4new solution")}
    x1 = (x0 - c(invj%*%f))
    # if(x1[2]<0){x1[2]=abs(x1[2])}
    p[[i]] = x1
    i = i + 1
    #print(i)
    if (norm(abs(x1 - x0),type="2") < tol) break
    x0 = x1
  }
  return(p)
}

ldpdnew=function(smp,alp,x0){
  
  v=seq(0,2000,1)
  fmd=function(p){if(p[2]>=0){
    sum((dnorm(smp,mean=p[1],sd=p[2])^alp)*((smp-p[1])/(p[2]^2)))/length(smp)}
    else{
      return("Start with 5new solution")
    }
  }
  
  fwd=function(p){if(p[2]>=0){
    sum((dnorm(smp,mean=p[1],sd=p[2])^alp))/length(smp)}
    else{
      return("Start with 6new solution")
    }
  }
  fmint=fwint=function(p){
    if(p[2]>=0){
    integrate(function(x){(dnorm(x,mean=p[1],sd=p[2])^(1+alp))}
              ,-Inf,Inf)$value}
    else{
      return("Start with 7new solution")
    }
    
  }
  if(class(fmint)=="try-error"){return("Start with 8new solution")}
  
  fwint=function(p){
    if(p[2]>=0){
      
      try(integrate(function(x){(dnorm(x,mean=p[1],sd=p[2])^(1+alp))*((x-p[1])/(p[2]^2))}
                    ,-Inf,Inf)$value,silent=T)}
    else{
      return("Start with 9new solution")
    }
  }
  if(class(fwint)=="try-error"){return("Start with new solution")}
  
  
  fmu=function(p){try(fmd(p)*fmint(p)-fwint(p)*fwd(p),silent=T)}
  
  fsd=function(p){if(p[2]>=0){sum((dnorm(smp,mean=p[1],sd=p[2])^alp)*(((smp-p[1])^2/(p[2]^2)-1)/p[2]))/length(smp)}
    else{return("Start with 10new solution")}}
  
  fqint=function(p){
    if(p[2]>=0){
      try(integrate(function(x){(dnorm(x,mean=p[1],sd=p[2])^(1+alp))*(((x-p[1])^2/(p[2]^2)-1)/p[2])},-Inf,Inf)$value,silent=T)}
    else{
      retrun("Start with 11new solution")}
  }
  if(class(fqint)=="try-error"){return("Start with 12new solution")}
  
  fsigma=function(p){try(fsd(p)*fmint(p)-fqint(p)*fwd(p),silent=T)}
  
  
  k=newton(fmu,fsigma,x0=x0)
  
  return(k[[length(k)]])
}