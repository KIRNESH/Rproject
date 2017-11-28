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
    if(class(f1value)=="try-error"){return("Start with new solution")}
    if(class(f2value)=="try-error"){return("Start with new solution")}
    j1=try(grad(f1,x0))
    j2=try(grad(f2,x0))
    if(class(j1)=="try-error"){return("Start with new solution")}
    if(class(j2)=="try-error"){return("Start with new solution")}
    j=rbind(j1,j2)
    
    
    f=rbind(f1value,f2value)
    if(det(j)==0){return("Start with new solution")}
    invj=try(solve(j),silent=TRUE)
    if(class(invj)=="try-error"){return("Start with new solution")}
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

bridgediv=function(smp,alp,lambda,x0){
  fmd=function(p){if(p[2]>=0){
    sum((dnorm(smp,mean=p[1],sd=p[2])^alp)*((smp-p[1])/(p[2]^2)))/length(smp)}
    else{
      return("Start with new solution")
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
                ,-Inf,Inf)$value}
    else{
      return("Start with new solution")
    }
    
  }
  if(class(fmint)=="try-error"){return("Start with new solution")}
  
  fwint=function(p){
    if(p[2]>=0){
      
      try(integrate(function(x){(dnorm(x,mean=p[1],sd=p[2])^(1+alp))*((x-p[1])/(p[2]^2))}
                    ,-Inf,Inf)$value,silent=T)}
    else{
      return("Start with new solution")
    }
  }
  if(class(fwint)=="try-error"){return("Start with new solution")}
  
  
  fmudpd=function(p){try(fmd(p)-fwint(p),silent=T)}
  fmuldpd=function(p){try(fmd(p)*fmint(p)-fwint(p)*fwd(p),silent=T)}
  
  fsd=function(p){if(p[2]>=0){sum((dnorm(smp,mean=p[1],sd=p[2])^alp)*(((smp-p[1])^2/(p[2]^2)-1)/p[2]))/length(smp)}
    else{return("Start with new solution")}}
  
  fqint=function(p){
    if(p[2]>=0){
      try(integrate(function(x){(dnorm(x,mean=p[1],sd=p[2])^(1+alp))*(((x-p[1])^2/(p[2]^2)-1)/p[2])},-Inf,Inf)$value,silent=T)}
    else{
      retrun("Start with new solution")}
  }
  if(class(fqint)=="try-error"){return("Start with new solution")}
  
  fsigmadpd=function(p){try(fsd(p)-fqint(p),silent=T)}
  fsigmaldpd=function(p){try(fsd(p)*fmint(p)-fqint(p)*fwd(p),silent=T)}
  
  fmu=function(p){lambda*fmudpd(p)+(1-lambda)*fmuldpd(p)}
  fsigma=function(p){lambda*fsigmadpd(p)+(1-lambda)*fsigmaldpd(p)}
  k=newton(fmu,fsigma,x0=x0)
  
  return(k[[length(k)]])
}