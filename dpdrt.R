library("numDeriv")
library("nleqslv")

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
    j1=grad(f1,x0)
    j2=grad(f2,x0)
    j=rbind(j1,j2)
    
    
    f=rbind(f1value,f2value)
    if(det(j)==0){return("Start with new solution")}
    invj=try(solve(j),silent=TRUE)
    if(class(invj)=="try-error"){return("Start with new solution")}
    x1 = (x0 - c(invj%*%f))
    
    p[[i]] = x1
    i = i + 1
    
    if (norm(abs(x1 - x0),type="2") < tol) break
    x0 = x1
  }
  return(p)
}

dpdrt=function(smp,alp,x0){
  
  v=seq(0,2000,1)
  fmd=function(p){if(p[2]>=0){
    sum((dnorm(smp,mean=p[1],sd=p[2])^alp)*((smp-p[1])/(p[2]^2)))/length(smp)}
    else{
      retrun("Start with new solution")
    }
  }
  
 
  
  fmu=function(p){try(fmd(p),silent=T)}
  fsd=function(p){if(p[2]>=0){sum((dnorm(smp,mean=p[1],sd=p[2])^alp)*(((smp-p[1])^2/(p[2]^2)-1)/p[2]))/length(smp)}
    else{return("Start with new solution")}}
  fqint=function(p){
    if(p[2]>=0){
      k=(1/(p[2]*sqrt(2*pi)))^(1+alp)
      z=-alp/(1+alp)
      z*k*sqrt(2*pi)/sqrt(1+alp)}
    else{
      return("Start with new solution")}
  }
  if(class(fqint)=="try-error"){return("Start with new solution")}
  fsigma=function(p){try(fsd(p)-fqint(p),silent=T)}
  
  
  k=newton(fmu,fsigma,x0=x0)
  
  
  
  return(k[[length(k)]])
}