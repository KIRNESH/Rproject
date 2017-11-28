source("normaltest.R")
source("checkldpd.R")
source("checkdpd.R")
source("ldpdtest.R")
source("bridgediv.R")
source("genldpd.R")


solve01genldpd=function(smp,beta){
  
  st=checkldpd(smp,0)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0=genldpd(smp,0,beta,st[[index]][1:2])
  for(j in 2:10){
    if(sol0=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0=genldpd(smp,0,beta,st[[index]][1:2])
    }
    else if (abs(sol0[1]) > 4) {
      index = which(valuef == valuesort[j])
      sol0=genldpd(smp,0,beta,st[[index]][1:2])
    }
    else if (abs(sol0[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0=genldpd(smp,0,beta,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  
  
  
  st=checkldpd(smp,0.2)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.2=genldpd(smp,0.2,beta,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.2=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.2=genldpd(smp,0.2,beta,st[[index]][1:2])
    }
    else if (abs(sol0.2[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.2=genldpd(smp,0.2,beta,st[[index]][1:2])
    }
    else if (abs(sol0.2[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.2=genldpd(smp,0.2,beta,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  st=checkldpd(smp,0.4)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.4=genldpd(smp,0.4,beta,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.4=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.4=genldpd(smp,0.4,beta,st[[index]][1:2])
    }
    else if (abs(sol0.4[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.4=genldpd(smp,0.4,beta,st[[index]][1:2])
    }
    else if (abs(sol0.4[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.4=genldpd(smp,0.4,beta,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  st=checkldpd(smp,0.6)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.6=genldpd(smp,0.6,beta,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.6=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.6=genldpd(smp,0.6,beta,st[[index]][1:2])
    }
    else if (abs(sol0.6[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.6=genldpd(smp,0.6,beta,st[[index]][1:2])
    }
    else if (abs(sol0.6[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.6=genldpd(smp,0.6,beta,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  
  st=checkldpd(smp,0.8)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.8=genldpd(smp,0.8,beta,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.8=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.8=genldpd(smp,0.8,beta,st[[index]][1:2])
    }
    else if (abs(sol0.8[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.8=genldpd(smp,0.8,beta,st[[index]][1:2])
    }
    else if (abs(sol0.8[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.8=genldpd(smp,0.8,beta,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  st=checkldpd(smp,1)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol1=genldpd(smp,1,beta,st[[index]][1:2])
  for(j in 2:10){
    if(sol1=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol1=genldpd(smp,1,beta,st[[index]][1:2])
    }
    else if (abs(sol1[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol1=genldpd(smp,1,beta,st[[index]][1:2])
    }
    else if (abs(sol1[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol1=genldpd(smp,1,beta,st[[index]][1:2])
    }
    else{
      break
    }
  }
  sol=list(sol0,sol0.2,sol0.4,sol0.6,sol0.8,sol1)
  return(sol)
}