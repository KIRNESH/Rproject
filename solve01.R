source("normaltest.R")
source("checkdpd.R")
source("checkldpd.R")
source("ldpdtest.R")

solve01=function(smp,fun1,fun2){
  
  st=fun2(smp,0)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0=fun1(smp,0,st[[index]][1:2])
  for(j in 1:20){
    if(sol0=="Start with new solution") {
      index = which(valuef == valuesort[length(valuef)-j])
      sol0=fun1(smp,0,st[[index]][1:2])
    }
    else if (abs(sol0[1]) > 4) {
      index = which(valuef == valuesort[length(valuef)-j])
      sol0=fun1(smp,0,st[[index]][1:2])
    }
    else if (abs(sol0[1]) > 5) {
      index = which(valuef == valuesort[length(valuef)-j])
      sol0=fun1(smp,0,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  
  
  
  st=fun2(smp,0.02)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.02=fun1(smp,0.02,st[[index]][1:2])
  for(j in 2:20){
    if(sol0.02=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.02=fun1(smp,0.02,st[[index]][1:2])
    }
    else if (abs(sol0.02[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.02=fun1(smp,0.02,st[[index]][1:2])
    }
    else if (abs(sol0.02[1]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.02=fun1(smp,0.02,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  st=fun2(smp,0.05)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.05=fun1(smp,0.05,st[[index]][1:2])
  for(j in 2:20){
    if(sol0.05=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.05=fun1(smp,0.05,st[[index]][1:2])
    }
    else if (abs(sol0.05[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.05=fun1(smp,0.05,st[[index]][1:2])
    }
    else if (abs(sol0.05[1]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.05=fun1(smp,0.05,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  
  
  
  st=fun2(smp,0.1)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.1=fun1(smp,0.1,st[[index]][1:2])
  for(j in 2:20){
    if(sol0.1=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.1=fun1(smp,0.1,st[[index]][1:2])
    }
    else if (abs(sol0.1[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.1=fun1(smp,0.1,st[[index]][1:2])
    }
    else if (abs(sol0.1[1]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.1=fun1(smp,0.1,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  st=fun2(smp,0.25)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.25=fun1(smp,0.25,st[[index]][1:2])
  for(j in 2:20){
    if(sol0.25=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.25=fun1(smp,0.25,st[[index]][1:2])
    }
    else if (abs(sol0.25[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.25=fun1(smp,0.25,st[[index]][1:2])
    }
    else if (abs(sol0.25[1]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.25=fun1(smp,0.25,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  st=fun2(smp,0.5)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.5=fun1(smp,0.5,st[[index]][1:2])
  for(j in 2:20){
    if(sol0.5=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.5=fun1(smp,0.5,st[[index]][1:2])
    }
    else if (abs(sol0.5[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.5=fun1(smp,0.5,st[[index]][1:2])
    }
    else if (abs(sol0.5[1]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.5=fun1(smp,0.5,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  st=fun2(smp,1)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol1=fun1(smp,1,st[[index]][1:2])
  for(j in 2:20){
    if(sol1=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol1=fun1(smp,1,st[[index]][1:2])
    }
    else if (abs(sol1[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol1=fun1(smp,1,st[[index]][1:2])
    }
    else if (abs(sol1[1]) > 5) {
      index = which(valuef == valuesort[j])
      sol1=fun1(smp,1,st[[index]][1:2])
    }
    else{
      break
    }
  }
  sol=list(sol0,sol0.02,sol0.05,sol0.1,sol0.25,sol0.5,sol1)
  return(sol)
}