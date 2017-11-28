source("normaltest.R")
source("checkdpd.R")
source("checkldpd.R")
source("ldpdtest.R")
source("bridgediv.R")

solve01bridge=function(smp,lam){
  
  st=checkbridge(smp,0.1,lam)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.1=bridgediv(smp,0.1,lam,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.1=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0=bridgediv(smp,0.1,lam,st[[index]][1:2])
    }
    else if (abs(sol0.1[1]) > 4) {
      index = which(valuef == valuesort[j])
      sol0.1=bridgediv(smp,0.1,lam,st[[index]][1:2])
    }
    else if (abs(sol0.1[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.1=bridgediv(smp,0.1,lam,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  
  
  
  st=checkbridge(smp,0.2,lam)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.2=bridgediv(smp,0.2,lam,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.2=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.2=bridgediv(smp,0.2,lam,st[[index]][1:2])
    }
    else if (abs(sol0.2[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.2=bridgediv(smp,0.2,lam,st[[index]][1:2])
    }
    else if (abs(sol0.2[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.2=bridgediv(smp,0.2,lam,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  st=checkbridge(smp,0.3,lam)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.3=bridgediv(smp,0.3,lam,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.3=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.3=bridgediv(smp,0.3,lam,st[[index]][1:2])
    }
    else if (abs(sol0.3[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.3=bridgediv(smp,0.3,lam,st[[index]][1:2])
    }
    else if (abs(sol0.3[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.3=bridgediv(smp,0.3,lam,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  
  
  
  st=checkbridge(smp,0.4,lam)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.4=bridgediv(smp,0.4,lam,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.4=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.4=bridgediv(smp,0.4,lam,st[[index]][1:2])
    }
    else if (abs(sol0.4[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.4=bridgediv(smp,0.4,lam,st[[index]][1:2])
    }
    else if (abs(sol0.4[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.4=bridgediv(smp,0.4,lam,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  st=checkbridge(smp,0.5,lam)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.5=bridgediv(smp,0.5,lam,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.5=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.5=bridgediv(smp,0.5,lam,st[[index]][1:2])
    }
    else if (abs(sol0.5[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.5=bridgediv(smp,0.5,lam,st[[index]][1:2])
    }
    else if (abs(sol0.5[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.5=bridgediv(smp,0.5,lam,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  st=checkbridge(smp,0.6,lam)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.6=bridgediv(smp,0.6,lam,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.6=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.6=bridgediv(smp,0.6,lam,st[[index]][1:2])
    }
    else if (abs(sol0.6[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.6=bridgediv(smp,0.6,lam,st[[index]][1:2])
    }
    else if (abs(sol0.6[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.6=bridgediv(smp,0.6,lam,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  st=checkbridge(smp,0.7,lam)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.7=bridgediv(smp,0.7,lam,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.7=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.7=bridgediv(smp,0.7,lam,st[[index]][1:2])
    }
    else if (abs(sol0.7[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.7=bridgediv(smp,0.7,lam,st[[index]][1:2])
    }
    else if (abs(sol0.7[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.7=bridgediv(smp,0.7,lam,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  st=checkbridge(smp,0.8,lam)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.8=bridgediv(smp,0.8,lam,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.8=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.8=bridgediv(smp,0.8,lam,st[[index]][1:2])
    }
    else if (abs(sol0.8[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.8=bridgediv(smp,0.8,lam,st[[index]][1:2])
    }
    else if (abs(sol0.8[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.8=bridgediv(smp,0.8,lam,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  st=checkbridge(smp,0.9,lam)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol0.9=bridgediv(smp,0.9,lam,st[[index]][1:2])
  for(j in 2:10){
    if(sol0.9=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol0.9=bridgediv(smp,0.9,lam,st[[index]][1:2])
    }
    else if (abs(sol0.9[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol0.9=bridgediv(smp,0.9,lam,st[[index]][1:2])
    }
    else if (abs(sol0.9[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol0.9=bridgediv(smp,0.9,lam,st[[index]][1:2])
    }
    else{
      break
    }
  }
  
  
  st=checkbridge(smp,1,lam)
  valuef=sapply(st,function(x) x[3])
  valuesort=sort(valuef)
  index=which(valuef==valuesort[1])
  sol1=bridgediv(smp,1,lam,st[[index]][1:2])
  for(j in 2:10){
    if(sol1=="Start with new solution") {
      index = which(valuef == valuesort[j])
      sol1=bridgediv(smp,1,lam,st[[index]][1:2])
    }
    else if (abs(sol1[1]) > 3) {
      index = which(valuef == valuesort[j])
      sol1=bridgediv(smp,1,lam,st[[index]][1:2])
    }
    else if (abs(sol1[2]) > 5) {
      index = which(valuef == valuesort[j])
      sol1=bridgediv(smp,1,lam,st[[index]][1:2])
    }
    else{
      break
    }
  }
  sol=list(sol0.1,sol0.2,sol0.3,sol0.4,sol0.5,sol0.6,sol0.7,sol0.8,sol0.9,sol1)
  return(sol)
}