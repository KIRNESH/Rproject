source("normaltest.R")
source("checkdpd.R")
library(xlsx)
simulation=function(){
  est0=list()
  est0.02=list()
  est0.05=list()
  est0.1=list()
  est0.25=list()
  est0.5=list()
  est1=list()
  x=c(0,0.02,0.05,0.1,0.25,0.5,1)
  i=1
  while(i<=500){
    smp=rnorm(20,0,1)
    cat(smp,file="testing/mutstcheck20.txt",sep=",",append=TRUE)
    cat("\n",file="testing/mutstcheck20.txt",append=TRUE)
    #r=median(smp)
    #s=median(abs(smp-rep(r,length(smp))))/0.6745
    #xst=c(r,s)
    st=checkdpd(smp,0)
    valuef=sapply(st,function(x) x[3])
    valuesort=sort(valuef)
    index=which(valuef==valuesort[length(valuef)])
    est0[[i]]=dpd(smp,0,st[[index]][1:2])
    for(j in 1:(length(valuef)-1)){
      if(est0[[i]]=="Start with new solution") {
        index = which(valuef == valuesort[length(valuef)-j])
        est0[[i]]=dpd(smp,0,st[[index]][1:2])
      }
      else if (abs(est0[[i]][1]) > 1) {
        index = which(valuef == valuesort[length(valuef)-j])
        est0[[i]]=dpd(smp,0,st[[index]][1:2])
      }
      else if (abs(est0[[i]][1]) > 2) {
        index = which(valuef == valuesort[length(valuef)-j])
        est0[[i]]=dpd(smp,0,st[[index]][1:2])
      }
      else{
        break
      }
    }
    
    
    
    
    
    #if(est0[[i]]=="Start with new solution"){next}
    st=checkdpd(smp,0.02)
    valuef=sapply(st,function(x) x[3])
    valuesort=sort(valuef)
    index=which(valuef==valuesort[1])
    est0.02[[i]]=dpd(smp,0.02,st[[index]][1:2])
    for(j in 2:length(valuef)){
      if(est0.02[[i]]=="Start with new solution") {
        index = which(valuef == valuesort[j])
        est0.02[[i]]=dpd(smp,0.02,st[[index]][1:2])
      }
      else if (abs(est0.02[[i]][1]) > 1) {
        index = which(valuef == valuesort[j])
        est0.02[[i]]=dpd(smp,0.02,st[[index]][1:2])
      }
      else if (abs(est0.02[[i]][1]) > 2) {
        index = which(valuef == valuesort[j])
        est0.02[[i]]=dpd(smp,0.02,st[[index]][1:2])
      }
      else{
        break
      }
    }
    #if(est0.02[[i]]=="Start with new solution"){next}
    
    st=checkdpd(smp,0.05)
    valuef=sapply(st,function(x) x[3])
    valuesort=sort(valuef)
    index=which(valuef==valuesort[1])
    est0.05[[i]]=dpd(smp,0.05,st[[index]][1:2])
    for(j in 2:length(valuef)){
      if(est0.05[[i]]=="Start with new solution") {
        index = which(valuef == valuesort[j])
        est0.05[[i]]=dpd(smp,0.05,st[[index]][1:2])
      }
      else if (abs(est0.05[[i]][1]) > 1) {
        index = which(valuef == valuesort[j])
        est0.05[[i]]=dpd(smp,0.05,st[[index]][1:2])
      }
      else if (abs(est0.05[[i]][1]) > 2) {
        index = which(valuef == valuesort[j])
        est0.05[[i]]=dpd(smp,0.05,st[[index]][1:2])
      }
      else{
        break
      }
    }
    
    
    
    
    #if(est0.05[[i]]=="Start with new solution"){next}
    st=checkdpd(smp,0.1)
    valuef=sapply(st,function(x) x[3])
    valuesort=sort(valuef)
    index=which(valuef==valuesort[1])
    est0.1[[i]]=dpd(smp,0.1,st[[index]][1:2])
    for(j in 2:length(valuef)){
      if(est0.1[[i]]=="Start with new solution") {
        index = which(valuef == valuesort[j])
        est0.1[[i]]=dpd(smp,0.1,st[[index]][1:2])
      }
      else if (abs(est0.1[[i]][1]) > 1) {
        index = which(valuef == valuesort[j])
        est0.1[[i]]=dpd(smp,0.1,st[[index]][1:2])
      }
      else if (abs(est0.1[[i]][1]) > 2) {
        index = which(valuef == valuesort[j])
        est0.1[[i]]=dpd(smp,0.1,st[[index]][1:2])
      }
      else{
        break
      }
    }
    #if(est0.1[[i]]=="Start with new solution"){next}
    st=checkdpd(smp,0.25)
    valuef=sapply(st,function(x) x[3])
    valuesort=sort(valuef)
    index=which(valuef==valuesort[1])
    est0.25[[i]]=dpd(smp,0.25,st[[index]][1:2])
    for(j in 2:length(valuef)){
      if(est0.25[[i]]=="Start with new solution") {
        index = which(valuef == valuesort[j])
        est0.25[[i]]=dpd(smp,0.25,st[[index]][1:2])
      }
      else if (abs(est0.25[[i]][1]) > 1.5) {
        index = which(valuef == valuesort[j])
        est0.25[[i]]=dpd(smp,0.25,st[[index]][1:2])
      }
      else if (abs(est0.25[[i]][1]) > 2) {
        index = which(valuef == valuesort[j])
        est0.25[[i]]=dpd(smp,0.25,st[[index]][1:2])
      }
      else{
        break
      }
    }
    #if(est0.25[[i]]=="Start with new solution"){next}
    st=checkdpd(smp,0.5)
    valuef=sapply(st,function(x) x[3])
    valuesort=sort(valuef)
    index=which(valuef==valuesort[1])
    est0.5[[i]]=dpd(smp,0.5,st[[index]][1:2])
    for(j in 2:length(valuef)){
      if(est0.5[[i]]=="Start with new solution") {
        index = which(valuef == valuesort[j])
        est0.5[[i]]=dpd(smp,0.5,st[[index]][1:2])
      }
      else if (abs(est0.5[[i]][1]) > 1.5) {
        index = which(valuef == valuesort[j])
        est0.5[[i]]=dpd(smp,0.5,st[[index]][1:2])
      }
      else if (abs(est0.5[[i]][1]) > 2) {
        index = which(valuef == valuesort[j])
        est0.5[[i]]=dpd(smp,0.5,st[[index]][1:2])
      }
      else{
        break
      }
    }
    #if(est0.5[[i]]=="Start with new solution"){next}
    st=checkdpd(smp,1)
    valuef=sapply(st,function(x) x[3])
    valuesort=sort(valuef)
    index=which(valuef==valuesort[1])
    est1[[i]]=dpd(smp,1,st[[index]][1:2])
    for(j in 2:length(valuef)){
      if(est1[[i]]=="Start with new solution") {
        index = which(valuef == valuesort[j])
        est1[[i]]=dpd(smp,1,st[[index]][1:2])
      }
      else if (abs(est1[[i]][1]) > 1.5) {
        index = which(valuef == valuesort[j])
        est1[[i]]=dpd(smp,1,st[[index]][1:2])
      }
      else if (abs(est1[[i]][1]) > 2) {
        index = which(valuef == valuesort[j])
        est1[[i]]=dpd(smp,1,st[[index]][1:2])
      }
      else{
        break
      }
    }
    #if(est1[[i]]=="Start with new solution"){next}
    #if(abs(est1[[i]][2])>2){next}
    #if(abs(est1[[i]][1])>1){next}
    cat(i,est0[[i]][1],est0.02[[i]][1],est0.05[[i]][1],est0.1[[i]][1]
        ,est0.25[[i]][1],est0.5[[i]][1],est1[[i]][1],
        file="testing/mutstcheck20.txt",sep="\t",append=TRUE)
    
    cat("\n",file="testing/mutstcheck20.txt",append=TRUE)
    i=i+1
  }
  mu0=sapply(est0,function(x) x[1])
  mu0.02=sapply(est0.02,function(x) x[1])
  mu0.05=sapply(est0.05,function(x) x[1])
  mu0.1=sapply(est0.1,function(x) x[1])
  mu0.25=sapply(est0.25,function(x) x[1])
  mu0.5=sapply(est0.5,function(x) x[1])
  mu1=sapply(est1,function(x) x[1])
  sigma0=sapply(est0,function(x) x[2])
  sigma0.02=sapply(est0.02,function(x) x[2])
  sigma0.05=sapply(est0.05,function(x) x[2])
  sigma0.1=sapply(est0.1,function(x) x[2])
  sigma0.25=sapply(est0.25,function(x) x[2])
  sigma0.5=sapply(est0.5,function(x) x[2])
  sigma1=sapply(est1,function(x) x[2])
  dmu=data.frame(mu0,mu0.02,mu0.05,mu0.1,mu0.25,mu0.5,mu1)
  dsigma=data.frame(sigma0,sigma0.02,sigma0.05,sigma0.1,sigma0.25,sigma0.5,sigma1)
  write.xlsx(dmu,file="testing/n20check.xlsx",sheetName="Sheet1ldpd",append=T)
  write.xlsx(dsigma,file="testing/n20check.xlsx",sheetName="Sheet2ldpd",append=T)
  cat("\n\nRelative Effeciency for n=20",file="testing/effeciencycheck.txt",sep="\n",append=T)
  cat("\nAlpha               relefmu            relefsigma",file="testing/effeciencycheck.txt",append=TRUE)
  relefmu=c()
  relefsigma=c()
  relefmu[1]=var(mu0,na.rm=T)/var(mu0,na.rm=T)
  relefmu[2]=var(mu0,na.rm=T)/var(mu0.02,na.rm=T)
  relefmu[3]=var(mu0,na.rm=T)/var(mu0.05,na.rm=T)
  relefmu[4]=var(mu0,na.rm=T)/var(mu0.1,na.rm=T)
  relefmu[5]=var(mu0,na.rm=T)/var(mu0.25,na.rm=T)
  relefmu[6]=var(mu0,na.rm=T)/var(mu0.5,na.rm=T)
  relefmu[7]=var(mu0,na.rm=T)/var(mu1,na.rm=T)
  relefsigma[1]=var(sigma0,na.rm=T)/var(sigma0,na.rm=T)
  relefsigma[2]=var(sigma0,na.rm=T)/var(sigma0.02,na.rm=T)
  relefsigma[3]=var(sigma0,na.rm=T)/var(sigma0.05,na.rm=T)
  relefsigma[4]=var(sigma0,na.rm=T)/var(sigma0.1,na.rm=T)
  relefsigma[5]=var(sigma0,na.rm=T)/var(sigma0.25,na.rm=T)
  relefsigma[6]=var(sigma0,na.rm=T)/var(sigma0.5,na.rm=T)
  relefsigma[7]=var(sigma0,na.rm=T)/var(sigma1,na.rm=T)
  for(i in 1:7){
    cat("\nAlpha=",x[i],"       ",relefmu[i],"        ",relefsigma[i],file="testing/effeciencycheck.txt",append=TRUE)
  }
}