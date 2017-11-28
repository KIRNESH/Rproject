source("solve01.R")
source("genrndprob.R")
library(xlsx)
simwithprob=function(){
  est0dpd=list()
  est0.02dpd=list()
  est0.05dpd=list()
  est0.1dpd=list()
  est0.25dpd=list()
  est0.5dpd=list()
  est1dpd=list()
  
  i=2
  cntmean=6
  cntsigma=1
  prob=0.8
  sample=read.xlsx("testing/sample100.xlsx",sheetName = "Sheet1")
  while(i<=1001){
    smp=sample[,i]
    cat("\n",smp,file="testing/mutstprob100dpd.txt",sep=",",append=TRUE)
    cat("\n",file="testing/mutstprob100dpd.txt",append=TRUE)
    estdpd=solve01(smp,dpd,checkdpd)
   
    est0dpd[[i]]=estdpd[[1]]
    est0.02dpd[[i]]=estdpd[[2]]
    est0.05dpd[[i]]=estdpd[[3]]
    est0.1dpd[[i]]=estdpd[[4]]
    est0.25dpd[[i]]=estdpd[[5]]
    est0.5dpd[[i]]=estdpd[[6]]
    est1dpd[[i]]=estdpd[[7]]
    
    if(est0dpd[[i]]=="Start with new solution" || abs(est0dpd[[i]][1])>4 || abs(est0dpd[[i]][2])>5){est0dpd[[i]]=c(NA,NA)}
    if(est0.02dpd[[i]]=="Start with new solution" || abs(est0.02dpd[[i]][1])>4 
       || abs(est0.02dpd[[i]][2])>5){est0.02dpd[[i]]=c(NA,NA)}
    if(est0.05dpd[[i]]=="Start with new solution" || abs(est0.05dpd[[i]][1])>4 
       || abs(est0.05dpd[[i]][2])>5){est0.05dpd[[i]]=c(NA,NA)}
    if(est0.1dpd[[i]]=="Start with new solution" || abs(est0.1dpd[[i]][1])>4 
       || abs(est0.1dpd[[i]][2])>5){est0.1dpd[[i]]=c(NA,NA)}
    if(est0.25dpd[[i]]=="Start with new solution" || abs(est0.25dpd[[i]][1])>4 
       || abs(est0.25dpd[[i]][2])>5){est0.25dpd[[i]]=c(NA,NA)}
    if(est0.5dpd[[i]]=="Start with new solution" || abs(est0.5dpd[[i]][1])>4 
       || abs(est0.5dpd[[i]][2])>5){est0.5dpd[[i]]=c(NA,NA)}
    if(est1dpd[[i]]=="Start with new solution" || abs(est1dpd[[i]][1])>4 
       || abs(est1dpd[[i]][2])>5){est1dpd[[i]]=c(NA,NA)}
    
    cat(i,est0dpd[[i]][1],est0.02dpd[[i]][1],est0.05dpd[[i]][1],est0.1dpd[[i]][1]
        ,est0.25dpd[[i]][1],est0.5dpd[[i]][1],est1dpd[[i]][1],
        file="testing/mutstprob100dpd.txt",sep="\t",append=TRUE)
    
    
    cat("\n",file="testing/mutstprob100dpd.txt",append=TRUE)
    i=i+1
  }
  mu0dpd=sapply(est0dpd,function(x) x[1])
  mu0.02dpd=sapply(est0.02dpd,function(x) x[1])
  mu0.05dpd=sapply(est0.05dpd,function(x) x[1])
  mu0.1dpd=sapply(est0.1dpd,function(x) x[1])
  mu0.25dpd=sapply(est0.25dpd,function(x) x[1])
  mu0.5dpd=sapply(est0.5dpd,function(x) x[1])
  mu1dpd=sapply(est1dpd,function(x) x[1])
  sigma0dpd=sapply(est0dpd,function(x) x[2])
  sigma0.02dpd=sapply(est0.02dpd,function(x) x[2])
  sigma0.05dpd=sapply(est0.05dpd,function(x) x[2])
  sigma0.1dpd=sapply(est0.1dpd,function(x) x[2])
  sigma0.25dpd=sapply(est0.25dpd,function(x) x[2])
  sigma0.5dpd=sapply(est0.5dpd,function(x) x[2])
  sigma1dpd=sapply(est1dpd,function(x) x[2])
  
  mu0dpd=unlist(mu0dpd)
  mu0.02dpd=unlist(mu0.02dpd)
  mu0.05dpd=unlist(mu0.05dpd)
  mu0.1dpd=unlist(mu0.1dpd)
  mu0.25dpd=unlist(mu0.25dpd)
  mu0.5dpd=unlist(mu0.5dpd)
  mu1dpd=unlist(mu1dpd)
  
  sigma0dpd=unlist(sigma0dpd)
  sigma0.02dpd=unlist(sigma0.02dpd)
  sigma0.05dpd=unlist(sigma0.05dpd)
  sigma0.1dpd=unlist(sigma0.1dpd)
  sigma0.25dpd=unlist(sigma0.25dpd)
  sigma0.5dpd=unlist(sigma0.5dpd)
  sigma1dpd=unlist(sigma1dpd)
  
  mudpd=data.frame(mu0dpd,mu0.02dpd,mu0.05dpd,mu0.1dpd,mu0.25dpd,mu0.5dpd,mu1dpd)
  sigmadpd=data.frame(sigma0dpd,sigma0.02dpd,sigma0.05dpd,sigma0.1dpd,sigma0.25dpd,sigma0.5dpd,sigma1dpd)
  
  
  
  write.xlsx(mudpd,file="testing/nprob100.xlsx",sheetName="Sheet1dpd",append=T)
  write.xlsx(sigmadpd,file="testing/nprob100.xlsx",sheetName="Sheet2dpd",append=T)
  
  
  
  avgbiasmudpd=c()
  avgmsemudpd=c()
  avgbiassigdpd=c()
  avgmsesigdpd=c()
  avgbiasmudpd[1]=mean((mu0dpd-rep(0,500)),na.rm=T)
  avgbiasmudpd[2]=mean((mu0.02dpd-rep(0,500)),na.rm=T)
  avgbiasmudpd[3]=mean((mu0.05dpd-rep(0,500)),na.rm=T)
  avgbiasmudpd[4]=mean((mu0.1dpd-rep(0,500)),na.rm=T)
  avgbiasmudpd[5]=mean((mu0.25dpd-rep(0,500)),na.rm=T)
  avgbiasmudpd[6]=mean((mu0.5dpd-rep(0,500)),na.rm=T)
  avgbiasmudpd[7]=mean((mu1dpd-rep(0,500)),na.rm=T)
  
  avgmsemudpd[1]=mean((mu0dpd-rep(0,500))^2,na.rm=T)
  avgmsemudpd[2]=mean((mu0.02dpd-rep(0,500))^2,na.rm=T)
  avgmsemudpd[3]=mean((mu0.05dpd-rep(0,500))^2,na.rm=T)
  avgmsemudpd[4]=mean((mu0.1dpd-rep(0,500))^2,na.rm=T)
  avgmsemudpd[5]=mean((mu0.25dpd-rep(0,500))^2,na.rm=T)
  avgmsemudpd[6]=mean((mu0.5dpd-rep(0,500))^2,na.rm=T)
  avgmsemudpd[7]=mean((mu1dpd-rep(0,500))^2,na.rm=T)
  
  avgbiassigdpd[1]=mean((sigma0dpd-rep(1,500)),na.rm=T)
  avgbiassigdpd[2]=mean((sigma0.02dpd-rep(1,500)),na.rm=T)
  avgbiassigdpd[3]=mean((sigma0.05dpd-rep(1,500)),na.rm=T)
  avgbiassigdpd[4]=mean((sigma0.1dpd-rep(1,500)),na.rm=T)
  avgbiassigdpd[5]=mean((sigma0.25dpd-rep(1,500)),na.rm=T)
  avgbiassigdpd[6]=mean((sigma0.5dpd-rep(1,500)),na.rm=T)
  avgbiassigdpd[7]=mean((sigma1dpd-rep(1,500)),na.rm=T)
  
  avgmsesigdpd[1]=mean((sigma0dpd-rep(1,500))^2,na.rm=T)
  avgmsesigdpd[2]=mean((sigma0.02dpd-rep(1,500))^2,na.rm=T)
  avgmsesigdpd[3]=mean((sigma0.05dpd-rep(1,500))^2,na.rm=T)
  avgmsesigdpd[4]=mean((sigma0.1dpd-rep(1,500))^2,na.rm=T)
  avgmsesigdpd[5]=mean((sigma0.25dpd-rep(1,500))^2,na.rm=T)
  avgmsesigdpd[6]=mean((sigma0.5dpd-rep(1,500))^2,na.rm=T)
  avgmsesigdpd[7]=mean((sigma1dpd-rep(1,500))^2,na.rm=T)
  
  
  
  avgbiasmudpd=round(avgbiasmudpd,5)
  avgbiassigdpd=round(avgbiassigdpd,5)
  avgmsemudpd=round(avgmsemudpd,5)
  avgmsesigdpd=round(avgmsesigdpd,5)
  
  

  alp=c("0.00","0.02","0.05","0.10","0.25","0.50","1.00")
  
  cat("\n\nAverage Bias and MSE for DPD(n=20)",file="testing/biasmse.txt",sep="\n",append=T)
  cat("\nAlpha       Avg Bias(mu)       Avg MSE(mu)       Avg Bias(sigma)       Avg MSE(sigma)",file="testing/biasmse.txt",append=TRUE)
   
  for(i in 1:7){
    cat("\n",alp[i],"        ",avgbiasmudpd[i],"            ",avgmsemudpd[i],"           "
        ,avgbiassigdpd[i],"               ",avgmsesigdpd[i],file="testing/biasmse.txt",append=TRUE)
  }
  
 
}