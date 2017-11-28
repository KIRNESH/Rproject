source("solve01.R")
source("genrndprob.R")
library(xlsx)
simwithprobldpd=function(){
  est0ldpd=list()
  est0.02ldpd=list()
  est0.05ldpd=list()
  est0.1ldpd=list()
  est0.25ldpd=list()
  est0.5ldpd=list()
  est1ldpd=list()
  i=2
  cntmean=6
  cntsigma=1
  prob=0.8
  sample=read.xlsx("testing/sample.xlsx",sheetName = "Sheet1")
  while(i<=501){
    smp=sample[,i]
    cat(smp,file="testing/mutstprob20ldpd.txt",sep=",",append=TRUE)
    cat("\n",file="testing/mutstprob20ldpd.txt",append=TRUE)
    
    estldpd=solve01(smp,ldpd,checkldpd)
    
    est0ldpd[[i]]=estldpd[[1]]
    est0.02ldpd[[i]]=estldpd[[2]]
    est0.05ldpd[[i]]=estldpd[[3]]
    est0.1ldpd[[i]]=estldpd[[4]]
    est0.25ldpd[[i]]=estldpd[[5]]
    est0.5ldpd[[i]]=estldpd[[6]]
    est1ldpd[[i]]=estldpd[[7]]
    
    if(est0ldpd[[i]]=="Start with new solution" || abs(est0ldpd[[i]][1])>4
       || abs(est0ldpd[[i]][2])>5){est0dpd[[i]]=c(NA,NA)}
    if(est0.02ldpd[[i]]=="Start with new solution" || abs(est0.02ldpd[[i]][1])>4 
       || abs(est0.02ldpd[[i]][2])>5){est0.02ldpd[[i]]=c(NA,NA)}
    if(est0.05ldpd[[i]]=="Start with new solution" || abs(est0.05ldpd[[i]][1])>4 
       || abs(est0.05ldpd[[i]][2])>5){est0.05ldpd[[i]]=c(NA,NA)}
    if(est0.1ldpd[[i]]=="Start with new solution" || abs(est0.1ldpd[[i]][1])>4 
       || abs(est0.1ldpd[[i]][2])>5){est0.1ldpd[[i]]=c(NA,NA)}
    if(est0.25ldpd[[i]]=="Start with new solution" || abs(est0.25ldpd[[i]][1])>4 
       || abs(est0.25ldpd[[i]][2])>5){est0.25ldpd[[i]]=c(NA,NA)}
    if(est0.5ldpd[[i]]=="Start with new solution" || abs(est0.5ldpd[[i]][1])>4 
       || abs(est0.5ldpd[[i]][2])>5){est0.5ldpd[[i]]=c(NA,NA)}
    if(est1ldpd[[i]]=="Start with new solution" || abs(est1ldpd[[i]][1])>4 
       || abs(est1ldpd[[i]][2])>5){est1ldpd[[i]]=c(NA,NA)}
    
    cat(i,est0ldpd[[i]][1],est0.02ldpd[[i]][1],est0.05ldpd[[i]][1],est0.1ldpd[[i]][1]
        ,est0.25ldpd[[i]][1],est0.5ldpd[[i]][1],est1ldpd[[i]][1],
        file="testing/mutstprob20ldpd.txt",sep="\t",append=TRUE)
    
    cat("\n",file="testing/mutstprob20ldpd.txt",append=TRUE)
    i=i+1
  }
  

  mu0ldpd=sapply(est0ldpd,function(x) x[1])
  mu0.02ldpd=sapply(est0.02ldpd,function(x) x[1])
  mu0.05ldpd=sapply(est0.05ldpd,function(x) x[1])
  mu0.1ldpd=sapply(est0.1ldpd,function(x) x[1])
  mu0.25ldpd=sapply(est0.25ldpd,function(x) x[1])
  mu0.5ldpd=sapply(est0.5ldpd,function(x) x[1])
  mu1ldpd=sapply(est1ldpd,function(x) x[1])
  sigma0ldpd=sapply(est0ldpd,function(x) x[2])
  sigma0.02ldpd=sapply(est0.02ldpd,function(x) x[2])
  sigma0.05ldpd=sapply(est0.05ldpd,function(x) x[2])
  sigma0.1ldpd=sapply(est0.1ldpd,function(x) x[2])
  sigma0.25ldpd=sapply(est0.25ldpd,function(x) x[2])
  sigma0.5ldpd=sapply(est0.5ldpd,function(x) x[2])
  sigma1ldpd=sapply(est1ldpd,function(x) x[2])

  mu0ldpd=unlist(mu0ldpd)
  mu0.02ldpd=unlist(mu0.02ldpd)
  mu0.05ldpd=unlist(mu0.05ldpd)
  mu0.1ldpd=unlist(mu0.1ldpd)
  mu0.25ldpd=unlist(mu0.25ldpd)
  mu0.5ldpd=unlist(mu0.5ldpd)
  mu1ldpd=unlist(mu1ldpd)
  
  sigma0ldpd=unlist(sigma0ldpd)
  sigma0.02ldpd=unlist(sigma0.02ldpd)
  sigma0.05ldpd=unlist(sigma0.05ldpd)
  sigma0.1ldpd=unlist(sigma0.1ldpd)
  sigma0.25ldpd=unlist(sigma0.25ldpd)
  sigma0.5ldpd=unlist(sigma0.5ldpd)
  sigma1ldpd=unlist(sigma1ldpd)
  
  
  muldpd=data.frame(mu0ldpd,mu0.02ldpd,mu0.05ldpd,mu0.1ldpd,mu0.25ldpd,mu0.5ldpd,mu1ldpd)
  sigmaldpd=data.frame(sigma0ldpd,sigma0.02ldpd,sigma0.05ldpd,sigma0.1ldpd,sigma0.25ldpd,sigma0.5ldpd,sigma1ldpd)
 
  
  write.xlsx(muldpd,file="testing/nprob20.xlsx",sheetName="Sheet1ldpd",append=T)
  write.xlsx(sigmaldpd,file="testing/nprob20.xlsx",sheetName="Sheet2ldpd",append=T)
  
  
  
  avgbiasmuldpd=c()
  avgmsemuldpd=c()
  avgbiassigldpd=c()
  avgmsesigldpd=c()
  avgbiasmuldpd[1]=mean((mu0ldpd-rep(0,500)))
  avgbiasmuldpd[2]=mean((mu0.02ldpd-rep(0,500)))
  avgbiasmuldpd[3]=mean((mu0.05ldpd-rep(0,500)))
  avgbiasmuldpd[4]=mean((mu0.1ldpd-rep(0,500)))
  avgbiasmuldpd[5]=mean((mu0.25ldpd-rep(0,500)))
  avgbiasmuldpd[6]=mean((mu0.5ldpd-rep(0,500)))
  avgbiasmuldpd[7]=mean((mu1ldpd-rep(0,500)))
  
  avgmsemuldpd[1]=mean((mu0ldpd-rep(0,500))^2)
  avgmsemuldpd[2]=mean((mu0.02ldpd-rep(0,500))^2)
  avgmsemuldpd[3]=mean((mu0.05ldpd-rep(0,500))^2)
  avgmsemuldpd[4]=mean((mu0.1ldpd-rep(0,500))^2)
  avgmsemuldpd[5]=mean((mu0.25ldpd-rep(0,500))^2)
  avgmsemuldpd[6]=mean((mu0.5ldpd-rep(0,500))^2)
  avgmsemuldpd[7]=mean((mu1ldpd-rep(0,500))^2)
  
  avgbiassigldpd[1]=mean((sigma0ldpd-rep(1,500)))
  avgbiassigldpd[2]=mean((sigma0.02ldpd-rep(1,500)))
  avgbiassigldpd[3]=mean((sigma0.05ldpd-rep(1,500)))
  avgbiassigldpd[4]=mean((sigma0.1ldpd-rep(1,500)))
  avgbiassigldpd[5]=mean((sigma0.25ldpd-rep(1,500)))
  avgbiassigldpd[6]=mean((sigma0.5ldpd-rep(1,500)))
  avgbiassigldpd[7]=mean((sigma1ldpd-rep(1,500)))
  
  avgmsesigldpd[1]=mean((sigma0ldpd-rep(1,500))^2)
  avgmsesigldpd[2]=mean((sigma0.02ldpd-rep(1,500))^2)
  avgmsesigldpd[3]=mean((sigma0.05ldpd-rep(1,500))^2)
  avgmsesigldpd[4]=mean((sigma0.1ldpd-rep(1,500))^2)
  avgmsesigldpd[5]=mean((sigma0.25ldpd-rep(1,500))^2)
  avgmsesigldpd[6]=mean((sigma0.5ldpd-rep(1,500))^2)
  avgmsesigldpd[7]=mean((sigma1ldpd-rep(1,500))^2)
  
  
  
  avgbiasmuldpd=round(avgbiasmuldpd,5)
  avgbiassigldpd=round(avgbiassigldpd,5)
  avgmsemuldpd=round(avgmsemuldpd,5)
  avgmsesigldpd=round(avgmsesigldpd,5)
  
  alp=c("0.00","0.02","0.05","0.10","0.25","0.50","1.00")
  
  
  
  cat("\nAverage Bias and MSE for LDPD(n=20)",file="testing/biasmse.txt",sep="\n",append=T)
  cat("\nAlpha       Avg Bias(mu)       Avg MSE(mu)       Avg Bias(sigma)       Avg MSE(sigma)",file="testing/biasmse.txt",append=TRUE)
  
  for(i in 1:7){
    cat("\n",alp[i],"        ",avgbiasmuldpd[i],"            ",avgmsemuldpd[i],"           "
        ,avgbiassigldpd[i],"               ",avgmsesigldpd[i],file="testing/biasmse.txt",append=TRUE)
  }
  
}