source("solve01gendpd.R")
source("genrndprob.R")
library(xlsx)
simgendpd=function(beta){
  cntmean=6
  cntsigma=1
  prob=0.8
  sample=read.xlsx("testing/sample.xlsx",sheetName = "Sheet1")
  est0gendpd=list()
  est0.2gendpd=list()
  est0.4gendpd=list()
  est0.6gendpd=list()
  est0.8gendpd=list()
  est1gendpd=list()
  
  i=2
  
  while(i<=1001){
    print(i)
    smp=sample[,i]
    cat("\n",smp,file="testing/mutst100gdpd4.txt",sep=",",append=TRUE)
    cat("\n",file="testing/mutst100gdpd4.txt",append=TRUE)
    estgendpd=solve01gendpdrt(smp,beta)
    
    est0gendpd[[i]]=estgendpd[[1]]
    est0.2gendpd[[i]]=estgendpd[[2]]
    est0.4gendpd[[i]]=estgendpd[[3]]
    est0.6gendpd[[i]]=estgendpd[[4]]
    est0.8gendpd[[i]]=estgendpd[[5]]
    est1gendpd[[i]]=estgendpd[[6]]
    
    
    if(est0gendpd[[i]]=="Start with new solution" || abs(est0gendpd[[i]][1])>4
       || abs(est0gendpd[[i]][2])>5){est0gendpd[[i]]=c(NA,NA)}
    if(est0.2gendpd[[i]]=="Start with new solution" || abs(est0.2gendpd[[i]][1])>4 
       || abs(est0.2gendpd[[i]][2])>5){est0.2gendpd[[i]]=c(NA,NA)}
    if(est0.4gendpd[[i]]=="Start with new solution" || abs(est0.4gendpd[[i]][1])>4 
       || abs(est0.4gendpd[[i]][2])>5){est0.4gendpd[[i]]=c(NA,NA)}
    if(est0.6gendpd[[i]]=="Start with new solution" || abs(est0.6gendpd[[i]][1])>4 
       || abs(est0.6gendpd[[i]][2])>5){est0.6gendpd[[i]]=c(NA,NA)}
    if(est0.8gendpd[[i]]=="Start with new solution" || abs(est0.8gendpd[[i]][1])>4 
       || abs(est0.8gendpd[[i]][2])>5){est0.8gendpd[[i]]=c(NA,NA)}
    if(est1gendpd[[i]]=="Start with new solution" || abs(est1gendpd[[i]][1])>4 
       || abs(est1gendpd[[i]][2])>5){est1gendpd[[i]]=c(NA,NA)}
    
    cat(i,est0gendpd[[i]][1],est0.2gendpd[[i]][1],est0.4gendpd[[i]][1]
        ,est0.6gendpd[[i]][1],est0.8gendpd[[i]][1],est1gendpd[[i]][1],
        file="testing/mutst100gdpd4.txt",sep="\t",append=TRUE)
    
    
    cat("\n",file="testing/mutst100gdpd4.txt",append=TRUE)
    i=i+1
  }
  mu0gendpd=sapply(est0gendpd,function(x) x[1])
  mu0.2gendpd=sapply(est0.2gendpd,function(x) x[1])
  mu0.4gendpd=sapply(est0.4gendpd,function(x) x[1])
  mu0.6gendpd=sapply(est0.6gendpd,function(x) x[1])
  mu0.8gendpd=sapply(est0.8gendpd,function(x) x[1])
  mu1gendpd=sapply(est1gendpd,function(x) x[1])
  
  sigma0gendpd=sapply(est0gendpd,function(x) x[2])
  sigma0.2gendpd=sapply(est0.2gendpd,function(x) x[2])
  sigma0.4gendpd=sapply(est0.4gendpd,function(x) x[2])
  sigma0.6gendpd=sapply(est0.6gendpd,function(x) x[2])
  sigma0.8gendpd=sapply(est0.8gendpd,function(x) x[2])
  sigma1gendpd=sapply(est1gendpd,function(x) x[2])
  
  mu0gendpd=unlist(mu0gendpd)
  mu0.2gendpd=unlist(mu0.2gendpd)
  mu0.4gendpd=unlist(mu0.4gendpd)
  mu0.6gendpd=unlist(mu0.6gendpd)
  mu0.8gendpd=unlist(mu0.8gendpd)
  mu1gendpd=unlist(mu1gendpd)
  
  sigma0gendpd=unlist(sigma0gendpd)
  sigma0.2gendpd=unlist(sigma0.2gendpd)
  sigma0.4gendpd=unlist(sigma0.4gendpd)
  sigma0.6gendpd=unlist(sigma0.6gendpd)
  sigma0.8gendpd=unlist(sigma0.8gendpd)
  sigma1gendpd=unlist(sigma1gendpd)
  
  mugendpd=data.frame(mu0gendpd,mu0.2gendpd,mu0.4gendpd,mu0.6gendpd,mu0.8gendpd,
                      mu1gendpd)
  sigmagendpd=data.frame(sigma0gendpd,sigma0.2gendpd,sigma0.4gendpd,
                         sigma0.6gendpd,sigma0.8gendpd,sigma1gendpd)
  
  
  
  write.xlsx(mugendpd,file="testing/npure100gendpdmu.xlsx",sheetName="Sheetgdp4",append=T)
  write.xlsx(sigmagendpd,file="testing/npure100gendpdsigma.xlsx",sheetName="Sheetgdp4",append=T)
  
  
  
  avgbiasmugendpd=c()
  avgmsemugendpd=c()
  avgbiassiggendpd=c()
  avgmsesiggendpd=c()
  avgbiasmugendpd[1]=mean((mu0gendpd-rep(0,500)),na.rm=T)
  avgbiasmugendpd[2]=mean((mu0.2gendpd-rep(0,500)),na.rm=T)
  avgbiasmugendpd[3]=mean((mu0.4gendpd-rep(0,500)),na.rm=T)
  avgbiasmugendpd[4]=mean((mu0.6gendpd-rep(0,500)),na.rm=T)
  avgbiasmugendpd[5]=mean((mu0.8gendpd-rep(0,500)),na.rm=T)
  avgbiasmugendpd[6]=mean((mu1gendpd-rep(0,500)),na.rm=T)
  
  avgmsemugendpd[1]=mean((mu0gendpd-rep(0,500))^2,na.rm=T)
  avgmsemugendpd[2]=mean((mu0.2gendpd-rep(0,500))^2,na.rm=T)
  avgmsemugendpd[3]=mean((mu0.4gendpd-rep(0,500))^2,na.rm=T)
  avgmsemugendpd[4]=mean((mu0.6gendpd-rep(0,500))^2,na.rm=T)
  avgmsemugendpd[5]=mean((mu0.8gendpd-rep(0,500))^2,na.rm=T)
  avgmsemugendpd[6]=mean((mu1gendpd-rep(0,500))^2,na.rm=T)
  
  
  avgbiassiggendpd[1]=mean((sigma0gendpd-rep(1,500)),na.rm=T)
  avgbiassiggendpd[2]=mean((sigma0.2gendpd-rep(1,500)),na.rm=T)
  avgbiassiggendpd[3]=mean((sigma0.4gendpd-rep(1,500)),na.rm=T)
  avgbiassiggendpd[4]=mean((sigma0.6gendpd-rep(1,500)),na.rm=T)
  avgbiassiggendpd[5]=mean((sigma0.8gendpd-rep(1,500)),na.rm=T)
  avgbiassiggendpd[6]=mean((sigma1gendpd-rep(1,500)),na.rm=T)
  
  
  avgmsesiggendpd[1]=mean((sigma0gendpd-rep(1,500))^2,na.rm=T)
  avgmsesiggendpd[2]=mean((sigma0.2gendpd-rep(1,500))^2,na.rm=T)
  avgmsesiggendpd[3]=mean((sigma0.4gendpd-rep(1,500))^2,na.rm=T)
  avgmsesiggendpd[4]=mean((sigma0.6gendpd-rep(1,500))^2,na.rm=T)
  avgmsesiggendpd[5]=mean((sigma0.8gendpd-rep(1,500))^2,na.rm=T)
  avgmsesiggendpd[6]=mean((sigma1gendpd-rep(1,500))^2,na.rm=T)

  
  
  
  avgbiasmugendpd=round(avgbiasmugendpd,5)
  avgbiassiggendpd=round(avgbiassiggendpd,5)
  avgmsemugendpd=round(avgmsemugendpd,5)
  avgmsesiggendpd=round(avgmsesiggendpd,5)
  
  
  
  alp=c("0.0","0.2","0.4","0.6","0.8","1.0")
  
  #cat("\n\nAverage Bias and MSE for gendpd(n=100) pure mu",file="testing/biasmsemupurgdpd100.txt",sep="\n",append=T)
  #cat("\nbeta                      Alpha0.0         Alpha0.2         Alpha0.4         Alpha0.6         Alpha0.8         Alpha1.0",file="testing/biasmsemupuregdpd100.txt",append=TRUE)
  
  
  cat("\n",beta,"     Average Bias       ",avgbiasmugendpd[1],
      "        ",avgbiasmugendpd[2],"         ",avgbiasmugendpd[3],
      "        ",avgbiasmugendpd[4],"        ",avgbiasmugendpd[5],
      "        ",avgbiasmugendpd[6],file="testing/biasmsemupuregdpd100.txt",append=TRUE)
  
  cat("\n",beta,"     Average MSE       ",avgmsemugendpd[1],
      "        ",avgmsemugendpd[2],"         ",avgmsemugendpd[3],
      "        ",avgmsemugendpd[4],"        ",avgmsemugendpd[5],
      "        ",avgmsemugendpd[6],"        ",file="testing/biasmsemupuregdpd100.txt",append=TRUE)
  
  #cat("\n\nAverage Bias and MSE for gendpd(n=100) pure sigma",file="testing/biasmsesigpuregdpd100.txt",sep="\n",append=T)
  #cat("\nbeta                      Alpha0.0         Alpha0.2         Alpha0.4         Alpha0.6         Alpha0.8         Alpha1.0",file="testing/biasmsesigpuregdpd100.txt",append=TRUE)
  
  
  cat("\n",beta,"     Average Bias       ",avgbiassiggendpd[1],
      "        ",avgbiassiggendpd[2],"         ",avgbiassiggendpd[3],
      "        ",avgbiassiggendpd[4],"        ",avgbiassiggendpd[5],
      "        ",avgbiassiggendpd[6],"        ",file="testing/biasmsesigpuregdpd100.txt",append=TRUE)
  
  cat("\n",beta,"     Average MSE       ",avgmsesiggendpd[1],
      "        ",avgmsesiggendpd[2],"         ",avgmsesiggendpd[3],
      "        ",avgmsesiggendpd[4],"        ",avgmsesiggendpd[5],
      "        ",avgmsesiggendpd[6],"        ",file="testing/biasmsesigpuregdpd100.txt",append=TRUE)
}