source("solve01genldpd.R")
source("genrndprob.R")
library(xlsx)
simgenldpd=function(beta){
  
  
  cntmean=6
  cntsigma=1
  prob=0.8
  sample=read.xlsx("testing/sample.xlsx",sheetName = "Sheet1")
  est0genldpd=list()
  est0.2genldpd=list()
  est0.4genldpd=list()
  est0.6genldpd=list()
  est0.8genldpd=list()
  est1genldpd=list()
  
  i=2
  while(i<=1001){
    print(i)
    smp=sample[,i]
    cat("\n",smp,file="testing/mutst100gldpd4.txt",sep=",",append=TRUE)
    cat("\n",file="testing/mutst100gldpd4.txt",append=TRUE)
    estgenldpd=solve01genldpd(smp,beta)
    
    est0genldpd[[i]]=estgenldpd[[1]]
    est0.2genldpd[[i]]=estgenldpd[[2]]
    est0.4genldpd[[i]]=estgenldpd[[3]]
    est0.6genldpd[[i]]=estgenldpd[[4]]
    est0.8genldpd[[i]]=estgenldpd[[5]]
    est1genldpd[[i]]=estgenldpd[[6]]
    
    
    if(est0genldpd[[i]]=="Start with new solution" || abs(est0genldpd[[i]][1])>4
       || abs(est0genldpd[[i]][2])>5){est0genldpd[[i]]=c(NA,NA)}
    if(est0.2genldpd[[i]]=="Start with new solution" || abs(est0.2genldpd[[i]][1])>4 
       || abs(est0.2genldpd[[i]][2])>5){est0.2genldpd[[i]]=c(NA,NA)}
    if(est0.4genldpd[[i]]=="Start with new solution" || abs(est0.4genldpd[[i]][1])>4 
       || abs(est0.4genldpd[[i]][2])>5){est0.4genldpd[[i]]=c(NA,NA)}
    if(est0.6genldpd[[i]]=="Start with new solution" || abs(est0.6genldpd[[i]][1])>4 
       || abs(est0.6genldpd[[i]][2])>5){est0.6genldpd[[i]]=c(NA,NA)}
    if(est0.8genldpd[[i]]=="Start with new solution" || abs(est0.8genldpd[[i]][1])>4 
       || abs(est0.8genldpd[[i]][2])>5){est0.8genldpd[[i]]=c(NA,NA)}
    if(est1genldpd[[i]]=="Start with new solution" || abs(est1genldpd[[i]][1])>4 
       || abs(est1genldpd[[i]][2])>5){est1genldpd[[i]]=c(NA,NA)}
    
    cat(i,est0genldpd[[i]][1],est0.2genldpd[[i]][1],est0.4genldpd[[i]][1]
        ,est0.6genldpd[[i]][1],est0.8genldpd[[i]][1],est1genldpd[[i]][1],
        file="testing/mutst100gldpd4.txt",sep="\t",append=TRUE)
    
    
    cat("\n",file="testing/mutst100gldpd4.txt",append=TRUE)
    i=i+1
  }
  mu0genldpd=sapply(est0genldpd,function(x) x[1])
  mu0.2genldpd=sapply(est0.2genldpd,function(x) x[1])
  mu0.4genldpd=sapply(est0.4genldpd,function(x) x[1])
  mu0.6genldpd=sapply(est0.6genldpd,function(x) x[1])
  mu0.8genldpd=sapply(est0.8genldpd,function(x) x[1])
  mu1genldpd=sapply(est1genldpd,function(x) x[1])
  
  sigma0genldpd=sapply(est0genldpd,function(x) x[2])
  sigma0.2genldpd=sapply(est0.2genldpd,function(x) x[2])
  sigma0.4genldpd=sapply(est0.4genldpd,function(x) x[2])
  sigma0.6genldpd=sapply(est0.6genldpd,function(x) x[2])
  sigma0.8genldpd=sapply(est0.8genldpd,function(x) x[2])
  sigma1genldpd=sapply(est1genldpd,function(x) x[2])
  
  mu0genldpd=unlist(mu0genldpd)
  mu0.2genldpd=unlist(mu0.2genldpd)
  mu0.4genldpd=unlist(mu0.4genldpd)
  mu0.6genldpd=unlist(mu0.6genldpd)
  mu0.8genldpd=unlist(mu0.8genldpd)
  mu1genldpd=unlist(mu1genldpd)
  
  sigma0genldpd=unlist(sigma0genldpd)
  sigma0.2genldpd=unlist(sigma0.2genldpd)
  sigma0.4genldpd=unlist(sigma0.4genldpd)
  sigma0.6genldpd=unlist(sigma0.6genldpd)
  sigma0.8genldpd=unlist(sigma0.8genldpd)
  sigma1genldpd=unlist(sigma1genldpd)
  
  mugenldpd=data.frame(mu0genldpd,mu0.2genldpd,mu0.4genldpd,mu0.6genldpd,mu0.8genldpd,
                      mu1genldpd)
  sigmagenldpd=data.frame(sigma0genldpd,sigma0.2genldpd,sigma0.4genldpd,
                         sigma0.6genldpd,sigma0.8genldpd,sigma1genldpd)
  
  
  
  write.xlsx(mugenldpd,file="testing/npure100genldpdmu.xlsx",sheetName="Sheetgldp4",append=T)
  write.xlsx(sigmagenldpd,file="testing/npure100genldpdsigma.xlsx",sheetName="Sheetgldp4",append=T)
  
  
  
  avgbiasmugenldpd=c()
  avgmsemugenldpd=c()
  avgbiassiggenldpd=c()
  avgmsesiggenldpd=c()
  avgbiasmugenldpd[1]=mean((mu0genldpd-rep(0,500)),na.rm=T)
  avgbiasmugenldpd[2]=mean((mu0.2genldpd-rep(0,500)),na.rm=T)
  avgbiasmugenldpd[3]=mean((mu0.4genldpd-rep(0,500)),na.rm=T)
  avgbiasmugenldpd[4]=mean((mu0.6genldpd-rep(0,500)),na.rm=T)
  avgbiasmugenldpd[5]=mean((mu0.8genldpd-rep(0,500)),na.rm=T)
  avgbiasmugenldpd[6]=mean((mu1genldpd-rep(0,500)),na.rm=T)
  
  avgmsemugenldpd[1]=mean((mu0genldpd-rep(0,500))^2,na.rm=T)
  avgmsemugenldpd[2]=mean((mu0.2genldpd-rep(0,500))^2,na.rm=T)
  avgmsemugenldpd[3]=mean((mu0.4genldpd-rep(0,500))^2,na.rm=T)
  avgmsemugenldpd[4]=mean((mu0.6genldpd-rep(0,500))^2,na.rm=T)
  avgmsemugenldpd[5]=mean((mu0.8genldpd-rep(0,500))^2,na.rm=T)
  avgmsemugenldpd[6]=mean((mu1genldpd-rep(0,500))^2,na.rm=T)
  
  
  avgbiassiggenldpd[1]=mean((sigma0genldpd-rep(1,500)),na.rm=T)
  avgbiassiggenldpd[2]=mean((sigma0.2genldpd-rep(1,500)),na.rm=T)
  avgbiassiggenldpd[3]=mean((sigma0.4genldpd-rep(1,500)),na.rm=T)
  avgbiassiggenldpd[4]=mean((sigma0.6genldpd-rep(1,500)),na.rm=T)
  avgbiassiggenldpd[5]=mean((sigma0.8genldpd-rep(1,500)),na.rm=T)
  avgbiassiggenldpd[6]=mean((sigma1genldpd-rep(1,500)),na.rm=T)
  
  
  avgmsesiggenldpd[1]=mean((sigma0genldpd-rep(1,500))^2,na.rm=T)
  avgmsesiggenldpd[2]=mean((sigma0.2genldpd-rep(1,500))^2,na.rm=T)
  avgmsesiggenldpd[3]=mean((sigma0.4genldpd-rep(1,500))^2,na.rm=T)
  avgmsesiggenldpd[4]=mean((sigma0.6genldpd-rep(1,500))^2,na.rm=T)
  avgmsesiggenldpd[5]=mean((sigma0.8genldpd-rep(1,500))^2,na.rm=T)
  avgmsesiggenldpd[6]=mean((sigma1genldpd-rep(1,500))^2,na.rm=T)
  
  
  
  
  avgbiasmugenldpd=round(avgbiasmugenldpd,5)
  avgbiassiggenldpd=round(avgbiassiggenldpd,5)
  avgmsemugenldpd=round(avgmsemugenldpd,5)
  avgmsesiggenldpd=round(avgmsesiggenldpd,5)
  
  
  
  alp=c("0.0","0.2","0.4","0.6","0.8","1.0")
  
  #cat("\n\nAverage Bias and MSE for genldpd(n=100) pure mu",file="testing/biasmsemupuregldpd100.txt",sep="\n",append=T)
  #cat("\nbeta                      Alpha0.0         Alpha0.2         Alpha0.4         Alpha0.6         Alpha0.8         Alpha1.0",file="testing/biasmsemupuregldpd100.txt",append=TRUE)
  
  
  cat("\n",beta,"     Average Bias       ",avgbiasmugenldpd[1],
      "        ",avgbiasmugenldpd[2],"         ",avgbiasmugenldpd[3],
      "        ",avgbiasmugenldpd[4],"        ",avgbiasmugenldpd[5],
      "        ",avgbiasmugenldpd[6],file="testing/biasmsemupuregldpd100.txt",append=TRUE)
  
  cat("\n",beta,"     Average MSE       ",avgmsemugenldpd[1],
      "        ",avgmsemugenldpd[2],"         ",avgmsemugenldpd[3],
      "        ",avgmsemugenldpd[4],"        ",avgmsemugenldpd[5],
      "        ",avgmsemugenldpd[6],"        ",file="testing/biasmsemupuregldpd100.txt",append=TRUE)
  
  #cat("\n\nAverage Bias and MSE for genldpd(n=100) pure sig",file="testing/biasmsesigpuregldpd100.txt",sep="\n",append=T)
  #cat("\nbeta                      Alpha0.0         Alpha0.2         Alpha0.4         Alpha0.6         Alpha0.8         Alpha1.0",file="testing/biasmsesigpuregldpd100.txt",append=TRUE)
  
  
  cat("\n",beta,"     Average Bias       ",avgbiassiggenldpd[1],
      "        ",avgbiassiggenldpd[2],"         ",avgbiassiggenldpd[3],
      "        ",avgbiassiggenldpd[4],"        ",avgbiassiggenldpd[5],
      "        ",avgbiassiggenldpd[6],"        ",file="testing/biasmsesigpuregldpd100.txt",append=TRUE)
  
  cat("\n",beta,"     Average MSE       ",avgmsesiggenldpd[1],
      "        ",avgmsesiggenldpd[2],"         ",avgmsesiggenldpd[3],
      "        ",avgmsesiggenldpd[4],"        ",avgmsesiggenldpd[5],
      "        ",avgmsesiggenldpd[6],"        ",file="testing/biasmsesigpuregldpd100.txt",append=TRUE)
}