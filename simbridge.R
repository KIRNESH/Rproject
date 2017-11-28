source("solve01bridge.R")
source("genrndprob.R")
library(xlsx)
simbridge=function(lam){
  
  est0.1bridge=list()
  est0.2bridge=list()
  est0.3bridge=list()
  est0.4bridge=list()
  est0.5bridge=list()
  est0.6bridge=list()
  est0.7bridge=list()
  est0.8bridge=list()
  est0.9bridge=list()
  est1bridge=list()
  
  i=2
  cntmean=6
  cntsigma=1
  prob=0.8
  sample=read.xlsx("testing/sample.xlsx",sheetName = "Sheet1")
  while(i<=501){
    smp=sample[,i]
    cat("\n",smp,file="testing/mutstprob20bri1.0.txt",sep=",",append=TRUE)
    cat("\n",file="testing/mutstprob20bri1.0.txt",append=TRUE)
    estbridge=solve01bridge(smp,lam)
    
    est0.1bridge[[i]]=estbridge[[1]]
    est0.2bridge[[i]]=estbridge[[2]]
    est0.3bridge[[i]]=estbridge[[3]]
    est0.4bridge[[i]]=estbridge[[4]]
    est0.5bridge[[i]]=estbridge[[5]]
    est0.6bridge[[i]]=estbridge[[6]]
    est0.7bridge[[i]]=estbridge[[7]]
    est0.8bridge[[i]]=estbridge[[8]]
    est0.9bridge[[i]]=estbridge[[9]]
    est1bridge[[i]]=estbridge[[10]]
    
    
    if(est0.1bridge[[i]]=="Start with new solution" || abs(est0.1bridge[[i]][1])>4
       || abs(est0.1bridge[[i]][2])>5){est0.1bridge[[i]]=c(NA,NA)}
    if(est0.2bridge[[i]]=="Start with new solution" || abs(est0.2bridge[[i]][1])>4 
       || abs(est0.2bridge[[i]][2])>5){est0.02bridge[[i]]=c(NA,NA)}
    if(est0.3bridge[[i]]=="Start with new solution" || abs(est0.3bridge[[i]][1])>4 
       || abs(est0.3bridge[[i]][2])>5){est0.3bridge[[i]]=c(NA,NA)}
    if(est0.4bridge[[i]]=="Start with new solution" || abs(est0.4bridge[[i]][1])>4 
       || abs(est0.4bridge[[i]][2])>5){est0.4bridge[[i]]=c(NA,NA)}
    if(est0.6bridge[[i]]=="Start with new solution" || abs(est0.6bridge[[i]][1])>4 
       || abs(est0.6bridge[[i]][2])>5){est0.6bridge[[i]]=c(NA,NA)}
    if(est0.5bridge[[i]]=="Start with new solution" || abs(est0.5bridge[[i]][1])>4 
       || abs(est0.5bridge[[i]][2])>5){est0.5bridge[[i]]=c(NA,NA)}
    if(est0.7bridge[[i]]=="Start with new solution" || abs(est0.7bridge[[i]][1])>4 
       || abs(est0.7bridge[[i]][2])>5){est0.7bridge[[i]]=c(NA,NA)}
    if(est0.8bridge[[i]]=="Start with new solution" || abs(est0.8bridge[[i]][1])>4 
       || abs(est0.8bridge[[i]][2])>5){est0.8bridge[[i]]=c(NA,NA)}
    if(est0.9bridge[[i]]=="Start with new solution" || abs(est0.9bridge[[i]][1])>4 
       || abs(est0.9bridge[[i]][2])>5){est0.9bridge[[i]]=c(NA,NA)}
    if(est1bridge[[i]]=="Start with new solution" || abs(est1bridge[[i]][1])>4 
       || abs(est1bridge[[i]][2])>5){est1bridge[[i]]=c(NA,NA)}
    
    cat(i,est0.1bridge[[i]][1],est0.2bridge[[i]][1],est0.3bridge[[i]][1],est0.4bridge[[i]][1]
        ,est0.5bridge[[i]][1],est0.6bridge[[i]][1],est0.7bridge[[i]][1],
        est0.8bridge[[i]][1],est0.9bridge[[i]][1],est1bridge[[i]][1],
        file="testing/mutstprob20bri1.0.txt",sep="\t",append=TRUE)
    
    
    cat("\n",file="testing/mutstprob20bri1.0.txt",append=TRUE)
    i=i+1
  }
  mu0.1bridge=sapply(est0.1bridge,function(x) x[1])
  mu0.2bridge=sapply(est0.2bridge,function(x) x[1])
  mu0.3bridge=sapply(est0.3bridge,function(x) x[1])
  mu0.4bridge=sapply(est0.4bridge,function(x) x[1])
  mu0.5bridge=sapply(est0.5bridge,function(x) x[1])
  mu0.6bridge=sapply(est0.6bridge,function(x) x[1])
  mu0.7bridge=sapply(est0.7bridge,function(x) x[1])
  mu0.8bridge=sapply(est0.8bridge,function(x) x[1])
  mu0.9bridge=sapply(est0.9bridge,function(x) x[1])
  mu1bridge=sapply(est1bridge,function(x) x[1])
  
  sigma0.1bridge=sapply(est0.1bridge,function(x) x[2])
  sigma0.2bridge=sapply(est0.2bridge,function(x) x[2])
  sigma0.3bridge=sapply(est0.3bridge,function(x) x[2])
  sigma0.4bridge=sapply(est0.4bridge,function(x) x[2])
  sigma0.5bridge=sapply(est0.5bridge,function(x) x[2])
  sigma0.6bridge=sapply(est0.6bridge,function(x) x[2])
  sigma0.7bridge=sapply(est0.7bridge,function(x) x[2])
  sigma0.8bridge=sapply(est0.8bridge,function(x) x[2])
  sigma0.9bridge=sapply(est0.9bridge,function(x) x[2])
  sigma1bridge=sapply(est1bridge,function(x) x[2])
  
  mu0.1bridge=unlist(mu0.1bridge)
  mu0.2bridge=unlist(mu0.2bridge)
  mu0.3bridge=unlist(mu0.3bridge)
  mu0.4bridge=unlist(mu0.4bridge)
  mu0.5bridge=unlist(mu0.5bridge)
  mu0.6bridge=unlist(mu0.6bridge)
  mu0.7bridge=unlist(mu0.7bridge)
  mu0.8bridge=unlist(mu0.8bridge)
  mu0.9bridge=unlist(mu0.9bridge)
  mu1bridge=unlist(mu1bridge)
  
  sigma0.1bridge=unlist(sigma0.1bridge)
  sigma0.2bridge=unlist(sigma0.2bridge)
  sigma0.3bridge=unlist(sigma0.3bridge)
  sigma0.4bridge=unlist(sigma0.4bridge)
  sigma0.5bridge=unlist(sigma0.5bridge)
  sigma0.6bridge=unlist(sigma0.6bridge)
  sigma0.7bridge=unlist(sigma0.7bridge)
  sigma0.8bridge=unlist(sigma0.8bridge)
  sigma0.9bridge=unlist(sigma0.9bridge)
  sigma1bridge=unlist(sigma1bridge)
  
  mubridge=data.frame(mu0.1bridge,mu0.2bridge,mu0.3bridge,mu0.4bridge,mu0.5bridge
                   ,mu0.6bridge,mu0.7bridge,mu0.8bridge,mu0.9bridge,mu1bridge)
  sigmabridge=data.frame(sigma0.1bridge,sigma0.2bridge,sigma0.3bridge,sigma0.4bridge,
                      sigma0.5bridge,sigma0.6bridge,sigma0.7bridge,sigma0.8bridge,
                      sigma0.9bridge,sigma1bridge)
  
  
  
  write.xlsx(mubridge,file="testing/nprob20bri1.xlsx",sheetName="Sheet1bri1.0",append=T)
  write.xlsx(sigmabridge,file="testing/nprob20bri1.xlsx",sheetName="Sheet2bri1.0",append=T)
  
  
  
  avgbiasmubridge=c()
  avgmsemubridge=c()
  avgbiassigbridge=c()
  avgmsesigbridge=c()
  avgbiasmubridge[1]=mean((mu0.1bridge-rep(0,500)),na.rm=T)
  avgbiasmubridge[2]=mean((mu0.2bridge-rep(0,500)),na.rm=T)
  avgbiasmubridge[3]=mean((mu0.3bridge-rep(0,500)),na.rm=T)
  avgbiasmubridge[4]=mean((mu0.4bridge-rep(0,500)),na.rm=T)
  avgbiasmubridge[5]=mean((mu0.5bridge-rep(0,500)),na.rm=T)
  avgbiasmubridge[6]=mean((mu0.6bridge-rep(0,500)),na.rm=T)
  avgbiasmubridge[7]=mean((mu0.7bridge-rep(0,500)),na.rm=T)
  avgbiasmubridge[8]=mean((mu0.8bridge-rep(0,500)),na.rm=T)
  avgbiasmubridge[9]=mean((mu0.9bridge-rep(0,500)),na.rm=T)
  avgbiasmubridge[10]=mean((mu1bridge-rep(0,500)),na.rm=T)
  
  avgmsemubridge[1]=mean((mu0.1bridge-rep(0,500))^2,na.rm=T)
  avgmsemubridge[2]=mean((mu0.2bridge-rep(0,500))^2,na.rm=T)
  avgmsemubridge[3]=mean((mu0.3bridge-rep(0,500))^2,na.rm=T)
  avgmsemubridge[4]=mean((mu0.4bridge-rep(0,500))^2,na.rm=T)
  avgmsemubridge[5]=mean((mu0.5bridge-rep(0,500))^2,na.rm=T)
  avgmsemubridge[6]=mean((mu0.6bridge-rep(0,500))^2,na.rm=T)
  avgmsemubridge[7]=mean((mu0.7bridge-rep(0,500))^2,na.rm=T)
  avgmsemubridge[8]=mean((mu0.8bridge-rep(0,500))^2,na.rm=T)
  avgmsemubridge[9]=mean((mu0.9bridge-rep(0,500))^2,na.rm=T)
  avgmsemubridge[10]=mean((mu1bridge-rep(0,500))^2,na.rm=T)
  
  avgbiassigbridge[1]=mean((sigma0.1bridge-rep(1,500)),na.rm=T)
  avgbiassigbridge[2]=mean((sigma0.2bridge-rep(1,500)),na.rm=T)
  avgbiassigbridge[3]=mean((sigma0.3bridge-rep(1,500)),na.rm=T)
  avgbiassigbridge[4]=mean((sigma0.4bridge-rep(1,500)),na.rm=T)
  avgbiassigbridge[5]=mean((sigma0.5bridge-rep(1,500)),na.rm=T)
  avgbiassigbridge[6]=mean((sigma0.6bridge-rep(1,500)),na.rm=T)
  avgbiassigbridge[7]=mean((sigma0.7bridge-rep(1,500)),na.rm=T)
  avgbiassigbridge[8]=mean((sigma0.8bridge-rep(1,500)),na.rm=T)
  avgbiassigbridge[9]=mean((sigma0.9bridge-rep(1,500)),na.rm=T)
  avgbiassigbridge[10]=mean((sigma1bridge-rep(1,500)),na.rm=T)
  
  avgmsesigbridge[1]=mean((sigma0.1bridge-rep(1,500))^2,na.rm=T)
  avgmsesigbridge[2]=mean((sigma0.2bridge-rep(1,500))^2,na.rm=T)
  avgmsesigbridge[3]=mean((sigma0.3bridge-rep(1,500))^2,na.rm=T)
  avgmsesigbridge[4]=mean((sigma0.4bridge-rep(1,500))^2,na.rm=T)
  avgmsesigbridge[5]=mean((sigma0.5bridge-rep(1,500))^2,na.rm=T)
  avgmsesigbridge[6]=mean((sigma0.6bridge-rep(1,500))^2,na.rm=T)
  avgmsesigbridge[7]=mean((sigma0.7bridge-rep(1,500))^2,na.rm=T)
  avgmsesigbridge[8]=mean((sigma0.8bridge-rep(1,500))^2,na.rm=T)
  avgmsesigbridge[9]=mean((sigma0.9bridge-rep(1,500))^2,na.rm=T)
  avgmsesigbridge[10]=mean((sigma1bridge-rep(1,500))^2,na.rm=T)
  
  
  
  avgbiasmubridge=round(avgbiasmubridge,5)
  avgbiassigbridge=round(avgbiassigbridge,5)
  avgmsemubridge=round(avgmsemubridge,5)
  avgmsesigbridge=round(avgmsesigbridge,5)
  
  
  
  alp=c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")
  
  cat("\n\nAverage Bias and MSE for Bridge(n=100)",file="testing/biasmsemubri.txt",sep="\n",append=T)
  cat("\nLambda                      Alpha0.1         Alpha0.2         Alpha0.3         Alpha0.4         Alpha0.5         Alpha0.6         Alpha0.7         Alpha0.8         Alpha0.9         Alpha1",file="testing/biasmsemubri.txt",append=TRUE)
  
  
    cat("\n",lam,"     Average Bias       ",avgbiasmubridge[1],
        "        ",avgbiasmubridge[2],"         ",avgbiasmubridge[3],
        "        ",avgbiasmubridge[4],"        ",avgbiasmubridge[5],
        "        ",avgbiasmubridge[6],"        ",avgbiasmubridge[7],
        "        ",avgbiasmubridge[8],"        ",avgbiasmubridge[9],
        "        ",avgbiasmubridge[10],file="testing/biasmsemubri.txt",append=TRUE)
    
    cat("\n",lam,"     Average MSE       ",avgmsemubridge[1],
        "        ",avgmsemubridge[2],"         ",avgmsemubridge[3],
        "        ",avgmsemubridge[4],"        ",avgmsemubridge[5],
        "        ",avgmsemubridge[6],"        ",avgmsemubridge[7],
        "        ",avgmsemubridge[8],"        ",avgmsemubridge[9],
        "        ",avgmsemubridge[10],file="testing/biasmsemubri.txt",append=TRUE)

    cat("\n\nAverage Bias and MSE for Bridge(n=100)",file="testing/biasmsesigbri.txt",sep="\n",append=T)
    cat("\nLambda                      Alpha0.1         Alpha0.2         Alpha0.3         Alpha0.4         Alpha0.5         Alpha0.6         Alpha0.7         Alpha0.8         Alpha0.9         Alpha1",file="testing/biasmsesigbri.txt",append=TRUE)
    
    
    cat("\n",lam,"     Average Bias       ",avgbiassigbridge[1],
        "        ",avgbiassigbridge[2],"         ",avgbiassigbridge[3],
        "        ",avgbiassigbridge[4],"        ",avgbiassigbridge[5],
        "        ",avgbiassigbridge[6],"        ",avgbiassigbridge[7],
        "        ",avgbiassigbridge[8],"        ",avgbiassigbridge[9],
        "        ",avgbiassigbridge[10],file="testing/biasmsesigbri.txt",append=TRUE)
    
    cat("\n",lam,"     Average MSE       ",avgmsesigbridge[1],
        "        ",avgmsesigbridge[2],"         ",avgmsesigbridge[3],
        "        ",avgmsesigbridge[4],"        ",avgmsesigbridge[5],
        "        ",avgmsesigbridge[6],"        ",avgmsesigbridge[7],
        "        ",avgmsesigbridge[8],"        ",avgmsesigbridge[9],
        "        ",avgmsesigbridge[10],file="testing/biasmsesigbri.txt",append=TRUE)
}