simbridgemu0.4=function(lam){
  est0.4bridge=list()
  avgbiasmubridge=c()
  avgmsemubridge=c()
  avgbiassigbridge=c()
  avgmsesigbridge=c()
  lam=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  mulam=data.frame(matrix(nrow=1000,ncol=11))
  sol0.4=c()
  i=2
  cntmean=6
  cntsigma=1
  prob=0.8
  for(j in 1:length(lam)){
    i=2
    est0.4bridge=list()
    sol0.4=c()
    print(j)
  while(i<=1001){
    smp=sample[,i]
    if((i%%100)==0){print(i)}
    #cat("\n",smp,file="testing/mutstprob20bri1.0.txt",sep=",",append=TRUE)
    #cat("\n",file="testing/mutstprob20bri1.0.txt",append=TRUE)
    st=checkbridge(smp,0.4,lam[j])
    valuef=sapply(st,function(x) x[3])
    valuesort=sort(valuef)
    index=which(valuef==valuesort[1])
    sol0.4=bridgediv(smp,0.4,lam[j],st[[index]][1:2])
    for(k in 2:10){
      if(sol0.4=="Start with new solution") {
        index = which(valuef == valuesort[k])
        sol0.4=bridgediv(smp,0.4,lam[j],st[[index]][1:2])
      }
      else if (abs(sol0.4[1]) > 3) {
        index = which(valuef == valuesort[k])
        sol0.4=bridgediv(smp,0.4,lam[j],st[[index]][1:2])
      }
      else if (abs(sol0.4[2]) > 5) {
        index = which(valuef == valuesort[k])
        sol0.4=bridgediv(smp,0.4,lam[j],st[[index]][1:2])
      }
      else{
        break
      }
    }
  
    est0.4bridge[[i]]=sol0.4
    
    
    
    if(est0.4bridge[[i]]=="Start with new solution" || abs(est0.4bridge[[i]][1])>4 
       || abs(est0.4bridge[[i]][2])>5){est0.4bridge[[i]]=c(NA,NA)}
    
    i=i+1
  }
    
  mu0.4bridge=sapply(est0.4bridge,function(x) x[1])
  sigma0.4bridge=sapply(est0.4bridge,function(x) x[2])
  mu0.4bridge=unlist(mu0.4bridge)
  sigma0.4bridge=unlist(sigma0.4bridge)
  mulam[,j]=mu0.4bridge
  
  
  avgbiasmubridge[j]=mean((mulam[,j]),na.rm=T)
  avgmsemubridge[j]=mean((mulam[,j])^2,na.rm=T)
  
  avgbiasmubridge[j]=round(avgbiasmubridge[j],5)
  avgmsemubridge[j]=round(avgmsemubridge[j],5)
  }
  cat("\n\n\n",file="testing/biasmsemubri100.txt",append=T)
  cat("lam        bias        mse ",file="testing/biasmsemubri100.txt",append=T)
  cat("\n",file="testing/biasmsemubri100.txt",append=T)
  
  for(i in 1:length(lam)){
    cat(lam[i],avgbiasmubridge[i],avgmsemubridge[i],"\n",file="testing/biasmsemubri100.txt",sep="        ",append=T)
    
  }
  write.xlsx(mulam,file="testing/nprob100bri.xlsx",sheetName = "SheetAlp0.4lambdacol",append=T)
}
