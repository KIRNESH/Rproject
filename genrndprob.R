generate=function(prob,cntmean,cntsigma,len){
  smp=c()
  for(i in 1:len){
    testprob=runif(1,0,1)
    if(testprob<=prob){
      smp[i]=rnorm(1,0,1)
    }
    else{
      smp[i]=rnorm(1,cntmean,cntsigma)
    }
  }
  return(smp)
}

generateexp=function(prob,rate,del,len){
  smp=c()
  for(i in 1:len){
    testprob=runif(1,0,1)
    if(testprob<=prob){
      smp[i]=rexp(1,rate)
    }
    else{
      smp[i]=del
    }
  }
}