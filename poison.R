

library("numDeriv")
newton <- function(f, tol = 1e-7 , x0 = 1, N = 100){
h = 1e-7
i = 1;
x1 = x0
p = numeric(N)

while (i <= N) {
df.dx=grad(f,x0)
x1 = (x0 - (f(x0) / df.dx))
p[i] = x1
i = i + 1
if (abs(x1 - x0) < tol) break
x0 = x1
}
return(p[1 : (i-1)])
}
library("rootSolve")
poisson=function(smp,alp){
  v=seq(0,2000,1)
  intf=function(l){sum((dpois(smp,lambda=l)^alp)*((smp-l)/l))/length(smp)-
                   sum((dpois(v,lambda=l)^(1+alp))*((v-l)/l))
}


k=newton(intf,x0=0.1)
return(k[length(k)])
}









