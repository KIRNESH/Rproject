newton <- function(f1,f2,tol = 1e-7 , x0 = rep(1,2), N = 100){
  h = 1e-7
  i = 1;
  x1 = x0
  p = list()
  
  while (i <= N) {
    j1=grad(f1,x0)
    j2=grad(f2,x0)
    j=rbind(j1,j2)
    f=rbind(f1(x0),f2(x0))
    x1 = (x0 - c(solve(j)%*%f))
    p[[i]] = x1
    i = i + 1
    if (norm(abs(x1 - x0),type="2") < tol) break
    x0 = x1
  }
  return(p)
}