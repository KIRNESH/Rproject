newton1 <- function(f, tol = 1e-7 , x0 = 1, N = 100){
h = 1e-7
i = 1;
x1 = x0
p = numeric(N)
while (i <= N) {
df.dx = (f(x0 + h) - f(x0)) / h
x1 = (x0 - (f(x0) / df.dx))
p[i] = x1
i = i + 1
if (abs(x1 - x0) < tol || abs(f(x1)) <tol) break
x0 = x1
}
return(p[(i-1)])
}