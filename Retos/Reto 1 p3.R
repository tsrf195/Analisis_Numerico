library(pracma)

f <- function(x){
  x^3-2*x^2+(4/3)*x-(8/27)
}
polyroot(c((-8/27),(4/3),-2,1))

uniroot(f,c(0,1),tol=2^-50)

brentDekker(f,-1,1,maxiter=39,tol=2^-50) 

print(uniroot.all(f,c(0,1),tol=2^-50),22) #rootsolve


