library(pracma)

f <- function(x){
  (sqrt(19)/sqrt(x))-((10-x^2)/x)
} 
polyroot(c(1,-1))

uniroot(f,lower = -3, upper = 4, f.lower = -3,  tol=2^-16)