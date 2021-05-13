f <- function(x){
  return(1+sin(3*x^3))
}

trapezoid <- function(f, a, b, n) {
  intval <- integrate(f,a,b)
  val2 = intval$value
  h <- (b-a)/n
  x <- seq(a, b, by=h)
  y <- f(x)
  s <- h * (abs(y[1]/2) + abs(sum(y[2:n])) + abs(y[n+1]/2))
  suma <- h * (abs(y[1]/2) + abs(sum(y[2:n])) + abs(y[n+1]/2))
  cat("Integral trapecio: ",suma,"\n")
  error = abs(val2-suma)
  if (error == 0) {
    cat("Error trapecio: 0 ","\n")
  } else {
    cat("Error trapecio: ",error,"\n")
  }
  
}
#Valores
a <- -1          # Cota inferior 
b <- 1      # Cota superior
tol <- 1e-8    # Tolerancia
trapezoid(f,-1,1,11)


