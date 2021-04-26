#Lagrange
library(rSymPy)
library(polynom)

x <- c(4410000,4830000,5250000,5670000)
y <- c(1165978,1329190,1501477,1682830)

plot(x ,y , type = "o", main="Datos",xlab = "Base Imponible", ylab = "Cuota Integra")

lagrange.poly <- function(x, y) {
  
  l <- list()
  k <- 1
  
  for (i in x) {
    num <- 1
    denom <- 1
    
    p <- x[! x %in% i]
    
    for (j in p) {
      num <- paste(num, "*", "(", 'x', " - ", as.character(j), ")", sep = "", collapse = "")
      denom <- paste(denom, "*", "(", as.character(i)," - ", as.character(j), ")", sep = "", collapse = "")
    }
    
    l[k] <- paste("(", num, ")", "/", "(", denom, ")", sep = "", collapse = "")
    k <- k + 1
  }
  
  eq <- 0
  
  for (i in 1:length(y)) {
    eq <- paste(eq, '+', as.character(y[i]), "*", l[[i]], sep = "", collapse = "")
  }
  
  x <- Var('x')
  
  return(sympy(paste("simplify(", eq, ")")))
}

lagrange.poly(x, y)

poly.calc(x, y)


f <- function(x) {
  return(2.572279e-08*x^2 + 0.1509214*x + 155.125)
}
plot(f, type = "l", main="Comportamiento polinomio de Lagrage")

38707/16 + 167449*(5000000)/1120000 + 6119*(5000000)**2/235200000000 - (5000000)**3/49392000000000000