
#Metodo de Halley 
i=1
VI<-mpfr(14.78125,56)
n=100
tolerancia=10^-56

f <- function(x){
  ((667.38/x)*(1-exp(-0.1468*x)))-40
}
dx <- function(x){
  (0.1468*exp(-0.1468*x))-(667.38/x^2)
}
dxx <- function(x){
  (-0.02155024*exp(-0.1468*x))+(1334.76/x^3)
}

while(i<=n){
  VS = VI - (f(VI)/dx(VI))*(1 - (f(VI)*dxx(VI)/dx(VI)^2))^(-1)
  if(abs(VS-VI)/abs(VS) < tolerancia){
    cat("Iteracion =",i,"\n Raiz =  ")
    print(VS,56)
    cat("Tolerancia =",tolerancia,"\n")
    break
  }
  i=i+1
  VI = VS
}

