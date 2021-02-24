
#Metodo de Halley 
i=1
VI <- mpfr(0.71875,56)
n=100
tolerancia=10^-56

f <- function(x){
  cos(x)^2-x^2
}
dx <- function(x){
  -(2*(sin(x)*cos(x))+2*x)
}
dxx <- function(x){
  -(2*(cos(x)*cos(x)-sin(x)*sin(x))+2)
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







