
#Metodo de Halley 
i=1
VI <- mpfr(1.09375,56)
n=100
tolerancia=10^-56

f <- function(x){
  x*sin(x)-1
}
dx <- function(x){
  sin(x)+x*cos(x)
}
dxx <- function(x){
  cos(x)+(cos(x)-x*sin(x))
}

while(i<=n){
  VS = VI - (f(VI)/dx(VI))*(1 - (f(VI)*dxx(VI)/dx(VI)^2))^(-1)
  if(f(VS) < tolerancia){
    cat("Iteracion =",i,"\n Raiz =  ")
    print(VS,56)
    cat("Tolerancia =",tolerancia,"\n")
    break
  }
  i=i+1
  VI = VS
}




