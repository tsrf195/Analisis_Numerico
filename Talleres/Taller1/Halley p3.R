
#Metodo de Halley 
i=1
VI<-mpfr(0.65625,56)
n=100
tolerancia=10^-56

f <- function(x){
  x^3-2*x^2+(4/3)*x-(8/27)
}
dx <- function(x){
  3*x^2-2*(2*x)+(4/3)
}
dxx <- function(x){
  3*(2*x)-2*2
}

while(i<=n){
  VS = VI - (f(VI)/dx(VI))*(1 - (f(VI)*dxx(VI)/dx(VI)^2))^(-1)
  if(abs(VS-VI)/abs(VS)< tolerancia){
    cat("Iteracion =",i,"\n Raiz =  ")
    print(VS,56)
    cat("Tolerancia =",tolerancia,"\n")
    break
  }
  i=i+1
  VI = VS
}



