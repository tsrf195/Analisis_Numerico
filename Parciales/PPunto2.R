
i=1
n=100
tolerancia=10^-9

f <- function(x){
  x^2-cos(x)-1
}

plot(f,from=1,to=2)

VI=f(1)
VIA=f(0)

while(i<=n){
  VS = VI - (f(VI)*(VI-VIA)/(f(VI)-f(VIA)))
  if(abs(VS-VI)/abs(VS) < tolerancia){
    cat("Iteracion =",i,"Raiz =",VS*-1,"Tolerancia =",tolerancia,"\n")
    break
  }
  i=i+1
  VI = VS
}
