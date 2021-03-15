#Metodo de Brent

f <- function(x){
  x^3-2*x^2+(4/3)*x-(8/27)
}

a = mpfr(-1,53) #-4.62962
b = mpfr(1,53) #0.03703
Error = 2^-50

#Intercambiar valores
if (abs(f(a)) < abs(f(b))){
  L = a
  a = b
  b = L
}

c = a
MFlag = 1
delta = Error
i = 0

while(abs(b-a) >= Error){
  i = i+1
  if(f(a) != f(c) && f(b) != f(c)){
    #Interpolacion Cuadratica Inversa
    s = ((a*f(b)*f(c))/((f(a)-f(b))*(f(a)-f(c)))) + 
        ((b*f(a)*f(c))/((f(b)-f(a))*(f(b)-f(c)))) +
        ((c*f(a)*f(b))/((f(c)-f(a))*(f(c)-f(b))))
    #cat("ICI \n")
  }
  else{
    #Metodo de la Secante
    s = b-f(b)*((b-a)/(f(b)-f(a)))
    #cat("Secante \n")
  }
  if( (s<=(3*a+b)/4 | s>=b)
    | (MFlag == 1 && abs(s-b) >= abs(b-c)/2)
    | (MFlag == 0 && abs(s-b) >= abs(c-d)/2)
    | (MFlag == 1 && abs(b-c) < abs(delta))
    | (MFlag == 0 && abs(c-d) < abs(delta))
    ){
    #Metodo de la Biseccion
    s=(a+b)/2
    MFlag = 1
    #cat("Biseccion \n")
  }
  else{
    MFlag = 0
  }
  
  #Calcular f(s)
  d = c #(d se asigna por primera vez aca, no se tiene en la 1 iteracion)
  c = b
  if(f(a)*f(s) < 0){
    b = s
  }
  else{
    a = s
  }
  
  #Intercambiar valores
  if(abs(f(a)) < abs(f(b))){
    L = a
    a = b
    b = L
  }
  
  cat("iteracion =",i,"Raiz =")
  print(s,50)
}