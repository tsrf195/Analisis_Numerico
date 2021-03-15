newton_rapshon = function( valor,tol){
  
  aux=valor
  num=0
  derivada=0
  
  plot(x = 1,                 
       xlab = "X", 
       ylab = "Resultado",
       xlim = c(-3, 4), 
       ylim = c(-3, 4),
       main = "Newton Rapshon",
       type = "n")
  
  
  repeat{
    resultado=aux-((sqrt(19)/sqrt(aux)-(10-aux^2)/aux)/(1+10/aux^2-sqrt(19)/((2*aux)^(3/2))))
    num=num+1
    funcion=sqrt((19)/sqrt(x))-((10-x^2)/x)
    if(funcion==0){
      break;
    }else{
      derivada=1+10/aux^2-sqrt(19)/((2*aux)^(3/2))
      if(derivada==0)
        break;
    }
    if(resultado < 2^(tol)){
      break;
    }else{
      cat ("Iteracion: ",num,"Resultado:\n\n")
      print(resultado,50)
      ## "valor de x",aux,"valor de f(x)",funcion,"valor f'(x)",derivada,"\n\n")
      points(x = aux, y = resultado)
      if(num>10)
        break;
    }
    
    aux=resultado
  }
}
x0 = mpfr(3,50)
tol=-16
newton_rapshon (x0,tol)