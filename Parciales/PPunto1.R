numero = 10
base = 1
contador = 0
aux = 0

while(contador<numero){
  aux = aux + base^2 
  contador = contador + 1
  base = base + 1
}

#Error Absoluto = Resultado Exacto - Aproximación.

Error_Absoluto = aux - (aux-0.1)

#Error Relativo = (Error Absoluto/ Resultado Exacto) x 100%

Error_Relativo = (Error_Absoluto/aux)*100

