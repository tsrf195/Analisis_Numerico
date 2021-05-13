#Librerias necesarias
library (pracma)
library(readxl)

#Extraer la informacion de las estaciones climáticas
Acarau <- read_excel("C:/Users/Usuario/Documents/Reto_2/Punto_2/Acarau.xlsx")
Aiauba <- read_excel("C:/Users/Usuario/Documents/Reto_2/Punto_2/Aiauba.xlsx")
datosOriginales = Aiauba$`Temp. Interna (ºC)`
datosTemp = Acarau$`Temp. Interna (ºC)`
tam = length(datosTemp)
porcentaje = length(datosTemp)*0.8
toma = sort(sample(1:length(datosTemp),porcentaje, replace = F))
datosTemp = datosTemp[toma]

#Interpolación spline 
inter = spline(toma,datosTemp,n=length(datosOriginales))
length(inter$y)
#Grafica correspondiente 
plot(1:length(datosOriginales),datosOriginales,type = "l",xlab = "Dias",ylab = "Temperatura",main = "Estacion Aiauba")
lines(1:length(datosOriginales),inter$y,col="green")
legend(555,46, legend=c("Original", "Spline"),
       col=c("black", "green"),lty=1:2, cex=0.8)

#Interpolación splinefun
c <- splinefun(toma, datosTemp )
c<-c(1:tam)
#Grafica correspondiente
plot(1:length(datosOriginales),datosOriginales,type = "l",xlab = "Dias",ylab = "Temperatura",main = "Estacion Aiauba")
lines(1:length(datosOriginales),c,col="red")
legend(555,46, legend=c("Original", "Splinefun"),
       col=c("black", "red"), lty=1:2, cex=0.8)

#Interpolación lineal
l<-approx(toma, datosTemp, method = "linear", n=length(datosOriginales))
#Grafica correspondiente
plot(1:length(datosOriginales),datosOriginales,type = "l",xlab = "Dias",ylab = "Temperatura",main = "Estacion Aiauba")
lines(1:length(datosOriginales),l$y,col="blue")
legend(555,46, legend=c("Original", "lineal"),
       col=c("black", "blue"), lty=1:2, cex=0.8)

#Manejo de errores
eSpline<- c()
ec <- c()
el <- c()

for(i in 1:tam){
  #error spline
  e = 0
  e = abs(datosOriginales[i]-inter$y[i])
  eSpline<- c(eSpline,e)
  #error de la costante
  e = 0
  e = abs(datosOriginales[i]-c[i])
  ec<- c(ec,e)
  #error lineal
  e = 0
  e = abs(datosOriginales[i]-l$y[i])
  el<- c(el,e)
  
} 
emcS = sqrt( (sum(eSpline^2))/tam)
emcC = sqrt( (sum(ec^2))/tam)
emcL = sqrt( (sum(el^2))/tam)
cat("error EMC del spline ",emcS,'\n')
cat("error medio del spline ",(sum(eSpline))/tam,'\n')
cat("error minimo  del spline ",min(eSpline),'\n')
cat("error maximo  del spline ",max(eSpline),'\n')
#criterio de aceptacion
(1 - (sum(eSpline)/sum(datosOriginales)))*100
#-----------
cat("error EMC del splinefun(hermite) ",emcC,'\n')
cat("error medio del splinefun(hermite) ",(sum(ec))/tam,'\n')
cat("error minimo  del splinefun(hermite) ",min(ec),'\n')
cat("error maximo  del splinefun(hermite) ",max(ec),'\n')
#criterio de aceptacion
(1 - (sum(ec)/sum(datosOriginales)))*100
#-----------
cat("error EMC del lineal ",emcL,'\n')
cat("error medio del lineal ",(sum(el))/tam,'\n')
cat("error minimo  del lineal ",min(el),'\n')
cat("error maximo  del lineal ",max(el),'\n')
#criterio de aceptacion
(1 - (sum(el)/sum(datosOriginales)))*100