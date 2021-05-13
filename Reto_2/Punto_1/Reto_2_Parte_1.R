#Librerias necesarias
library(readr)
library(PolynomF)
library(pracma)
library(readr)

#Leer los valores de la Temperatura en la estacion Fortaleza-UECE
Estacion <- read.csv("C:/Users/Usuario/Documents/Reto_2/Punto_1/Datos_Estacion.csv")
Temperatura <-Estacion$Temp..Interna..ï..C.

#Grafica de la informacion obtenida previamente
tam<-length(Temperatura)
plot(1:tam, Temperatura , type="l",xlab = "Dias",ylab = "Temperatura",main = "Estacion Fortaleza-UECE")

#Escoger valores al azar que representan el 80% de los datos originales
por<-tam*0.8
sorteado<-sort(sample(1:tam,por, replace = F))
faltantes<-1:tam
faltantes <- faltantes[-sorteado]
datos<-Temperatura[sorteado]

#interpolacion por spline
spli<-spline(1:por, datos, n=tam)
#Grafica correspondiente
length(spli$y)
plot(1:tam, Temperatura , type="l",xlab = "Dias",ylab = "Temperatura",main = "Estacion Fortaleza-UECE")
lines(1:tam, spli$y, col="blue")

#interpolacion por splinefun
sf <- splinefun(sorteado, datos)
sf <- sf(1:tam)
#Grafica correspondiente
plot(1:tam, Temperatura , type="l",xlab = "Dias",ylab = "Temperatura",main = "Estacion Fortaleza-UECE")
lines(1:tam, sf, col="yellow")

#interpolacion lineal
l <-approx(1:por, datos, method = "linear", n=tam)
#Grafica correspondiente
plot(1:tam, Temperatura , type="l",xlab = "Dias",ylab = "Temperatura",main = "Estacion Fortaleza-UECE")
lines(1:tam, l$y, col="orange")


#Manejo de errores 
eSpline<- c()
ec <- c()
el <- c()

for(i in 1:tam){
  #error spline
  e = 0
  e = abs(Temperatura[i]-spli$y[i])
  eSpline<- c(eSpline,e)
  #error de la costante
  e = 0
  e = abs(Temperatura[i]-sf[i])
  ec<- c(ec,e)
  #error lineal
  e = 0
  e = abs(Temperatura[i]-l$y[i])
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
(1 - (sum(eSpline)/sum(datos)))*100
#-----------
cat("error EMC del splinefun(hermite) ",emcC,'\n')
cat("error medio del splinefun(hermite) ",(sum(ec))/tam,'\n')
cat("error minimo  del splinefun(hermite)",min(ec),'\n')
cat("error maximo  del splinefun(hermite)",max(ec),'\n')
#criterio de aceptacion
(1 - (sum(ec)/sum(datos)))*100
#-----------
cat("error EMC de la lineal ",emcL,'\n')
cat("error medio de la lineal ",(sum(el))/tam,'\n')
cat("error minimo  de la lineal ",min(el),'\n')
cat("error maximo  de la lineal ",max(el),'\n')
#criterio de aceptacion
(1 - (sum(el)/sum(datos)))*100