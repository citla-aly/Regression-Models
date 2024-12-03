
#------Ejemplo de Ajustes de Recta------#

#1.-Solucion Analitica
#2.-Usando la funcion lm() 
#3.-Por minimos cuadros


#LECTURA DE LOS DATOS
generacion <- read.csv("C:/Users/Usuario/Documents/Ayudantia/Estadistica II/02_generacion.csv")

#Grafica de Datos
plot(generacion,main='Estatura de Hijos', xlab='Estatura Padre', ylab='Estatura Hijo', col=2,cex=0.8,cex.axis=0.7,cex.lab=0.7,mgp=c(1.5,0.5,0))
grid(5, 5, lwd = 2) # se especifica la cuadrícula y el grosor de las líneas

#1.- SOLUCION ANALITICA
X=generacion[,1] #Se asigna a X la columna uno de la base de datos (estatura del padre)
Y=generacion[,2] #Se asigna a Y la columna dos de la base de datos (estatura del hijo)

#Definimos Sxx y Sxy
Sxx=sum((X-mean(X))^2)            
Sxy=sum((X-mean(X))*(Y-mean(Y)))  

#Encontramos Beta1 y Beta0
beta1=Sxy/Sxx 
beta1 # 0.06462304
beta0=mean(Y)-beta1*mean(X)
beta0 # 181.6538

#2.- AJUSTE DE MODELOS LINEALES POR MEDIO DE LA FUNCIÓN "lm"

#Utilizamos la función "lm" para encontrar los valores de Beta0 y Beta1
lm(Y~X) #estatura del hijo ~ estatura del padre
#Coefficients:
# (Intercept)       X  
#181.65376      0.06462  


#3.- SOLUCION NUMERICA POR MINIMOS CUADRADOS

#FUNCION QUE CALCULA LA SUMA DEL CUADRADO DEL ERROR SEGUN LOS PARAMESTRO EN B
sum_error<-function(B){
  b0=B[1]
  b1=B[2]
  return (sum((Y-X*b1-b0)^2))
} 

#AJUSTE DE UNA RECTA POR MINIMOS CUADRADOS (Non-Linear Minimization)
#Como primer parámetro necesitamos una función
# el segundo parámetro son los valores iniciales que queremos que tomen b0 y b1(pueden variar)
nlm(sum_error,c(1,1))
#$estimate
#[1] 181.65374080  0.06462316


#GRAFICAMOS AJUSTE Y RESIDUALES
par(mfrow=c(2, 1))
#Ajuste
plot(generacion,main='Estatura de Hijos', xlab='Estatura Padre', ylab='Estatura Hijo', col=2,cex=0.8,cex.axis=0.7,cex.lab=0.7,mgp=c(1.5,0.5,0))
grid(5, 5, lwd = 2) # grid only in y-direction
lines(X,beta0+beta1*X)

#Residuales
plot(X,Y-beta0-beta1*X,main='Residuales', xlab='Estatura Padre', ylab='Error', col=2,cex=0.8,cex.axis=0.7,cex.lab=0.7,mgp=c(1.5,0.5,0))
grid(5, 5, lwd = 2) # grid only in y-direction
lines(X,Y-beta0-beta1*X)


#AJUSTE DE UN MODELO CUADRATICO POR MINIMOS CUADRADOS POR MEDIO DE LA FUNCIÓN "nlm"
#creamos una función que calcule la suma del cuadrado de los errores según los parámetros en B
sum_error2<-function(B){
  b0=B[1]
  b1=B[2]
  b2=B[3]
  return (sum((Y-(X^2*b2+X*b1+b0))^2))
}

optim=nlm(sum_error2,c(1,2,4)) #Introducimos a la función nlm la función que creamos y los valores iniciales de b0,b1 y b2
optim$estimate #queremos conocer los valores estimados

#GRAFICAMOS AJUSTE Y RESIDUALES
par(mfrow=c(2, 1))
#Ajuste
plot(generacion,main='Estatura de Hijos', xlab='Estatura Padre', ylab='Estatura Hijo', col=2,cex=0.8,cex.axis=0.7,cex.lab=0.7,mgp=c(1.5,0.5,0))
grid(5, 5, lwd = 2) # grid only in y-direction
lines(X,optim$estimate[1]+optim$estimate[2]*X+optim$estimate[3]*X^2)

#Residuales
plot(X,Y-(optim$estimate[1]+optim$estimate[2]*X+optim$estimate[3]*X^2),main='Residuales', xlab='Estatura Padre', ylab='Error', col=2,cex=0.8,cex.axis=0.7,cex.lab=0.7,mgp=c(1.5,0.5,0))
grid(5, 5, lwd = 2) # grid only in y-direction
lines(X,Y-(optim$estimate[1]+optim$estimate[2]*X+optim$estimate[3]*X^2))


#COMPARACION DE MODELOS
#Modelo 1
sum((Y-(beta0+beta1*X))^2)
#Modelo 2
sum((Y-(optim$estimate[1]+optim$estimate[2]*X+optim$estimate[3]*X^2))^2)
#El modelo que minimiza el error es el modelo 1
