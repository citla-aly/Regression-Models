rm(list = ls(all.names = TRUE))
gc()

### Continuación de ejemplo. Intervalos de predicción

library(ALSM)

Datos=TolucaCompany
head(Datos)
str(Datos)

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
library(latex2exp)
plot(Datos$x, Datos$y, xlab = TeX("$x$"), ylab=TeX("$y$") )


(xbar=mean(Datos$x))
(SSx=sum((Datos$x-xbar)^2))
(ybar=mean(Datos$y))
(SSy=sum((Datos$y-ybar)^2))
(SSxy=sum((Datos$y-ybar)*(Datos$x-xbar)))
(beta1=SSxy/SSx)
(beta0=ybar-beta1*xbar)
n=length(Datos$x)
Datos$yhat=beta0+beta1*Datos$x
Datos$error=Datos$y-Datos$yhat
MSE=sum((Datos$error)^2)/(n-2)

fit=lm(y~x, data=Datos)
summary(fit)


# a. Suponga que el siguiente lote que se producirá será de tamaño 100,
# el directivo está interesado en saber cuántas horas se trabajarán 
# para la producción de ese lote. Dé un intervalo al 90% de predicción.


# Cuando sólo se realiza la predicción de una sóla observación se puede
# usar directamente la función predict en R con interval = "prediction" 


newdata <- data.frame(x = c(100) )
Predx100 <- predict(fit, newdata, interval = "prediction", level = 0.90)
head(Predx100)

# A mano siguiendo las fórmula de las notas

var.PredYh_x100=MSE*(1+1/n+(100-xbar)^2/SSx)
alpha=.1
tcuantil=qt(1-alpha/2, n-2,lower.tail = TRUE)
print(c(beta0+beta1*100-tcuantil*sqrt(var.PredYh_x100),
        beta0+beta1*100+tcuantil*sqrt(var.PredYh_x100)))

#Notar que los intervalos de predicción son en general más amplios que los asociados
# a la E(Y|x)

predict(fit, newdata, interval = "confidence", level = 0.90)

# b. Suponga que la compañía ha firmado un contrato para producir en tres 
# de sus unidades, un lote de tamaño 100 en cada uno. 
# El directivo está interesado en saber cuántas horas 
# en promedio por unidad se usarán para cumplir con lo establecido en el contrato.

# Sólo se puede a mano, buscamos la estimación de sum_{j=1}^3 y_{hj}/3, 
# con x_{h1}= x_{h2}=x_{h3}=100. De donde a_j=1/3 para j=1,2,3. 

sumA_j=(1/3+1/3+1/3)
sumA_j_2=(1/3^2+1/3^2+1/3^2)
SumA_jXhj=((1/3)*100+(1/3)*100+(1/3)*100)
var.PredMeanYh_x100=MSE*(sumA_j_2+sumA_j^2/n+(SumA_jXhj-sumA_j*xbar)^2/SSx)
alpha=.1
tcuantil=qt(1-alpha/2, n-2,lower.tail = TRUE)
print(c(sumA_j*beta0+beta1*SumA_jXhj-tcuantil*sqrt(var.PredMeanYh_x100),
        sumA_j*beta0+beta1*SumA_jXhj+tcuantil*sqrt(var.PredMeanYh_x100)))

# Notar que cuando se habla de predecir un promedio de m observaciones en un 
# valor fijo de Xh, entonces la fórmula se simplifica y sólo difiere de la de 
# predicción de un valor por que en la varianza se cambia 1 por 1/m
m=3
var.PredMeanYh_x100v2=MSE*(1/m+1/n+(100-xbar)^2/SSx)
alpha=.1
tcuantil=qt(1-alpha/2, n-2,lower.tail = TRUE)
print(c(beta0+beta1*100-tcuantil*sqrt(var.PredMeanYh_x100v2),
        beta0+beta1*100+tcuantil*sqrt(var.PredMeanYh_x100v2)))



# c. Suponga que la compañía ha firmado un contrato para producir en 
# tres de sus unidades, un lote de tamaño 100 en cada uno. 
# El directivo está interesado en saber cuántas horas en total 
# se usarán para cumplir con lo establecido en el contrato.

# Este intervalo se puede obtener multiplicando por tres el obtenido en b.

print(3* c(beta0+beta1*100-tcuantil*sqrt(var.PredMeanYh_x100v2),
        beta0+beta1*100+tcuantil*sqrt(var.PredMeanYh_x100v2)) )

# A mano

# Buscamos la estimación de sum_{j=1}^ m y_{hj} con m=3 y 
# con x_{h1}= x_{h2}=x_{h3}=100. De donde a_j=1 para j=1,2,3. 

sumA_j=(1+1+1)
sumA_j_2=(1^2+1^2+1^2)
SumA_jXhj=((1)*100+(1)*100+(1)*100)
var.PredSumYh_x100=MSE*(sumA_j_2+sumA_j^2/n+(SumA_jXhj-sumA_j*xbar)^2/SSx)
alpha=.1
tcuantil=qt(1-alpha/2, n-2,lower.tail = TRUE)
print(c(sumA_j*beta0+beta1*SumA_jXhj-tcuantil*sqrt(var.PredSumYh_x100),
        sumA_j*beta0+beta1*SumA_jXhj+tcuantil*sqrt(var.PredSumYh_x100)))

