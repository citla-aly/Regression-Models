rm(list = ls(all.names = TRUE))
gc()

### Continuación de ejemplo. Verificación sobre aleatoriedad

library(ALSM)

Datos=TolucaCompany
head(Datos)
str(Datos)

library(latex2exp)
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(Datos$x, Datos$y, xlab = TeX("$x$"), ylab=TeX("$y$") )



fit=lm(y~x, data=Datos)
summary(fit)

#R tiene una función para crear errores de forma automatizada
library(broom)
Datosfit=augment(fit)
head(Datosfit)
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,2)) 
plot(Datosfit$.fitted, Datosfit$.resid, xlab = TeX("$\\widehat{y}$"), ylab=TeX("$e$")   )
plot(Datos$x, Datosfit$.resid, xlab = TeX("$X$"), ylab=TeX("$e$") )

#gráficas para linealidad-homocedasticidad-normalidad
par(mar=c(4, 5, 2, 1))
par(mfrow=c(1,3))
plot(fit, 1)
plot(fit, 3)
plot(fit, 2)



#gráfica sobre el índice de los datos
par(mar=c(4, 5, 3, 1))
par(mfrow=c(1,3))
plot(1:length(Datosfit$.std.resid), Datosfit$.std.resid, xlab = TeX("$i$"), ylab=TeX("$e_s$")   )

#autocorrelograma de los errores
acf(Datosfit$.std.resid)


#Prueba de rachas
library(lawstat)
lawstat::runs.test(Datosfit$.std.resid, plot.it = TRUE)


library(randtests)
randtests::runs.test(Datosfit$.std.resid)

#Prueba para autocorrelación de orden 1
lmtest::dwtest(fit)
