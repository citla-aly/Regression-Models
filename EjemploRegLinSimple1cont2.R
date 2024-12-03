rm(list = ls(all.names = TRUE))
gc()

### Continuación de ejemplo. Verificación sobre linealidad

library(ALSM)

Datos=TolucaCompany
head(Datos)
str(Datos)

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



#R tiene una gráfica propia para verificar linealidad
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit, 1)


# Herramienta para argumentar la no necesidad de transformación de X
boxTidwell(y~x, data=Datos)

#Test Ho: lambda=1 vs Ha:lambda != 1.
#Si no se rechaza, entonces es plausible considerar el modelo con X directamente
#Si se rechaza, entonces conviene usar el estimador de lambda para transformar X
