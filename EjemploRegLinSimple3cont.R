rm(list = ls(all.names = TRUE))
gc()

### Ejemplo. Verificación sobre normalidad

setwd("C:/Users/Gonzalo/Documents/Semestre 2021-1/ModNoParyReg")
Datos = read.csv("ejemplo3.csv")

head(Datos)
str(Datos)

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(Datos$x, Datos$y, xlab = TeX("$x$"), ylab=TeX("$y$") )



fit=lm(y~x, data=Datos)
summary(fit)

#Se transforma X a X2 para lograr linealidad
Datos$Xprima=Datos$x^2
fit2=lm(y~Xprima, data=Datos)
summary(fit2)

#Linealidad

Datosfit2=augment(fit2)
head(Datosfit2)
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,2)) 
plot(Datosfit2$.fitted, Datosfit2$.resid, xlab = TeX("$\\widehat{y}$"), ylab=TeX("$e$")   )
plot(Datos$x^2, Datosfit2$.resid, xlab = TeX("$X^2$"), ylab=TeX("$e$") )

#gráficas para linealidad y homocedasticidad
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,2))
plot(fit2, 1)
plot(fit2, 3)

#n es grande
#gráfica QQplot directa de R
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1))
plot(fit2, 2)
#Se usan los residuales estandarizados y los cuantiles de una normal

#Pruebas de normalidad

shapiro.test(Datosfit2$.std.resid)

library(nortest)
nortest::lillie.test(Datosfit2$.std.resid)

library(normtest)
normtest::jb.norm.test(Datosfit2$.std.resid)


#Si no se hubiera transformado X, se podría asumir normalidad?
#gráfica QQplot directa de R
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1))
plot(fit, 2)

Datosfit=augment(fit)
shapiro.test(Datosfit$.std.resid)
nortest::lillie.test(Datosfit$.std.resid)
normtest::jb.norm.test(Datosfit$.std.resid)

