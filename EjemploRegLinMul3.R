# Regresión lineal múltiple
# Ajuste e interpretación

rm(list = ls(all.names = TRUE))
gc()

setwd("C:/Users/Gonzalo/Documents/GitHub/Notas 2021-2/Mod Reg/")
Datos=read.csv("ejemplo3RLM.csv", header=TRUE )

summary(Datos)
str(Datos)

Datos$X1c=factor(Datos$X1c)
str(Datos)

par(mfrow=c(1,1),mar=c(4.5,4.5,1,1))
plot(Datos)
library(GGally)
ggpairs(Datos)

ggpairs(data=Datos, title="tips data", aes(colour = X1c))

with(Datos, plot(X2, y, col=c("red", "magenta", "blue")[Datos[,2]] ))
legend("topleft",levels(Datos[,2]), col=c("red", "magenta", "blue"), pch = c(0,0,0) )


levels(Datos$X1c)
# E(y|x)=b0+b1A2+b2A3+b3x2

fit=lm(y~X1c+X2, data=Datos)

summary(fit)



#Observemos que con la prueba asociada a la tabla ANOVA, se puede concluir
#que con una significancia de .05, se rechaza H0 en el contraste
#H0: b1=0 y b2=0 y b3=0 vs Ha: b1!=0 o b2!=0 o b3!=0

#Además, si analizamos la prueba t, para el coeficiente b3, asociado a X2,
#podemos observar que se rechaza Ho en el contraste H0: b3=0 vs Ha: b3 != 0
#esto nos indicaría que condicional en la variable categórica X1c, la variable 
#X2 nos está agregando información al modelo

#Para analizar si la variable X1c nos agrega información al modelo condicional en X2,
#debemos constrastar H0: b1=0 y b2=0 vs Ha: b1!=0 o b2!=0

#Opción 1, usando multcomp
library(multcomp)
K=matrix(c(0,1,0,0,
           0,0,1,0), ncol=4, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

#Opción 2, usando anova con modelos reducidos
fitred=lm(y~X2, data=Datos)
anova(fitred, fit)

#Opción 3, usar directamente la función drop1
drop1(fit, test = "F")

#Opcón 4, usar directamente la función Anova en librería car
library(car)
Anova(fit, type="II")
#Notar que lo anterior sólo se vale cuando no hay interacciones entre variables.

#Se observa que se rechaza H0, entonces la variable X1c nos ayuda a
#modelar E(Y|x) aun cuando en el modelo ya está la variable X2.

#Por otro lado, si analizamos la prueba t, para el coeficiente b1
#se rechaza H0. Aquí el contraste es H0: b1=0 vs Ha: b1 != 0
#Esto nos indica, que condicional en un valor fijo de X2,
#la esperanza de Y es diferente entre el nivel A2 y el A1 (de referencia)

#Por otro lado, si analizamos la prueba t, para el coeficiente b2
#se rechaza H0. Aquí el contraste es H0: b2=0 vs Ha: b2 != 0
#Esto nos indica, que condicional en un valor fijo de X2,
#la esperanza de Y es diferente entre el nivel A3 y el A1 (de referencia)

#Con base en lo anterior, parece que no podríamos reducir el modelo, todos 
#los coeficientes parecen significativos.

#Vamos a interpretar este modelo

summary(fit)
#R2, el coeficiente de determinación, nos indica que se está
#explicando el 99.9% de la variabilidad observada en Y a través 
#del modelo que incluye ambas variables X1c y X2:
#y=b0+b1A2+b2A3+b3x2

#Además, b1 se puede interpretar como
#condicionado en un valor fijo de X2, el promedio de la variable 
#Y aumenta en 1.31 unidades al comparar el nivel A2 contra el A1.
#(mayor esperanza en nivel A2 contra nivel A1)

#b2 se puede interpretar como
#condicionado en un valor fijo de X2, el promedio de la variable 
#Y aumenta en .5 unidades al comparar el nivel A3 contra el A1.

#b3 se puede interpretar como
#condicionado en un nivel de la variable X1c, el promedio de la variable 
#Y aumenta en 1.5 unidades al aumentar en una unidad la variable X2.

#¿Cómo se comparan los niveles A2 y A3, dado un valor fijo de X2?
#E(Y|A2, X2)=bo+b1+b3X2
#E(Y|A3, X2)=bo+b2+b3X2

#E(Y|A3, X2)-E(Y|A2, X2)=b2-b1

#Usando multcomp
library(multcomp)
K=matrix(c(0,-1,1,0), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())
summary(glht(fit, linfct=K, rhs=m))

#Se rechaza la igualdad de media, es decir, dado un valor de X2, 
#la esperanza de Y es diferente entre los niveles A3 y A2
#De hecho, la esperanza disminuye en .8135 unidades
#al comparar el nivel A3 contra el A2



#y=b0+b1A2+b2A3+b3x2
#Además notar que en este caso
#Si fijamos el nivel A1
#entonces el modelo para E(Y|A1,x2) es
#E(Y|A1,x2)=b0+b3x2

#para nivel A2: E(Y|A2,x2)
#E(Y|A2,x2)=b0+b1+b3x2=(b0+b1)+b3x2

#para nivel A3: E(Y|A3,x2)
#E(Y|A3,x2)=b0+b2+b3x2=(b0+b2)+b3x2

#Es decir como función de X2, son tres rectas que tienen diferente intercepto
#pero misma pendiente b3
fit$coefficients
fA1 <- function(X2) {fit$coefficients[1]+ fit$coefficients[4]*X2}
fA2 <- function(X2) {fit$coefficients[1]+fit$coefficients[2]+ fit$coefficients[4]*X2}
fA3 <- function(X2) {fit$coefficients[1]+fit$coefficients[3]+ fit$coefficients[4]*X2}

with(Datos, plot(X2, y, col=c("red", "green", "blue")[Datos[,2]] ))
legend("topleft",levels(Datos[,2]), col=c("red", "green", "blue"), pch = c(0,0,0), title = "X1c" )
curve(fA1, from = min(Datos$X2), to = max(Datos$X2),
      col = "red", add = T)
curve(fA2, from = min(Datos$X2), to = max(Datos$X2),
      col = "green", add = T)
curve(fA3, from = min(Datos$X2), to = max(Datos$X2),
      col = "blue", add = T)

