# Regresión lineal múltiple
# Ajuste e interpretación

rm(list = ls(all.names = TRUE))
gc()

setwd("C:/Users/Gonzalo/Documents/GitHub/Notas 2021-2/Mod Reg/")
Datos=read.csv("ejemplo2RLM.csv", header=TRUE )


par(mfrow=c(1,2),mar=c(4.5,4.5,1,1))
pairs(Datos)

# E(y|x1,x2)=b0+b1x1+b2x2

fit=lm(y~X1+X2, data=Datos)

summary(fit)

#Observemos que con la prueba asociada a la tabla ANOVA, se puede concluir
#que con una significancia de .05, se rechaza H0 en el contraste
#H0: b1=0 y b2=0 vs Ha: b1!=0 o b2!=0

#Además, si analizamos la prueba t, para el coeficiente b1
#se rechaza H0. Aquí el contraste es H0: b1=0 vs Ha: b1 != 0
#Esto nos indica, que aún considerando a la variable X2 en el modelo
#la variable X1 nos está agregando información para modelar E(Y|X1,X2).

#Por otro lado, si analizamos la prueba t, para el coeficiente b2
#se rechaza H0. Aquí el contraste es H0: b2=0 vs Ha: b2 != 0
#Esto nos indica, que aún considerando a la variable X1 en el modelo
#la variable X2 nos está agregando información para modelar E(Y|X1,X2).

#Con base en lo anterior, parece que no podríamos reducir el modelo, todos 
#los coeficientes parecen significativos.

#Vamos a interpretar este modelo

#R2, el coeficiente de determinación, nos indica que se está
#explicando el 99.8% de la variabilidad observada en Y a través 
#del modelo y=b0+b1x1+b2x2+e

#Además, b1 se puede interpretar como
#condicionado en un valor fijo de X2, el promedio de la variable 
#Y aumenta en .5 unidades al aumentar en una unidad la variable X1.

#Por otro lado, b2 se puede interpretar como
#condicionado en un valor fijo de X1, el promedio de la variable 
#Y aumenta en .4 unidades al aumentar en una unidad la variable X2.

#Notemos además que al incluir dos variables en el modelo
#se puede tener un mejor ajuste con base en el coeficiente de
#determinación y que ambas variables resultan significativas
fitred1=lm(y~X2, data=Datos)
summary(fitred1)

fitred2=lm(y~X1, data=Datos)
summary(fitred2)

