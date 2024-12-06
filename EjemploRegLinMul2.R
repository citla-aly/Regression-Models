# Regresi�n lineal m�ltiple
# Ajuste e interpretaci�n

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

#Adem�s, si analizamos la prueba t, para el coeficiente b1
#se rechaza H0. Aqu� el contraste es H0: b1=0 vs Ha: b1 != 0
#Esto nos indica, que a�n considerando a la variable X2 en el modelo
#la variable X1 nos est� agregando informaci�n para modelar E(Y|X1,X2).

#Por otro lado, si analizamos la prueba t, para el coeficiente b2
#se rechaza H0. Aqu� el contraste es H0: b2=0 vs Ha: b2 != 0
#Esto nos indica, que a�n considerando a la variable X1 en el modelo
#la variable X2 nos est� agregando informaci�n para modelar E(Y|X1,X2).

#Con base en lo anterior, parece que no podr�amos reducir el modelo, todos 
#los coeficientes parecen significativos.

#Vamos a interpretar este modelo

#R2, el coeficiente de determinaci�n, nos indica que se est�
#explicando el 99.8% de la variabilidad observada en Y a trav�s 
#del modelo y=b0+b1x1+b2x2+e

#Adem�s, b1 se puede interpretar como
#condicionado en un valor fijo de X2, el promedio de la variable 
#Y aumenta en .5 unidades al aumentar en una unidad la variable X1.

#Por otro lado, b2 se puede interpretar como
#condicionado en un valor fijo de X1, el promedio de la variable 
#Y aumenta en .4 unidades al aumentar en una unidad la variable X2.

#Notemos adem�s que al incluir dos variables en el modelo
#se puede tener un mejor ajuste con base en el coeficiente de
#determinaci�n y que ambas variables resultan significativas
fitred1=lm(y~X2, data=Datos)
summary(fitred1)

fitred2=lm(y~X1, data=Datos)
summary(fitred2)

