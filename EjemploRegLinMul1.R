# Regresión lineal múltiple
# Ajuste e interpretación

rm(list = ls(all.names = TRUE))
gc()

setwd("C:/Users/Gonzalo/Documents/GitHub/Notas 2021-2/Mod Reg/")
Datos=read.csv("ejemplo1RLM.csv", header=TRUE )


par(mfrow=c(1,2),mar=c(4.5,4.5,1,1))
pairs(Datos)

# E(y|x1, x2)=b0+b1x1+b2x2 

fit=lm(y~X1+X2, data=Datos)

summary(fit)

sigma(fit)


#Anova. Interpretación

#H0: b1=0 y b2=0 vs Ha: b1!=0 o b2!=0

#Obtención de la prueba Anova vía modelos la prueba lineal general.
library(multcomp)
K=matrix(c(0,1,0,
           0,0,1), ncol=3, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

#Comparación con pruebas t.

#Las pruebas t están asociadas a cada coeficiente de forma "no simultánea"
#Para su interpretación de debe responder que significa el modelo que considera
#ese parámetro igual a cero.

#En este caso tenemos b0, b1 y b2. 

#La prueba H0: b1=0 vs Ha: b1 != 0, nos lleva a concluir que no hay evidencia 
# para rechazar H0 con una significancia de alpha=.05
# p-value=0.29 > .05

#El modelo que considera que b1=0, sería uno en el que sólo está X2
#En este caso, la interpretación de la prueba se basa en:
#¿La inclusión de la variable X1 una vez que se tiene el modelo y=b0+b2X2+e 
#nos está o no agregando información adicional; en otras palabras,
#condicional en X2, X1 nos agrega información adicional para modelar E(Y|X)?
#Cuando no se rechaza Ho, parece que los datos nos sugieren que es plausible considerar
#el modelo "reducido" y=bo+b2X2+e contra el modelo completo y=bo+b1X1+b2X2+e,
# mientras que si se rechaza H0, entonces sí nos agrega X1
#información al modelo y se prefiere y=bo+b1X1+b2X2+e.


#Un análisis similar se puede hacer con b2. Lo importante es notar que estos análisis
#corresponden a preguntas independientes sobre cada parámetro en el modelo. 


#Las pruebas t, es decir, sobre una sóla combinación de los parámetros
#también se pueden obtener de forma directa con el paquete multcomp de dos 
#maneras

#H0: b1=0 vs Ha: b1 != 0
library(multcomp)
K=matrix(c(0,1,0), ncol=3, nrow=1, byrow=TRUE)
m=c(0)
#usando prueba F equivalente (no permite pruebas de una cola)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

#usando prueba t (permite prueba de una cola)
summary(glht(fit, linfct=K, rhs=m))

#H0: b1<=0 vs Ha: b1 > 0
summary(glht(fit, linfct=K, rhs=m, alternative = "greater") )


#Nota. Se puede ver que las pruebas F y t de dos colas son casos 
#particulares de modelos reducidos

#por ejemplo, la función anova nos sirve para comparar modelos dos modelos anidados
#sin necesidad de usar la definición matricial

#por ejemplo si b1=0, entonces el modelo reducido es

fitred1=lm(y~X2, data=Datos)
summary(fitred1)

anova(fitred1, fit)

#(SCEred-SCEc)/SCEc  * (n-p-1)/r
SCEred=sigma(fitred1)^2*fitred1$df.residual 
SCEcom=sigma(fit)^2*fit$df.residual 
#n-p-1 son los grados de libertad del modelo completo
#r las combinaciones lineales necesarias para obtener el reducido

( (SCEred-SCEcom)/SCEcom *(fit$df.residual/1) )


#algo similar para obtener la prueba de la tabla anova
# en ese caso el modelo reducido es y=b0+e cuando H0: b1=0 y b2=0 

fitred2=lm(y~1, data=Datos)
summary(fitred2)
anova(fitred2, fit)


## Selección entre los modelos. Aquí casi los tres modelos
## indican un mismo coeficiente de determinación, se seleccionaría en 
## general el de menos parámetros y con el mayor coeficiente de determinación
## aunque entrarán otros criterios BIC y AIC (se verán más adelante)

summary(fit)


fitred1=lm(y~X2, data=Datos)
summary(fitred1)

fitred2=lm(y~X1, data=Datos)
summary(fitred2)

