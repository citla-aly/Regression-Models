# Datos Ejemplo- ANOVA dos grupos

rm(list = ls(all.names = TRUE))
gc()


par(mar=c(4, 5, 2, 1))
par(mfrow=c(1,1)) 

library(AID)
data(AADT)
Datos=AADT

#AADT: Average Annual Daily Traffic Data
#the response is taken as AADT, which is the annual average of the number
#of vehicles that pass through a road each day
#Sólo noninterstate. Rural vs Urban  
summary(Datos)

levels(Datos$class)
library(tidyverse)
Datosfil=Datos %>% filter (class %in% c( "rural noninterstate","urban noninterstate"))
Datosfil$class=droplevels(Datosfil$class)
is.factor(Datosfil$class)
levels(Datosfil$class)
levels(Datosfil$class) <- list(RNI = "rural noninterstate", UNI = "urban noninterstate")
levels(Datosfil$class)

boxplot(aadt ~ class, data = Datosfil )

fit <- lm(aadt ~ class, data=Datosfil)

Datosfil=Datosfil %>% select(aadt, class)
Datosfil$zclassUNI= (Datosfil$class=="UNI")*1

fitB <- lm(aadt ~ zclassUNI, data=Datosfil)

library(jtools)
export_summs(fit, fitB, scale = FALSE)

#E(aadt|class=RNI)=b0=mu_1$      3464.55
#E(aadt|class=UNI)=b0+b1=mu_2$   3464.55+13756.47=17221.02


fitC <- lm(aadt ~ I(class=="RNI"), data=Datosfil)
options(digits=7)
summary(fitC)
coef(fitC)

#Usando modelo fitC
#E(aadt|class=RNI)=b0+b1=mu_1$      17221.03 -13756.47 = 3464.56
#E(aadt|class=UNI)=b0=mu_2$         17221.03



#Homocedasticidad
plot(fit, 3)
library(lmtest)
lmtest::bptest(fit)
library(car)
car::ncvTest(fit)


#Normalidad
Datosfit=augment(fit)
head(Datosfit)
plot(fit, 2)
shapiro.test(Datosfit$.std.resid)
library(nortest)
nortest::lillie.test(Datosfit$.std.resid)
library(normtest)
normtest::jb.norm.test(Datosfit$.std.resid)

hist(Datosfit$.std.resid)

### Otras pruebas para grupos

#Pruebas de homocedasticidad para grupos
# H_o: las varianzas de los grupos es la misma vs H_a: al menos un grupo tiene una varianza diferente
bartlett.test(aadt ~ class, data=Datosfil)

#Otra prueba más robusta 
library(car)
leveneTest(aadt ~ class, data=Datosfil)


#Para normalidad
shapiro.test( Datosfil$aadt[Datosfil$class=="RNI"])
nortest::lillie.test(Datosfil$aadt[Datosfil$class=="RNI"])
normtest::jb.norm.test(Datosfil$aadt[Datosfil$class=="RNI"])
shapiro.test( Datosfil$aadt[Datosfil$class=="UNI"])
nortest::lillie.test(Datosfil$aadt[Datosfil$class=="UNI"])
normtest::jb.norm.test(Datosfil$aadt[Datosfil$class=="UNI"])


summary(powerTransform(fit))

Datosfil$Ytransf=bcPower(Datosfil$aadt, lambda=0)


boxplot(Ytransf  ~ class, data = Datosfil )

fitln <- lm(Ytransf  ~ class, data=Datosfil)
plot(fitln, 3)
lmtest::bptest(fitln)
car::ncvTest(fitln)


Datosfitln=augment(fitln)
head(Datosfitln)


plot(fitln, 2)
shapiro.test(Datosfitln$.std.resid)
nortest::lillie.test(Datosfitln$.std.resid)
normtest::jb.norm.test(Datosfitln$.std.resid)

library(MASS)
ResStudent=studres(fitln)
ks.test(ResStudent, "pt", length(ResStudent)-3)
hist(ResStudent)


### Otras pruebas para grupos

#Pruebas de homocedasticidad para grupos
# H_o: las varianzas de los grupos es la misma vs H_a: al menos un grupo tiene una varianza diferente
bartlett.test(Ytransf  ~ class, data = Datosfil)

#Otra prueba más robusta 
leveneTest(Ytransf  ~ class, data = Datosfil)


#Para normalidad
shapiro.test( Datosfil$Ytransf[Datosfil$class=="RNI"])
nortest::lillie.test(Datosfil$Ytransf[Datosfil$class=="RNI"])
normtest::jb.norm.test(Datosfil$Ytransf[Datosfil$class=="RNI"])
shapiro.test( Datosfil$Ytransf[Datosfil$class=="UNI"])
nortest::lillie.test(Datosfil$Ytransf[Datosfil$class=="UNI"])
normtest::jb.norm.test(Datosfil$Ytransf[Datosfil$class=="UNI"])

summary(fitln)

#El primer nivel es la referencia o valor 0 en la variable Z

fitln2 <- lm(Ytransf  ~ relevel(class, ref = "RNI"), data=Datosfil)
summary(fitln2)

fitln3 <- lm(Ytransf  ~ relevel(class, ref = "UNI"), data=Datosfil)
summary(fitln3)


library(multcomp)
summary(fitln)
#E(log(aadt)|class=RNI)=b0=mu_1$
#E(log(aadt)|class=UNI)=b0+b1=mu_2$

##Supongamos que el investigador tenía la hipótesis
##En las carreteras "urban noninterstate" hay más tráfico
# H_0: mu_1>=mu_2 vs H_a: mu_1<mu_2
#H_0: b0>=b0+b1 vs H_a: b0<b0+b1
#H_0: 0>=b1 vs H_a: 0<b1     

#theta=zo bo+z1 b1
MatZ0Z1=matrix(c(0,1), ncol=2, nrow=1)
c=0
prueba1=glht(fitln, linfct=MatZ0Z1, rhs=c, alternative ="greater")
summary(prueba1)

# Se rechaza H0. Hay evidencia con una significancia de .05
# de que en promedio el
# "Average Annual Daily Traffic Data" en escala logarítmica es mayor en 
# las carreteras "urban noninterstate"

# Hay evidencia con una significancia de .05
# de que la mediana del
# "Average Annual Daily Traffic Data" es mayor en 
# las carreteras "urban noninterstate"





#Ejemplo. Comparación de medias cuando se tienen dos grupos con diferente varianza,
# pero se puede asumir normalidad (en este ejemplo no es así, pero se ejemplifica)

### oneway.test (diferente varianzas, posiblemente más de dos niveles)
# Sólo comparación H_0: mu_1=mu_2 vs H_a: mu_1!=mu_2
oneway.test(aadt ~ class, data = Datosfil, var.equal = FALSE)

# Si var.equal = TRUE, sería similar a lo que se obtiene con lm
oneway.test(aadt ~ class, data = Datosfil, var.equal = TRUE)

summary(fit)

#Otra alternativa, sólo dos niveles
levels(Datosfil$class)
#H_0: mu_1=mu_2 vs H_a: mu_1!=mu_2
t.test(aadt ~ class, data = Datosfil, var.equal = FALSE)
t.test(Datosfil$aadt[Datosfil$class=="RNI"], Datosfil$aadt[Datosfil$class=="UNI"], var.equal = FALSE)

# H_0: mu_1>=mu_2 vs H_a: mu_1<mu_2
#Considera como mu_1 a la media asociada al primer nivel de la variable.
t.test(aadt ~ class, data = Datosfil, alternative ="less", var.equal = FALSE)
t.test(Datosfil$aadt[Datosfil$class=="RNI"], Datosfil$aadt[Datosfil$class=="UNI"], alternative ="less", var.equal = FALSE)
