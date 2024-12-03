#### 1) Graficar datos (scatterplot), sin transformar y luego transformando

# Cargamos los datos
setwd("C:/Users/dell/Desktop/OCTAVO SEMESTRE/Estadística 2 Ayudantía")
initech  <-  read.csv("initech.csv")

# Graficamos
par(mfrow=c(2,2))
plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1,
     main = "salary ~ years")
plot(log(salary) ~ years, data = initech, col = "grey", pch = 20, cex = 1,
     main = "log(salary) ~ years")
plot(salary ~ log(years), data = initech, col = "grey", pch = 20, cex = 1,
     main = "salary ~ log(years)")
plot(log(salary) ~ log(years), data = initech, col = "grey", pch = 20, cex = 1,
     main = "log(salary) ~ log(years)")
par(mfrow=c(1,1))

#### 2) Ajustar la regresión a los datos transformados que mejor ajusten un modelo de RLS.

# En este caso el mejor modelo es log(salary) ~ years

# a) A mano

attach(initech)
x = years
y = log(salary)
detach(initech)
sxy = sum((x-mean(x))*(y-mean(y)))
sxx = sum((x-mean(x))^2)
(b1 = sxy/sxx)
(b0 = mean(y)-b1*mean(x))

# b) Usando lm
m1 = lm(y ~ x)
summary(m1)

# De aquí tenemos el modelo es log(salary) = 10.483806 + 0.078879 years
# - >  log(y) = 10.483806 + 0.078879x

#### 3) Interpretación de parámetros

# b0: Para una persona que acaba de entrar a la empresa, el promedio del logaritmo 
#     de su salario es de 10.48381.

# b1: Por cada año que incrementa el trabajador dentro de la empresa, hay un aumento
#     en el promedio del logatimo del salario en 0.07888.

# Aplicamos la función exponencial al modelo log(salary) = 10.483806 + 0.078879 years
# - >  exp (log(y)) = exp(10.483806 + 0.078879x)
# - >  y = exp(10.483806 + 0.078879x)
# - >  salary = exp( 10.483806 + 0.078879 years)

# b0: Para una persona que acaba de entrar a la empresa, el promedio de su salario es 
#     de exp(10.483806) = $35,732.15

# b1: Por cada año que incrementa el trabajador dentro de la empresa, hay un aumento
#     del 8.21%

#     salary = exp( 10.483806 + 0.078879 (1)) = $38,664.80
#     $38,664.80 / $35,732.15 = 1.082073
#     1.082073 - 1= 0.082073 -> 8.21%

#### 4) Graficar datos transformados (scatterplot) y recta ajustada en la misma gráfica.

plot(log(salary) ~ years, data = initech, col = "grey", pch = 20, cex = 1,
     main = "log(salary) ~ years")
abline(m1, col="red")

#### 5) Graficar datos no transformados (scatterplot) y curva asociada a la recta
#       ajustada en la misma gráfica.

mod = function(x){exp(b0+b1*x)}
plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1,
     main = "salary ~ years")
curve(mod,add=TRUE, col="red")


