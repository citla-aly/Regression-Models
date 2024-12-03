datos <- read.csv("VentasMensuales.csv")
plot(datos, main="Ventas Mensuales", xlab="Mes", ylab="Ventas")

# grafica las prediccion
plot_predictions <- function(yhat) {
  plot(datos, main="Ventas Mensuales", xlab="Mes", ylab="Ventas", col=1)
  lines(x, yhat, col=2)
  legend("topleft", c("Datos", "Pred"), fill=c(1, 2))
}

# Ejercicio 1
## Ajustamos un modelo lineal y mostramos la suma de los errores

x <- datos[,1]
y <- datos[,2]
estimaciones <- lm(y~x)
residuales <- estimaciones[2][[1]]
err1 <- sum(abs(residuales))  # suma del absoluto de los residuos
err1

b0 <- estimaciones[1][[1]][[1]]
b1 <- estimaciones[1][[1]][[2]]
plot_predictions(b0 + b1*x)

# Ejercicio 2
## Modelo periodico cada 48 meses

modelo2 <- function(b0, b1, b2)
  return(b0 + b1*sin(2*pi*x/48) + b2*cos(2*pi*x/48))

sum_error <- function(b){
  b0 = b[1]
  b1 = b[2]
  b2 = b[3]
  return(sum((y - modelo2(b0, b1, b2))^2))
}

estimaciones <- nlm(sum_error, c(0, 0, 0))
b0 <- estimaciones$estimate[[1]]
b1 <- estimaciones$estimate[[2]]
b2 <- estimaciones$estimate[[3]]
err2 <- sum_error(c(b0, b1, b2))  # suma de los errores para el modelo 2
err2

plot_predictions(modelo2(b0, b1, b2))

# Ejercicio 3
## Ajustamos modelo no lineal estimando la periodicidad también

modelo3 <- function(b0, b1, b2, b3, b4, x_=x)
  return(b0 + b1*sin(2*pi*x_/b3) + b2*cos(2*pi*x_/b4))

sum_error <- function(b){
  b0 = b[1]
  b1 = b[2]
  b2 = b[3]
  b3 = b[4]
  b4 = b[5]
  return(sum((y - modelo3(b0, b1, b2, b3, b4))^2))
}

estimaciones <- nlm(sum_error, c(0, 0, 0, 35, 35))
b0 <- estimaciones$estimate[[1]]
b1 <- estimaciones$estimate[[2]]
b2 <- estimaciones$estimate[[3]]
b3 <- estimaciones$estimate[[4]]
b4 <- estimaciones$estimate[[5]]
err3 <- sum_error(c(b0, b1, b2, b3, b4))  # suma de los errores para el modelo 2
err3

estimaciones$estimate  # parametros estimados
plot_predictions(modelo3(b0, b1, b2, b3, b4))

# Comparamos los tres errores:
errores <- c(err1, err2, err3)
names <- c("Lineal simple", "No lineal Periodico", "No lineal")
tabla_comparativa <- data.frame(names, errores)
tabla_comparativa



# Ejercicio 4
## Cual escogeriamos?

"Depende de la situacion."
"Si no conocemos la periodicidad de nuestros datos, escogemos el tercero pues"
"es el que cuenta con menor error con nuestros datos\n"

"Sin embargo, si sabemos de la periodicidad nos vamos con el modelo dos"
"En comparacion con el primer modelo, el error del dos y tres es"
"considerablemente mas bajo. Podemos observar esto en las graficas generadas\n"

modelo3(b0, b1, b2, b3, b4, x_=c(max(x) + 1))  # estimado del siguiente mes

