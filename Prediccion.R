datos <- read.csv("C:\\Users\\citla\\OneDrive\\Documents\\CEMX.csv")
plot(datos, main="Share price", xlab="Mes", ylab="Precio")

# grafica las prediccion
plot_predictions <- function(yhat) {
  plot(datos, main="Share price", xlab="Mes", ylab="Precio", col=1)
  lines(x, yhat, col=2)
  legend("topleft", c("Datos", "Pred"), fill=c(1, 2))
}

x <- datos[,1]
y <- datos[,2]

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

estimaciones <- nlm(sum_error, c(0, 0, 0, 70,70)) #Se da una propuesta inicial, tomando una periodicidad mensual (72 meses = 6 años)
b0 <- estimaciones$estimate[[1]]
b1 <- estimaciones$estimate[[2]]
b2 <- estimaciones$estimate[[3]]
b3 <- estimaciones$estimate[[4]]
b4 <- estimaciones$estimate[[5]]
err3 <- sum_error(c(b0, b1, b2, b3, b4))  # suma de los errores para el modelo 2
err3

estimaciones$estimate  # parametros estimados
plot_predictions(modelo3(b0, b1, b2, b3, b4))


for (i in 1:13) {
  print(modelo3(b0, b1, b2, b3, b4, x_=c(max(x) + i)) - 1)  # estimado de los siguientes 12 meses 
}




