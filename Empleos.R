#install.packages("readr")
library(readr) #leer archivos csv
library(car) #graficar

empleo <- data.frame(read_csv("empleo.csv"))  #Lectura de la base de datos
  # La base contiene la siguiente información : 
  # Y: People employed
  # X1: Percentage price deflation
  # X2: GNP in millions of dollars 
  # X3: Number of unemployed in thousands 
  # X4: Number of people employed by the military
  # X5: Number of people over 14
  # X6: Year

# Diseño de la variable respuesta 
  Y <- empleo$Y

# Elaboración de la mátriz de diseño 
  X <- as.matrix(empleo[,-7])

# Número de observaciones
  n <- length(Y)

# Número de covariables
  k <- 6 #No hay intercepto
  p <- k

#Ajuste del modelo lineal-> Nos da información de R^2 y R^2 ajustada
# yi = b1x1 + b2x2 + b3x3 + b4x4 + b5x5 +b6x6 + ei --> sin intercepto
  model <- lm(Y ~ -1+ X) 
  summary(model) #F prueba conjunta, T pruebas ind.

#a) Construya la tabla ANOVA y haga la prueba de significancia de la regresión. alpha = 0.05 
  anova(model) #F-statistic: 5.075e+04 >  0.05 entonces se rechaza la hipótesis. 

#b) Cálcule R^2 y R^2 ajustada
  #R-squared:      1,	Adjusted R-squared:  0.9999  

#c) Encuentre los intervalos de confianza al 98% de confianza 
#para cada uno de los parámetros de este modelo.
  confint(model,level = 0.98)

#d) Lleve a cabo la prueba de hipótesis H0:bi=0, alpha = 0.03
  #De acuerdo al ajuste del modelo tenemos que las variables que no se rechazan son:
  # Para x1 0.6888 > 0.03 
  # Para x2 0.0401 > 0.03
  # Para x3 0.3311 > 0.03
  # Para x4 0.0668 > 0.03 
  # Para x5 0.2280 > 0.03

#e) Lleve a cabo la prueba de hipótesis H0 : Bi = 2 vs H1 : Bi > 2, alpha = 0.03
  alpha = 0.03 
  beta_est <- solve(t(X)%*%X)%*%t(X)%*%Y #Vector de las estimaciones de beta i. 
  y_aj <- X%*%beta_est # Vector de y ajustada
  sigma_estimada <- sum((Y-y_aj)^2/(n-p)) #Sigma cuadrada insesgada
  Sxx <- sum((X - mean(X))*(X-mean(X))) 
  
  t <- vector()
  t = lapply((beta_est[1:6] - 2)/sqrt(sigma_estimada/Sxx), c) #Vector de las estadisticas calculadas para cada beta i
  cuantil <- qt(1-alpha/2,n-p) #Cuantil
  
  #Se hace la prueba para cada uno de los parámetros, aplicando el sig criterio : t >= cuantil
  vector_respuestas <- vector() 
  for (i in 1:6) {
    vector_respuestas <- c(vector_respuestas,ifelse(t[i] > cuantil,"Rechazar" ,"No se rechaza"))
  }
  vector_betas <- c("b1","b2","b3","b4","b5","b6")
  resultados <- data.frame("Parámetro" = vector_betas, "Prueba:" = vector_respuestas)
  
#f) H_0 :b1 = b2 = b5 = 0 
  X2 <- X[,c(1,2,5)] # Las covariables distintas de cero
  X1 <- X[,-c(1,2,5)] # Las variables iguales a cero

  modelo2 <- lm(Y ~ -1 + X1 + X2) 
  anova(modelo2)
  #Pr(>F) = 2.2e-16  es muy pequeño, para cualquier alpha se rechazaría. Así que los  parámetros
  #pueden ser cero simultaneamente.
