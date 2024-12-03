# install.packages("rapportools")
# install.packages("report")
library(rapportools)
library(report)
df <- read.csv("FootballLeague.csv")

x <- df[,"x8"]
y <- df[,"y"]
n <- length(x)
n

# Ajustar modelo
est <- lm(y~x)
b0 <- coefficients(est)[[1]]
b1 <- coefficients(est)[[2]]
yhat <- fitted.values(est)
est

# Intervalo de confianza para b1
alpha <- 0.05
xbar <- sum(x)/n
sxx <- sum((x - xbar)^2)
sigma2 = sum((y - yhat)^2)/(n - 2)
deviate <- qt(1.0 - alpha/2, n-2)*sqrt(sigma2/sxx)
a <- b1 - deviate
b <- b1 + deviate
c(a, b)  # Intervalo de confianza
a <= 0 && 0 <= b  # 0 esta en el intervalo?

# Intervalo de confianza para b0
deviate <- qt(1.0 - alpha/2, n-2)*sqrt(sigma2*(1/n + xbar^2/sxx))
a <- b0 - deviate
b <- b0 + deviate
c(a, b)  # Intervalo de confianza
confint(est, level = 1 - alpha) # los intervalos de confianza coinciden

# Intervalo de confianza sigma2
sce <- sum((y - yhat)^2)
a <- sce/qchisq(1 - alpha/2, n-2)
b <- sce/qchisq(alpha/2, n-2)
c(a, b)  # Intervalo de confianza

# Construcción de la tabla ANOVA 
df$y <- as.factor(df$y)
anova(lm(y~x))
summary(lm(y~x))  #p value  
plot(lm(y~x))
report(lm(y~x)) #Rechazamos H0

# Confint para E[Y | X = 2000]
predict.lm(est, data.frame(x = c(2000)), interval = "confidence", level = 0.95)
# Confint para Y | X = 2072
predict.lm(est, data.frame(x = c(2072)), interval = "prediction", level = 0.99)
