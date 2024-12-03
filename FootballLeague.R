intervalos <- function(SCE, n, alfa) {
	longitud <- function(x)
		qchisq(1 - alfa + pchisq(x, n - 2), n - 2) - x

	distancia <- function(x)
		SCE/x - SCE/(x + longitud(x))

	x0 <- 1.0
	paso <- 0.0001
	for (i in 1:200000)
		if (distancia(x0 + paso) < distancia(x0 - paso))
			x0 <- x0 + paso
		else
			x0 <- x0 - paso
	return(c(SCE/(x0 + longitud(x0)), SCE/x0))
}

datos <- read.csv("FootballLeague.csv")
X <- datos[,"x8"]
Y <- datos[,"y"]
n <- length(X)
est <- lm(Y~X)

alfa = 0.05
SCE <- sum((Y - fitted.values(est))^2)
intervalos(SCE, n, alfa)
