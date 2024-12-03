####################### EJERCICIO EXTRA ###########################
# En un nuevo script deberán copiar y pegar esta parte, posteriormente
# deberán responder en orden y por último guardar el script
# como "A2. Nombre completo comenzando por apellido" y subirlo a classroom:

# Vectores: En un vector llamado famosos escribe tres de ellos.
famosos <- c("Shakira","Justin Bieber","Robert Downey")
famosos 
# En otro vector llamado edades colocarÃ¡s sus edades respectivamente.
edades <- c(40,28,52)
# En otro llamado preferido(a) escogerán a uno de los famosos utilizando el vector anterior.
preferido <- c(famosos[1])
preferido 

# Listas: En una lista coloca los vectores famosos y edades,
# llÃ¡malo list_famosos
list_famosos <- list(famosos, edades)
list_famosos

# Matrices: Declara la matriz identidad de 3x3, llámala iden3
iden31 <-matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3 , ncol = 3) #primerforma
iden3 <- diag(3) #segundaforma
iden31
iden3

# Factores: 5 de ustedes responderán en el chat la pregunta ¿Qué edad tienes?
# y todos deberán declarar una variable de tipo factor llamada edades_clase
# con las respuestas ahí dadas, posteriormente utilicen la función table para
# observar las frecuencias
edades_clase <- factor(c(20,21,19,24,20))
edades_clase
table(edades_clase) 
# Data frame: Crea un data frame con los vectores de famosos y edades,
# llámalo data_famosos
data_famosos <- data.frame(famosos, edades)
data_famosos

# Operaciones básicas: Cuánto es...
# 23 elevado a la 4?
elevar <- 23^4 
elevar 
# 542 módulo 51?
modulo <- 542%%51 #residuo de la división o módulo
modulo

# las combinaciones de 53 en 5?
combinaciones <- choose(53,5)
combinaciones 

# Operaciones vectoriales: Supón que durante 5 días de la semana fuiste al casino.
# Las ganancias que obtuviste en la ruleta fueron -24, -50, 100, -350, 10
# Las ganancias que obtuviste en el póker fueron 140, -50, 20, -120, 240
# Utilizando vectores obtén la ganancia de la semana por juego y el total.
 
gananciasRuleta <- c(-24,-50, 100, -350, 10)
gananciasPoker <- c(-140, -50, 20, -120, 240)

gananciaTotalR <- sum(gananciasRuleta)
gananciaTotalR
gananciaTotalP <- sum(gananciasPoker)
gananciaTotalP

# Operaciones matriciales: Resuelve el siguiente sistema de ecuaciones:
# 4x - y + 5z = 23
# x + 3y -10z = -20
# -3x + 4z = -1


# Estructuras de control: Crea una funciÃ³n que arroje del 1 al 10 y que diga
# si es par o impar
