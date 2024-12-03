

###########################################################################
######## LAB. 2 - AN?LISIS EXPLORATORIO Y VISUALIZACI?N DE DATOS ##########
###########################################################################

# ?Por qu? es importante hacer el an?lisis exploratorio de datos antes de ajustar alg?n
# modelo estad?stico o aplicar un algoritmo de machine learning? Ir a la b?squeda
# El an?lisis exploratorio de datos es una forma de analizar datos definido por 
# John W. Tukey (E.D.A.: Exploratory data analysis) es el tratamiento estad?stico 
# al que se someten las muestras recogidas durante un proceso de investigaci?n en cualquier 
# campo cient?fico. El an?lisis previo de los datos nos permite detectar problemas de 
# representatividad en el muestreo, describir la existencia de dependencia entre las variables,
# e incluso detectar datos at?picos. 


# Estudios observacionales han sugerido que la ingesta diet?tica de bajas concentraciones
# de retinol y betacaroteno podr?an estar asociadas con un mayor riesgo de desarrollar
# ciertos tipos de c?ncer. 

# Sin embargo, poco se han estudiado los factores que determinan las concentraciones
# estos micronutrientes en plasma. 

# OBJETIVO DEL LABORATORIO: Investigar la relaci?n entre las caracter?sticas personales,
# los factores diet?ticos, y las concentraciones de retinol, betacaroteno y 
# otros carotenoides en plasma. Nos interesa, desde luego, conocer aquellos que causan 
# bajas concentraciones. Los 315 sujetos de estudio fueron pacientes que se sometieron a un
# procedimiento para tomar una biopsia o extirpar una lesi?n org?nica.

options(digits = 2)
setwd("~/Documents/Clases/ML20212/Intro")
#Cargar base de datos. 
library(readxl)
retinol <- read_excel("retinol_beta_caroteno.xlsx", col_names = FALSE) #usamos col_names=FALSE porque la base
                                                       #no incluye nombre de columnas

# La base de datos también puede precargarse a través del paquete qrjoint, en data(plasma).

# Nombre de las variables
names(retinol) <- c('edad','sexo','statfu','imc','usovit','calorias','grasa','fibra','alcohol','colesterol','betadieta','retidieta','betaplasma','retiplasma')

#Variables incluidas en la base de datos. 

#| Nombre     | Tipo | Descripci?n                                              |
#|------------|------|----------------------------------------------------------|
#| edad       | num  | en a?os                                                  |
#| sexo       | cat  | 1=hombre, 2=mujer                                        |
#| statfu     | cat  | estado de fumar (1=nunca, 2=ex, 3=s?)                    |
#| imc        | num  | ?ndice de masa corporal                                  |
#| usovit     | cat  | uso vitaminas (1=s?, frecuente; 2=si, no frecuente; 3=no |
#| calorias   | num  | calor?as consumidas por d?a                              |
#| grasa      | num  | g de grasa consumidos por d?a                            |
#| fibra      | num  | g de fibra consumidos por d?a                            |
#| alcohol    | num  | n?mero de bebidas alcoh?licas por semana                 |
#| colesterol | num  | colesterol consumido (mg por d?a)                        |
#| betadieta  | num  | betacaroteno diet?tico consumido (mg por d?a)            |
#| retidieta  | num  | consumo de retinol en la dieta (mg por d?a)              |
#| betaplasma | num  | betacaroteno en plasma (ng / ml)                         |

#Para ver que las variables est?n codificadas correctamente;
str(retinol) #Variables categ?ricas no son del tipo *factor*, por lo que se tiene que hacer
             #la modificaci?n correspondiente.

retinol$sexo<-as.factor(retinol$sexo); retinol$statfu<-as.factor(retinol$statfu); retinol$usovit<-as.factor(retinol$usovit) #Convertimo a factor

str(retinol) #Se observa que las variables ya son de tipo factor
anyNA(retinol) #No hay datos faltantes

###### Lo básico de graficar ######

plot(retinol$imc, retinol$colesterol) # == with(retinol, plot(imc, colesterol))
abline(1,1)
points(c(1,1))

par(mfrow=c(2,2))
plot(runif(10), runif(10))
plot(runif(10), runif(10), col="blue")
plot(runif(10), runif(10), col="red")
plot(runif(10), runif(10), col="darkgreen")

###############################  AN?LISIS EXPLORATORIO DE DATOS #############################

#Para realizar un breve resumen de las variables se puede usar el comando *summary*, que 
#arroja la media y medidas de dispersi?n para las variables continuas y las frecuencias para
#las variables categ?ricas. 

summary(retinol)


#Es importante recordar que estas medidas no son suficientes para realizar un correcto 
#an?lisis exploratorio. La visualizaci?n de datos permite visualizar de mejor manera la
#distribuci?n de cada una de las variables y su comportamiento. 

#########################    AN?LISIS DE VARIABLES CONTINUAS    #############################


##### CORRELACI?N Y COVARIANZAS

cor(retinol[,-c(2,3,5)]) #La correlaci?n mide la asociaci?n entre variables CONTINUAS
                         #es por eso que no consideramos las categ?ricas.
                         #?qu? podr?a significar una correlaci?n alta cuando ajustamos una
                         #regresi?n lineal? ?qu? problemas puede ocasionar?
cor(retinol$calorias, retinol$alcohol)

cov(retinol[,-c(2,3,5)]) #covarianza

#Visualizaci?n de la correlaci?n de las variables
# install.packages("corrplot")
library("corrplot")
retinol_r <- retinol[,-c(2,3,5)]
corr <- cor(retinol_r)
corrplot(corr)
corrplot(corr, method = "number")   #OJO: Las matrices de correlaci?n (igual que la
                                    #covarianza) son sim?tricas.

corrplot(corr, method = "ellipse", 
         col = colorRampPalette(c("seagreen3","salmon2"))(20))


corrplot(cor(retinol[,-c(2,3,5)]), method = "square", col = rainbow(20))


##### SCATTERPLOT (o diagramas de dispersi?n)

## GR?FICOS DE R

pairs(retinol[,-c(2,3,5)], pch = 20, col = "coral3", 
      main = "Dispersión de las variables")

# Le quitamos un número arbitrario sólo para que se vea algo
pairs(retinol[,-c(2,3,5:10)], pch = c(2,3,16)[retinol$statfu], cex = 1.2,
      col = c("plum3", "seagreen3")[retinol$sexo], 
      main = "Dispersi?n de las variables") # Diferenciaci?n entre sexo y estado de fumador

#Si s?lo se desea observar la dispersi?n entre dos variables (por ejemplo, para una 
#regresi?n lineal simple)

plot(retinol$grasa,retinol$calorias, pch = c(2,3,16)[retinol$statfu], cex = 2,
     col = c("plum3", "seagreen3")[retinol$sexo], xlab = "Gramos de grasa por día", ylab = "Calorías por día", 
     frame.plot = F, cex.axis = 1.5, cex.lab = 1.2, 
     main = "Gramos de grasa vs. calorías consumidas (por d?a) ") #cex va a indicar la escala
# por ejemplo, cex=2 significa que el tama?o de los puntos se duplicar? seg?n la escala default.
# tambi?n podemos agregar l?neas
abline(h = mean(retinol$calorias), lty = 2, col = "navyblue", lwd = 3)
text(100, mean(retinol$calorias) + 0.01, paste("Media: ", round(mean(retinol$calorias),40)))


## GGPLOT

# install.packages("GGally")
library(GGally)
ggpairs(retinol[,-c(2,3,5)]) #Este gr?fico nos permite visualizar 3 cosas: los diagramas
                             #de dispersi?n para las variables continuas, su correlaci?n
                             #y su densidad. 

##### HISTOGRAMAS

## GR?FICOS DE R
hist(retinol$calorias, main = "Calorías consumidas por día", xlab = " ",
     border = colorRampPalette(c("salmon", "blue"))(6), col = "darkslategray3")

hist(retinol$grasa, main = "Gramos de grasa consumidos por día", col = rainbow(15),
     xlab = " ", prob = TRUE, breaks = 15) 

#Si especificamos prob=TRUE, nos mostrar? la densidad (los valores estar?n entre 0 y 1)
#mientras que si no lo hacemos, nos muestra la frecuencia. breaks=15 hacemos que el "ancho"
#de las barras sea de 15 unidades. 

## GGPLOT

ggplot(retinol, aes(alcohol)) +
  geom_histogram(bins=30, colour = "white",
                 fill = colorRampPalette(c("powderblue", "thistle3"))(30)) +
  labs(x="Bebidas alcohólicas por semana", y="Frecuencia", title="Histograma del n?mero 
       de bebidas alcohólicas por semana") +
  theme_minimal()+
  theme(plot.title = element_text(size = 14, hjust = 0.5)) 

ggplot(retinol, aes(x = grasa, y = calorias, shape = statfu, colour = sexo)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(1, 2, 3)) +
  scale_colour_manual(values = c("violetred4", "powderblue")) +
  ggtitle("Gramos de grasa vs. Calorías consumidas (por día)") +
  xlab("Peso") + ylab("Altura") + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) 


  

#Se observa que hay una persona que consume m?s de 200 bebidas alcoh?licas a la semana.
#Esto podr?a ser ejemplo de un dato at?pico o un error de dedo por parte del capturista.
#?Qu? se podr?a hacer cu?ndo se presentan datos at?picos? ?Es correcto eliminarlos?

max(retinol$alcohol)
which(retinol$alcohol==203); which(retinol$alcohol==max(retinol$alcohol)) 
retinol[62,]  #Observamos el resto de los datos para este individuo

#Si vemos su registro, tiene una ingesta diaria de calor?as muy alta,
#lo cual es consistente con su consumo de bebidas alcoh?licas.
#Sin ambargo, un dato at?pico as? de influyente (con consumo de calor?as y alcohol tan alto)
#puede sesgar las estimaciones de los par?metros de la regresi?n.
#Por ello decidimos quitar el registro.


##### BOXPLOTS

## GR?FICOS DE R
boxplot(retinol[,-c(2,3,5)]) #en los boxplots se ven medidas de localizaci?n (cuartiles,
                             #y mediana), adem?s, se pueden ir identificando datos at?picos.


boxplot(betadieta ~ usovit, data = retinol, frame.plot = FALSE, font.main = 1,
        border = colorRampPalette(c("salmon", "blue"))(6), outline = FALSE,
        cex.lab = 1.5, cex.axis = 2.5, cex.main = 1.8, lwd = 3,
        main = "Boxplot de betacaroteno consumido por día (mg)") 

#Usando los boxplots anteriores como referencia, se podr?a comenzar a tener evidencia de que
#la distribuci?n del betacaroteno consumido por d?a en persona que usan vitaminas de manera
#no frecuente y aquellas que no las utilizan, es igual. 
#Si se quisiera ajustar una regresi?n, posiblemente se podr?an colapsar estas dos categor?as
#para crear una sola. 

boxplot(betadieta ~ usovit, data = retinol, frame.plot = TRUE, font.main = 1,
        border = c("salmon", "cornflowerblue", "plum"), outline = FALSE,
        cex.lab = 1.2, cex.axis = 1.1, cex.main = 1.2, lwd = 3,
        main = "Boxplot de betacaroteno consumido por día (mg) según uso de vitaminas",
        xlab="Uso de vitaminas", ylab="Betacaroteno por día (mg)") 
stripchart(betadieta ~ usovit, vertical = TRUE, data = retinol, pch = 16, 
           method = "jitter", add = TRUE, col = c("salmon", "cornflowerblue", "plum"),
           jitter = 0.3, cex = 0.5)

## GGPLOT

ggplot(retinol, aes(usovit, betadieta)) +
  geom_boxplot(fill = c("powderblue", "thistle3", "seagreen2")) +
  xlab("Uso de vitaminas") + ylab("Betacaroteno por día (mg)") +
  ggtitle("Boxplot de betacaroteno consumido por día (mg) según uso de vitaminas")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(retinol, aes(x=sexo, y=betaplasma)) + geom_boxplot(size = 1, 
  fill = c("powderblue", "thistle3"))+ 
  geom_jitter(aes(group = sexo, alpha=0.02))+
  labs(x="Sexo", y="Betaplasma", title="Distribución de betaplasma por sexo")+
  theme(plot.title = element_text(hjust = 0.5))

#########################   AN?LISIS DE VARIABLES CATEG?RICAS   #############################

##### GR?FICAS DE PASTEL

## GR?FICAS EN R
pie(table(retinol$statfu)) 
par(lwd = 4)

lbls = paste(names(table(retinol$statfu)), "\n", 
             round(table(retinol$statfu)*100/length(retinol$statfu),1), "%", 
             sep = "")

pie(table(retinol$statfu), col = "white", border = c("seagreen3", "salmon", "plum"), 
    labels = lbls, cex = 1.5, radius = 1, main = "Tipos de abulones en Tasmania")
par(lwd = 1)

## GGPLOT

tb = as.data.frame(table(retinol$usovit))
pie<-ggplot(tb, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start=0) +
  scale_fill_manual(values = c("powderblue", "thistle3", "seagreen2")) +
  ggtitle("Frecuencia en uso de vitaminas") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5))+
  geom_text(aes(y = Freq/3 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = scales::percent(Freq/sum(Freq))), size=5)
pie
##### GR?FICAS DE BARRAS (no son lo mismo que los histogramas, estos se utilizan
                        # ?nicamente para variables discretas)
## GR?FICAS EN R

barplot(table(retinol$statfu), width=2, col = "lightblue",border = "cornflowerblue", axes=TRUE,
        main = "Estado de fumador (frecuencias)", xlab="Estado de fumador",
        ylab="Frecuencia")

## GGPLOT

barras<-ggplot(retinol)+geom_bar(aes(x=usovit), fill="lightblue", 
                         color="midnightblue", linetype="twodash", size=1.2)+
  labs(title="Frecuencias en el uso de vitaminas", 
       x="Uso de vitaminas", y="Frecuencia", caption="Descripción gráfica")+
  theme_dark()+ theme(plot.title = element_text(hjust = 0.5))
barras

##### ?c?mo exportar gr?ficos?

getwd() #no vayan a perder las gráfica

## GR?FICOS EN R
pdf("barras.pdf", height = 6, width = 10)

barplot(table(retinol$statfu), width=2, col = "lightblue",border = "cornflowerblue", axes=TRUE,
        main = "Estado de fumador (frecuencias)", xlab="Estado de fumador",
        ylab="Frecuencia")
dev.off()
## GGPLOT
#install.packages("gridExtra"); install.packages("ggpubr")
library(gridExtra); library(ggpubr)

#Podemos crear un grid con varios gr?ficos ggplot
graf<-grid.arrange(pie, barras, ncol=2,
                   top = text_grob("Gráficas de frecuencia en uso de vitaminas"
                                  ,just="centre"))
ggsave(filename="graf.pdf", graf, width=6, height=6) #Guardamos el grid en formato pdf
ggsave(filename = "pie.pdf", pie) #Guardamos s?lo uno de los gr?ficos, podemos no indicar dimensiones

