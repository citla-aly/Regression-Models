library(readxl)
Delitos <- read_excel("~/Facultad Ciencias/Ayudantías/Estadistica2/Clases/Delitos ocurridos.xlsx")
View(Delitos)

# Generamos las frecuencias relativas
Delitos <- Delitos[-11,] #Quitamos el total
Frecuencias <- Delitos[,2]/sum(Delitos[,2]); sum(Frecuencias)

Delitos <- cbind(Delitos, Frecuencias)
names(Delitos)<- c("Tipo de delito", "Delitos ocurridos", "Frecuencias")

# Generamos gráficas
barplot(Delitos[,2], names.arg = Delitos[,1])

library(ggplot2)
g1<-ggplot(Delitos)+geom_col(aes(x=`Tipo de delito`,y=`Delitos ocurridos`/1000), fill="lightsteelblue3")+
  labs(x="Tipo de Delito", y="Delitos ocurridos", title="Delitos ocurridos por tipo 2019 (en miles)")+
  theme_bw()+coord_flip()+theme(plot.title = element_text(hjust = 0.5))
g1; ggsave("Delitos.pdf",plot=g1, width=6)

g2<-ggplot(Delitos)+geom_col(aes(x=`Tipo de delito`,y=Frecuencias), fill="lightsteelblue3")+
  labs(x="Tipo de Delito", y="Frecuencias de los delitos", title="Frecuencias de Delitos ocurridos por tipo 2019")+
  theme_bw()+coord_flip()+theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(0,2.5,0.05))
g2; ggsave("Frecuencias.pdf", plot=g2, width=6.5)
