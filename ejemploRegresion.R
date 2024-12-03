library(readxl)


ageandheight <- read_excel("ageandheight.xls", sheet = "Hoja2") 

#Data frame = Base de datos
class(ageandheight)

# y=B0+B1x
# y=mx+b=b+mx m-Pendiente b-ordenada al origen 
#y-Altura, x-Edad

lmHeight = lm(height~age, data = ageandheight)

lmHeight1 = lm(height~age+no_siblings, data = ageandheight)


summary((lmHeight1))
#Observado
plot(ageandheight$age, ageandheight$height, col='red', main='Resumen del Modelo', xlab='Age', ylab='Height')

#Agregando el modelo
abline(lmHeight,col='blue')

summary(lmHeight ) 


#B1= Sxy/Sxx
Sxx<-sum((ageandheight$age-mean(ageandheight$age))^2)
Sxy<-sum((ageandheight$age-mean(ageandheight$age))*(ageandheight$height-mean(ageandheight$height)))
B1_est<-Sxy/Sxx


#B0= y^-B1x^
B0_est<-mean(ageandheight$height)-B1_est*mean(ageandheight$age)



#sigma^2=sum(yi-yi^)/n-2

ageandheight[1,]
predict<-B0_est+B1_est*ageandheight$age

cbind(ageandheight$height,predict)


sigma_est<-(sum((ageandheight$height-predict)^2))/(nrow(ageandheight)-2)


t<-B1_est/sqrt(sigma_est/Sxx)


#Predicción manual 
predict<-B0_est+B1_est*18

predict(lmHeight, newdata = data.frame(age=18))
predict(lmHeight, newdata = data.frame(age=24))
predict(lmHeight, newdata = data.frame(age=c(17,23,24,25)))


#p-value explícito
summary(lmHeight)

p_value<- (1-pt(0.210,10))*2



#Quitando la observación atípica
data<-ageandheight[-c(7),]

regresion<-lm(formula = height~age,data = data)
summary(regresion)


plot(data$age, data$height, col='red', main='Resumen del Modelo', xlab='Age', ylab='Height')
abline(regresion,col='blue')


#ANOVA
anova(lmHeight)


###########Para siguiente clase 
setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path))





