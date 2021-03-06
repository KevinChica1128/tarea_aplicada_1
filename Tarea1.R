install.packages("readr")
library("readr")
cadata <- read_delim("~/GitHub/tarea_aplicada_1/cadata.txt", 
                     " ", escape_double = FALSE, trim_ws = TRUE)
View(cadata)
cadata1<-cadata[,-c(1,2,4,6,8,10,12,14,16)]
cadata<- cadata1
rm(cadata1)
sample(1:20640,1)
## El n�mero aleatorio generado fue 15529 ##
# Eliminamos las filas que no necesitamos, nos quedamos con las filas desde la 15529 hasta la 16029 #
cadata<- cadata[-c(1:15528,16029:20640),]

##############################
#ANALISIS EXPLORATORIO
#ESTADISTICAS DESCRIPTIVAS
rbind(Valor_mediano_de_la_casa=summary(cadata[,1]),Ingreso_mediano=summary(cadata[,2]),Edad_mediana=summary(cadata[,3]),
      Total_de_habitaciones=summary(cadata[,4]),Total_de_dormitorios=summary(cadata[,5]),Poblacion=summary(cadata[,6]),
      Hogares=summary(cadata[,7]))
sd(cadata$Valor_mediano_de_la_casa)

#GR�FICOS:
x11()
par(mfrow=c(1,2))
hist(cadata$Valor_mediano_de_la_casa,freq = F,col="gray52",main="Histograma del valor mediano de las viviendas",ylab = "densidades",xlab = "valor mediano de la vivienda")
curve(dnorm(x, mean(cadata$Valor_mediano_de_la_casa), sd(cadata$Valor_mediano_de_la_casa)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Valor_mediano_de_la_casa),main="Densidad del valor mediano de la vivienda",col="darkblue",ylab = "densidades",xlab = "valor mediano de la vivienda")
x11()
boxplot(cadata$Valor_mediano_de_la_casa,col="gray52",main="Gr�fico de cajas para la variable valor mediano de la vivienda",ylab="valor mediano de la casa")

x11()
par(mfrow=c(1,2))
hist(cadata$Ingreso_mediano,freq = F,col="gray52",main="Histograma del ingreso mediano de la casa",ylab = "densidades",xlab = "Ingreso mediano")
curve(dnorm(x, mean(cadata$Ingreso_mediano), sd(cadata$Ingreso_mediano)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Ingreso_mediano),main="Densidad del ingreso mediano",col="darkblue",ylab = "densidades",xlab = "Ingreso mediano")
x11()
boxplot(cadata$Ingreso_mediano,col="gray52",main="Gr�fico de cajas para la variable ingreso mediano de la vivienda",ylab="Ingreso mediano de la casa")


x11()
par(mfrow=c(1,2))
hist(cadata$Edad_mediana_de_la_vivienda,freq = F,col="gray52",main="Histograma de la edad mediana de la casa",ylab = "densidades",xlab = "edad mediana de la casa")
curve(dnorm(x, mean(cadata$Edad_mediana_de_la_vivienda), sd(cadata$Edad_mediana_de_la_vivienda)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Edad_mediana_de_la_vivienda),main="Densidad de la edad mediana de la casa",col="darkblue",ylab = "densidades",xlab = "edad mediana de la casa")
x11()
boxplot(cadata$Edad_mediana_de_la_vivienda,col="gray52",main="Gr�fico de cajas para la variable edad mediana de la casa",ylab="Edad mediana de la casa")


x11()
par(mfrow=c(1,2))
hist(cadata$Total_de_habitaciones, freq = F,col="gray52",main="Histograma del total de habitaciones",ylab = "densidades",xlab = "total de habitaciones")
curve(dnorm(x, mean(cadata$Total_de_habitaciones), sd(cadata$Total_de_habitaciones)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Total_de_habitaciones),main="Densidad del total de habitaciones",col="darkblue",ylab = "densidades",xlab = "total de habitaciones")
x11()
boxplot(cadata$Total_de_habitaciones,col="gray52",main="Gr�fico de cajas para el total de habitaciones",ylab="total de habitaciones")


x11()
par(mfrow=c(1,2))
hist(cadata$Total_de_dormitorios, freq = F,col="gray52",main="Histograma del total de dormitorios",ylab = "densidades",xlab = "total de dormitorios")
curve(dnorm(x, mean(cadata$Total_de_dormitorios), sd(cadata$Total_de_dormitorios)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Total_de_dormitorios),main="Densidad del total de dormitorios",col="darkblue",ylab = "densidades",xlab = "total de dormitorios")
x11()
boxplot(cadata$Total_de_dormitorios,col="gray52",main="Gr�fico de cajas para el total de dormitorios",ylab="Total de dormitorios")


x11()
par(mfrow=c(1,2))
hist(cadata$Poblacion,freq = F,col="gray52",main="Histograma de la poblaci�n",ylab = "densidades",xlab = "poblacion")
curve(dnorm(x, mean(cadata$Poblacion), sd(cadata$Poblacion)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Poblacion),main="Densidad de la poblaci�n",col="darkblue",ylab = "densidades",xlab = "poblacion")
x11()
boxplot(cadata$Poblacion,col="gray52",main="Gr�fico de cajas para la variable poblaci�n",ylab="Poblaci�n")


x11()
par(mfrow=c(1,2))
hist(cadata$Hogares, freq = F,col="gray52",main="Histograma de los hogares",ylab = "densidades",xlab = "hogares")
curve(dnorm(x, mean(cadata$Hogares), sd(cadata$Hogares)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Hogares),main="Densidad de los hogares",col="darkblue",ylab = "densidades",xlab = "hogares")
x11()
boxplot(cadata$Hogares,col="gray52",main="Gr�fico de cajas para la variable hogares",ylab="Cantidad de hogares")


#Relaciones entre variables explicativas:
X = cbind(Valor_Casa=cadata$Valor_mediano_de_la_casa,Ingreso=cadata$Ingreso_mediano,Edad=cadata$Edad_mediana_de_la_vivienda,Habitaciones=cadata$Total_de_habitaciones,
          Dormitorios=cadata$Total_de_dormitorios,Poblacion=cadata$Poblacion,Hogares=cadata$Hogares,Latitud=cadata$Latitud,Longitud=cadata$Longitud)
R = cor(X)
R
#Poblaci�n y hogares:
x11()
plot(cadata$Poblacion,cadata$Hogares,xlab = 'Poblaci�n',ylab = 'Hogares',main='Gr�fico de puntos entre la variable "Poblaci�n" y la variable "Hogares" ')
cor(cadata$Poblacion,cadata$Hogares) #correlaci�n de Pearson
cor(cadata$Poblacion,cadata$Hogares,method='spearman') #Correlaci�n de Spearman
#Total de habitaciones y Total de dormitorios:
x11()
plot(cadata$Total_de_habitaciones,cadata$Total_de_dormitorios,xlab = 'Total de habitaciones',ylab = 'Total de dormitorios',main='Gr�fico de puntos entre la variable "total de habitaciones" y la variable "total de dormitorios" ')
cor(cadata$Total_de_habitaciones,cadata$Total_de_dormitorios) #correlaci�n de Pearson
cor(cadata$Total_de_habitaciones,cadata$Total_de_dormitorios,method='spearman') #Correlaci�n de Spearman
#Latitud y longitud
x11()
plot(cadata$Latitud,cadata$Longitud,xlab = 'Latitud',ylab = 'Longitud',main='Gr�fico de puntos entre la variable "Latitud" y la variable "Longitud" ')
cor(cadata$Latitud,cadata$Longitud) #correlaci�n de Pearson
cor(cadata$Latitud,cadata$Longitud,method='spearman') #Correlaci�n de Spearman
#Hogares y dormitorios
x11()
plot(cadata$Total_de_dormitorios,cadata$Hogares,xlab = 'Dormitorios',ylab = 'Hogares',main='Gr�fico de puntos entre la variable "Total de dormitorios" y la variable "Hogares" ')
cor(cadata$Total_de_dormitorios,cadata$Hogares) #correlaci�n de Pearson
cor(cadata$Total_de_dormitorios,cadata$Hogares,method='spearman') #Correlaci�n de Spearman

#Estimaci�n y ajuste del modelo:
Regresion<- lm(cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano+cadata$Edad_mediana_de_la_vivienda+cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares)
summary(Regresion)
summary(Regresion)$sigma^2

#MAPAS
install.packages("ggmap")
library(ggmap)
map <- get_map(location = "california, san diego", zoom = 7, maptype = "terrain")
x11()
posicion=ggmap(map)+geom_point(data=cadata,aes(x=cadata$Longitud,y=cadata$Latitud))

#Selecci�n de variables:
install.packages("MASS")
library("MASS")
mod0 <- lm(cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano)
mod8 <- lm(cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano+cadata$Edad_mediana_de_la_vivienda+
        cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares)
#Hacia adelante:
mod.forward <- stepAIC(mod0, scope = list(upper = mod8), direction = "forward")
modeloresultante<-lm(cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano + cadata$Edad_mediana_de_la_vivienda+
  cadata$Total_de_dormitorios + cadata$Poblacion + cadata$Hogares +cadata$Total_de_habitaciones )
summary(modeloresultante)
summary(modeloresultante)$sigma^2
#Hacia atr�s:
mod.backward <- stepAIC(mod8, scope = list(lower = mod0),direction = "backward")
modeloresultante1<-lm(cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano + cadata$Edad_mediana_de_la_vivienda + 
                        cadata$Total_de_habitaciones + cadata$Poblacion + cadata$Hogares )
summary(modeloresultante1)
summary(modeloresultante1)$sigma^2
#Paso a paso:
mod.step <- stepAIC(mod0, scope = list(upper = mod8),direction = "both")
modeloresultante2<-lm(cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano+ 
                        cadata$Poblacion + cadata$Edad_mediana_de_la_vivienda + cadata$Total_de_habitaciones + 
                        cadata$Hogares)
summary(modeloresultante2)
summary(modeloresultante2)$sigma^2


