library(readr)
cadata <- read_delim("~/GitHub/tarea_aplicada_1/cadata.txt", 
                     " ", escape_double = FALSE, trim_ws = TRUE)
View(cadata)
cadata1<-cadata[,-c(1,2,4,6,8,10,12,14,16)]
cadata<- cadata1
rm(cadata1)
sample(1:20640,1)
## El número aleatorio generado fue 15529 ##
# Eliminamos las filas que no necesitamos, nos quedamos con las filas desde la 15529 hasta la 16029 #
cadata<- cadata[-c(1:15528,16029:20640),]

##############################
#ANALISIS EXPLORATORIO
#ESTADISTICAS DESCRIPTIVAS
rbind(Valor_medio_de_la_casa=summary(cadata[,1]),Ingreso_medio=summary(cadata[,2]),Edad_media=summary(cadata[,3]),
      Total_de_habitaciones=summary(cadata[,4]),Total_de_dormitorios=summary(cadata[,5]),Poblacion=summary(cadata[,6]),
      Hogares=summary(cadata[,7]),Latitud=summary(cadata[,8]),Longitud=summary(cadata[,9]))
sd(cadata$Longitud)

#GRÁFICOS:
x11()
par(mfrow=c(1,2))
hist(cadata$Valor_medio_de_la_casa,freq = F,col="gray52",main="Histograma del valor medio de la casa",ylab = "densidades",xlab = "valor medio de la casa")
curve(dnorm(x, mean(cadata$Valor_medio_de_la_casa), sd(cadata$Valor_medio_de_la_casa)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Valor_medio_de_la_casa),main="Densidad del valor medio de la casa",col="darkblue",ylab = "densidades",xlab = "valor medio de la casa")

x11()
par(mfrow=c(1,2))
hist(cadata$Ingreso_medio,freq = F,col="gray52",main="Histograma del ingreso medio",ylab = "densidades",xlab = "ingreso medio")
curve(dnorm(x, mean(cadata$Ingreso_medio), sd(cadata$Ingreso_medio)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Ingreso_medio),main="Densidad del ingreso medio",col="darkblue",ylab = "densidades",xlab = "ingreso medio")


x11()
par(mfrow=c(1,2))
hist(cadata$Edad_media_de_la_vivienda,freq = F,col="gray52",main="Histograma de la edad media de la casa",ylab = "densidades",xlab = "edad media de la casa")
curve(dnorm(x, mean(cadata$Edad_media_de_la_vivienda), sd(cadata$Edad_media_de_la_vivienda)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Edad_media_de_la_vivienda),main="Densidad de la edad media de la casa",col="darkblue",ylab = "densidades",xlab = "edad media de la casa")

x11()
par(mfrow=c(1,2))
hist(cadata$Total_de_habitaciones, freq = F,col="gray52",main="Histograma del total de habitaciones",ylab = "densidades",xlab = "total de habitaciones")
curve(dnorm(x, mean(cadata$Total_de_habitaciones), sd(cadata$Total_de_habitaciones)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Total_de_habitaciones),main="Densidad del total de habitaciones",col="darkblue",ylab = "densidades",xlab = "total de habitaciones")

x11()
par(mfrow=c(1,2))
hist(cadata$Total_de_dormitorios, freq = F,col="gray52",main="Histograma del total de dormitorios",ylab = "densidades",xlab = "total de dormitorios")
curve(dnorm(x, mean(cadata$Total_de_dormitorios), sd(cadata$Total_de_dormitorios)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Total_de_dormitorios),main="Densidad del total de dormitorios",col="darkblue",ylab = "densidades",xlab = "total de dormitorios")

x11()
par(mfrow=c(1,2))
hist(cadata$Poblacion,freq = F,col="gray52",main="Histograma de la población",ylab = "densidades",xlab = "poblacion")
curve(dnorm(x, mean(cadata$Poblacion), sd(cadata$Poblacion)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Poblacion),main="Densidad de la población",col="darkblue",ylab = "densidades",xlab = "poblacion")

x11()
par(mfrow=c(1,2))
hist(cadata$Hogares, freq = F,col="gray52",main="Histograma de los hogares",ylab = "densidades",xlab = "hogares")
curve(dnorm(x, mean(cadata$Hogares), sd(cadata$Hogares)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Hogares),main="Densidad de los hogares",col="darkblue",ylab = "densidades",xlab = "hogares")

x11()
par(mfrow=c(1,2))
hist(cadata$Latitud,freq = F,col="gray52",main="Histograma de la latitud",ylab = "densidades",xlab = "latitud")
curve(dnorm(x, mean(cadata$Latitud), sd(cadata$Latitud)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Latitud),main="Densidad de la latitud",col="darkblue",ylab = "densidades",xlab = "latitud")

x11()
par(mfrow=c(1,2))
hist(cadata$Longitud, freq = F,col="gray52",main="Histograma de la longitud",ylab = "densidades",xlab = "longitud")
curve(dnorm(x, mean(cadata$Longitud), sd(cadata$Longitud)), col = 2, lty = 2, lwd = 2, add=T)
plot(density(cadata$Longitud),main="Densidad de la longitud",col="darkblue",ylab = "densidades",xlab = "longitud")

#Relaciones entre variables explicativas:
#Población y hogares:
x11()
plot(cadata$Poblacion,cadata$Hogares,xlab = 'Población',ylab = 'Hogares',main='Gráfico de puntos entre la variable "Población" y la variable "Hogares" ')
cor(cadata$Poblacion,cadata$Hogares) #correlación de Pearson
cor(cadata$Poblacion,cadata$Hogares,method='spearman') #Correlación de Spearman
#Total de habitaciones y Total de dormitorios:
x11()
plot(cadata$Total_de_habitaciones,cadata$Total_de_dormitorios,xlab = 'Total de habitaciones',ylab = 'Total de dormitorios',main='Gráfico de puntos entre la variable "total de habitaciones" y la variable "total de dormitorios" ')
cor(cadata$Total_de_habitaciones,cadata$Total_de_dormitorios) #correlación de Pearson
cor(cadata$Total_de_habitaciones,cadata$Total_de_dormitorios,method='spearman') #Correlación de Spearman


#Estimación y ajuste del modelo:
Regresion<- lm(cadata$Valor_medio_de_la_casa ~ cadata$Ingreso_medio+cadata$Edad_media_de_la_vivienda+
               cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares+
               cadata$Latitud+cadata$Longitud)
summary(Regresion)
