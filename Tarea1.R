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


#GRÁFICOS:
x1=c()
for(i in 1:500){
  x1=c(x1,cadata[i,1])
  
}
x11()
par(mfrow=c(1,2))
plot(density(x1),main="Densidad del valor medio de la casa")
hist(x1)

x11()
par(mfrow=c(1,2))
hist(cadata$Ingreso_medio)
plot(density(cadata$Ingreso_medio),main="Densidad del ingreso medio")

x11()
par(mfrow=c(1,2))
hist(cadata$Edad_media_de_la_vivienda)
plot(density(cadata$Edad_media_de_la_vivienda),main="Densidad de la edad media de la casa")

x11()
par(mfrow=c(1,2))
hist(cadata$Total_de_habitaciones)
plot(density(cadata$Total_de_habitaciones),main="Densidad del total de habitaciones")

x11()
par(mfrow=c(1,2))
hist(cadata$Total_de_dormitorios)
plot(density(cadata$Total_de_dormitorios),main="Densidad del total de dormitorios")

x11()
par(mfrow=c(1,2))
hist(cadata$Poblacion)
plot(density(cadata$Poblacion),main="Densidad de la población")

x11()
par(mfrow=c(1,2))
hist(cadata$Hogares)
plot(density(cadata$Hogares),main="Densidad de los hogares")

x11()
par(mfrow=c(1,2))
hist(cadata$Latitud)
plot(density(cadata$Latitud),main="Densidad de la latitud")

x11()
par(mfrow=c(1,2))
hist(cadata$Longitud)
plot(density(cadata$Longitud),main="Densidad de la longitud")

#Estimación y ajuste del modelo:
Regresion<- lm(cadata$Valor_medio_de_la_casa ~ cadata$Ingreso_medio+cadata$Edad_media_de_la_vivienda+
               cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares+
               cadata$Latitud+cadata$Longitud)
summary(Regresion)
