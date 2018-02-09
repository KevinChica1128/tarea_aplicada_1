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

