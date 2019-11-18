#Librerias excel
library(readxl)
library(writexl)

#Librerias leer
library(readr)

#Paso de coordenadas UTM a GPS
library(rgdal)

#Dibujo de los locales en el mapa
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

#Comparacion entre poligonos
library(polyclip)

#Codigos postales
library(geojsonio)

poligCeldas=read_csv("poligonosCeldasValidas.csv")
cod_Post = geojson_read("MADRID.geojson",what="sp")
plot(cod_Post)


#Obtener las coordenadas de un código postal
pruebaCP=as.data.frame(cod_Post@polygons[[which(file_js$COD_POSTAL==28020)]]@Polygons[[1]]@coords)
ciudad=getbb("Alcorcon")
ciudad[1,1]=min(pruebaCP$V1) #minX
ciudad[1,2]=max(pruebaCP$V1) #maxX
ciudad[2,1]=min(pruebaCP$V2) #minY
ciudad[2,2]=max(pruebaCP$V2) #maxY

mad_map <- get_map(ciudad,maptype = "roadmap")
ggmap(mad_map)+geom_polygon(data=pruebaCP,aes(x=V1,y=V2),colour="blue",fill=NA)+geom_vline(xintercept=rejX,color="red")+geom_hline(yintercept=rejY,color="red")


#Calculo de las comparaciones entre celdas y codigos postales
A=list(list())
B=list(list())

matrizCP=matrix(0,dim(poligCeldas)[1],length(cod_Post))

#Calculo de los polígonos de las celdas
for(i in 1:dim(poligCeldas)[1]){
  x=c(poligCeldas[i,4],poligCeldas[i,4],poligCeldas[i,6],poligCeldas[i,6])
  y=c(poligCeldas[i,5],poligCeldas[i,7],poligCeldas[i,7],poligCeldas[i,5])
  A[[i]]=list(x=x,y=y)
}

#Calculo de los polígonos de los códigos postales
for(i in 1:length(cod_Post)){
  polAux=as.data.frame(cod_Post@polygons[[i]]@Polygons[[1]]@coords)
  B[[i]]=list(x=polAux$V1,y=polAux$V2)
}

#Comparación entre ambos tipos de polígonos uno a uno
for(i in 1:dim(poligCeldas)[1]){
  for(j in 1:length(cod_Post)){
    
    result=polyclip(A[[i]],B[[j]],op="intersection")
    
    if(length(result)!=0){
      matrizCP[i,j]=1
    }
    
  }
  if(i%%500==0) print(i)
}

#Arreglos: puerto de navacerrada
which(rowSums(matrizCP)==0)
matrizCP[1242,356]=1
matrizCP[1297,356]=1

plot(cod_Post)

aux=as.data.frame(cod_Post@polygons[[31]]@Polygons[[1]]@coords)
aux2=as.data.frame(cod_Post@polygons[[356]]@Polygons[[1]]@coords)

polygon(aux,col="red")
polygon(aux2,col="blue")

plot(A[[1297]],xlim=c(-4.01,-3.99),ylim=c(40.76,40.82))
polygon(A[[1297]],col="blue")
polygon(A[[1242]],col="red")
polygon(aux)
polygon(aux1)

#El numero maximo de codigo postal por celda es 4
which(rowSums(matrizCP)>4)

#Crear archivo con los codigos postales por celda
ConversionCPMapa <- read_excel("ConversionCPMapa.xlsx")

CPxCelda=matrix(0,dim(poligCeldas)[1],4)

for(i in 1:dim(poligCeldas)[1]){
  filas=which(matrizCP[i,]>0)
  for(j in 1:length(filas)){
    indFila=which(ConversionCPMapa[,2]==filas[j])
    num=as.numeric(ConversionCPMapa[indFila,1])
    CPxCelda[i,j]=num
  }
  if(i%%500==0) print(i)
}

CPxCelda=cbind(poligCeldas[,1],CPxCelda)
colnames(CPxCelda)=c("Celda", "CP1", "CP2", "CP3", "CP4")

write.csv(CPxCelda,file="ConversionCeldaCP.csv",row.names=FALSE)
