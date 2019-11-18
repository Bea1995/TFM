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

#Mapa de la comunidad de Madrid
mad_map <- get_map(getbb("Comunidad de Madrid"),maptype = "roadmap")

#Creacion de la rejilla de la comunidad de Madrid
comMadrid=getbb("Comunidad de Madrid")
minX=comMadrid[1,1];maxX=comMadrid[1,2]
minY=comMadrid[2,1];maxY=comMadrid[2,2]

longX=maxX-minX
longY=maxY-minY

rejX=seq(from=minX,to=maxX,by=longX/500)
rejY=seq(from=minY,to=maxY,by=longY/500)

matriz_Mapa=matrix(0,500,500)


#Metro Madrid
Metro <- read_delim("Metro_Estaciones.csv",";", escape_double = FALSE, col_types = cols(LINEAS = col_character()),trim_ws = TRUE)

#Paso de coordenadas decimales a latitud y longitud
coordX=Metro$X
coordY=Metro$Y
union=cbind(coordX,coordY)

utmcoor<-SpatialPoints(union, proj4string=CRS("+proj=utm +zone=30"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
Metro_df=as.data.frame(longlatcoor)
Metro_df=cbind(Metro$OBJECTID,Metro_df)

#ggmap(mad_map)+geom_point(data=Metro_df,aes(x=coordX,y=coordY,fill="red"), size = 1, shape = 21)+ theme(legend.position = "none")

rejilla_Metro=matriz_Mapa
for(i in 1:dim(Metro_df)[1]){
  fila=last(which(Metro_df[i,2]>rejX))
  columna=last(which(Metro_df[i,3]>rejY))
  rejilla_Metro[fila,columna]=rejilla_Metro[fila,columna]+1
}

#Metro ligero
MetroLigero <- read_delim("MetroLigero_Estaciones.csv",";", escape_double = FALSE, trim_ws = TRUE)

#Paso de coordenadas decimales a latitud y longitud
coordX=MetroLigero$X
coordY=MetroLigero$Y
union=cbind(coordX,coordY)

utmcoor<-SpatialPoints(union, proj4string=CRS("+proj=utm +zone=30"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
MetroLigero_df=as.data.frame(longlatcoor)
MetroLigero_df=cbind(MetroLigero$OBJECTID,MetroLigero_df)

#ggmap(mad_map)+geom_point(data=MetroLigero_df,aes(x=coordX,y=coordY,fill="red"), size = 1, shape = 21)+ theme(legend.position = "none")

rejilla_MetroLigero=matriz_Mapa
for(i in 1:dim(MetroLigero_df)[1]){
  fila=last(which(MetroLigero_df[i,2]>rejX))
  columna=last(which(MetroLigero_df[i,3]>rejY))
  rejilla_MetroLigero[fila,columna]=rejilla_MetroLigero[fila,columna]+1
}

#Cercanias Metro
Cercanias <- read_delim("Cercanias_Estaciones.csv",";", escape_double = FALSE, trim_ws = TRUE)

#Paso de coordenadas decimales a latitud y longitud
coordX=Cercanias$X
coordY=Cercanias$Y
union=cbind(coordX,coordY)

utmcoor<-SpatialPoints(union, proj4string=CRS("+proj=utm +zone=30"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
Cercanias_df=as.data.frame(longlatcoor)
Cercanias_df=cbind(Cercanias$OBJECTID,Cercanias_df, Cercanias$LINEAS)
Cercanias_df$`Cercanias$LINEAS`=as.character(Cercanias_df$`Cercanias$LINEAS`)

#ggmap(mad_map)+geom_point(data=Cercanias_df,aes(x=coordX,y=coordY,fill="red"), size = 1, shape = 21)+ theme(legend.position = "none")

rejilla_Cercanias=matriz_Mapa
for(i in 1:dim(Cercanias_df)[1]){
  fila=last(which(Cercanias_df[i,2]>rejX))
  columna=last(which(Cercanias_df[i,3]>rejY))
  suma=length(unlist(strsplit(Cercanias_df[i,4], split=",")))
  rejilla_Cercanias[fila,columna]=rejilla_Cercanias[fila,columna]+suma
}

#Buses urbanos
Urbanos <- read_delim("Urbanos_Estaciones.csv",";", escape_double = FALSE, trim_ws = TRUE)

#Paso de coordenadas decimales a latitud y longitud
coordX=Urbanos$X
coordY=Urbanos$Y
union=cbind(coordX,coordY)

utmcoor<-SpatialPoints(union, proj4string=CRS("+proj=utm +zone=30"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
Urbanos_df=as.data.frame(longlatcoor)
Urbanos_df=cbind(Urbanos$OBJECTID,Urbanos_df, Urbanos$LINEAS)
Urbanos_df$`Urbanos$LINEAS`=as.character(Urbanos_df$`Urbanos$LINEAS`)

#ggmap(mad_map)+geom_point(data=Urbanos_df,aes(x=coordX,y=coordY,fill="red"), size = 1, shape = 21)+ theme(legend.position = "none")

rejilla_Urbanos=matriz_Mapa
for(i in 1:dim(Urbanos_df)[1]){
  fila=last(which(Urbanos_df[i,2]>rejX))
  columna=last(which(Urbanos_df[i,3]>rejY))
  suma=length(unlist(strsplit(Urbanos_df[i,4], split=",")))
  rejilla_Urbanos[fila,columna]=rejilla_Urbanos[fila,columna]+suma
}

#Buses EMT
EMT <- read_delim("EMT_Madrid_Estaciones.csv",";", escape_double = FALSE, trim_ws = TRUE)
#Paso de coordenadas decimales a latitud y longitud
coordX=EMT$X
coordY=EMT$Y
union=cbind(coordX,coordY)

utmcoor<-SpatialPoints(union, proj4string=CRS("+proj=utm +zone=30"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
EMT_df=as.data.frame(longlatcoor)
EMT_df=cbind(EMT$OBJECTID,EMT_df, EMT$LINEAS)
EMT_df$`EMT$LINEAS`=as.character(EMT_df$`EMT$LINEAS`)

#mad_map <- get_map(getbb("Madrid"),maptype = "roadmap")
#ggmap(mad_map)+geom_point(data=EMT_df,aes(x=coordX,y=coordY,fill="red"), size = 1, shape = 21)+ theme(legend.position = "none")

rejilla_EMT=matriz_Mapa
for(i in 1:dim(EMT_df)[1]){
  fila=last(which(EMT_df[i,2]>rejX))
  columna=last(which(EMT_df[i,3]>rejY))
  suma=length(unlist(strsplit(EMT_df[i,4], split=",")))
  rejilla_EMT[fila,columna]=rejilla_EMT[fila,columna]+suma
}

#Buses Interurbanos
Interurbanos <- read_delim("Interurbanos_Estaciones.csv",";", escape_double = FALSE, trim_ws = TRUE)
coordX=Interurbanos$X
coordY=Interurbanos$Y
union=cbind(coordX,coordY)

utmcoor<-SpatialPoints(union, proj4string=CRS("+proj=utm +zone=30"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
Interurbanos_df=as.data.frame(longlatcoor)
Interurbanos_df=cbind(Interurbanos$OBJECTID,Interurbanos_df, Interurbanos$LINEAS)
Interurbanos_df$`Interurbanos$LINEAS`=as.character(Interurbanos_df$`Interurbanos$LINEAS`)

#ggmap(mad_map)+geom_point(data=Interurbanos_df,aes(x=coordX,y=coordY,fill="red"), size = 1, shape = 21)+ theme(legend.position = "none")

rejilla_Interurbanos=matriz_Mapa
for(i in 1:dim(Interurbanos_df)[1]){
  fila=last(which(Interurbanos_df[i,2]>rejX))
  columna=last(which(Interurbanos_df[i,3]>rejY))
  suma=length(unlist(strsplit(Interurbanos_df[i,4], split=",")))
  rejilla_Interurbanos[fila,columna]=rejilla_Interurbanos[fila,columna]+suma
}

#Union de todos los datos
tablaRejillas=matrix(0,250000,7)

ini=1
for(i in 1:500){
  for(j in 1:500){
    tablaRejillas[ini,1]=ini
    tablaRejillas[ini,2]=rejilla_EMT[i,j]
    tablaRejillas[ini,3]=rejilla_Urbanos[i,j]
    tablaRejillas[ini,4]=rejilla_Interurbanos[i,j]
    tablaRejillas[ini,5]=rejilla_Metro[i,j]
    tablaRejillas[ini,6]=rejilla_MetroLigero[i,j]
    tablaRejillas[ini,7]=rejilla_Cercanias[i,j]
    ini=ini+1
  }
}

tablaRejillas=as.data.frame(tablaRejillas)
names(tablaRejillas)=c("Celda", "EMT", "Urbanos", "Interurbanos", "Metro", "Metro Ligero", "Cercanias")

#Solo admitimos las celdas validas

celdasValidas <- read_csv("celdasValidas.csv")
indValidas=which(celdasValidas[,2]!=0)

tablaRejillas=tablaRejillas[indValidas,]

write.csv(tablaRejillas, file = "Tabla Transporte.csv",row.names=FALSE)
