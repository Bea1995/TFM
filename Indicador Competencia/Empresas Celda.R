#Librerias excel
library(readxl)
library(writexl)

#Paso de coordenadas UTM a GPS
library(rgdal)

#Dibujo de los locales en el mapa
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

comMadrid=getbb("Comunidad de Madrid")
minX=comMadrid[1,1];maxX=comMadrid[1,2]
minY=comMadrid[2,1];maxY=comMadrid[2,2]

longX=maxX-minX
longY=maxY-minY

rejX=seq(from=minX,to=maxX,by=longX/500)
rejY=seq(from=minY,to=maxY,by=longY/500)

#Lectura de todas las empresas de la Comunidad de Madrid
Empresas<- as.data.frame(read_excel("Locales Comunidad.xlsx"))
Empresas<-Empresas[!is.na(Empresas$`Coordenada X`),]

#Locales sin actividad
indSA=which(Empresas$Actividad=="ZZZZ")
NoEmpresas<-Empresas[indSA,]

#Empresas
Empresas<-Empresas[-indSA,]

#Paso de coordenadas UTM XY a geográficas latitud y longitud
coordX=as.double(Empresas$`Coordenada X`)
coordY=as.double(Empresas$`Coordenada Y`)

union=cbind(coordX,coordY)

utmcoor<-SpatialPoints(union, proj4string=CRS("+proj=utm +zone=30"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
Emp_df=as.data.frame(longlatcoor)

#Variable auxiliar: actividad de las empresas
Emp_Categ=Empresas$Actividad

ConvCelda <- read_excel("ConversionCeldas.xlsx")
TiposEmp <- read_excel("TiposEmpresa.xlsx")

#Ejemplos
which(TiposEmp==Emp_Categ[1])
TiposEmp[578,]

ConvCelda[1,]
which(ConvCelda[,2]==379 & ConvCelda[,3]==1)
ConvCelda[189001,]

#Calculo de la matriz
matriz_Mapa=matrix(0,250000,578)
total=578

for(i in 1:dim(Emp_df)[1]){
  fila=which(ConvCelda[,2]==last(which(Emp_df[i,1]>rejX)) & ConvCelda[,3]==last(which(Emp_df[i,2]>rejY)))
  columna=which(TiposEmp==Emp_Categ[i])

  matriz_Mapa[fila,columna]=matriz_Mapa[fila,columna]+1
  matriz_Mapa[fila,total]=matriz_Mapa[fila,total]+1
  
  if(i%%10000==0) print(i)
}

#Celdas validas
validas=which(matriz_Mapa[,578]>0)

CeldasValidas=cbind(ConvCelda[,1],matriz_Mapa[578])
CeldasValidas[validas,2]=1

names(CeldasValidas)=c("Celda","Valida")
write.csv(CeldasValidas,file="celdasValidas.csv", row.names=FALSE)

#Matriz con los datos de las celdas validas
matriz_Mapa2=cbind(ConvCelda[,1],matriz_Mapa)
matriz_Mapa2=matriz_Mapa2[validas,]

nombres=TiposEmp$Tipos
names(matriz_Mapa2)=c("Celda", nombres)
write.csv(matriz_Mapa2,file="EmpXCeldasValidas.csv",row.names=FALSE)

