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

#Codigos postales
library(geojsonio)

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

#Celdas en la comunidad de madrid
mad_map <- get_map(getbb("Madrid"),maptype = "roadmap")
ggmap(mad_map)+geom_vline(xintercept=rejX,color="red")+geom_hline(yintercept=rejY,color="red")#+geom_point(data=Emp_df[indCodPostal,],aes(x=coordX,y=coordY,fill=Emp_Categ[indCodPostal]), size = Emp_Size[indCodPostal], shape = 21)

file_js = geojson_read("MADRID.geojson",what="sp")

#Buscar un código postal en la comunidad
pruebaCP=as.data.frame(file_js@polygons[[which(file_js$COD_POSTAL==28020)]]@Polygons[[1]]@coords)
mad_map <- get_map(getbb("Comunidad de Madrid"),maptype = "roadmap")
ggmap(mad_map)+geom_polygon(data=pruebaCP,aes(x=V1,y=V2),colour="blue",fill=NA)

#Buscar dos codigos postales a la vez
which(file_js$COD_POSTAL==28020)
mad_map <- get_map(getbb("Alcorcon"),maptype = "roadmap")
pruebaCP=as.data.frame(file_js@polygons[[238]]@Polygons[[1]]@coords)
pruebaCP1=as.data.frame(file_js@polygons[[365]]@Polygons[[1]]@coords)
ggmap(mad_map)+geom_polygon(data=pruebaCP,aes(x=V1,y=V2),colour="blue",fill=NA)+geom_polygon(data=pruebaCP1,aes(x=V1,y=V2),colour="blue",fill=NA)

#Buscar 6 códigos postales a la vez
which(file_js$COD_POSTAL==28191)
mad_map <- get_map(getbb("Alcorcon"),maptype = "roadmap")
pruebaCP=as.data.frame(file_js@polygons[[133]]@Polygons[[1]]@coords)
pruebaCP1=as.data.frame(file_js@polygons[[360]]@Polygons[[1]]@coords)
pruebaCP2=as.data.frame(file_js@polygons[[361]]@Polygons[[1]]@coords)
pruebaCP3=as.data.frame(file_js@polygons[[362]]@Polygons[[1]]@coords)
pruebaCP4=as.data.frame(file_js@polygons[[363]]@Polygons[[1]]@coords)
pruebaCP5=as.data.frame(file_js@polygons[[364]]@Polygons[[1]]@coords)
ggmap(mad_map)+geom_polygon(data=pruebaCP,aes(x=V1,y=V2),colour="blue",fill=NA)+geom_polygon(data=pruebaCP1,aes(x=V1,y=V2),colour="blue",fill=NA)+geom_polygon(data=pruebaCP2,aes(x=V1,y=V2),colour="blue",fill=NA)+geom_polygon(data=pruebaCP3,aes(x=V1,y=V2),colour="blue",fill=NA)+geom_polygon(data=pruebaCP4,aes(x=V1,y=V2),colour="blue",fill=NA)+geom_polygon(data=pruebaCP5,aes(x=V1,y=V2),colour="blue",fill=NA)